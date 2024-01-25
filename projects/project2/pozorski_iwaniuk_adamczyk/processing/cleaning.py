import os
import re
import threading
import queue
import json
import warnings
import logging
import spacy
import string
import gender_guesser.detector as gender

from langdetect import detect
from typing import List, Any, Tuple

from setup import Config
from helpers import GenderPredictorForPolishNames, Counter, encode, BannedWords


class CleaningExecutor:
    LOCK = threading.Lock()
    Q = queue.Queue()

    USERS = Counter(LOCK)
    TITLES = Counter(LOCK)
    GENDER = gender.Detector(case_sensitive=False)
    GENDER_POLISH = GenderPredictorForPolishNames()
    LEMMATIZER_EN = spacy.load("en_core_web_sm")
    LEMMATIZER_PL = spacy.load("pl_core_news_sm")
    TRANSLATION_TABLE = str.maketrans("", "", string.punctuation)

    @staticmethod
    def get_file_path(path: str) -> str:
        if not os.path.isabs(path):
            return os.path.join(Config.get("input_dir_path"), path)
        return path

    @staticmethod
    def get_parent_directory(path: str) -> str:
        return os.path.basename(os.path.dirname(CleaningExecutor.get_file_path(path)))

    @staticmethod
    def get_folders(path: str) -> List[str]:
        path = CleaningExecutor.get_file_path(path)
        return [p for p in os.listdir(path) if os.path.isdir(os.path.join(path, p))]

    @staticmethod
    def get_messages_folders(dirs: List[str] = None) -> List[str]:
        if dirs is None:
            dirs = CleaningExecutor.get_folders(Config.get("input_dir_path"))

        messages_dir = []
        for d in dirs:
            for subdir in CleaningExecutor.get_folders(d):
                sub_path = os.path.join(d, subdir)
                if "messages" in sub_path.lower():
                    messages_dir.append(sub_path)

        return messages_dir

    @staticmethod
    def get_json_message_files(path: str, prefix: str = "message_") -> List[str]:
        return [
            p
            for p in os.listdir(CleaningExecutor.get_file_path(path))
            if os.path.splitext(p)[-1] == ".json"
            # and not os.path.basename(p).startswith(".")
            and os.path.basename(p).startswith(prefix)
        ]

    @staticmethod
    def get_messages_files(
        dirs: List[str] = None, dir: str = None, prefix: str = "message_"
    ) -> None:
        """Saves json message files absolute paths to Q"""
        assert not (
            dirs is not None and dir is not None
        ), "Provide up to one of dir or dirs"

        if dirs is None and dir is None:
            dirs = CleaningExecutor.get_messages_folders(dir)
        elif dir is not None:
            dirs = [dir]

        def walk_(path: str) -> None:
            path = CleaningExecutor.get_file_path(path)

            for f in CleaningExecutor.get_json_message_files(path, prefix):
                CleaningExecutor.Q.put(os.path.join(path, f))
            for dir in CleaningExecutor.get_folders(path):
                walk_(os.path.join(path, dir))

        for dir in dirs:
            walk_(dir)

    @staticmethod
    def read_json(path: str) -> Any:
        path = CleaningExecutor.get_file_path(path)
        with open(path, "r", encoding="utf-8") as file:
            return json.load(file)

    @staticmethod
    def save_json(data: Any, path: str) -> None:
        if not os.path.isabs(path):
            path = os.path.join(Config.get("output_dir_path"), path)
        os.makedirs(os.path.dirname(path), exist_ok=True)

        if os.path.exists(path):
            logging.info(f"Extending {path}")
            with CleaningExecutor.LOCK:
                dt = CleaningExecutor.read_json(path)
                data.extend(dt)

        with CleaningExecutor.LOCK:
            with open(path, "w") as file:
                return json.dump(data, file, ensure_ascii=False)

    @staticmethod
    def get_gender(name: str) -> str:
        with CleaningExecutor.LOCK:
            gender = CleaningExecutor.GENDER_POLISH.predict_gender(name)
            if gender == "unknown":
                gender = CleaningExecutor.GENDER.get_gender(name)
                if gender == "mostly_male":
                    return "male"
                elif gender == "mostly_female":
                    return "female"

            return gender

    @staticmethod
    def get_participant_key(name: str, map: dict = None, warn: bool = False) -> Any:
        if map is not None:
            if name in map.keys():
                return map[name], map

        if warn:
            warnings.warn(f"New user found: {name}.")

        key = CleaningExecutor.USERS.get(name)
        if key is None:
            key = CleaningExecutor.USERS.get_id()
            gender = CleaningExecutor.get_gender(name.split(" ")[0])
            CleaningExecutor.USERS.set(name, (key, gender))
        else:
            key = key[0]

        if map is not None:
            map[name] = key
            return key, map
        return key

    @staticmethod
    def encode_participants(participants: List[dict]) -> dict:
        participants_map = {}
        for participant in participants:
            name = encode(participant.get("name", "unknown"))[0]
            if name == "unknown":
                warnings.warn(f"Unknown participant.")

            _, participants_map = CleaningExecutor.get_participant_key(
                name, participants_map
            )
        return participants_map

    @staticmethod
    def detect_language(message):
        try:
            language = detect(message)
            return language
        except Exception as e:
            return Config.get("default_language")

    @staticmethod
    def clean_content(message: str) -> List[str]:
        message = message.lower()
        message = re.sub(
            r"https?://\S+|www\.\S+",
            "",
            message,
        )

        message = message.translate(CleaningExecutor.TRANSLATION_TABLE)

        language = CleaningExecutor.detect_language(message)
        if language == "pl":
            with CleaningExecutor.LOCK:
                doc = CleaningExecutor.LEMMATIZER_PL(message)
        else:
            with CleaningExecutor.LOCK:
                doc = CleaningExecutor.LEMMATIZER_EN(message)
        tokens = [token.lemma_ for token in doc if not token.is_stop]

        return [token.strip() for token in tokens if token.strip() != ""]

    @staticmethod
    def encode_messages(
        messages: List[dict], participants_map: dict
    ) -> List[Tuple[int, str]]:
        res = [None] * len(messages)
        for i, message in enumerate(messages):
            key, participants_map = CleaningExecutor.get_participant_key(
                encode(message.get("sender_name", ""))[0], participants_map, warn=True
            )

            content = message.get("content", "")
            res[i] = (
                key,
                CleaningExecutor.clean_content(encode(content)[0])
                if not BannedWords.is_banned(content)
                else "MetaCommand",
                message.get("timestamp_ms", None),
            )
        return res

    @staticmethod
    def encode_json_file(path: str) -> None:
        path = CleaningExecutor.get_file_path(path)
        data = CleaningExecutor.read_json(path)
        assert isinstance(data, dict), f"{path}: read data is {type(data)}"

        title = encode(data.pop("title", ""))[0]
        if title == "":
            warnings.warn(f"File {path} has no title")
            return
        unique_title = CleaningExecutor.get_parent_directory(path) + "___" + title
        key = CleaningExecutor.TITLES.get(unique_title)
        if key is None:
            key = CleaningExecutor.TITLES.get_id()
            CleaningExecutor.TITLES.set(unique_title, key)

        participants = data.pop("participants", {})
        if participants == {}:
            warnings.warn(f"File {path} has no participants")
            return
        participants_map = CleaningExecutor.encode_participants(participants)

        messages = data.pop("messages", {})
        if messages == {}:
            warnings.warn(f"File {path} has no messages")
            return
        messages = CleaningExecutor.encode_messages(messages, participants_map)

        new_path = f"conversation_{key}.json"

        CleaningExecutor.save_json(messages, new_path)
        logging.info(f"Encoded {unique_title}")

    @staticmethod
    def reverse(dt: dict, has_list: bool = False) -> dict:
        if has_list:
            return {v[0]: [k, *v[1:]] for k, v in dt.items()}
        return {v: k for k, v in dt.items()}

    @staticmethod
    def get_processed_messages(messages: list, path: str) -> list:
        conversation_id = os.path.splitext(os.path.basename(path))[0].lstrip(
            "conversation_"
        )
        return [[conversation_id, *message] for message in messages]

    @staticmethod
    def join_message_files():
        CleaningExecutor.get_messages_files(
            dir=Config.get("output_dir_path"), prefix="conversation_"
        )

        messages = []
        while True:
            try:
                path = CleaningExecutor.Q.get_nowait()
                dt = CleaningExecutor.read_json(path)
                messages.extend(CleaningExecutor.get_processed_messages(dt, path))
            except queue.Empty:
                break
        return messages

    @staticmethod
    def clean_files() -> None:
        while True:
            try:
                path = CleaningExecutor.Q.get_nowait()
                CleaningExecutor.encode_json_file(path)
            except queue.Empty:
                break


def clean():
    CleaningExecutor.get_messages_files()

    threads = []
    for i in range(Config.get("n_threads")):
        thread = threading.Thread(
            target=CleaningExecutor.clean_files, name=f"Thread-{i+1}"
        )
        threads.append(thread)
        thread.start()
    for thread in threads:
        thread.join()

    CleaningExecutor.save_json(
        CleaningExecutor.USERS.dt, Config.get("prefix") + "_" + "users.json"
    )
    CleaningExecutor.save_json(
        CleaningExecutor.TITLES.dt, Config.get("prefix") + "_" + "titles.json"
    )

    users = CleaningExecutor.reverse(CleaningExecutor.USERS.dt, has_list=True)
    conversations = CleaningExecutor.reverse(CleaningExecutor.TITLES.dt)

    CleaningExecutor.save_json(
        users, Config.get("prefix") + "_" + "users_reversed.json"
    )
    CleaningExecutor.save_json(
        conversations, Config.get("prefix") + "_" + "titles_reversed.json"
    )

    messages = CleaningExecutor.join_message_files()
    CleaningExecutor.save_json(
        messages, Config.get("prefix") + "_" + "conversations.json"
    )

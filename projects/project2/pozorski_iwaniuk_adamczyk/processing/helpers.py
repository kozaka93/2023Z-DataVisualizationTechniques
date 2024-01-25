import os
import re
import json
import logging
import warnings
import pandas as pd

from typing import Any, List, Tuple
from itertools import count
from datetime import datetime

from setup import Config


def encode(*args: List[Any]) -> List[Any]:
    args = list(args)
    for i in range(len(args)):
        try:
            if isinstance(args[i], str):
                args[i] = args[i].encode("latin-1").decode("utf-8")
        except Exception as e:
            warnings.warn(f"Could not encode: {args[i]} - {e}")

    return args


# returns (name, gender)
def encode_user(
    user_id: str,
    users: dict,
    faked_users: dict = None,
    fake_name: bool = True,
) -> Tuple[str, str]:
    if fake_name:
        assert faked_users is not None, f"Faking name requires fake_users dict."

    name = users[user_id] if user_id in users.keys() else ("unknown", "unknown")

    if name[0] != "unknown" and fake_name:
        name[0] = faked_users[user_id] if user_id in faked_users.keys() else "unknown"
    return name


# returns (is_group, number_of_participants)
encode_group = (
    lambda conversation_id, groups: (
        len(groups[conversation_id]) > 2,
        len(groups[conversation_id]),
    )
    if conversation_id in groups.keys()
    else ("unknown", "unknown")
)


class Query:
    def __init__(
        self,
        id: str,
        result_extension: str = ".csv",
        timestamp_group_format: str = "%Y-%m-%d %H:00",
    ) -> None:
        self.id = id
        self.path = f"{Config.get('prefix')}_query_{id}{result_extension}"
        self.timestamp_group_format = timestamp_group_format

    def __call__(self, data: List[tuple], **kwargs):
        logging.info(f"Query_{self.id}:Execution started.")
        assert len(data) > 0, "Empty data list."
        result = self.execute(data, **kwargs)
        Query.save(result, self.path)
        logging.info(
            f"Query_{self.id}:Execution finished, results saved to {self.path}."
        )

    def execute(self, data: List[tuple], **kwargs) -> Any:
        return data

    def get_date(self, timestamp: int):
        return datetime.fromtimestamp(timestamp / 1000).strftime(
            self.timestamp_group_format
        )

    def get_from_kw(self, kw: dict, name: str, assert_val: Any = None) -> Any:
        val = kw.pop(name, None)
        assert val is not assert_val, f"Query: {self.id} requires {name} to work."
        return val

    @staticmethod
    def save(result: Any, path: str) -> None:
        if not os.path.isabs(path):
            path = os.path.join(Config.get("output_dir_path"), path)
        os.makedirs(os.path.dirname(path), exist_ok=True)

        if os.path.exists(path):
            logging.info(f"Overwritting {path}")

        _, extension = os.path.splitext(path)

        if extension == ".csv":
            if not isinstance(result, pd.DataFrame):
                result = pd.DataFrame(result)
            result.to_csv(path, index=None)
        else:
            with open(path, "w") as file:
                return json.dump(result, file, ensure_ascii=False)

    @staticmethod
    def get_groups(data: List[tuple]) -> dict:
        conversation_users = {}
        for conversation_id, user_id, _, timestamp in data:
            if conversation_id in conversation_users.keys():
                conversation_users[conversation_id].add(user_id)
            else:
                conversation_users[conversation_id] = set([user_id])
        # return set(key for key, value in conversation_users.items() if len(value) > 2)
        return conversation_users

    @staticmethod
    def reverse_df(
        df: pd.DataFrame, by: str = "date", sort: bool = True
    ) -> pd.DataFrame:
        columns = df[by]
        df.drop(by, axis=1, inplace=True)
        df = df.T
        df = df.reset_index()
        df.columns = [by] + list(columns)

        if sort:
            return df.sort_values(by=by)
        return df


class GenderPredictorForPolishNames:
    def __init__(self) -> None:
        self.names = pd.read_excel(
            os.path.join(os.path.dirname(__file__), "resources", "imiona_polskie.xlsx")
        )
        self.names = {
            name.lower(): gender
            for name, gender in zip(self.names.imie, self.names.plec)
        }

    def predict_gender(self, name: str) -> str:
        gender = self.names.get(name.lower(), "unknown")
        if gender == "unknown" and name.endswith("a"):
            return "female"
        return gender


class Counter:
    COUNTER = count(start=1)

    def __init__(self, lock) -> None:
        self.counter = count(start=1)
        self.dt = {}
        self.lock = lock

    def set(self, key: str, value: Any) -> None:
        with self.lock:
            self.dt[key] = value

    def get(self, key: str) -> Any:
        return self.dt.get(key, None)

    def get_id(self) -> int:
        with self.lock:
            return f"{Config.get('prefix')}_{next(self.counter)}"


class BannedWords:
    BANNED_PHRASES = [
        "ustawiono nick u\u00c5\u00bcytownika",
        "ustawi\u00c5\u0082a nick",
        "ustawi\u00c5\u0082 nick",
        "ustawi\u00c5\u0082 Tw\u00c3\u00b3j nick",
        "ustawi\u00c5\u0082a Tw\u00c3\u00b3j nick",
        "ustawi\u00c5\u0082e\u00c5\u009b(a\u00c5\u009b) szybk\u00c4\u0085 reakcj\u00c4\u0099",
        "ustawi\u00c5\u0082(a) szybk\u00c4\u0085 reakcj\u00c4\u0099",
        "ustawi\u00c5\u0082 szybk\u00c4\u0085 reakcj\u00c4\u0099",
        "ustawi\u00c5\u0082a szybk\u00c4\u0085 reakcj\u00c4\u0099",
        "zmieni\u00c5\u0082(a) zdj\u00c4\u0099cie grupy",
        "zmieni\u00c5\u0082 zdj\u00c4\u0099cie grupy",
        "zmieni\u00c5\u0082a zdj\u00c4\u0099cie grupy",
        "zmieni\u00c5\u0082e\u00c5\u009b zdj\u00c4\u0099cie grupy",
        "zmieni\u00c5\u0082a\u00c5\u009b zdj\u00c4\u0099cie grupy",
        "zmieni\u00c5\u0082e\u00c5\u009b(a\u00c5\u009b) zdj\u00c4\u0099cie grupy",
        "doda\u00c5\u0082 Ci\u00c4\u0099 do grupy",
        "doda\u00c5\u0082(a) Ci\u00c4\u0099 do grupy",
        "doda\u00c5\u0082a Ci\u00c4\u0099 do grupy",
        r"doda\u00c5\u0082 (.+?) do grupy",
        r"doda\u00c5\u0082a (.+?) do grupy",
        r"doda\u00c5\u0082e\u00c5\u009b(a\u00c5\u009b) (.+?) do grupy",
        r"doda\u00c5\u0082e\u00c5\u009b (.+?) do grupy",
        r"doda\u00c5\u0082a\u00c5\u009b (.+?) do grupy",
        "opusci\u00c5\u0082e\u00c5\u009b(a\u00c5\u009b) grup\u00c4\u0099",
        "opu\u00c5\u009bci\u00c5\u0082(a) grup\u00c4\u0099",
        "opu\u00c5\u009bci\u00c5\u0082 grup\u00c4\u0099",
        "opu\u00c5\u009bci\u00c5\u0082a grup\u00c4\u0099",
        "usun\u00c4\u0085\u00c5\u0082(\u00c4\u0099\u00c5\u0082a) Ci\u00c4\u0099 z grupy",
        "usun\u00c4\u0085\u00c5\u0082 Ci\u00c4\u0099 z grupy",
        "usun\u00c4\u0099\u00c5\u0082a Ci\u00c4\u0099",
        "zosta\u00c5\u0082(a) usuni\u00c4\u0099ty(a)",
        "usun\u00c4\u0085\u00c5\u0082 u\u00c5\u00bcytkownika",
        "usun\u00c4\u0099\u00c5\u0082a u\u00c5\u00bcytkonika",
        r"usun\u00c4\u0085\u00c5\u0082e\u00c5\u009b(a\u00c5\u009b) (.+?) z grupy",
        r"usun\u00c4\u0085\u00c5\u0082e\u00c5\u009b (.+?) z grupy",
        r"usun\u00c4\u0099\u00c5\u0082a\u00c5\u009b (.+?) z grupy",
        r"usun\u00c4\u0085\u00c5\u0082 (.+?) z grupy",
        r"usun\u00c4\u0099\u00c5\u0082a (.+?) z grupy",
        "zmieni\u00c5\u0082 motyw",
        "zmieni\u00c5\u0082a motyw",
        "zmieni\u00c5\u0082e\u00c5\u009b(a\u00c5\u009b) motyw",
        "zmieni\u00c5\u0082e\u00c5\u009b motyw",
        "zmieni\u00c5\u0082a\u00c5\u009b motyw",
        "dzwoni\u00c5\u0082 do Ciebie",
        "Zadzwoni\u00c5\u0082e\u00c5\u009b do",
        "Zadzwoni\u00c5\u0082a\u00c5\u009b do",
        "masz nieodebrane po\u00c5\u0082\u00c4\u0085czenie od",
        "ta ankieta nie jest ju\u00c5\u00bc dost\u00c4\u0099pna",
        "do\u00c5\u0082\u00c4\u0085czy\u00c5\u0082 do rozmowy",
        "do\u00c5\u0082\u00c4\u0085czy\u00c5\u0082a do rozmowy",
        "do\u00c5\u0082\u00c4\u0085czy\u00c5\u0082e\u00c5\u009b do rozmowy",
        "do\u00c5\u0082\u00c4\u0085czy\u00c5\u0082a\u00c5\u009b do rozmowy",
        "do\u00c5\u0082\u00c4\u0085czy\u00c5\u0082e\u00c5\u009b(a\u00c5\u009b) do rozmowy",
        "rozpocz\u00c4\u0085\u00c5\u0082 rozmow\u00c4\u0099",
        "rozpocz\u00c4\u0099\u00c5\u0082a rozmow\u00c4\u0099",
        "rozpocz\u00c4\u0085\u00c5\u0082e\u00c5\u009b rozmow\u00c4\u0099",
        "rozpocz\u00c4\u0085\u00c5\u0082e\u00c5\u009b rozmow\u00c4\u0099",
    ]

    @staticmethod
    def is_banned(message: str) -> bool:
        for to_delete in BannedWords.BANNED_PHRASES:
            if re.search(re.escape(to_delete).lower(), message.lower()):
                return True
        return False

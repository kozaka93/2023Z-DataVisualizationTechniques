import os
import logging
import json

from faker import Faker
from typing import List, Any

from setup import Config
from queries import (
    CountMessagesQuery,
    MostCommonStrings,
    TimeToResponde,
    MostCommonEmoji,
)
from helpers import Query
from cleaning import CleaningExecutor


QUERIES = (
    CountMessagesQuery(),
    MostCommonStrings(),
    TimeToResponde(),
    MostCommonEmoji(),
)


class QueryExecutor:
    def __init__(
        self,
        data_file_path: str = None,
        users_ids_file_path: str = None,
        conversations_ids_file_path: str = None,
    ) -> None:
        self.data_file_path = QueryExecutor.get_conversations_file_path(data_file_path)
        self.users_ids_file_path = QueryExecutor.get_users_ids_file_path(
            users_ids_file_path
        )
        self.conversations_ids_file_path = (
            QueryExecutor.get_conversations_ids_file_path(conversations_ids_file_path)
        )

        self.data = QueryExecutor.load(self.data_file_path)
        self.kwargs = {
            "users_map": QueryExecutor.load(self.users_ids_file_path),
            "conversations_map": QueryExecutor.load(self.conversations_ids_file_path),
            "faked_users": MyFaker.get_fake_names(),
        }

        # check if it is exacly data format we expect, ie produced by our cleaner:
        assert isinstance(
            self.data, list
        ), "Wrong data format (check expected format produced by CleaningExecutor)"
        for l in self.data:
            assert (
                len(l) >= 3 and isinstance(l[-2], list | str) and isinstance(l[-1], int)
            ), "Wrong data format (check expected format produced by CleaningExecutor)"
        assert isinstance(self.kwargs["users_map"], dict), "Users map should be dict."
        assert isinstance(
            self.kwargs["conversations_map"], dict
        ), "Conversations map should be dict."

        self.pre_calculate()

    def pre_calculate(self):
        self.kwargs["groups"] = Query.get_groups(self.data)

    def __call__(self, *args: List[int], **kwargs) -> None:
        """
        By default passes *args, users_map, conversations_map, groups and **kwargs to
        __call__() method of each query that should be ran.
        """

        if len(args) == 0:
            args = Config.get("queries")

            if len(args) == 0:  # user provided no queries - take all
                args = list(range(len(QUERIES)))

        for q in args:
            assert 0 <= int(q) < len(QUERIES), f"Wrong query id provided (got {q})."
            QUERIES[int(q)](
                self.data,
                **self.kwargs,
                **kwargs,
            )

    @staticmethod
    def load(path: str) -> Any:
        with open(path, "r", encoding="utf-8") as file:
            return json.load(file)

    @staticmethod
    def get_conversations_file_path(path: str) -> str:
        return QueryExecutor.get_path(path, "conversations")

    @staticmethod
    def get_users_ids_file_path(path: str) -> str:
        return QueryExecutor.get_path(path, "users_reversed")

    @staticmethod
    def get_conversations_ids_file_path(path: str) -> str:
        return QueryExecutor.get_path(path, "titles_reversed")

    @staticmethod
    def get_path(path: str, name: str = None) -> str:
        if path is None:
            config_path = Config.get("output_dir_path")
            if config_path.startswith("/"):
                path = config_path
            else:
                path = os.path.join(
                    os.path.dirname(__file__), Config.get("output_dir_path")
                )

            if name is not None:
                path = os.path.join(
                    path,
                    Config.get("prefix") + "_" + f"{name}.json",
                )

        return path


def query(data_file_path: str = None, *args: List[int], **kwargs):
    executor = QueryExecutor(data_file_path)
    executor(*args, **kwargs)


class MyFaker:
    @staticmethod
    def get_fake_names(users_file_path: str = None) -> dict:
        faked_path = QueryExecutor.get_path(users_file_path, "users_faked")
        if os.path.exists(faked_path):
            logging.info("Faker:faked users are already generated")
            return QueryExecutor.load(faked_path)

        fake = Faker("pl_PL")
        path = QueryExecutor.get_path(users_file_path, "users")
        names = QueryExecutor.load(path)

        fake_names = {}
        taken_names = set()
        for name, (user_id, gender) in names.items():
            if user_id == Config.get("user_id"):
                fake_names[user_id] = name
                taken_names.add(name)
                continue

            name = fake.first_name() + " " + fake.last_name()
            while name in taken_names and CleaningExecutor.get_gender(name) != gender:
                name = fake.first_name() + " " + fake.last_name()
            taken_names.add(name)
            fake_names[user_id] = name

        CleaningExecutor.save_json(
            fake_names, Config.get("prefix") + "_" + "users_faked.json"
        )
        return fake_names

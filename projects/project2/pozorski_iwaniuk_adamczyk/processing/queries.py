import emoji
import pandas as pd

from typing import Any, List
from setup import Config
from helpers import Query, encode_user, encode_group, encode
from datetime import datetime


class CountMessagesQuery(Query):
    """
    Groups messages by conversation_id, date (rrrr-mm-dd hh:minmin:00) and returns
    data as conversation_id, data, is_group, number of messages that exceed length num for
    each entry defined in min_messages_num
    """

    def __init__(
        self,
        min_messages_num: List[int] = [0, 3, 7, 15],
    ) -> None:
        super().__init__("count_messages", ".csv")
        self.min_messages_num = min_messages_num

    def execute(self, data: List[tuple], **kwargs) -> Any:
        users = self.get_from_kw(kwargs, "users_map", None)
        groups = self.get_from_kw(kwargs, "groups", None)
        faked_users = self.get_from_kw(kwargs, "faked_users", None)

        counts = {}
        for conversation_id, user_id, message, timestamp in data:
            if message == "MetaCommand":
                continue

            date = self.get_date(timestamp)
            key = (conversation_id, user_id, date)

            if key in counts.keys():
                for i, num in enumerate(self.min_messages_num):
                    if len(message) >= num:
                        counts[key][i] += 1
            else:
                counts[key] = [
                    1 if len(message) >= num else 0 for num in self.min_messages_num
                ]

        return pd.DataFrame(
            [
                encode(
                    *key,
                    *encode_user(key[1], users, faked_users=faked_users),
                    *encode_group(key[0], groups),
                    *value,
                )
                for key, value in counts.items()
            ],
            columns=[
                "conversation_id",
                "user_id",
                "date",
                "name",
                "gender",
                "is_group",
                "participants_num",
            ]
            + [f"min_messages_is_{num}" for num in self.min_messages_num],
        )


class MostCommonStrings(Query):
    """
    Returning most common sequences of --words_count (default 2) words.
    Data frame has columns:
    user - user --user_id, that send this,
    sequence of strings and count
    """

    def __init__(self) -> None:
        super().__init__(id="most_common_strings", result_extension=".csv")

    def execute(self, data: List[tuple], **kwargs) -> Any:
        users = self.get_from_kw(kwargs, "users_map", None)
        how_many_words = Config.get("words_count")
        faked_users = self.get_from_kw(kwargs, "faked_users", None)

        conuts = {}
        for line in data:
            user_id, message = line[-3], line[-2]

            if message == "MetaCommand" or (
                Config.get("user_id") != "all" and user_id != Config.get("user_id")
            ):
                continue

            for i in range(len(message) - how_many_words + 1):
                words_streak = " ".join(message[i : i + how_many_words])
                key = (user_id, words_streak)

                if key in conuts.keys():
                    conuts[key] += 1
                else:
                    conuts[key] = 1

        df = pd.DataFrame(
            [
                encode(
                    key[0],
                    *encode_user(key[0], users, faked_users=faked_users),
                    key[1],
                    value,
                )
                for key, value in conuts.items()
            ],
            columns=["user_id", "name", "gender", "sequence_of_strings", "count"],
        )
        df = df.sort_values("count", ascending=False)
        return df


class TimeToResponde(Query):
    """
    Returns data frame with columns:
    sender - user_id, not root,
    time_send - time that user sent last massege before response
    time_response - time that root sent first message on response
    delta_times - difference between last message sent to first response
    """

    def __init__(self) -> None:
        super().__init__(
            id="time_to_responde",
            result_extension=".csv",
            timestamp_group_format="%Y-%m-%d %H:%M:%S",
        )

    def execute(self, data: List[tuple], **kwargs) -> Any:
        groups = self.get_from_kw(kwargs, "groups", None)
        faked_users = self.get_from_kw(kwargs, "faked_users", None)

        data = [
            [conversation_id, user_id, message, self.get_date(timestamp)]
            for conversation_id, user_id, message, timestamp in data
        ]
        data.sort(
            key=lambda line: (
                line[0],
                line[3],
            )
        )

        last_user_id = None
        last_time = None
        df_dict = {}
        root_id = Config.get("user_id")
        for line in data:
            conversation_id, user_id, messege, date = line[0], line[1], line[2], line[3]
            if messege == "MetaCommand" or len(groups.get(conversation_id)) > 2:
                continue

            # we have previous message from not-root user
            # or if user_id == "all" from another user
            # and now user sent message - calculate time delta
            if last_user_id is not None and (
                (root_id == "all" and last_user_id != user_id) or user_id == root_id
            ):
                diff = datetime.strptime(date, "%Y-%m-%d %H:%M:%S") - datetime.strptime(
                    last_time, "%Y-%m-%d %H:%M:%S"
                )

                if 0 <= diff.days < 1:
                    delta = str(diff)
                    key = (last_user_id, user_id, last_time)
                    df_dict[key] = (date, delta)
                last_user_id = None

            if user_id != root_id:
                last_user_id = user_id
                last_time = date

        # create data frame with user_id(sender not root), time_send, time_responde, delta times
        df = pd.DataFrame(
            [
                (
                    key[0],
                    key[1],
                    faked_users[key[0]],
                    faked_users[key[1]],
                    key[2],
                    value[0],
                    value[1],
                )
                for key, value in df_dict.items()
            ],
            columns=[
                "responded_to_id",
                "responded_by_id",
                "responded_to_name",
                "responded_by_name",
                "time_send",
                "time_responded",
                "delta",
            ],
        )

        return df


class MostCommonEmoji(Query):
    """
    Creates a data frame that each entry besides common informations
    have emoji and (if exists) word before and after this emoji.
    """

    def __init__(self) -> None:
        super().__init__("emoji", ".csv")

    def execute(self, data: List[tuple], **kwargs) -> Any:
        users = self.get_from_kw(kwargs, "users_map", None)
        groups = self.get_from_kw(kwargs, "groups", None)
        faked_users = self.get_from_kw(kwargs, "faked_users", None)

        emojis = []
        for conversation_id, user_id, message, timestamp in data:
            if message == "MetaCommand" or (
                Config.get("user_id") != "all" and user_id != Config.get("user_id")
            ):
                continue

            date = self.get_date(timestamp)
            for i in range(len(message)):
                if emoji.is_emoji(message[i]):
                    word_behind = "" if i == 0 else message[i - 1]
                    word_next = "" if i + 1 == len(message) else message[i + 1]
                    emojis.append(
                        encode(
                            user_id,
                            *encode_user(user_id, users, faked_users=faked_users),
                            encode_group(conversation_id, groups)[0],
                            date,
                            message[i],
                            word_behind,
                            word_next,
                        )
                    )

        df = pd.DataFrame(
            emojis,
            columns=[
                "user_id",
                "name",
                "gender",
                "is_group",
                "date",
                "emoji",
                "word_behind",
                "word_next",
            ],
        )
        return df

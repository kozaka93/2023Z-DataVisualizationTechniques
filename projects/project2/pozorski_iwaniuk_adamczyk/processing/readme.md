# Data preprocessing for messanger

### How to use?

1. Install all dependencies from [requirements.txt](requirements.txt).
2. Run all commands listed in [additional_commands.txt](resources/additional_commands.txt).

All tasks are performed from [main.py](main.py) that accepts corresponding flags:

```bash
python main.py -h         
```

```
usage: main.py [-h] [--input_dir_path INPUT_DIR_PATH]
               [--output_dir_path OUTPUT_DIR_PATH] [--n_threds N_THREDS]
               [--prefix PREFIX] [--default_language DEFAULT_LANGUAGE]
               [--verbose VERBOSE] [--preprocess PREPROCESS]
               [--queries QUERIES] [--user_id USER_ID]
               [--words_count WORDS_COUNT]

Facebook data formatter. Drops photos, encodes all found users into unique
ids, performs messages lemmatization and links and emoji encodings. Can
Tokenize message contents.

options:
  -h, --help            show this help message and exit
  --input_dir_path INPUT_DIR_PATH
                        Directory holding folders containing facebook data in
                        json format. Defaults to 'data' directory in setup's
                        folder.
  --output_dir_path OUTPUT_DIR_PATH
                        Directory to which output should be written. Defaults
                        to 'output' directory in setup's folder.
  --n_threads N_THREADS   Number of threads to be used for processing. Defaults
                        to 8.
  --prefix PREFIX       Prefix added to each identifier. Defaults to '0'.
  --default_language DEFAULT_LANGUAGE
                        If program will be unable to detect language it will
                        use that. Defaults to 'pl.
  --verbose VERBOSE     Verbosity mode - 0 for None (default), 1 for logging
                        without warnings, 2 for all.
  --preprocess PREPROCESS
                        1 - preprocessing included, 0 (default) for just query
                        execution.
  --queries QUERIES     Indexes of queries to be executed. By default will
                        execute all queries.
  --user_id USER_ID     Index of user in query 3. By default d_1
  --words_count WORDS_COUNT
                        How long a sequence of words we want to count for the
                        query MostCommonStrings.
```

For --queries indexes refer to [QUERIES](./query_manager.py) constant - query index in this call is index of desired query in that tuple. --input_dir_path should lead to directory with unzipped messenger data folders in .json format (no need to delete no-json files, just put here exacly what you've downloaded from facebook).

> Warning: file processing may take very long, depending on hardware and number of conversations. Be prepared to wait for around 2 hours on average.

Example call:
```bash
python main.py --input_dir /Users/user/Desktop/my_messenger_data --output_dir /Users/user/Desktop/results --n_threads 4 --prefix p --defalut_language en --verbose 1 --preprocess 1
```
Or just for preprocessing query of index 0:
```bash
python main.py --output_dir /Users/user/Desktop/results --prefix "prefix" --queries 0 --user_id "user_id"
```

> Note: num_threads affects only data cleaning performance, queries are not executed in parallel.

### Outputs description

Program produces following files in output directory:
- **conversation_prefix_title_id.jon** files - conversation files for each of conversations in messenger. Each file holds list of entries *(sender_id, words, timestamp)*, where words are already preprocessed yet not encoded.
- **prefix_users.json **- map how to get (user_id, gender) from user_name
- **prefix_titles.json** - map how to get title_id from title_name
- **prefix_users_reversed.json** - map how to get (user_name, gender) from user_id
- **prefix_titles_reversed.json** - map how to get title_name from title_id
- **prefix_conversations.json** - merged conversations into one file - a list of entries *(title_id, sender_id, words, timestamp*), where words are already preprocessed yet not encoded.
- **prefix_query_query_id.query_extension** files - results of queries performed on **conversation_prefix_title_id.jon** data.

### How to write your own query?
All you need to do is to override the [Query](./helpers.py).execute method - put your class in [queries.py](./queries.py), and then add it to  [QUERIES](./query_manager.py) constant. For example:

```python
# ./queries.py

class CountMessagesQuery(Query):
    def __init__(
        self,
        min_messages_num: List[int] = [0, 3, 7, 15],
    ) -> None:
        # define your own query id (unique!!) and output extension
        super().__init__("count_messages", ".csv") 
        # additional fields inclusive for this query
        self.min_messages_num = min_messages_num

    # override execute method that accepts data from
    # **conversation_prefix_title_id.jon**. Remember to 
    # return data matching the earlier specified output
    def execute(self, data: List[tuple], **kwargs) -> Any:
        dates = {}

        for line in data:
            message, timestamp = line[-2], line[-1]
            date = self.get_date(timestamp)

            if date not in dates:
                dates[date] = {str(key): 0 for key in self.min_messages_num}

            for min_message_num in self.min_messages_num:
                if len(message) > min_message_num:
                    dates[date][str(min_message_num)] += 1

        result = pd.DataFrame(dates)
        result = result.T
        columns = ["date"] + [f"min_messsages={num}" for num in result.columns]
        result = result.reset_index()
        result.columns = columns
        return result
```

```python
# ./query_manager.py

QUERIES = (
  ...
  CountMessagesQuery(), # add your queue
  )
```

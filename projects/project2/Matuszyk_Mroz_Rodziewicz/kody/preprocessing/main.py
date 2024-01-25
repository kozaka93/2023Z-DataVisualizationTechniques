import csv
import json
import os
import glob
from tqdm import tqdm
import re
from functools import partial
from collections import Counter


def find_json_files(directory):
    pattern = os.path.join(directory, '**', '*.json')
    json_files = glob.glob(pattern, recursive=True)
    return json_files


def count_unique_words(file_path):
    unique_words = Counter()
    with open(file_path, 'r', encoding='utf-8') as file:
        for line in file:
            words = line.split()
            unique_words.update(words)
    return unique_words


def count_letter_occurrences(file_path):
    letter_counts = Counter()
    with open(file_path, 'r', encoding='utf-8') as file:
        for line in file:
            for letter in line:
                letter_counts[letter] += 1
    return letter_counts


def save_word_counts_to_csv(word_counts, csv_file_path):
    with open(csv_file_path, 'w', newline='', encoding='utf-8') as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(['Word', 'Count'])
        for word, count in word_counts.items():
            csv_writer.writerow([word, count])


def save_letter_counts_to_csv(letter_counts, csv_file_path):
    with open(csv_file_path, 'w', newline='', encoding='utf-8') as csv_file:
        csv_writer = csv.writer(csv_file)
        csv_writer.writerow(['Letter', 'Count'])
        for letter, count in letter_counts.items():
            csv_writer.writerow([letter, count])


def extract_message_conent(filepath):
    data = fix_facebook_json(filepath)
    if 'messages' not in data:
        print("brak messages w JSON o sciezce:", filepath)
        return []
    messages = []
    for row in data['messages']:
        if 'content' in row:
            messages.append(row['content'])
    return messages


def extract_message_content_time_sender(filepath):
    data = fix_facebook_json(filepath)
    if 'messages' not in data:
        print("brak messages w JSON o sciezce:", filepath)
        return []
    messages = []
    participant_count = 0
    if 'participants' in data:
        participant_count = len(data['participants'])
    for row in data['messages']:
        row['participant_count'] = participant_count
        if 'content' in row and 'sender_name' in row and 'timestamp_ms' in row:
            messages.append(row)
    return messages


def fix_facebook_json(filepath):
    with open(filepath, 'rb') as f:
        fix_mojibake_escapes = partial(
            re.compile(rb'\\u00([\da-f]{2})').sub,
            lambda m: bytes.fromhex(m[1].decode()),
        )
        repaired = fix_mojibake_escapes(f.read())
        data = json.loads(repaired)

        return data


def get_keys(filepath):
    return fix_facebook_json(filepath).keys()


def json_to_csv(json_file_path, csv_file_path):
    data = fix_facebook_json(json_file_path)
    header = data.keys()
    with open(csv_file_path, 'w') as csv_file:
        csv_writer = csv.DictWriter(csv_file, fieldnames=header)
        csv_writer.writeheader()
        if isinstance(data, list):
            csv_writer.writerows(data)
        elif isinstance(data, dict):
            csv_writer.writerow(data)


def join_csv_files(input_folder, output_file):
    csv_files = [file for file in os.listdir(input_folder) if file.endswith('.csv')]
    unique_columns = set()

    for csv_file in tqdm(csv_files, desc="Extracting Column Names", unit="file"):
        with open(os.path.join(input_folder, csv_file), 'r') as file:
            csv_reader = csv.reader(file)
            header = next(csv_reader, None)
            if header:
                unique_columns.update(header)
    with open(output_file, 'w', newline='') as output_csv:
        csv_writer = csv.DictWriter(output_csv, fieldnames=list(unique_columns))
        csv_writer.writeheader()
        for csv_file in tqdm(csv_files, desc="Combining CSV Files", unit="file"):
            with open(os.path.join(input_folder, csv_file), 'r') as file:
                csv_reader = csv.DictReader(file)
                for row in csv_reader:
                    csv_writer.writerow(row)


def save_list_of_dicts_to_csv(data_list, csv_file_path):
    if not data_list:
        print("No data to save.")
        return

    header = set(key for data_dict in data_list for key in data_dict.keys())

    with open(csv_file_path, 'w', newline='', encoding='utf-8') as csv_file:
        csv_writer = csv.DictWriter(csv_file, fieldnames=header)
        csv_writer.writeheader()

        for data_dict in data_list:
            csv_writer.writerow(data_dict)


if (True):
    # scieżka do your_activity_across_facebook
    directory_path = r"C:\Users\wojow\Downloads\facebook-kacperrodziewicz773-2023-12-19-RlWfxbh8\your_activity_across_facebook\messages\inbox"
    # gdzie ma zapisać wszystkie wiadomości do jednego pliku
    all_messages_file_path = r"C:\Users\wojow\Downloads\PROJEKT_TWD\messeges.txt"
    # gdzie ma zapisać liczności poszczególnych wyrazów
    output_csv_file_path_word_counts = r"C:\Users\wojow\Downloads\PROJEKT_TWD\words_counts.csv"
    # gdzie ma zapisać liczności poszczególnych liter
    output_csv_file_path_letter_counts = r'C:\Users\wojow\Downloads\PROJEKT_TWD\letter_count.csv'
    # sciezka do pliku, który będzie zawierał inforamcje o wszystkich wiadomościach z json, jako plik csv
    master_messages_csv_file = r'C:\Users\wojow\Downloads\PROJEKT_TWD\master_count.csv'
    directory_path_with_wildcard = os.path.join(directory_path, '*')
    json_files = find_json_files(directory_path_with_wildcard)
    all_messages_with_content_time_sender = []
    with open(all_messages_file_path, 'wb') as file:
        categories = []
        file_number = 0

        for json_file in tqdm(json_files, desc="Przerabiam pliki json", unit="plików"):
            wiadomosci = extract_message_conent(json_file)
            for wiadomosc in wiadomosci:
                if len(wiadomosc) != 0: # niektóre chaty są puste
                    file.write((wiadomosc + '\n').encode('utf-8'))
            all_messages_with_content_time_sender.extend(extract_message_content_time_sender(json_file))
    print("Zapisuje wszystkie wiadomosci do csv")
    save_list_of_dicts_to_csv(all_messages_with_content_time_sender, master_messages_csv_file)

    print("Zapisuje liczności słow do csv")
    word_counts = count_unique_words(all_messages_file_path)
    letter_counts = count_letter_occurrences(all_messages_file_path)

    print("Zapisuje liczności liter do csv")
    save_word_counts_to_csv(word_counts, output_csv_file_path_word_counts)
    save_letter_counts_to_csv(letter_counts, output_csv_file_path_letter_counts)

    print("Koniec")

import os
import json
import csv

def json_to_csv_fixed(input_folder_path, output_csv_path):
    # Create output folder if it doesn't exist
    os.makedirs(os.path.dirname(output_csv_path), exist_ok=True)

    with open(output_csv_path, 'w', encoding='utf-8', newline='') as csv_file:
        csv_writer = csv.writer(csv_file)
        # Write header row
        csv_writer.writerow(['Sender', 'Timestamp', 'Content', 'Reactions', 'GroupOrPriv'])

        for filename in os.listdir(input_folder_path):
            if filename.endswith(".json"):
                input_file_path = os.path.join(input_folder_path, filename)

                with open(input_file_path, 'r', encoding='utf-8-sig') as file:
                    data = json.load(file)

                # Extract messages from fixed JSON data and write to CSV
                extract_and_write_messages(data, csv_writer)


def extract_and_write_messages(data, csv_writer):
    participants = data.get('participants', [])
    group_or_priv = 'priv' if len(participants) <= 2 else 'group'

    messages = data.get('messages', [])
    for message in messages:
        sender = message.get('sender_name', '')
        timestamp = message.get('timestamp_ms', '')
        content = message.get('content', '')
        reactions = message.get('reactions', [])

        csv_writer.writerow([sender, timestamp, content, reactions, group_or_priv])


# Example usage:
# Example usage:
input_folder_path = r'C:\\Users\\Zosia\\Desktop\\AAAPROJEKT2\\poufne_dane\\messenger\\znaki_zmienione'
output_csv_path = r'C:\\Users\\Zosia\\Desktop\\AAAPROJEKT2\\poufne_dane\\messenger\\csvs.csv'
json_to_csv_fixed(input_folder_path, output_csv_path)
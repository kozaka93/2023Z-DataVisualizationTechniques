"""
By Michał Matuszyk
on 17/01/2024
"""

import pandas as pd

csv_file_path = r"C:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/projekt_2/master_messages.csv"
sender_name = 'Michał Matuszyk'


column_types = {
    'share': str,
    'timestamp_ms': str,
    'sender_name': str,
    'gifs': str,
    'files': str,
    'content': str,
    'reactions': str,
    'missed': str,
    'videos': str,
    'ip': str,
    'audio_files': str,
    'participant_count': int,
    'is_geoblocked_for_viewer': str,
    'call_duration': str,
    'photos': str
}

df = pd.read_csv(csv_file_path, dtype=column_types, low_memory=False)
filtered_df = df[df['sender_name'].str.contains(sender_name, na=False)]

filtered_df['timestamp_ms'] = pd.to_datetime(filtered_df['timestamp_ms'], unit='ms')
filtered_df['Year'] = filtered_df['timestamp_ms'].dt.year
filtered_df['Month'] = filtered_df['timestamp_ms'].dt.month_name()

filtered_df['word'] = filtered_df['content'].str.lower().str.split()
print(filtered_df.columns.values)
# filtered_df = filtered_df[['content', 'Year', 'Month', 'word']]
# Explode the 'word' column to create a new row for each word
exploded_df = filtered_df.explode('word')
print(exploded_df[['Year', 'Month', 'word']])


# Group by Year, Month, and Word, and count occurrences
word_count_df = exploded_df.groupby(['Year', 'Month', 'word']).size().reset_index(name='count')
print(word_count_df)

# Find the top 50 word for each Year and Month
top_50_word_df = word_count_df.groupby(['Year', 'Month']).apply(lambda x: x.nlargest(50, 'count')).reset_index(drop=True)

# Add an 'order' column
top_50_word_df['order'] = top_50_word_df.groupby(['Year', 'Month']).cumcount() + 1
# exit()
# Save the result to a CSV file
top_50_word_df.to_csv('top_50_word_per_month.csv', index=False)
# print(filtered_df['content'])
exit()

# Convert 'timestamp_ms' to a datetime object correctly


# Extract Year and Month from the 'timestamp_ms' column


# Group by Year, Month, and count occurrences of each word
grouped_df = filtered_df.groupby(['Year', 'Month', filtered_df['content'].str.lower()]).size().reset_index(name='count')

# Find the top 50 word for each Year and Month
top_50_df = grouped_df.groupby(['Year', 'Month']).apply(lambda x: x.nlargest(50, 'count')).reset_index(drop=True)

# Add an 'order' column
top_50_df['order'] = top_50_df.groupby(['Year', 'Month']).cumcount() + 1

# Save the result to a CSV file
top_50_df.to_csv('top_50_year_month.csv', index=False)

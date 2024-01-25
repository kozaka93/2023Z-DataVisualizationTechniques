"""
By Michał Matuszyk
on 17/01/2024
"""

import pandas as pd

csv_file_path = r"C:/nextcloud/Studia - PW/semestr 3/Techniki Wizualizacji Danych/projekt_2/master_messages.csv"

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
filtered_df = df[df['sender_name'].str.contains('Michał Matuszyk', na=False)]

# Convert 'timestamp_ms' to a datetime object correctly
filtered_df['timestamp_ms'] = pd.to_datetime(filtered_df['timestamp_ms'], unit='ms')

# Extract Year and Month from the 'timestamp_ms' column
filtered_df['Year'] = filtered_df['timestamp_ms'].dt.year
filtered_df['Month'] = filtered_df['timestamp_ms'].dt.month_name()

# Group by Year, Month, and count occurrences of each letter
grouped_df = filtered_df.groupby(['Year', 'Month', filtered_df['content'].str.lower().str.extract(r'([a-zA-Z])')[0]]).size().reset_index(name='count')

# Save the result to a CSV file
grouped_df.to_csv('letter_counts_split_year_month.csv', index=False)

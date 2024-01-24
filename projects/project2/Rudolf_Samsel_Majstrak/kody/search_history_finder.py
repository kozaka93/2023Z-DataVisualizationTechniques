import pandas as pd
import json
from datetime import datetime
from prophet import Prophet

file_path1 = "data/HistoriaIgor.json"
file_path2 = "data/HistoriaGabi.json"

# Read the entire JSON file with explicit encoding
with open(file_path1, 'r', encoding='utf-8') as file1:
    json_data = json.load(file1)

with open(file_path2, 'r', encoding='utf-8') as file2:
    json_data2 = json.load(file2)

# Extract only the "Browser History" entries
browser_history_entries = json_data.get("Browser History", [])
browser_history_entries_2 = json_data2.get("Browser History", [])

# Create a DataFrame from the flattened list of entries
df_needed1 = pd.json_normalize(browser_history_entries)
df_needed2 = pd.json_normalize(browser_history_entries_2)


def filter_entries(df_data, some_text):
    filtered_entries = []

    # Ensure the search text is case-insensitive
    some_text_lower = some_text.lower()

    # Check if the search text is present in the 'url' column
    filtered_entries = df_data[df_data['url'].str.lower().str.contains(some_text_lower, na=False)]

    return filtered_entries


def getAndConvertToDate(df_data):
    needed_column = df_data[['time_usec']]
    needed_column['time_usec'] = pd.to_datetime(needed_column['time_usec'], unit='us')
    needed_column = needed_column.reset_index()

    needed_column['day'] = needed_column['time_usec'].dt.day
    needed_column['month'] = needed_column['time_usec'].dt.month

    return needed_column


def getFilteredCounts(data_range, searching_site, whatPerson):
    if whatPerson == 1 or whatPerson == 3:
        filtered_entries = filter_entries(df_needed1, searching_site)
    else:
        filtered_entries = filter_entries(df_needed2, searching_site)

    filtered_entries_right_format = getAndConvertToDate(filtered_entries)

    beg_date = pd.to_datetime(data_range[0])
    end_date = pd.to_datetime(data_range[1])

    df_object = pd.DataFrame(filtered_entries_right_format)

    df_object = df_object[(df_object['time_usec'] >= beg_date) & (df_object['time_usec'] <= end_date)]

    df_object_grouped = df_object.groupby(["day", "month"]).size().reset_index(name="Count")

    df_object_grouped['exact_date'] = pd.to_datetime(df_object_grouped[['month', 'day']].assign(year=2023))

    return df_object_grouped


def getImportantInfo(data_range, searching_site, whatPerson):
    if whatPerson == 1 or whatPerson == 3:
        filtered_entries = filter_entries(df_needed1, searching_site)
    else:
        filtered_entries = filter_entries(df_needed2, searching_site)

    filtered_entries_right_format = getAndConvertToDate(filtered_entries)

    beg_date = pd.to_datetime(data_range[0])
    end_date = pd.to_datetime(data_range[1])

    df_object = pd.DataFrame(filtered_entries_right_format)

    df_object = df_object[(df_object['time_usec'] >= beg_date) & (df_object['time_usec'] <= end_date)]

    df_object_grouped = df_object.groupby(["day", "month"]).size().reset_index(name="Count")

    total_entries = df_object_grouped['Count'].sum()

    by_day_grouped = df_object_grouped.groupby(['day']).size().reset_index(name="Count")

    by_month_grouped = df_object_grouped.groupby(['month']).size().reset_index(name="Count")

    avg_day = by_day_grouped['Count'].sum() / by_day_grouped.shape[0]
    avg_month = by_month_grouped['Count'].sum() / by_month_grouped.shape[0]

    return [total_entries, avg_day, avg_month]


def predict_upcoming_values(previous_values):
    looking_data = pd.DataFrame(previous_values)

    extended_df = pd.DataFrame({
        'ds': pd.to_datetime(looking_data[['month', 'day']].assign(year=2023)),
        'y': looking_data['Count']
    })

    m = Prophet()
    m.fit(extended_df)

    future = m.make_future_dataframe(periods=365)
    forecast = m.predict(future)

    prediction_data = forecast[['ds', 'yhat_upper']]

    return prediction_data


import pandas as pd
import numpy as np

# path to file
file_path = "data/aktywnoscGoogleIgor.csv"

# create data frame
df = pd.read_csv(file_path)

# taking selected columns
selected_data = df[["Activity Timestamp", "User Agent String", "Product Name"]]


def extract_day(timestamp):
    """
    :param timestamp: gives object to convert to data type
    :return: object representation of date
    """

    return pd.to_datetime(timestamp).day


def extract_day_name(timestamp):
    """
    :param timestamp: gives object to convert to data type
    :return: object representation of date
    """

    return pd.to_datetime(timestamp).day_name()


def extract_hour(timestamp):
    """
    :param timestamp: gives object to convert to data type
    :return: object representation of date
    """

    return pd.to_datetime(timestamp).hour


def extract_month(timestamp):
    """
    :param timestamp: gives object to convert to data type
    :return: object representation of date
    """

    return pd.to_datetime(timestamp).month


def extract_minute(timestamp):
    """
    :param timestamp: gives object to convert to data type
    :return: object representation of date
    """

    return pd.to_datetime(timestamp).minute


def type1_data(days, products):
    """
    :param days: give range of days to consider [beg, end] format
    :param products: give list of products to consider [product1, product2, ...]
    :return: data frame of products and days to amount
    """

    rows = selected_data['Product Name'].isin(products)

    filtered_df = selected_data[rows].copy()

    filtered_df['Day'] = filtered_df['Activity Timestamp'].apply(extract_day)
    grouped_data = filtered_df.groupby(['Product Name', 'Day']).size().reset_index(name='Count')

    beginning_day = days[0]
    ending_day = days[1]

    filtered_grouped_data = grouped_data[(grouped_data['Day'] >= beginning_day) & (grouped_data['Day'] <= ending_day)]

    return filtered_grouped_data


def type2_data_fix(seasons, products):
    df = selected_data.copy()

    df['Activity Timestamp'] = pd.to_datetime(df['Activity Timestamp'])

    # desired_products = ['Other', 'Chrome Sync']  # Add other product names as needed

    # Filter the DataFrame based on the desired products
    filtered_df = df[df['Product Name'].isin(products)]

    # Extract hour from the timestamp
    filtered_df['Hour'] = filtered_df['Activity Timestamp'].dt.hour

    # Group by 'Product Name' and 'Hour' and count the entries
    result = filtered_df.groupby(['Product Name', 'Hour']).size().reset_index(name='Entry Count')

    return result


def type2_data(seasons, products):
    """
    :param seasons: boolean, whenever true show 4 density plots instead of 1: spring, summer, autumn, winter
    :param products: give list of products to consider [product1, product2, ...]
    :return: data frame of products and hours to amount (and seasons whenever seasons==true)
    """
    rows = selected_data['Product Name'].isin(products)

    filtered_df = selected_data[rows].copy()

    filtered_df['Hour'] = filtered_df['Activity Timestamp'].apply(extract_hour)

    if seasons is False:
        grouped_data = filtered_df.groupby(['Product Name', 'Hour']).size().reset_index(name='Count')
        return grouped_data
    else:
        filtered_df['Month'] = filtered_df['Activity Timestamp'].apply(extract_month)
        grouped_data = filtered_df.groupby(['Product Name', 'Hour', 'Month']).size().reset_index(name='Count')

        unique_values = grouped_data['Month'].unique()

        first_part = grouped_data[grouped_data['Month'] == unique_values[0]]
        second_part = grouped_data[grouped_data['Month'] == unique_values[1]]

        return [first_part, second_part]


def type3_data(days, products):
    """
    :param days: give range of days to consider [beg, end] format
    :param products: give list of products to consider [product1, product2, ...]
    :return: data frame of products and days to amount
    """

    rows = selected_data['Product Name'].isin(products)

    filtered_df = selected_data[rows].copy()

    filtered_df['Day Name'] = filtered_df['Activity Timestamp'].apply(extract_day_name)

    grouped_data = filtered_df.groupby(['Product Name', 'Day Name']).size().reset_index(name='Count')

    day_mapping = {'Monday': 1, 'Tuesday': 2, 'Wednesday': 3, 'Thursday': 4, 'Friday': 5, 'Saturday': 6, 'Sunday': 7}

    grouped_data['Day Name'] = grouped_data['Day Name'].map(day_mapping)

    beginning = int(days[0])
    ending = int(days[1])

    grouped_filtered_data = grouped_data[(beginning <= grouped_data['Day Name']) & (grouped_data['Day Name'] <= ending)]

    day_mapping_2 = {1: 'Pon', 2: 'Wt', 3: 'Śr', 4: 'Czw', 5: 'Pt', 6: 'Sob', 7: 'Niedz'}

    grouped_filtered_data['Day Name'] = grouped_filtered_data['Day Name'].map(day_mapping_2)

    return grouped_filtered_data


def type4_data(days, products):
    """
    :param days: give range of days to consider [beg, end] format
    :param products: give list of products to consider [product1, product2, ...]
    :return: data frame of products and days to amount
    """

    needed_copy = selected_data.copy()

    needed_copy['Day'] = needed_copy['Activity Timestamp'].apply(extract_day)

    needed_copy['Activity Timestamp'] = pd.to_datetime(needed_copy['Activity Timestamp'])

    needed_copy['Time Difference'] = needed_copy['Activity Timestamp'].diff()

    beginning = int(days[0])
    ending = int(days[1])

    needed_copy = needed_copy[(needed_copy['Day'] >= beginning) & (needed_copy['Day'] <= ending)]

    rows = needed_copy['Product Name'].isin(products)

    filtered_data = needed_copy[rows]

    total_time_spent = filtered_data.groupby('Product Name')['Time Difference'].sum().reset_index()

    return total_time_spent


def type5_data(hours, minutes, product):
    """
    :param hours: give range of hours to create heatmap
    :param minutes: give range of minutes to create heatmap
    :param product: give specifi product on which you want to consider heatmap
    :return: returns data frame of hours and minutes to amount
    """

    needed_copy = selected_data.copy()

    rows = needed_copy['Product Name'] == product

    filtered_data = needed_copy[rows]

    filtered_data['hour'] = filtered_data['Activity Timestamp'].apply(extract_hour)

    filtered_data['minute'] = filtered_data['Activity Timestamp'].apply(extract_minute)

    total_entries = filtered_data.groupby(['hour', 'minute']).size().reset_index(name='Count')

    hour_beg = int(hours[0])
    hour_end = int(hours[1])

    minutes_beg = int(minutes[0])
    minutes_end = int(minutes[1])

    total_entries_filtered = total_entries[(total_entries['hour'] >= hour_beg) & (total_entries['hour'] <= hour_end)]
    total_entries_filtered = total_entries_filtered[
        (total_entries_filtered['minute'] >= minutes_beg) & (total_entries_filtered['minute'] <= minutes_end)]

    return total_entries_filtered


def type6_data(days, products):
    """
    :param days: give range of days to consider [beg, end] format
    :param products: give list of products to consider [product1, product2, ...]
    :return: data frame of products and days to amount
    """

    rows = selected_data['Product Name'].isin(products)

    filtered_df = selected_data[rows].copy()

    df = filtered_df

    df['Activity Timestamp'] = pd.to_datetime(df['Activity Timestamp'])

    df = df.sort_values(by='Activity Timestamp')

    df['previous_product'] = df['Product Name'].shift()
    df['next_product'] = df['Product Name'].shift(-1)

    mask_not_other = (df['Product Name'] != 'Other') & (df['next_product'] != 'Other')
    mask_not_same_next = df['next_product'] != df['Product Name']

    filtered_df = df[mask_not_other & mask_not_same_next]

    change_counts = filtered_df.groupby(
        [df['Activity Timestamp'].dt.date, 'previous_product', 'Product Name', 'next_product']).size().reset_index(
        name='change_count')

    needed_data = change_counts[['Activity Timestamp', 'previous_product', 'next_product', 'change_count']]

    summed_needed_data = needed_data.groupby(['previous_product', 'next_product'])['change_count'].sum().reset_index()

    summed_needed_data = summed_needed_data[
        summed_needed_data['previous_product'] != summed_needed_data['next_product']]

    return summed_needed_data


# type2 = type2_data_fix(False, ["Chrome Sync", "Other"])
#
# print(type2)

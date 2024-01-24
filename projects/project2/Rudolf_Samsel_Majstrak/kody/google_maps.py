from shinywidgets import output_widget
from shiny import ui
from shiny.ui import div
from ipyleaflet import Map, Marker
from sklearn.cluster import KMeans
import os
import json
import pandas as pd
import re


def find_lat_long(text, year, month):
    lat = -1
    result = []

    for line in text.splitlines():

        if "latitude" in line:
            lat = int(re.findall(r'-?\d+', line)[1]) / 10 ** 7
        elif "longitude" in line:
            lon = int(re.findall(r'-?\d+', line)[1]) / 10 ** 7
            if lat != -1:
                result.append((year, month, lat, lon))
                lat = -1

    return result


def return_df():
    locations = []

    for root, dirs, files in os.walk(r'data/location'):
        for file in files:
            if file.endswith('.json'):
                file_path = os.path.join(root, file)

                with open(file_path, 'r', encoding='utf-8') as json_file:
                    try:
                        data = json.load(json_file)
                        json_string = json.dumps(data, indent=4)
                        # print(f"Content of {file_path}")
                        locations += find_lat_long(json_string, file[:4], file[file.find("_") + 1:file.find(".")])
                    except json.JSONDecodeError as e:
                        print(f"Error reading {file_path}: {e}")

    df = pd.DataFrame(locations, columns=['Year', 'Month', 'Latitude', 'Longitude'])

    months_mapping = {
        'JANUARY': 'STYCZEŃ', 'FEBRUARY': 'LUTY', 'MARCH': 'MARZEC',
        'APRIL': 'KWIECIEŃ', 'MAY': 'MAJ', 'JUNE': 'CZERWIEC',
        'JULY': 'LIPIEC', 'AUGUST': 'SIERPIEŃ', 'SEPTEMBER': 'WRZESIEŃ',
        'OCTOBER': 'PAŹDZIERNIK', 'NOVEMBER': 'LISTOPAD', 'DECEMBER': 'GRUDZIEŃ'
    }

    df['Month'] = df['Month'].replace(months_mapping)
    return df


def maps_div():

    df = return_df()

    result = div(
        div(
            div(
                ui.img(src="left_arrow.png", id="small_button"),
                ui.img(src="right_arrow.png", id="small_button"),
                ui.img(src="reload.png", id="small_button"),
                id="fun_buttons"
            ),
            div(
                ui.input_text(label="", id="url5", value="https://meegle.com/maps"),
                id="url_div"
            ),
            id="header_div"
        ),
        div(
            div(
                # ui.input_action_button("button_dots", HTML('<p class="button"> ...<br>...<br>...</p>')),
                div(
                    ui.input_action_button(id="menu_button", label="", style="height: 40px; width: 40px;"),
                    ui.img(src="menu.png", id="menu_image"),
                    id="menu_button_div"
                ),
                id="menu_div"
            ),
            id="lower_header_div"
        ),
        div(
            ui.panel_title("Sklep Google Play"),
            ui.layout_sidebar(
                ui.panel_sidebar(
                    ui.input_select("year_input", "Select Year:", choices=list(df['Year'].unique()) + ['WSZYSTKIE'],
                                    selected='2023'),
                    ui.input_select("month_input", "Select Month:",
                                    choices=["STYCZEŃ", "LUTY", "MARZEC", "KWIECIEŃ", "MAJ", "CZERWIEC", "LIPIEC",
                                             "SIERPIEŃ", "WRZESIEŃ", "PAŹDZIERNIK", "LISTOPAD", "GRUDZIEŃ", "WSZYSTKIE"],
                                    selected='WSZYSTKIE'),
                ),
                ui.panel_main(
                    output_widget("map")
                )
            ),
            id="master_div"
        ),
        div(
            id="footer_div"
        ),
        id="body_div"
    )

    return result


def return_map(df):

    if df is None or len(df) == 0:
        return Map(center=(52.221999, 21.007003), zoom=5)

    else:
        my_map = Map(center=(50.221999, -25.007003), zoom=4)

        k = 20
        kmeans = KMeans(n_clusters=k)
        df['Cluster'] = kmeans.fit_predict(df[['Latitude', 'Longitude']])
        centroids = kmeans.cluster_centers_

        for row in centroids:
            point = Marker(location=(row[0], row[1]), draggable=False)
            my_map.add_layer(point)

        return my_map


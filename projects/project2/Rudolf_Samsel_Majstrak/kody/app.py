from shiny import App, reactive, render, ui
from shiny.ui import div, h1, head_content, tags
from pathlib import Path
import pandas as pd
import plotly.express as px
from shinywidgets import output_widget, render_widget
from activity_analysis import type1_data, type2_data, type3_data, type4_data, type5_data, type6_data, type2_data_fix
from search_suggestions import give_search_result
from htmltools import HTML
import plotly.graph_objects as go
import plotly.subplots as sp
from search_history_finder import getFilteredCounts, predict_upcoming_values, getImportantInfo
import numpy as np
import matplotlib.pyplot as plt

import meegle_main
import play_store
import google_maps
import youtube
from search_suggestions import give_search_result


# do przekopiowania do swojego pliku i na końcu wywalić
def colored_letters(word):
    return HTML("".join(f'<span class="letter-{i}">{letter}</span>' for i, letter in enumerate(word)))


main_page = meegle_main.main_page_fun()

play_store_page = play_store.play_store_div()

maps_page = google_maps.maps_div()

youtube_page = youtube.youtube_div()

activity_page = div(
    div(
        div(
            ui.img(src="left_arrow.png", id="small_button"),
            ui.img(src="right_arrow.png", id="small_button"),
            ui.img(src="reload.png", id="small_button"),
            id="fun_buttons"
        ),
        div(
            ui.input_text(label="", id="url3", value="https://meegle.analytics.com"),
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
        div(

            div(
                div(
                    "Przeglądaj wykaz swojej aktywności, ze wszystkich aplikacji od Google",
                    id="activity_title"
                ),
                div(
                    "Po prostu wybierz programy oraz interesujące Cię urządzenia a my zrobimy pracę za Ciebie.",
                    id="appliactions_chooser"
                ),
                id="activity_text_space"
            ),
        ),

        div(
            div(
                ui.input_action_button(id="chrome_activity_button", label="", style="height: 70px; width: 70px;"),
                ui.img(src="chrome.png", id="chrome_image_activity"),
                id="first_app_to_choose_activity"
            ),
            div(
                ui.input_action_button(id="playstore_activity_button", label="", style="height: 70px; width: 70px;"),
                ui.img(src="playstore.png", id="playstore_image_activity"),
                id="second_app_to_choose_activity"
            ),
            div(
                ui.input_action_button(id="youtube_activity_button", label="", style="height: 70px; width: 70px;"),
                ui.img(src="youtube.png", id="youtube_image_activity"),
                id="third_app_to_choose_activity"
            ),
            div(
                ui.input_action_button(id="gmail_activity_button", label="", style="height: 70px; width: 70px;"),
                ui.img(src="gmail.png", id="gmail_image_activity"),
                id="fourth_app_to_choose_activity"
            ),
            div(
                ui.input_action_button(id="photos_activity_button", label="", style="height: 70px; width: 70px;"),
                ui.img(src="photos.png", id="photos_image_activity"),
                id="fifth_app_to_choose_activity"
            ),

            id="apps_to_choose_activity"
        ),

        div(

            div(

                div(

                    ui.layout_sidebar(
                        ui.sidebar(
                            ui.input_action_button(id="plot_1_chooser", label="Wybierz wykres liniowy",
                                                   style="height: 10%; width: 55%; margin-left:auto; margin-right: auto;  background-color: #A7ECFF; border-radius: 25px;"),

                            ui.h6("Wybierz zakres ustawień", style="margin-left: auto; margin-right: auto;"),

                            ui.input_slider("range_days_plot1", "Wybierz zakres interesujących Cię dni:",
                                            min=1, max=30,
                                            value=[1, 30]),

                            width="100%;"
                        ),
                        output_widget("activity_plot_1", width="100%;", height="100%;"), height="100%;"

                    ),
                    id="activity_plot_1_space"
                ),
                div(
                    ui.layout_sidebar(
                        ui.sidebar(
                            ui.input_action_button(id="plot_2_chooser", label="Histogram plot chooser",
                                                   style="height: 10%; width: 55%; margin-left:auto; margin-right: auto; background: #A7ECFF; border-radius: 25px;"),

                            ui.h6("Wybierz zakres ustawień", style="margin-left: auto; margin-right: auto;"),

                            ui.input_checkbox("month_comparison_second_plot", "Porównywanie poprzedniego miesiąca",
                                              False),
                            ui.output_ui("value"),

                            width="100%;"
                        ),
                        output_widget("activity_plot_2", width="100%;", height="100%;"), height="100%;"

                    ),
                    id="activity_plot_2_space"
                ),
                div(

                    ui.layout_sidebar(
                        ui.sidebar(
                            ui.input_action_button(id="plot_3_chooser", label="Wybierz wykres słupkowy",
                                                   style="height: 10%; width: 55%; margin-left:auto; margin-right: auto; background: #A7ECFF; border-radius: 25px;"),

                            ui.h6("Wybierz zakres ustawień", style="margin-left: auto; margin-right: auto;"),

                            ui.input_slider("range_days_plot3", "Wybierz zakres interesujących Cię dni:",
                                            min=1, max=7,
                                            value=[1, 7]),

                            width="100%;"
                        ),
                        output_widget("activity_plot_3", width="100%;", height="100%;"), height="100%;"

                    ),

                    id="activity_plot_3_space"
                ),

                div(

                    ui.layout_sidebar(
                        ui.sidebar(
                            # w rzeczywistosci plot_6_chooser
                            ui.input_action_button(id="plot_6_chooser", label="Wybierz zestawienie informacyjne",
                                                   style="height: 10%; width: 55%; margin-left:auto; margin-right: auto; background: #A7ECFF; border-radius: 25px;"),

                            ui.h6("Wybierz zakres ustawień", style="margin-left: auto; margin-right: auto;"),

                            ui.input_slider("range_days_plot6", "Wybierz zakres interesujących Cię dni:",
                                            min=1, max=30,
                                            value=[1, 30]),

                            width="100%;"
                        ),
                        ui.output_ui("activity_plot_6", width="120%;", height="120%;"), height="100%;"

                    ),

                    id="activity_plot_6_space"
                ),

                # tutaj dodajemy obszar na 6 wykresow do analizy
                id="sub_panel_main_right_activity"
            ),

            id="main_activity_container"
        ),

        id="master_div"
    ),
    div(
        id="footer_div"
    ),
    id="body_div"
)

chrome_page = div(
    div(
        div(
            ui.img(src="left_arrow.png", id="small_button"),
            ui.img(src="right_arrow.png", id="small_button"),
            ui.img(src="reload.png", id="small_button"),
            id="fun_buttons"
        ),
        div(
            ui.input_text(label="", id="url4", value="https://meegle.com/chrome"),
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
        div(
            div(
                div(
                    ui.h1("Wyszukaj z Chrome",
                          style="padding-top: 6%; margin-top: 6%; font-family: Arial, Helvetica, sans-serif; "),
                    id="logo_type_1"
                ),
                div(
                    ui.img(src="chrome.png", id="chrome_image_additional"),
                    id="logo_type_2"
                ),

                id="chrome_left_side_top"
            ),
            div(
                div(
                    ui.input_text(label="Wpisz nazwę strony", id="site_name", value=""),
                    id="site_name_div"
                ),
                div(
                    ui.input_action_button(id="search_chrome_button", label="Search",
                                           style="height: 45px; width: 130px; background-color: #E9E9E9;"),
                    id="search_name_div"
                ),
                ui.input_date_range("dateNext", "Wybierz zakres czasu:",
                                    start="2023-01-01", end="2023-12-16",
                                    min="2023-01-01", max="2023-12-16"),

                id="chrome_left_side_bottom"
            ),
            div(

                ui.output_ui("data_page", width="120%;", height="120%;"), height="100%;",
                id="chrome_info_holder"
            ),
            id="chrome_left_side"
        ),
        div(

            div(
                output_widget("site_analysis", width="100%;", height="120%;"),
                id="chrome_right_side_top"
            ),
            div(
                output_widget("site_prediction_analysis", width="100%;", height="120%;"),
                id="chrome-right_side_bottom"
            ),

            id="chrome_right_side"
        ),

        id="master_div"
    ),
    div(
        id="footer_div"
    ),
    id="body_div"
)

app_ui = ui.page_fluid(
    head_content(
        tags.style((Path(__file__).parent / "style.css").read_text()),
    ),
    {"id": "main-content"},
    ui.navset_card_tab(
        ui.nav("Meegle", main_page, value="meegle", icon=""),
        ui.nav("Play Store", play_store_page),
        ui.nav("Aktywność", activity_page),
        ui.nav("Chrome", chrome_page),
        ui.nav("Maps", maps_page),
        ui.nav('Youtube-Gabi', youtube_page),
        id="tabset"
    ),
)


def server(input, output, session):
    # gabi youtube
    x = reactive.Value(0)

    @output
    @render_widget
    def youtube_plot():
        return youtube.youtube_charts(input)

    @output
    @render.text
    def comments():
        return youtube.comments_count_output()

    @output
    @render.text
    def title_of_video():
        return youtube.video_title(input)

    @output
    @render.text
    def which_person():
        return youtube.which_person()

    @output
    @render.text
    def how_many_subscribers():
        return youtube.how_many_subscribers(input)

    @render.ui
    @reactive.event(input.switch1)
    # @reactive.event(input.switch2)
    def update_switch2():
        ui.update_switch(input.switch2, value=not input.switch1())
        # ui.update_switch(input.switch2, label='hey',value=True)

    @render.ui
    @reactive.event(input.switch2)
    def update_switch1():
        ui.update_switch(input.switch1, value=not input.switch2())

    @output
    @render.text
    def description_text():
        return youtube.description_text(input)

    # @render.ui
    # @reactive.event(input.switch1)
    # def update_switch2():
    #     if input.switch1():
    #         ui.update_switch(input.switch2, value=False)
    #
    # @render.ui
    # @reactive.event(input.switch2)
    # def update_switch1():
    #     if input.switch2():
    #         ui.update_switch(input.switch1, value=False)

    # Ola - play store
    @output
    @render_widget
    def histogram():
        return play_store.histogram_play_store(input)

    @output
    @render_widget
    def wordcloud():
        return play_store.wordcloud_play_store(input)

    # Ola - google maps
    filtered_df = reactive.Value(None)

    @reactive.Effect
    @reactive.event(input.year_input, input.month_input)
    def _():
        df = google_maps.return_df()
        if input.year_input() == "WSZYSTKIE" and input.month_input() != "WSZYSTKIE":
            filtered_df.set(df[(df['Month'] == input.month_input())])
        elif input.year_input() != "WSZYSTKIE" and input.month_input() == "WSZYSTKIE":
            filtered_df.set(df[(df['Year'] == input.year_input())])
        elif input.year_input() != "WSZYSTKIE" and input.month_input() != "WSZYSTKIE":
            filtered_df.set(df[(df['Year'] == input.year_input()) & (df['Month'] == input.month_input())])
        else:
            filtered_df.set(df)

    @output
    @render_widget
    def map():
        return google_maps.return_map(filtered_df.get())

    @output
    @render.ui
    @reactive.event(input.search_chrome_button)
    def data_page():
        our_tag_list = ui.TagList()

        beg_data = str(input.dateNext()[0])
        end_data = str(input.dateNext()[1])

        looking_data = getImportantInfo([beg_data, end_data], input.site_name(), logged_person.get())

        total_entries = str(round(looking_data[0], 2))
        avg_day = str(round(looking_data[1], 2))
        avg_month = str(round(looking_data[2], 2))

        our_tag_list.append(ui.h5("Informacje na temat rejestrowanych wejść:",
                                  style="font-weight: bold; font-family: 'Times New Roman', Times, serif;"))
        our_tag_list.append(ui.h6("Łączna liczba aktywności: " + total_entries))
        our_tag_list.append(ui.h6("Średnia dzienna liczba aktywności: " + avg_day))
        our_tag_list.append(ui.h6("Średnia miesięczna liczba aktywności: " + avg_month))

        return our_tag_list

    @output
    @render_widget
    def site_analysis():

        if input.search_chrome_button() % 2 == 1:
            beg_data = str(input.dateNext()[0])
            end_data = str(input.dateNext()[1])

            looking_data = getFilteredCounts([beg_data, end_data],
                                             input.site_name(), logged_person.get())

            data_for_plot = pd.DataFrame(looking_data)
            data_for_plot['exact_date'] = pd.to_datetime(data_for_plot['exact_date'])
            data_for_plot = data_for_plot.sort_values(by="exact_date")

            fig = px.line(data_for_plot, x='exact_date', y='Count', labels={'Count': 'Count'},
                          title='Rejestrowana liczba wejść na stronę w przedziale czasu')

            fig.update_xaxes(title_text='Dokładna data')
            fig.update_yaxes(title_text='Liczba wejść na stronę')

            fig.update_traces(line_shape='linear', line=dict(color='#00b4d8'))

            return fig

    @output
    @render_widget
    def site_prediction_analysis():

        if input.search_chrome_button() % 2 == 1:
            beg_data = str(input.dateNext()[0])
            end_data = str(input.dateNext()[1])

            looking_data = getFilteredCounts([beg_data, end_data],
                                             input.site_name(), logged_person.get())
            prediction_data = predict_upcoming_values(looking_data)

            data_for_plot = pd.DataFrame(prediction_data)

            data_for_plot['ds'] = pd.to_datetime(data_for_plot['ds'])

            data_for_plot = data_for_plot[data_for_plot['ds'] >= '2024-01-01']

            data_for_plot = data_for_plot.sort_values(by="ds")

            fig = px.line(data_for_plot, x='ds', y='yhat_upper', labels={'yhat_upper': 'yhat_upper'},
                          title='Przewidywana liczba wejść na stronę w przedziale czasu')

            fig.update_xaxes(title_text='Dokładna data')
            fig.update_yaxes(title_text='Przewidywane wejścia')

            fig.update_traces(line_shape='linear', line=dict(color='#0077b6'))

            return fig

    # @output
    # @render.ui
    # @reactive.event(input.search)
    # def search_result():
    #
    #     user_text = input.search()
    #
    #     suggestion_list = give_search_result(user_text)
    #
    #     our_tag_list = ui.TagList()
    #
    #     our_tag_list.append(ui.h6(str(suggestion_list)))
    #
    #     return our_tag_list

    choose_1_plot = reactive.Value(False)
    choose_2_plot = reactive.Value(False)
    choose_3_plot = reactive.Value(False)
    choose_4_plot = reactive.Value(False)
    choose_5_plot = reactive.Value(False)
    choose_6_plot = reactive.Value(False)

    chooser_1_apps = reactive.Value([])
    chooser_2_apps = reactive.Value([])
    chooser_3_apps = reactive.Value([])
    chooser_4_apps = reactive.Value([])
    chooser_5_apps = reactive.Value([])
    chooser_6_apps = reactive.Value([])

    # put this value into method, to choose plot data
    logged_person = reactive.Value(0)

    @reactive.Effect
    @reactive.event(input.person_1_button)
    def change_logged_1():
        logged_person.set(1)
        ui.update_text("url", value="https://meegle.com/main_page/igorr")
        ui.update_text("url2", value="https://meegle.com/playstore/igorr")
        ui.update_text("url3", value="https://meegle.analytics.com/igorr")
        ui.update_text("url4", value="https://meegle.com/chrome/igorr")
        ui.update_text("url5", value="https://meegle.com/maps/igorr")
        ui.update_text("url6", value="https://meegle.com/youtube/gabrielam")

        ui.remove_ui("#popup_window")
        ui.remove_ui("#locker_button_div")

    @reactive.Effect
    @reactive.event(input.person_2_button)
    def change_logged_2():
        logged_person.set(2)
        ui.update_text("url", value="https://meegle.com/main_page/gabrielam")
        ui.update_text("url2", value="https://meegle.com/playstore/gabrielam")
        ui.update_text("url3", value="https://meegle.analytics.com/gabrielam")
        ui.update_text("url4", value="https://meegle.com/chrome/gabrielam")
        ui.update_text("url5", value="https://meegle.com/maps/gabrielam")
        ui.update_text("url6", value="https://meegle.com/youtube/gabrielam")

        ui.remove_ui("#popup_window")
        ui.remove_ui("#locker_button_div")

    @reactive.Effect
    @reactive.event(input.person_3_button)
    def change_logged_1():
        logged_person.set(3)
        ui.update_text("url", value="https://meegle.com/main_page/samsela")
        ui.update_text("url2", value="https://meegle.com/playstore/samsela")
        ui.update_text("url3", value="https://meegle.analytics.com/samsela")
        ui.update_text("url4", value="https://meegle.com/chrome/samsela")
        ui.update_text("url5", value="https://meegle.com/maps/samsela")
        ui.update_text("url6", value="https://meegle.com/youtube/gabrielam")

        ui.remove_ui("#popup_window")
        ui.remove_ui("#locker_button_div")

    @output
    @render.text
    def txt():
        return str(choose_1_plot())

    @output
    @render.text
    def txt2():
        return str(chooser_1_apps())

    @output
    @render.text
    def txt3():
        return str(choose_2_plot())

    @output
    @render.text
    def txt4():
        return str(chooser_2_apps())

    @reactive.Effect
    @reactive.event(input.plot_1_chooser)
    def marked_1():
        choose_1_plot.set(not choose_1_plot())

        apps_list = []

        if choose_1_plot() and input.chrome_activity_button() % 2 == 1:
            if "Search" not in chooser_1_apps():
                apps_list.append("Search")
                apps_list.append("Chrome Sync")

        if choose_1_plot() and input.playstore_activity_button() % 2 == 1:
            if "Play" not in chooser_1_apps():
                apps_list.append("Play")

        if choose_1_plot() and input.youtube_activity_button() % 2 == 1:
            if "YouTube" not in chooser_1_apps():
                apps_list.append("YouTube")

        if choose_1_plot() and input.gmail_activity_button() % 2 == 1:
            if "Gmail" not in chooser_1_apps():
                apps_list.append("Gmail")

        if choose_1_plot() and input.photos_activity_button() % 2 == 1:
            if "Photos" not in chooser_1_apps():
                apps_list.append("Photos")

        chooser_1_apps.set(apps_list)

    @reactive.Effect
    @reactive.event(input.plot_2_chooser)
    def marked_2():
        choose_2_plot.set(not choose_2_plot())

        apps_list = []

        if choose_2_plot() and input.chrome_activity_button() % 2 == 1:
            if "Search" not in chooser_2_apps():
                apps_list.append("Search")
                apps_list.append("Chrome Sync")

        if choose_2_plot() and input.playstore_activity_button() % 2 == 1:
            if "Play" not in chooser_2_apps():
                apps_list.append("Play")

        if choose_2_plot() and input.youtube_activity_button() % 2 == 1:
            if "YouTube" not in chooser_2_apps():
                apps_list.append("YouTube")

        if choose_2_plot() and input.gmail_activity_button() % 2 == 1:
            if "Gmail" not in chooser_2_apps():
                apps_list.append("Gmail")

        if choose_2_plot() and input.photos_activity_button() % 2 == 1:
            if "Photos" not in chooser_2_apps():
                apps_list.append("Photos")

        chooser_2_apps.set(apps_list)

    @reactive.Effect
    @reactive.event(input.plot_3_chooser)
    def marked_3():
        choose_3_plot.set(not choose_3_plot())

        apps_list = []

        if choose_3_plot() and input.chrome_activity_button() % 2 == 1:
            if "Search" not in chooser_3_apps():
                apps_list.append("Search")
                apps_list.append("Chrome Sync")

        if choose_3_plot() and input.playstore_activity_button() % 2 == 1:
            if "Play" not in chooser_3_apps():
                apps_list.append("Play")

        if choose_3_plot() and input.youtube_activity_button() % 2 == 1:
            if "YouTube" not in chooser_3_apps():
                apps_list.append("YouTube")

        if choose_3_plot() and input.gmail_activity_button() % 2 == 1:
            if "Gmail" not in chooser_3_apps():
                apps_list.append("Gmail")

        if choose_3_plot() and input.photos_activity_button() % 2 == 1:
            if "Photos" not in chooser_3_apps():
                apps_list.append("Photos")

        chooser_3_apps.set(apps_list)

    @reactive.Effect
    @reactive.event(input.plot_4_chooser)
    def marked_4():
        choose_4_plot.set(not choose_4_plot())

        apps_list = []

        if choose_4_plot() and input.chrome_activity_button() % 2 == 1:
            if "Search" not in chooser_4_apps():
                apps_list.append("Search")
                apps_list.append("Chrome Sync")

        if choose_4_plot() and input.playstore_activity_button() % 2 == 1:
            if "Play" not in chooser_4_apps():
                apps_list.append("Play")

        if choose_4_plot() and input.youtube_activity_button() % 2 == 1:
            if "YouTube" not in chooser_4_apps():
                apps_list.append("YouTube")

        if choose_4_plot() and input.gmail_activity_button() % 2 == 1:
            if "Gmail" not in chooser_4_apps():
                apps_list.append("Gmail")

        if choose_4_plot() and input.photos_activity_button() % 2 == 1:
            if "Photos" not in chooser_4_apps():
                apps_list.append("Photos")

        chooser_4_apps.set(apps_list)

    @reactive.Effect
    @reactive.event(input.plot_5_chooser)
    def marked_5():
        choose_5_plot.set(not choose_5_plot())

        apps_list = []

        if choose_5_plot() and input.chrome_activity_button() % 2 == 1:
            if "Search" not in chooser_5_apps():
                apps_list.append("Search")
                apps_list.append("Chrome Sync")

        if choose_5_plot() and input.playstore_activity_button() % 2 == 1:
            if "Play" not in chooser_5_apps():
                apps_list.append("Play")

        if choose_5_plot() and input.youtube_activity_button() % 2 == 1:
            if "YouTube" not in chooser_5_apps():
                apps_list.append("YouTube")

        if choose_5_plot() and input.gmail_activity_button() % 2 == 1:
            if "Gmail" not in chooser_5_apps():
                apps_list.append("Gmail")

        if choose_5_plot() and input.photos_activity_button() % 2 == 1:
            if "Photos" not in chooser_5_apps():
                apps_list.append("Photos")

        chooser_5_apps.set(apps_list)

    @reactive.Effect
    @reactive.event(input.plot_6_chooser)
    def marked_6():
        choose_6_plot.set(not choose_6_plot())

        apps_list = []

        if choose_6_plot() and input.chrome_activity_button() % 2 == 1:
            if "Search" not in chooser_6_apps():
                apps_list.append("Search")
                apps_list.append("Chrome Sync")

        if choose_6_plot() and input.playstore_activity_button() % 2 == 1:
            if "Play" not in chooser_6_apps():
                apps_list.append("Play")

        if choose_6_plot() and input.youtube_activity_button() % 2 == 1:
            if "YouTube" not in chooser_6_apps():
                apps_list.append("YouTube")

        if choose_6_plot() and input.gmail_activity_button() % 2 == 1:
            if "Gmail" not in chooser_6_apps():
                apps_list.append("Gmail")

        if choose_6_plot() and input.photos_activity_button() % 2 == 1:
            if "Photos" not in chooser_6_apps():
                apps_list.append("Photos")

        chooser_6_apps.set(apps_list)

    @reactive.Effect
    def _():
        if input.menu_button() % 2 == 1:

            div1 = div(

                ui.h6("Wybierz konto użytkownika:", style="font-weight: bold; font-size: 15px; "
                                                          "margin-left:auto, margin-right: auto; color: grey; padding-top: 10px;"),

                div(
                    div(
                        ui.img(src="ILetter.png", id="ILetter_login"),
                        id="first_person_login_left"
                    ),
                    div(
                        ui.h6("Igor Rudolf"),
                        id="first_person_login_right"
                    ),

                    ui.input_action_button(id="person_1_button", label="",
                                           style="height: 40px; width: 270px; padding-top: -78px; margin-top: -78px;"),

                    id="first_person_login_div"
                ),
                div(
                    div(
                        ui.img(src="GLetter.png", id="GLetter_login"),
                        id="second_person_login_left"
                    ),
                    div(
                        ui.h6("Gabriela Majstrak"),
                        id="second_person_login_right"
                    ),

                    ui.input_action_button(id="person_2_button", label="",
                                           style="height: 40px; width: 270px; padding-top: -78px; margin-top: -78px;"),

                    id="second_person_login_div"
                ),
                div(
                    div(
                        ui.img(src="ALetter.png", id="ALetter_login"),
                        id="third_person_login_left"
                    ),
                    div(
                        ui.h6("Aleksandra Samsel"),
                        id="third_person_login_right"
                    ),

                    ui.input_action_button(id="person_3_button", label="",
                                           style="height: 40px; width: 270px; padding-top: -78px; margin-top: -78px;"),

                    id="third_person_login_div"
                ),

            )

            ui.insert_ui(
                div(
                    div1,

                    id="popup_window"),
                selector="#popup_window_div",
                where="beforeEnd",
            )
        elif input.menu_button() > 0:
            pass
            ui.remove_ui("#popup_window")

    @output
    @render_widget
    def activity_plot_1():

        beg_day = int(input.range_days_plot1()[0])
        end_day = int(input.range_days_plot1()[1])

        if choose_1_plot():
            list_of_apps = chooser_1_apps.get()

            df_activity_1 = type1_data([beg_day, end_day], list_of_apps)
            fig = px.line(df_activity_1, x="Day", y="Count", color="Product Name", markers=True,
                          title='Zależność dni od liczby aktywności', width=580, height=350, template='plotly_white')

            fig.update_xaxes(title_text='Dokładny dzień zeszłego miesiąca')
            fig.update_yaxes(title_text='Liczba aktywności')

            fig.update_layout(legend_title_text='Usługa')

            return fig
        else:
            df_activity_1 = type1_data([beg_day, end_day], [])
            fig = px.line(df_activity_1, x="Day", y="Count", color="Product Name", markers=True,
                          title='Zależność dni od liczby aktywności', width=580, height=350, template='plotly_white')

            fig.update_xaxes(title_text='Dokładny dzień zeszłego miesiąca')
            fig.update_yaxes(title_text='Liczba aktywności')

            fig.update_layout(legend_title_text='Usługa')
            return fig

    @output
    @render.ui
    def value():
        return input.month_comparison_second_plot()

    @output
    @render_widget
    def activity_plot_2():

        beg_day = int(input.range_days_plot1()[0])
        end_day = int(input.range_days_plot1()[1])

        if choose_2_plot():
            list_of_apps = chooser_2_apps.get()

            needed_data = type2_data_fix(False, list_of_apps)

            df = pd.DataFrame(needed_data)

            pivot_df = df.pivot(index='Hour', columns='Product Name', values='Entry Count')

            # Create a stacked bar chart using Plotly
            fig = px.bar(pivot_df, x=pivot_df.index, y=pivot_df.columns, title='Zależność godziny od liczby aktywności',
                         labels={'value': 'Count', 'variable': 'Product Name'},
                         template='plotly_white', barmode='stack', width=580, height=350)

            fig.update_xaxes(title_text='Dokładna godzina')
            fig.update_yaxes(title_text='Liczba aktywności')

            fig.update_layout(legend_title_text='Usługa')

            return fig
        else:

            list_of_apps = chooser_2_apps.get()

            needed_data = type([beg_day, end_day], list_of_apps)

            df_activity_2 = type1_data([beg_day, end_day], [])
            fig = px.line(df_activity_2, x="Day", y="Count", color="Product Name", markers=True,
                          title='Zależność dni tygodnia od liczby aktywności')

            fig.update_xaxes(title_text='Dokładny dzień tygodnia')
            fig.update_yaxes(title_text='Liczba aktywności')

            fig.update_layout(legend_title_text='Usługa')
            return fig

    @output
    @render_widget
    def activity_plot_3():

        beg_day = int(input.range_days_plot3()[0])
        end_day = int(input.range_days_plot3()[1])

        if choose_3_plot():

            list_of_apps = chooser_3_apps.get()

            needed_data = type3_data([beg_day, end_day], list_of_apps)

            fig = px.bar(needed_data, x="Day Name", y="Count", color="Product Name",
                         title="Zależność dni tygodnia od liczby aktywności", width=580, height=350,
                         template="plotly_white")

            fig.update_xaxes(title_text='Dokładny dzień tygodnia')
            fig.update_yaxes(title_text='Liczba aktywności')

            fig.update_layout(legend_title_text='Usługa')

            return fig

        else:
            df_activity_3 = type1_data([beg_day, end_day], [])
            fig = px.line(df_activity_3, x="Day", y="Count", color="Product Name", markers=True,
                          title='Zależność dni tygodnia od liczby aktywności')

            fig.update_xaxes(title_text='Dokładny dzień tygodnia')
            fig.update_yaxes(title_text='Liczba aktywności')

            fig.update_layout(legend_title_text='Usługa')
            return fig

    @output
    @render.ui
    @reactive.event(input.plot_4_chooser)
    def activity_plot_4():
        beg_day = int(input.range_days_plot4()[0])
        end_day = int(input.range_days_plot4()[1])

        if choose_4_plot():
            list_of_apps = chooser_4_apps.get()

            needed_data = type4_data([beg_day, end_day], list_of_apps)

            out_tag_list = ui.TagList()

            list_of_apps_pd = needed_data

            rows_amount = len(list_of_apps_pd.axes[0])

            for i in range(rows_amount):
                element_name = str(list_of_apps_pd['Product Name'].iloc[i])
                needed_time = str(list_of_apps_pd['Time Difference'].iloc[i])

                out_tag_list.append(ui.h6(element_name + ' ' + needed_time,
                                          style="font-weight: bold; margin-left:auto; margin-right: auto;"))

            return out_tag_list

        else:
            df_activity_4 = type1_data([beg_day, end_day], [])
            fig = px.line(df_activity_4, x="Day", y="Count", color="Product Name", markers=True,
                          title='Product Count Over Days')
            return fig

    @output
    @render_widget
    def activity_plot_5():
        beg_day = int(input.range_days_plot4()[0])
        end_day = int(input.range_days_plot4()[1])

        if choose_5_plot():
            list_of_apps = chooser_5_apps.get()

            if len(list_of_apps) > 1 or len(list_of_apps) == 0:
                pass
            else:
                product = list_of_apps[0]

                hours_beg = int(input.range_hours_plot5()[0])
                hours_end = int(input.range_hours_plot5()[1])

                minutes_beg = int(input.range_minutes_plot5()[0])
                minutes_end = int(input.range_minutes_plot5()[1])

                needed_data = type5_data([hours_beg, hours_end], [minutes_beg, minutes_end], product)

                fig = go.Figure(go.Heatmap(
                    z=needed_data['Count'],
                    x=needed_data['hour'],
                    y=needed_data['minute'],
                    colorscale='Viridis',
                ))

                fig.update_layout(
                    xaxis=dict(title='Hour'),
                    yaxis=dict(title='Minute'),
                    title='Heatmap of Counts by Hour and Minute for ' + str(product)
                )

                return fig


        else:
            df_activity_5 = type1_data([beg_day, end_day], [])
            fig = px.line(df_activity_5, x="Day", y="Count", color="Product Name", markers=True,
                          title='Product Count Over Days')
            return fig

    @output
    @render.ui
    @reactive.event(input.plot_5_chooser)
    def activity_plot_5_potential():
        list_of_apps = chooser_5_apps.get()

        if len(list_of_apps) == 0 or len(list_of_apps) > 1:
            return ui.TagList(
                ui.h6("Należy wybrać tylko jedną aplikację")
            )

    @output
    @render.ui
    @reactive.event(input.plot_6_chooser)
    def activity_plot_6():
        beg_day = int(input.range_days_plot1()[0])
        end_day = int(input.range_days_plot1()[1])

        if choose_6_plot():
            list_of_apps = chooser_6_apps.get()

            needed_data = type6_data([beg_day, end_day], list_of_apps)

            our_tag_list = ui.TagList()

            rows_amount = len(needed_data.axes[0])

            our_tag_list.append(ui.h6("Wykaz przejść z jednej aplikacji do drugiej w danym miesiącu:",
                                      style='font-weight: bold; margin-left: auto; margin-right: auto;'))

            for i in range(rows_amount):
                previous_product = str(needed_data['previous_product'].iloc[i])
                next_product = str(needed_data['next_product'].iloc[i])
                amount = str(needed_data['change_count'].iloc[i])

                our_tag_list.append(ui.h6(previous_product + '-' + next_product + ' liczba przejść: ' + amount,
                                          style='margin-left: auto; margin-right: auto;'))

            return our_tag_list

        else:
            df_activity_6 = type1_data([beg_day, end_day], [])
            fig = px.line(df_activity_6, x="Day", y="Count", color="Product Name", markers=True,
                          title='Product Count Over Days')
            return fig


www_dir = Path(__file__).parent / "www"

app = App(app_ui, server, static_assets=www_dir)

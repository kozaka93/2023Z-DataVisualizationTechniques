from shiny import App, reactive, render, ui
from shiny.ui import div, h1, head_content, tags, HTML
from pathlib import Path
import pandas as pd
import plotly.express as px
from shinywidgets import output_widget, render_widget
import matplotlib.pyplot as plt
from sklearn.utils.estimator_checks import _yield_outliers_checks
from wordcloud import WordCloud
import re
import os


def youtube_div():
    youtube_page = div(
        div(
            div(
                ui.img(src="left_arrow.png", id="small_button"),
                ui.img(src="right_arrow.png", id="small_button"),
                ui.img(src="reload.png", id="small_button"),
                id="fun_buttons"
            ),
            div(
                ui.input_text(label="", id="url6", value="https://meegle.com/youtube"),
                id="url_div"
            ),

            id='header_div'
        ),
        div(
            div(ui.img(src='youtube_logo.png', id='youtube_icon')
                ),
            id='youtube_logo'
        ),
        div(

            div(
                div(
                    ui.layout_sidebar(
                        ui.sidebar(

                            width="0%;"
                        ),
                        output_widget("youtube_plot", width="100%;", height="100%;"), height="100%;"

                    ),

                    id="youtube_play_plot"
                ),
                div(
                    ui.output_text(id='title_of_video'),
                    id='video_title'
                ),
                div(
                    div(
                        ui.img(src="badi.png", id='gabi_pic'),
                        id='choose_user'
                    ),
                    div(
                        div(
                            ui.output_text(id="which_person"),
                            id='nick'
                        ),
                        div(
                            ui.output_text(id="how_many_subscribers"),
                            id='subscribers_count'

                        ),
                        id='nick_and_subscribers'
                    ),

                    div(

                        ui.input_action_button(id="click_subscribe", label="subscribe"),
                        div(
                            ui.img(src='notification.png', id="subscribe_bell"),
                            id='bell'
                        ),
                        div(
                            "Subscribe",
                            id='subscribe_text'
                        ),


                        id='subscribe'
                    ),

                    div(
                        div(
                            ui.img(src='like.png', id='like_button'),
                            id='like'
                        ),
                        div(
                            ui.img(src='dislike.png', id='dislike_button'),
                            id='dislike'
                        ),

                        id='likes'
                    ),


                    id="youtube_play_button"
                ),
                id='youtube_videos_charts'
            ),
            div(
                div(
                    ui.input_switch('switch1', 'Sprawdź twoje top10 kanałów w danym okresie', value=True),
                    id='top_channels'
                ),
                div(
                    ui.input_switch('switch2',
                                    "Sprawdź w jakich porach najczęściej oglądałeś youtube w każdym dniu tygodnia"),
                    id='time_watch'
                ),
                div(
                    ui.input_date_range("yt_date", "Wybierz zakres czasu:",
                                        start='2017-05-09', end='2023-12-05',
                                        min=min(gabi_df['Date']), max=max(gabi_df['Date'])
                                        ),

                    id='watching_date'
                ),

                id="youtube_plot_chooser"
            ),

            id='videos_div'
        ),
        div(
            div(
                div(
                    div(
                        'Opis',
                        id='description_caption'
                    ),
                    div(
                        ui.output_text(id="description_text"),
                        id='description'
                    ),
                    id='video_description'
                ),
                div(
                    div(
                        ui.output_text(id='comments'),
                        id='comments_count_output'
                    ),

                    id="comment_count"
                ),
                div(
                    div(
                        div(
                            ui.img(src='frenchie.png', id='frenchie_pic'),
                            id='profile_picture1'
                        ),
                        div(
                            div(
                                '@frenchie',
                                id='nickname1'
                            ),
                            div(
                                'Gabi pierwszy komentarz opublikowała 28 lutego 2016 roku.',
                                id='text1'
                            ),

                            id='comment1_text'
                        ),

                        id='comment1'
                    ),
                    div(
                        div(
                            ui.img(src='whippet.png', id='whippet_pic'),
                            id='profile_picture2'
                        ),
                        div(
                            div(
                                "@whippet",
                                id='nickname2'
                            ),
                            div(
                                'Najwięcej kometarzy Gabi dodała w 2019 roku, komentowała wtedy 61 razy!',
                                id='text2'
                            ),

                            id='comment2_text'
                        ),

                        id='comment2'
                    ),
                    div(
                        div(
                            ui.img(src='corgi.png', id='corgi_pic'),
                            id='profile_picture2'
                        ),
                        div(
                            div(
                                "@corgi",
                                id='nickname3'
                            ),
                            div(
                                'Najdłuższy Kometarz jaki Gabi opublikowała miał 1051 znaków!',
                                id ='text3'
                            ),

                            id='comment3_text'
                        ),

                        id='comment3'
                    ),


                    id='comments_section'
                ),

                id='youtube_comment_charts'
            ),

            div(
                id='youtube_comment_buttons'
            ),

            id='comments_div'
        ),

        id='body_div'
    )

    return youtube_page


def process_file(file_path):
    with open(file_path, 'r', encoding='utf-8') as file:
        data = file.read()

    records = re.findall(r'YouTube.*?tutaj\.', data, re.DOTALL)

    def remove_part(record):
        modified_record = re.sub(r'Produkty:.*', '', record, flags=re.DOTALL)
        return modified_record

    records = [remove_part(record) for record in records]

    def remove_first_two_lines(record):
        lines = record.split('\n')
        modified_record = '\n'.join(lines[2:])
        return modified_record

    records = [remove_first_two_lines(record) for record in records]

    def replace_second_line(record):
        lines = record.split('\n')
        if len(lines) > 1 and lines[1].startswith('Obejrzane o:'):
            lines[1] = 'reklama'
        modified_record = '\n'.join(lines)
        return modified_record

    records = [replace_second_line(record) for record in records]
    records = [record for record in records if len(record.split('\n')) > 3]

    titles = []
    channels = []
    dates = []

    for record in records:
        lines = record.split('\n')
        titles.append(lines[0])
        channels.append(lines[1])
        dates.append(lines[2])

    df = pd.DataFrame({'Tytuł': titles, 'Kanał': channels, 'Data': dates})
    df[['Day', 'Month', 'Year', 'Time', 'cet']] = df['Data'].str.split(' ', expand=True)
    df.drop(['Data', 'cet'], axis=1, inplace=True)
    df['Year'] = df['Year'].str.replace(r'\D', '')
    df['Year'] = pd.to_numeric(df['Year'], errors='coerce')
    df['Day'] = pd.to_numeric(df['Day'], errors='coerce')
    df = df.dropna(subset=['Year'])
    df = df.dropna(subset=['Day'])

    month_mapping = {
        'sty': 1,
        'lut': 2,
        'mar': 3,
        'kwi': 4,
        'maj': 5,
        'cze': 6,
        'lip': 7,
        'sie': 8,
        'wrz': 9,
        'paÅº': 10,
        'lis': 11,
        'gru': 12
    }

    df['Month'] = df['Month'].replace(month_mapping)
    df['Time'] = pd.to_datetime(df['Time'], format='%H:%M:%S', errors='coerce')
    df['Time'] = df['Time'].dt.time

    df['Year'] = df['Year'].astype(int)
    df['Day'] = df['Day'].astype(int)
    df['Date'] = pd.to_datetime(df[['Year', 'Month', 'Day']], errors='coerce')
    df['DayOfWeek'] = df['Date'].dt.dayofweek

    return df


file_path = 'data/historia-oglądania-gabi.csv'
gabi_df = process_file(file_path)


def youtube_charts(Input):
    selected_dates = gabi_df[
        (gabi_df['Date'] >= str(Input.yt_date()[0])) &
        (gabi_df['Date'] <= str(Input.yt_date()[1]))
        ]

    selected_dates = selected_dates.groupby('Kanał').size().reset_index(name='n')
    selected_dates = selected_dates[selected_dates['Kanał'] != 'reklama']
    selected_dates = selected_dates.sort_values(by='n', ascending=False)
    selected_dates = selected_dates.head(10)
    fig1 = px.bar(selected_dates, y='Kanał', x='n', template='plotly_dark', color='Kanał',color_discrete_sequence=['red'],
                  height=420, width=800)
    fig1.update_layout(showlegend=False,
                       xaxis_title='Liczba wystąpień',
                       yaxis_title='Kanał'
                       )
    gabi_df['Time'] = pd.to_timedelta(gabi_df['Time'].astype(str))
    selected_dates = gabi_df[
        (gabi_df['Date'] >= str(Input.yt_date()[0])) &
        (gabi_df['Date'] <= str(Input.yt_date()[1]))
        ]


    fig2 = px.imshow(
        selected_dates.groupby(['DayOfWeek', pd.cut(selected_dates['Time'].dt.total_seconds(), bins=24)])[
            'Tytuł'].count().unstack().fillna(0),
        labels=dict(color="Liczba Wystąpień"),
        x=['00:00 - 01:00', '01:00 - 02:00', '02:00 - 03:00', '03:00 - 04:00',
           '04:00 - 05:00', '05:00 - 06:00', '06:00 - 07:00', '07:00 - 08:00',
           '08:00 - 09:00', '09:00 - 10:00', '10:00 - 11:00', '11:00 - 12:00',
           '12:00 - 13:00', '13:00 - 14:00', '14:00 - 15:00', '15:00 - 16:00',
           '16:00 - 17:00', '17:00 - 18:00', '18:00 - 19:00', '19:00 - 20:00',
           '20:00 - 21:00', '21:00 - 22:00', '22:00 - 23:00', '23:00 - 00:00'],
        y=['Poniedziałek', 'Wtorek', 'Środa', 'Czwartek', 'Piątek', 'Sobota', 'Niedziela'],
        color_continuous_scale="Emrld",
        template='plotly_dark',
        height=420,
        width=750
    )
    fig2.update_layout(
        xaxis_title='przedział godzinowy',
        yaxis_title='Dzień Tygodnia'

    )


    if Input.switch1()==True and Input.switch2()==False:
        return fig1

    if Input.switch2()==True and Input.switch1()==False:
        return fig2



gabi_comments = pd.read_csv(Path(__file__).parent / "data/komentarze.csv", sep=";")

def comments_count_output():
    num_rows = gabi_comments.shape[0]
    return f'{num_rows} komentarzy'

def video_title(Input):
    if Input.switch1()==True and Input.switch2()==False:
        return "Twoje Top 10 najczęściej oglądanych kanałów"
    if Input.switch2()==True and Input.switch1()==False:
        return "W jakich porach najczęsciej oglądałeś youtube"
    if Input.switch1()==True and Input.switch2()==True:
        return "Wybierz jedną analizę do oglądania!"
    if Input.switch2()==False and Input.switch1()==False:
        return "Wybierz po prawo analize do oglądania!"



def which_person():
    return "Gabi"

def how_many_subscribers(Input):

    return f"{Input.click_subscribe()} subskrybentów "

def description_text(Input):
    if Input.switch1()==True and Input.switch2()==False:
        return "Powyższy wykres przedstwia 10 najczęsciej oglądanych kanałów przez Gabi w okresie czasu, ktory możesz wybrać z prawej strony. " \
               "Nie ma zdziwień, że najczęściej są oglądane kanały Eurowizyjne. W okresie obejmującym pierwszy i  drugi semestr widzimy kanały z treściami matematycznymi. " \
               "Natomiast w okresie obejmującym trzeci semstr widzimy więcej kanałów z treściami informatycznymi."
    if Input.switch2()==True and Input.switch1()==False:
        return "Powyższy wykres pokazuje w jakich przedziałach czasowych w danym dniu tygodnia najczęściej Gabi oglądała Youtuba. Gabi raczej nie ogląda " \
               "youtuba po godzinie 1 w nocy. Widzimy, że zdecydowanie częściej zagląda tam w weekend. Jeśli Gabi ogląda youtuba w porannych godzinach " \
               "to robi to od poniedziałku do piątku, ponieważ w weekedny raczej nie wstaje przed 10."




from shiny import App, reactive, render, ui
from shiny.ui import div, h1, head_content, tags, HTML
from pathlib import Path
import pandas as pd
import plotly.express as px
from shinywidgets import output_widget, render_widget
import matplotlib.pyplot as plt
from wordcloud import WordCloud


def dataLib():
    df = pd.read_csv(Path(__file__).parent / "data/Library_csv.csv", sep=";")
    df['acquisitionTime'] = df['acquisitionTime'].str[:10]
    df['date'] = pd.to_datetime(df['acquisitionTime'])
    return df


def dataIns():
    df = pd.read_csv(Path(__file__).parent / "data/Installs_csv.csv", sep=";")
    df['dateUpdate'] = pd.to_datetime(df['lastUpdateTime']).dt.normalize()
    df['dateInstall'] = pd.to_datetime(df['firstInstallationTime']).dt.normalize()
    return df


def play_store_div():

    play_store = div(
        div(
            div(
                ui.img(src="left_arrow.png", id="small_button"),
                ui.img(src="right_arrow.png", id="small_button"),
                ui.img(src="reload.png", id="small_button"),
                id="fun_buttons"
            ),
            div(
                ui.input_text(label="", id="url2", value="https://meegle.com/playstore"),
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
                    ui.input_date_range("date", "Wybierz zakres czasu:",
                                        start='2020-07-22', end='2022-10-15',
                                        min=min(dataLib()['date']), max=max(dataLib()['date'])),
                    ui.input_radio_buttons("bins", "Wybierz okres czasu dla słupków:", selected=2,
                                           choices={0: 'Rok', 1: 'Pół roku', 2: 'Miesiąc', 3: 'Tydzień', 4: 'Dzień'}),
                    # ui.output_text("text")
                ),
                ui.panel_main(
                    output_widget("histogram")
                )
            ),
            ui.layout_sidebar(
                ui.panel_sidebar(
                    ui.input_slider("range", "Wybierz zakres dla liczby instalacji aplikacji",
                                    min=1, max=max(dataIns().groupby(['title'])['Unnamed: 0'].count()), value=[2, 5])
                ),
                ui.panel_main(
                    output_widget("wordcloud")
                )
            ),
            id="master_div"
        ),
        div(
            id="footer_div"
        ),
        id="body_div"
    )

    return play_store


def histogram_play_store(Input):

    days = (Input.date()[1] - Input.date()[0]).days
    bins_list = [round(days / 365, 0), round(days / 183, 0), round(days / 30, 0), round(days / 7, 0), days]
    df = dataLib()
    df = df.loc[(df['date'] >= str(Input.date()[0])) & (df['date'] <= str(Input.date()[1]))]
    fig = px.histogram(
        df, x="date", nbins=int(bins_list[int(Input.bins())]),
        title="Histogram pobranych aplikacji"
    )

    return fig


def wordcloud_play_store(Input):
    df = dataIns()
    df_agg = df.groupby(['title'])['documentType'].count().reset_index('title')
    df_agg = df_agg.loc[
        (df_agg['documentType'] >= int(Input.range()[0])) & (df_agg['documentType'] <= int(Input.range()[1]))]
    df = df[df['title'].apply(lambda x: x in list(df_agg['title']))]
    titles_modified = df['title'].str.replace(' ', '_', regex=False)
    long_string = ' '.join(titles_modified)
    long_string = long_string.replace('.', '_')
    long_string = long_string.replace(':', '')
    long_string = long_string.replace('[', '')
    long_string = long_string.replace(']', '')
    long_string = long_string.replace('-', '_')

    plt.figure(figsize=(12, 9))
    word_cloud = WordCloud(background_color='white', width=1000, height=500, colormap='plasma').generate(
        long_string)

    fig = px.imshow(word_cloud)
    fig.update_layout(coloraxis_showscale=False, width=900, height=450, margin=dict(l=10, r=10, t=10, b=10),
                      title_text="Word colud dla liczby instalacji")
    fig.update_xaxes(showticklabels=False)
    fig.update_yaxes(showticklabels=False)

    return fig

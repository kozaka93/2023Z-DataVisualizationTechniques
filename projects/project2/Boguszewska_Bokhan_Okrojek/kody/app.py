import dash
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from dash import dcc, html

radar_plot = go.Figure()
global button_id
app = dash.Dash(__name__)


def get_season(date):
    if date.month in [3, 4, 5]:
        return 'Spring'
    elif date.month in [6, 7, 8]:
        return 'Summer'
    elif date.month in [9, 10, 11]:
        return 'Fall'
    else:
        return 'Winter'


app.layout = html.Div(
    style={"font-family": "Ubuntu", "background-color": "#F2E9DD", 'margin-top':'-3.0%'},
    children=[
        html.Div(
            [
                html.H1(
                    "RhythmicLife: Tune into Your Day",
                    style={"text-align": "center", 'padding': '1%', 'fontSize': 80, 'color': "#1DB954", 'font-family': 'Cooper Black', 'margin-bottom': '10px'},
                ),
            ],
            style={'text-align': 'center', 'margin-bottom': '10px'}),
        html.Div(
            [
                html.H1(
                    "Choose One Of Us",
                    style={"text-align": "center"},
                ),
                html.Div(
                    [
                        html.H2("Kasia" + "'s data was chosen."),
                    ], id='chosen_person'
                ),
                html.Button(
                    id='buttonBogusia',
                    children='Bogusia',
                    n_clicks=0,
                    style={'background-color': "#1DB954", 'font-size': '16px', 'margin': '10px',
                           'border-radius': '5px', 'font-size': '28px', 'padding': '16px 40px',
                           'text-shadow': '0px 1px 0px #2f6627', 'font-family': 'Ubuntu', 'color': 'white',
                           'width': '15%'}
                ),

                html.Button(
                    id='buttonWiktoria',
                    children='Wiktoria',
                    n_clicks=0,
                    style={'background-color': "#1DB954", 'font-size': '16px', 'margin': '10px',
                           'border-radius': '5px', 'font-size': '28px', 'padding': '16px 40px',
                           'text-shadow': '0px 1px 0px #2f6627', 'font-family': 'Ubuntu', 'color': 'white',
                           'width': '15%'}
                ),
                html.Button(
                    id='buttonKasia',
                    children='Kasia',
                    n_clicks=0,
                    style={'background-color': "#1DB954", 'font-size': '16px', 'margin': '10px',
                           'border-radius': '5px', 'font-size': '28px', 'padding': '16px 40px',
                           'text-shadow': '0px 1px 0px #2f6627', 'font-family': 'Ubuntu', 'color': 'white',
                           'width': '15%'}
                ),
            ],
            style={'text-align': 'center'}),
        dcc.Tabs(
            [
                dcc.Tab(
                    label="Most Listend Artists And Albums",
                    children=[
                        html.Div(
                            [
                                html.H1(
                                    "Top 10 most listened artists in this semester",
                                    style={"text-align": "center"}),
                                dcc.Loading(
                                    color="#1DB954",
                                    type="circle",
                                    children=[
                                        html.Div(
                                            [

                                            ],
                                            className="row",
                                            id='div_1_content',
                                            style={'text-align': 'center'}
                                        )]),
                            ], style={'padding': '0% 2% 0% 2%'}
                        ),
                        html.Div(
                            [
                                html.H1(
                                    "Top 10 most listened albums in this semester",
                                    style={"text-align": "center"}
                                ),
                                dcc.Loading(
                                    color="#1DB954",
                                    type="circle",
                                    children=[
                                        html.Div(
                                            [

                                            ],
                                            className="row",
                                            id='div_2_content',
                                            style={'text-align': 'center'}
                                        )
                                    ]),
                            ], style={'padding': '2%'}
                        ),
                        html.Div(
                            children=[
                                html.P("Boguszewska Wiktoria",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                                html.P("Bokhan Katsiaryna",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                                html.P("Okrojek Bogumiła",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'})
                            ],
                            style={
                                'display': 'flex',
                                'flex-direction': 'column',
                                "height": "10%",
                                "background-color": "#1DB953",
                                "margin-top": "20px",
                            }),
                    ], style={'fontSize': 20, "background-color": "#1DB953", 'color': 'white'}
                ),
                dcc.Tab(
                    label="Productivity VS Music Preferences",
                    children=[
                        dcc.Loading(
                            color="#1DB954",
                            type="circle",
                            children=[
                                html.Div(
                                    [

                                    ],
                                    id='div_3_content'
                                ), ]),
                        html.Div(
                            [
                                html.H2(
                                    "Music genre/artists/songs(and features) preferences depending on Productivity",
                                    style={"text-align": "center"},
                                ),
                                html.Div(
                                    [
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose Productivity Level:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Checklist(
                                                    id="checkboxes1",
                                                    options=[
                                                        {
                                                            "label": "High",
                                                            "value": "High",
                                                        },
                                                        {
                                                            "label": "Medium",
                                                            "value": "Medium",
                                                        },
                                                        {
                                                            "label": "Low",
                                                            "value": "Low",
                                                        },
                                                    ],
                                                    value=["High", "Medium", "Low"],
                                                    style={"font-size": "20px"},
                                                ),
                                            ],
                                            style={
                                                "width": "20%",
                                                "float": "left",
                                                "margin": "2%",
                                            },
                                        ),
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose Grouping:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Dropdown(
                                                    id="grouping-dropdown",
                                                    options=[
                                                        {
                                                            "label": "Artists",
                                                            "value": "master_metadata_album_artist_name",
                                                        },
                                                        {
                                                            "label": "Track Name",
                                                            "value": "master_metadata_track_name",
                                                        },
                                                        {
                                                            "label": "Track Genre",
                                                            "value": "track_genre",
                                                        },
                                                        {
                                                            "label": "Album",
                                                            "value": "master_metadata_album_album_name",
                                                        },
                                                    ],
                                                    value="track_genre",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right",
                                            },
                                        ),
                                    ],
                                    style={"display": "flex", "margin": "0% 2% 0% 2%"},
                                ),
                                dcc.Loading(
                                    color="#1DB954",
                                    type="circle",
                                    children=[
                                        html.Div(
                                            dcc.Graph(id="genre_preference",
                                                      config={"modeBarButtonsToRemove": ["toImage", "hoverClosestPie",
                                                                                         "zoom2d", "pan2d", "select2d",
                                                                                         "lasso2d",
                                                                                         "zoomIn2d", "zoomOut2d",
                                                                                         "autoScale2d", "resetScale2d"]}
                                                      ),
                                            style={
                                                "width": "93%",
                                                "box-shadow": "2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)",
                                                "margin": "auto",
                                                "background-color": "#E8DAC5",
                                            },
                                        ), ]),
                                html.Div(
                                    [
                                        html.Div(
                                            dcc.Loading(
                                                color="#1DB954",
                                                type="circle",
                                                children=[
                                                    dcc.Graph(
                                                        id="radar_plot",
                                                        figure=radar_plot,
                                                        config={"modeBarButtonsToRemove": ["toImage", "hoverClosestPie",
                                                                                           "zoom2d", "pan2d",
                                                                                           "select2d", "lasso2d",
                                                                                           "zoomIn2d", "zoomOut2d",
                                                                                           "autoScale2d",
                                                                                           "resetScale2d"]}
                                                    ), ]),
                                            style={
                                                "width": "47.5%",
                                                "box-shadow": "2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)",
                                                "margin": "1.5%",
                                                "background-color": "#E8DAC5",
                                            },
                                        ),
                                        html.Div(
                                            [
                                                html.H2(
                                                    "How to read this radar plot?",
                                                    style={"text-align": "center"},
                                                ),
                                                dcc.Tabs(
                                                    [
                                                        dcc.Tab(label='acousticness', children=[
                                                            # Content for Tab 1
                                                            html.H3(
                                                                "A confidence measure from 0.0 to 1.0 of whether the "
                                                                "track is acoustic. 1.0 represents high confidence "
                                                                "the track is acoustic.",
                                                                style={"text-align": "left", 'margin': '10px'}),
                                                        ], style={"background-color": "#1DB953", 'color': 'white'}),
                                                        dcc.Tab(label='danceability', children=[
                                                            # Content for Tab 2
                                                            html.H3(
                                                                "Danceability describes how suitable a track is for "
                                                                "dancing based on a combination of musical elements "
                                                                "including tempo, rhythm stability, beat strength, "
                                                                "and overall regularity. A value of 0.0 is least "
                                                                "danceable and 1.0 is most danceable.",
                                                                style={"text-align": "left", 'margin': '10px'}),
                                                        ], style={"background-color": "#1DB953", 'color': 'white'}),
                                                        dcc.Tab(label='energy', children=[
                                                            html.H3(
                                                                "Energy is a measure from 0.0 to 1.0 and represents a"
                                                                "perceptual measure of intensity and activity. "
                                                                "Typically, energetic tracks feel fast, loud, "
                                                                "and noisy. For example, death metal has high energy, "
                                                                "while a Bach prelude scores low on the scale. "
                                                                "Perceptual features contributing to this attribute "
                                                                "include dynamic range, perceived loudness, timbre, "
                                                                "onset rate, and general entropy.",
                                                                style={"text-align": "left", 'margin': '10px'}),
                                                        ], style={"background-color": "#1DB953", 'color': 'white'}),
                                                        dcc.Tab(label='loudness', children=[
                                                            html.H3(
                                                                "Ranges from 0 (least loud) to 1 (loudest). Loudness"
                                                                "values are averaged across the entire track and are "
                                                                "useful for comparing relative loudness of tracks. "
                                                                "Loudness is the quality of a sound that is the "
                                                                "primary psychological correlate of physical strength "
                                                                "(amplitude).",
                                                                style={"text-align": "left", 'margin': '10px'}),
                                                        ], style={"background-color": "#1DB953", 'color': 'white'}),
                                                    ],
                                                    style={"width": "100%"},
                                                ),
                                                dcc.Tabs(
                                                    [
                                                        dcc.Tab(label='speechiness', children=[
                                                            html.H3(
                                                                "Speechiness detects the presence of spoken words in "
                                                                "a track. Ranges from 0 (least speechiness) to 1 ("
                                                                "most speechiness).",
                                                                style={"text-align": "left", 'margin': '10px'}),
                                                        ], style={"background-color": "#1DB953", 'color': 'white'}),
                                                        dcc.Tab(label='valence', children=[
                                                            html.H3(
                                                                "A confidence measure from 0.0 to 1.0 of whether the "
                                                                "track is acoustic. 1.0 represents high confidence "
                                                                "the track is acoustic.",
                                                                style={"text-align": "left", 'margin': '10px'}),

                                                        ], style={"background-color": "#1DB953", 'color': 'white'}),
                                                        dcc.Tab(label='liveness', children=[
                                                            html.H3(
                                                                "Detects the presence of an audience in the "
                                                                "recording. Higher liveness values represent an "
                                                                "increased probability that the track was performed "
                                                                "live. A value above 0.8 provides strong likelihood "
                                                                "that the track is live.",
                                                                style={"text-align": "left", 'margin': '10px'}),

                                                        ], style={"background-color": "#1DB953", 'color': 'white'}),
                                                    ],
                                                    style={"width": "100%"},
                                                ),
                                            ],
                                            style={
                                                "width": "47.5%",
                                                "box-shadow": "2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)",
                                                "margin": "1.5%",
                                                "background-color": "#E8DAC5",
                                            },
                                        ),
                                    ],
                                    style={"display": "flex", "margin": "2%"},
                                )
                            ]
                        ),
                        html.Div(
                            children=[
                                html.P("Boguszewska Wiktoria",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                                html.P("Bokhan Katsiaryna",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                                html.P("Okrojek Bogumiła",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'})
                            ],
                            style={
                                'display': 'flex',
                                'flex-direction': 'column',
                                "height": "10%",
                                "background-color": "#1DB953",
                                "margin-top": "20px",
                            }),
                    ], style={'fontSize': 20, "background-color": "#1DB953", 'color': 'white'}
                ),
                dcc.Tab(
                    label="Date",
                    children=[

                        html.Div(
                            [
                                html.H1(
                                    "The total amount of time spent on Spotify vs different time periods",
                                    style={"text-align": "center"},
                                ),
                                html.Div(
                                    [
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose Grouping - date range:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Dropdown(
                                                    id="grouping-dropdown3",
                                                    options=[
                                                        {
                                                            "label": "Full Spotify History",
                                                            "value": "full",
                                                        },
                                                        {
                                                            "label": "This semester",
                                                            "value": "sem",
                                                        }
                                                    ],
                                                    value="full",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right",
                                            },
                                        ),
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose Grouping - time frame:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Dropdown(
                                                    id="grouping-dropdown2",
                                                    options=[
                                                        {
                                                            "label": "days",
                                                            "value": "Date",
                                                        },
                                                        {
                                                            "label": "day of the week",
                                                            "value": "day-name",
                                                        },
                                                        {
                                                            "label": "week number",
                                                            "value": "week_number",
                                                        },
                                                        {
                                                            "label": "month",
                                                            "value": "month",
                                                        },
                                                        {
                                                            "label": "year",
                                                            "value": "year",
                                                        }
                                                    ],
                                                    value="Date",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right",
                                            },
                                        ),
                                    ],
                                    style={"display": "flex", "margin": "4%"},
                                ),
                                dcc.Loading(
                                    color="#1DB954",
                                    type="circle",
                                    children=[
                                        html.Div(
                                            dcc.Graph(id="ListeningTimeVsDate",
                                                      config={"modeBarButtonsToRemove": ["toImage", "hoverClosestPie",
                                                                                         "zoom2d", "pan2d", "select2d",
                                                                                         "lasso2d",
                                                                                         "zoomIn2d", "zoomOut2d",
                                                                                         "autoScale2d", "resetScale2d"]}
                                                      ),
                                            style={
                                                "width": "92%",
                                                "box-shadow": "2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)",
                                                "margin": "auto",
                                                "background-color": "#E8DAC5",
                                            },
                                        ),
                                    ]
                                ),
                            ]
                        ),
                        html.Div(
                            [
                                html.H2(
                                    "The average amount of time spent on Spotify during different time periods",
                                    style={"text-align": "center"},
                                ),
                                html.Div(
                                    [
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose included data:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Checklist(
                                                    id="checkboxes2",
                                                    options=[
                                                        {
                                                            "label": "Include days without Spotify use",
                                                            "value": "all",
                                                        }
                                                    ],
                                                    value=["all"],
                                                    style={"font-size": "20px"},
                                                ),
                                            ],
                                            style={
                                                "width": "20%",
                                                "float": "left",
                                            },
                                        ),
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose Grouping - time frame:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Dropdown(
                                                    id="grouping-dropdown4",
                                                    options=[
                                                        {
                                                            "label": "day of the week",
                                                            "value": "day-name",
                                                        },
                                                        {
                                                            "label": "week number",
                                                            "value": "week_number",
                                                        },
                                                        {
                                                            "label": "month",
                                                            "value": "month",
                                                        },
                                                        {
                                                            "label": "season",
                                                            "value": "season",
                                                        },
                                                        {
                                                            "label": "year",
                                                            "value": "year",
                                                        }
                                                    ],
                                                    value="day-name",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right",
                                            },
                                        ),
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose Grouping - date range:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Dropdown(
                                                    id="grouping-dropdown5",
                                                    options=[
                                                        {
                                                            "label": "Full Spotify History",
                                                            "value": "full",
                                                        },
                                                        {
                                                            "label": "This semester",
                                                            "value": "sem",
                                                        }
                                                    ],
                                                    value="full",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right",
                                            },
                                        ),
                                    ],
                                    style={"display": "flex", "margin": "4%"},
                                ),
                                dcc.Loading(
                                    color="#1DB954",
                                    type="circle",
                                    children=[
                                        html.Div(
                                            dcc.Graph(id="MeanListeningTimeVsTime",
                                                      config={"modeBarButtonsToRemove": ["toImage", "hoverClosestPie",
                                                                                         "zoom2d", "pan2d", "select2d",
                                                                                         "lasso2d",
                                                                                         "zoomIn2d", "zoomOut2d",
                                                                                         "autoScale2d", "resetScale2d"]}
                                                      ),
                                            style={
                                                "width": "92%",
                                                "box-shadow": "2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)",
                                                "margin": "auto",
                                                "background-color": "#E8DAC5",
                                            },
                                        ), ]),
                            ]
                        ),
                        html.Div(
                            [
                                html.H1(
                                    "The daily amount of time spent on Spotify vs different time periods",
                                    style={"text-align": "center"},
                                ),
                                html.Div(
                                    [
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose included data:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Checklist(
                                                    id="checkboxes3",
                                                    options=[
                                                        {
                                                            "label": "Include days without Spotify use",
                                                            "value": "all",
                                                        }
                                                    ],
                                                    value=["all"],
                                                    style={"font-size": "20px"},
                                                ),
                                            ],
                                            style={
                                                "width": "20%",
                                                "float": "left",
                                                "margin": "2%",
                                            },
                                        ),
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose Grouping - date range:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Dropdown(
                                                    id="grouping-dropdown6",
                                                    options=[
                                                        {
                                                            "label": "Full Spotify History",
                                                            "value": "full",
                                                        },
                                                        {
                                                            "label": "This semester",
                                                            "value": "sem",
                                                        }
                                                    ],
                                                    value="full",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right",
                                            },
                                        ),
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose Grouping - time frame:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Dropdown(
                                                    id="grouping-dropdown7",
                                                    options=[
                                                        {
                                                            "label": "day of the week",
                                                            "value": "day-name",
                                                        },
                                                        {
                                                            "label": "month",
                                                            "value": "month",
                                                        },
                                                        {
                                                            "label": "season",
                                                            "value": "season",
                                                        },
                                                        {
                                                            "label": "year",
                                                            "value": "year",
                                                        }
                                                    ],
                                                    value="day-name",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right",
                                            },
                                        ),
                                    ],
                                    style={"display": "flex", "margin": "2%"},
                                ),
                                dcc.Loading(
                                    color="#1DB954",
                                    id="loading-listening-time-graph1",
                                    type="circle",
                                    children=[
                                        html.Div(
                                            dcc.Graph(id="ListeningTimeVsDateBoxPlot",
                                                      config={"modeBarButtonsToRemove": ["toImage", "hoverClosestPie",
                                                                                         "zoom2d", "pan2d", "select2d",
                                                                                         "lasso2d",
                                                                                         "zoomIn2d", "zoomOut2d",
                                                                                         "autoScale2d", "resetScale2d"]}
                                                      ),
                                            style={
                                                "width": "92%",
                                                "box-shadow": "2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)",
                                                "margin": "auto",
                                                "background-color": "#E8DAC5",
                                            },
                                        ),
                                    ]),
                            ],
                        ),
                        html.Div(
                            children=[
                                html.P("Boguszewska Wiktoria",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                                html.P("Bokhan Katsiaryna",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                                html.P("Okrojek Bogumiła",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'})
                            ],
                            style={
                                'display': 'flex',
                                'flex-direction': 'column',
                                "height": "10%",
                                "background-color": "#1DB953",
                                "margin-top": "20px",
                            }),
                    ], style={'fontSize': 20, "background-color": "#1DB953", 'color': 'white'}
                ),
                dcc.Tab(
                    label="Music Listening VS Our Calendar",
                    children=[
                        html.Div(
                            [

                            ],
                            id='div_4_content'
                        ),
                        html.Div(
                            [
                                html.H1(
                                    "Music Listening VS Our Calendar",
                                    style={"text-align": "center"},
                                ),
                                html.Div(
                                    [
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose the variable for axis X:",
                                                    style={"font-size": "20px"},
                                                ),
                                                dcc.Dropdown(
                                                    id="x-axis-dropdown",
                                                    options=[
                                                        {
                                                            "label": "Total Academic Activities",
                                                            "value": "TotalAcademicActivities",
                                                        },
                                                        {
                                                            "label": "Coffee Cups",
                                                            "value": "CoffeeCups",
                                                        },
                                                        {
                                                            "label": "Average Energy Level",
                                                            "value": "AverageEnergyLevel",
                                                        },
                                                        {
                                                            "label": "Productivity",
                                                            "value": "Productivity",
                                                        },
                                                        {
                                                            "label": "Steps Count",
                                                            "value": "StepsCount",
                                                        },
                                                        {
                                                            "label": "Meeting",
                                                            "value": "Meeting",
                                                        },
                                                        {
                                                            "label": "Number Of Lectures Today",
                                                            "value": "NumberOfLecturesToday",
                                                        },
                                                        {
                                                            "label": "Working Hours",
                                                            "value": "WorkingHours",
                                                        },
                                                        {
                                                            "label": "Sleep Minutes",
                                                            "value": "SleepMinutes"
                                                        }
                                                    ],
                                                    value="SleepMinutes",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right",
                                            },
                                        ),
                                        html.Div(
                                            [
                                                html.Label(
                                                    "Choose the other variable:",
                                                    style={"font-size": "20px"}
                                                ),
                                                dcc.Dropdown(
                                                    id="color-dropdown",
                                                    options=[
                                                        {
                                                            "label": "Total Academic Activities",
                                                            "value": "TotalAcademicActivities",
                                                        },
                                                        {
                                                            "label": "Coffee Cups",
                                                            "value": "CoffeeCups",
                                                        },
                                                        {
                                                            "label": "Average Energy Level",
                                                            "value": "AverageEnergyLevel",
                                                        },
                                                        {
                                                            "label": "Productivity",
                                                            "value": "Productivity",
                                                        },
                                                        {
                                                            "label": "Steps Count",
                                                            "value": "StepsCount",
                                                        },
                                                        {
                                                            "label": "Meeting",
                                                            "value": "Meeting",
                                                        },
                                                        {
                                                            "label": "Number Of Lectures Today",
                                                            "value": "NumberOfLecturesToday",
                                                        },
                                                        {
                                                            "label": "Working Hours",
                                                            "value": "WorkingHours",
                                                        },
                                                        {
                                                            "label": "Sleep Minutes",
                                                            "value": "SleepMinutes"
                                                        }
                                                    ],
                                                    value="NumberOfLecturesToday",
                                                    style={
                                                        "font-size": "20px",
                                                    },
                                                ),
                                            ],
                                            style={
                                                "width": "30%",
                                                "float": "right"
                                            },
                                        ),
                                    ],
                                    style={"display": "flex", "margin": "4%"},
                                ),
                                dcc.Loading(
                                    color="#1DB954",
                                    id="loading-listening-time-graph",
                                    type="circle",
                                    children=[
                                        html.Div(
                                            dcc.Graph(id="scatter-plot",
                                                      config={"modeBarButtonsToRemove": ["toImage", "hoverClosestPie",
                                                                                         "zoom2d", "pan2d", "select2d",
                                                                                         "lasso2d",
                                                                                         "zoomIn2d", "zoomOut2d",
                                                                                         "autoScale2d", "resetScale2d"]}
                                                      ),
                                            style={
                                                "width": "92%",
                                                "margin": "auto",
                                                "background-color": "#E8DAC5",
                                            },
                                        ), ]),
                            ]
                        ),
                        html.Div(
                            children=[
                                html.P("Boguszewska Wiktoria",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                                html.P("Bokhan Katsiaryna",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                                html.P("Okrojek Bogumiła",
                                       style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'})
                            ],
                            style={
                                'display': 'flex',
                                'flex-direction': 'column',
                                "height": "10%",
                                "background-color": "#1DB953",
                                "margin-top": "20px",
                            }),
                    ], style={'fontSize': 20, "background-color": "#1DB953", 'color': 'white'}
                ),
                dcc.Tab(label="About Data", children=[
                    html.Div([
                        html.H1("About The Data That Was Used", style={"text-align": "center"}),
                        html.H2(
                            "This App is our project for Data Visualization Techniques course at our studies. Our "
                            "task was to create an interactive visualization - a dashboard. We have decided to create "
                            "the visualization of our streaming data from Spotify connected with our daily lives. For "
                            "this purpose, we have made a request for our datasets from Spotify. In order to create the "
                            "dashboard, we have used also Spotify Web API which returns metadata about music artists, "
                            "albums and tracks, directly from the Spotify Data Catalogue.",
                            style={'margin': '4%'}),
                        html.Div([
                            html.Div([
                                html.H1("Spotify Data", style={"text-align": "center"}),
                                html.H3(
                                    "We have used requested data from Spotify about our listening history as well as "
                                    "information obtained through Spotify's Web API."),
                                html.Ul([
                                    html.Li([
                                        " ",
                                        html.Span("Date", style={'color': 'green', 'font-weight': 'bold'}),
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("ts", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - timestamp of the moment when the song started playing "
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("master_metadata_track_name",
                                                  style={'color': 'green', 'font-weight': 'bold'}),
                                        " - name of the song"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("master_metadata_album_album_name",
                                                  style={'color': 'green', 'font-weight': 'bold'}),
                                        " - name of the album"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("master_metadata_album_artist_name",
                                                  style={'color': 'green', 'font-weight': 'bold'}),
                                        " - name of the artist"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("spotify_track_uri", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - unique identifier of the song on Spotify"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span(
                                            "acousticness, danceability, energy, liveness, loudness, speechiness, "
                                            "valence",
                                            style={'color': 'green', 'font-weight': 'bold'}),
                                        " - musical features of the song"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("track_genre", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - genre of the song"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("preview_url", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - link to the preview of the song"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("album_cover", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - link to the cover of the album"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("artists_photo", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - link to the photo of the artist"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("album_on_spotify", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - link to the album on Spotify"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("artist_on_spotify", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - link to the artist on Spotify"
                                    ]),
                                ], style={'margin': '1.5%', 'font-size': '20px'}),
                            ], style={"display": "inline-block", "width": "49%", 'background-color': '#E8DAC5',
                                      'margin': '1.5%', 'box-shadow': '2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)',
                                      'padding': '1.5%', 'margin-top': '0%'}),
                            html.Div([
                                html.H1("Data On Our Daily Life", style={"text-align": "center"}),
                                html.H3(
                                    "From 01.10.2023 to 31.12.2023 we collected data about our daily life (we wrote "
                                    "them down in Google calendar and in notes). Our final data frame contained such "
                                    "data:"),
                                html.Ul([
                                    html.Li([
                                        " ",
                                        html.Span("Date", style={'color': 'green', 'font-weight': 'bold'}),
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("SleepMinutes", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - number of minutes, which we slept during that day"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("CoffeeCups", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - number of coffee cups, which we drank during that day"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("EnergyLevelMorning",
                                                  style={'color': 'green', 'font-weight': 'bold'}),
                                        " - our energy level in the morning (from 1 to 10)"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("EnergyLevelEvening",
                                                  style={'color': 'green', 'font-weight': 'bold'}),
                                        " - our energy level in the evening (from 1 to 10)"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("Productivity", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - our productivity in the day (Low, Medium, or High)"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("StepsCount", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - number of steps, which we did during that day"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("Meeting", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - number of meetings (with friends or family), which we had during that day"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("Projects", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - number of projects (or HW), which we had to hand in during that day"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("Tests", style={'color': 'green', 'font-weight': 'bold'}),
                                        "- number of tests (kolokwium, kartkówka, laboratorium), which we had during "
                                        "that day"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("NumberOfLecturesToday",
                                                  style={'color': 'green', 'font-weight': 'bold'}),
                                        " - number of lectures, which we had during that day (45 min = 0.5 lecture)"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("SportActivity", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - 1 if we did some sport activity during that day, 0 if not"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("WorkingHours", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - number of hours, which we worked during that day"
                                    ]),
                                    html.Li([
                                        " ",
                                        html.Span("WeekDay", style={'color': 'green', 'font-weight': 'bold'}),
                                        " - day of the week"
                                    ]),
                                ], style={'margin': '1.5%', 'font-size': '20px'}),
                            ], style={"display": "inline-block", "width": "49%", 'background-color': '#E8DAC5',
                                      'margin': '1.5%', 'box-shadow': '2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)',
                                      'padding': '1.5%', 'margin-top': '0%'}),
                        ], className="row",
                            style={"display": "inline-flex", "width": "100%", 'justify-content': 'center',
                                   'margin': '2px'}),
                    ], style={'margin': '2%'}),
                    html.Div(
                        children=[
                            html.P("Boguszewska Wiktoria",
                                   style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                            html.P("Bokhan Katsiaryna",
                                   style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'}),
                            html.P("Okrojek Bogumiła", style={'color': 'white', 'font-size': '1.2em', 'margin': '0.5%'})
                        ],
                        style={
                            'display': 'flex',
                            'flex-direction': 'column',
                            "height": "10%",
                            "background-color": "#1DB953",
                            "margin-top": "20px",
                        }),
                ], style={'fontSize': 20, "background-color": "#1DB953", 'color': 'white'}),
            ]
        )
    ],
)


@app.callback(
    dash.dependencies.Output('chosen_person', 'children'),
    dash.dependencies.Output('div_1_content', 'children'),
    dash.dependencies.Output('div_2_content', 'children'),
    dash.dependencies.Output('div_3_content', 'children'),
    dash.dependencies.Output('div_4_content', 'children'),
    [
        dash.dependencies.Input('buttonBogusia', 'n_clicks'),
        dash.dependencies.Input('buttonWiktoria', 'n_clicks'),
        dash.dependencies.Input('buttonKasia', 'n_clicks')
    ]
)
def update_content(buttonBogusia_clicks, buttonWiktoria_clicks, buttonKasia_clicks):
    person = "Kasia"
    ctx = dash.callback_context
    global button_id
    button_id_1 = 'none'
    if ctx.triggered_id:
        button_id_1 = ctx.triggered_id.split('.')[0]
    if buttonBogusia_clicks == 0 and buttonWiktoria_clicks == 0 and buttonKasia_clicks == 0:
        button_id = 'buttonKasia'
    if button_id_1 == 'buttonBogusia':
        button_id = button_id_1
    elif button_id_1 == 'buttonWiktoria':
        button_id = button_id_1
    elif button_id_1 == 'buttonKasia':
        button_id = button_id_1

    if button_id == 'buttonBogusia':
        history = pd.read_csv("./Data/Bogusia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Bogusia/personalDataFull.csv")
        all_data = pd.read_csv("Data/Bogusia/all_data.csv")  # data from DataPreprocessing
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
        data_merged = pd.merge(history, personalData, on="Date")
        data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]
        person = "Bogusia"
    elif button_id == 'buttonWiktoria':
        history = pd.read_csv("./Data/Wiktoria/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Wiktoria/personalDataFull.csv")
        all_data = pd.read_csv("Data/Wiktoria/all_data.csv")  # data from DataPreprocessing
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
        data_merged = pd.merge(history, personalData, on="Date")
        data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]
        person = "Wiktoria"
    elif button_id == 'buttonKasia':
        history = pd.read_csv("./Data/Kasia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Kasia/personalDataFull.csv")
        all_data = pd.read_csv("Data/Kasia/all_data.csv")  # data from DataPreprocessing
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
        data_merged = pd.merge(history, personalData, on="Date")
        data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]
        person = "Kasia"

    image_urls = \
        all_data.groupby(['master_metadata_album_artist_name', 'artists_photo'])['ms_played'].sum().sort_values(
            ascending=False).head(10).reset_index()['artists_photo']
    links = \
        all_data.groupby(['master_metadata_album_artist_name', 'artist_on_spotify'])['ms_played'].sum().sort_values(
            ascending=False).head(10).reset_index()['artist_on_spotify']
    artists_names = \
        all_data.groupby(['master_metadata_album_artist_name'])['ms_played'].sum().sort_values(ascending=False).head(
            10).reset_index()['master_metadata_album_artist_name']
    image_albums_urls = \
        all_data.groupby(['master_metadata_album_album_name', 'album_cover'])['ms_played'].sum().sort_values(
            ascending=False).head(10).reset_index()['album_cover']
    links_albums = \
        all_data.groupby(['master_metadata_album_album_name', 'album_on_spotify'])['ms_played'].sum().sort_values(
            ascending=False).head(10).reset_index()['album_on_spotify']
    albums_names = \
        all_data.groupby(['master_metadata_album_album_name', 'album_cover'])['ms_played'].sum().sort_values(
            ascending=False).head(10).reset_index()['master_metadata_album_album_name']

    chosen_person = \
        [
            html.Div(
                [
                    html.H2(person + "'s data was chosen."),
                ]
            )
        ]

    # ########wkładka 1 - zdjęcia artystów i albumów
    div_1_content = \
        [
            html.Div(
                [
                    html.A(
                        html.Div(
                            [
                                html.Img(src=image_url, style={"width": "70%", "margin": "2.5%"}),
                            ]
                        ),
                        href=link,
                        target="_blank"
                    ),
                    html.Div(
                        [
                            html.H3(artist_name, style={"text-align": "center"})
                        ], style={'height': '50px'})
                ],
                style={"width": "20%", 'display': 'inline-block'})
            for image_url, link, artist_name in zip(image_urls, links, artists_names)

        ]

    div_2_content = \
        [
            html.Div([
                html.A(
                    html.Div([
                        html.Img(src=image_url, style={"width": "70%", "margin": "2.5%"}),
                    ]),
                    href=link,
                    target="_blank"
                ),
                html.H3(artist_name, style={"text-align": "center"})
            ],
                style={"width": "20%", 'display': 'inline-block'})
            for image_url, link, artist_name in zip(image_albums_urls, links_albums, albums_names)
        ]

    category_order_reversed = ["High", "Medium", "Low"]
    category_order = ["Low", "Medium", "High"]
    # ############rokład productivity
    grouped_data_productivity_distr_plot = (
        personalData.groupby(["Productivity"])["Date"].count().sort_values().reset_index()
    )
    try:
        productivity_distribution_plot = \
            px.bar(
                grouped_data_productivity_distr_plot,
                x="Date",
                y="Productivity",
                title="Productivity distribution",
                labels={"Productivity": "Productivity", "Date": "Number of days in this semester"},
                orientation="h",
                color_discrete_sequence=["#1DB953"],
                category_orders={"Productivity": category_order_reversed},
            )
    except:
        productivity_distribution_plot = \
            px.bar(
                grouped_data_productivity_distr_plot,
                x="Date",
                y="Productivity",
                title="Productivity distribution",
                labels={"Productivity": "Productivity", "Date": "Number of days in this semester"},
                orientation="h",
                color_discrete_sequence=["#1DB953"],
                category_orders={"Productivity": category_order_reversed},
            )
    productivity_distribution_plot.update_layout(
        xaxis_title="Number of days in this semester",
        yaxis_title="Productivity",
        font=dict(family="Ubuntu"),
        plot_bgcolor="rgba(0,0,0,0)",
        paper_bgcolor="rgba(0,0,0,0)",
        xaxis=dict(
            showgrid=True,
            gridcolor="white",
            gridwidth=2,
        ),
    )
    # #############violin-plot productivity plot
    grouped_data_median_ms_played_producctivity_plot = (
        data_merged.groupby(["Productivity", "Date"])["ms_played"].sum().reset_index()
    )
    grouped_data_median_ms_played_producctivity_plot["ms_played"] /= 60000
    boxplot_ms_played_producctivity_plot = px.violin(
        grouped_data_median_ms_played_producctivity_plot,
        x="Productivity",
        y="ms_played",
        box=True,
        category_orders={"Productivity": category_order},
    )
    boxplot_ms_played_producctivity_plot.update_layout(
        title="Plot of Productivity and Total Duration of listened music (Minutes) per Day",
        xaxis=dict(title="Productivity"),
        yaxis=dict(
            title="Total Duration of listened music (Minutes)",
            showgrid=True,
            gridcolor="white",
            gridwidth=2,
        ),
        font=dict(family="Ubuntu"),
        plot_bgcolor="rgba(0,0,0,0)",
        paper_bgcolor="rgba(0,0,0,0)",
    )
    boxplot_ms_played_producctivity_plot.update_traces(
        marker=dict(color="#1DB954"),
        spanmode='hard')

    div_3_content = \
        [
            html.H1(
                "Productivity VS Music Preferences",
                style={"text-align": "center"},
            ),
            html.Div(
                [
                    html.Div(
                        dcc.Graph(
                            id="productivity-distribution",
                            figure=productivity_distribution_plot,
                            config={
                                "modeBarButtonsToRemove": ["toImage", "hoverClosestPie", "zoom2d", "pan2d", "select2d",
                                                           "lasso2d",
                                                           "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d"]}
                        ),
                        style={
                            "width": "50%",
                            "box-shadow": "2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)",
                            "background-color": "#E8DAC5",
                            "margin": "2% 2% 2% 4%",
                        }, ),
                    html.Div(
                        dcc.Graph(
                            id="median_ms_played-producctivity-plot",
                            figure=boxplot_ms_played_producctivity_plot,
                            config={
                                "modeBarButtonsToRemove": ["toImage", "hoverClosestPie", "zoom2d", "pan2d", "select2d",
                                                           "lasso2d",
                                                           "zoomIn2d", "zoomOut2d", "autoScale2d", "resetScale2d"]}
                        ),
                        style={
                            "width": "50%",
                            "box-shadow": "2px 2px 2px 2px rgba(0.3, 0.3, 0.3, 0.3)",
                            "background-color": "#E8DAC5",
                            "margin": "2% 2% 2% 1.5%",
                        },
                    ),

                ], style={"display": "flex"},
            )

        ]

    # Panele

    history_grouped = history.groupby('Date').agg({'ms_played': 'sum'})
    history_grouped['MusicMinutes'] = history_grouped['ms_played'] / 60000

    combined_data = pd.merge(personalData, history_grouped, how='left', on='Date')

    combined_data['MusicMinutes'] = combined_data['MusicMinutes'].fillna(0)

    combined_data['TotalAcademicActivities'] = combined_data['Projects'] + combined_data['Tests'] + combined_data[
        'NumberOfLecturesToday']

    combined_data['AverageEnergyLevel'] = (combined_data['EnergyLevelMorning'] + combined_data[
        'EnergyLevelEvening']) / 2

    average_coffee_cups = combined_data['CoffeeCups'].mean()

    total_steps_count = combined_data['StepsCount'].sum()

    shortest_sleep = combined_data['SleepMinutes'].min()

    longest_sleep = combined_data['SleepMinutes'].max()

    average_number_of_lectures = combined_data['NumberOfLecturesToday'].mean()

    total_meeting_count = combined_data['Meeting'].sum()

    combined_data['TotalTestsAndProjects'] = combined_data['Projects'] + combined_data['Tests']

    #    total_tests_and_projects = combined_data['TotalTestsAndProjects'].sum()

    div_4_content = [
        html.H1(
            "Our Calendar Data",
            style={"text-align": "center"},
        ),
        html.Div(
            [
                html.Div(
                    [
                        html.H3(f"{average_coffee_cups:.2f}",
                                style={"color": "#333", "fontSize": "2.5em", "margin": "0"}),
                        html.P("Average Number of Coffee Cups per Day",
                               style={"color": "#888", "fontSize": "1em", "marginTop": "0.25em"})
                    ],
                    style={
                        'margin': '2px',
                        'padding': '6px',
                        'height': '100px',
                        "backgroundColor": "#f9f9f9",
                        "borderLeft": "5px solid #333",
                        "boxShadow": "2px 2px 10px #aaa",
                        "width": "15%",
                        "borderRadius": "8px",
                        "textAlign": "center",
                        'justify-content': 'center',
                    },
                ),
                html.Div(
                    [
                        html.H3(f"{total_steps_count}",
                                style={"color": "#333", "fontSize": "2.5em", "margin": "0"}),
                        html.P("Total Steps Count", style={"color": "#888", "fontSize": "1em", "marginTop": "0.25em"
                                                           }),
                    ],
                    style={
                        'margin': '2px',
                        'padding': '6px',
                        'height': '100px',
                        "backgroundColor": "#f9f9f9",
                        "borderLeft": "5px solid #333",
                        "boxShadow": "2px 2px 10px #aaa",
                        "width": "15%",
                        "borderRadius": "8px",
                        "textAlign": "center",
                        'justify-content': 'center',
                    },
                ),
                html.Div(
                    [
                        html.H3(f"{shortest_sleep} min",
                                style={"color": "#333", "fontSize": "2.5em", "margin": "0",
                                       }),
                        html.P("Shortest Sleep Duration",
                               style={"color": "#888", "fontSize": "1em", "marginTop": "0.25em",
                                      }),
                    ],
                    style={
                        'margin': '2px',
                        'padding': '6px',
                        'justify-content': 'center',
                        'height': '100px',
                        "backgroundColor": "#f9f9f9",
                        "borderLeft": "5px solid #333",
                        "boxShadow": "2px 2px 10px #aaa",
                        "width": "15%",
                        "borderRadius": "8px",
                        "textAlign": "center"
                    },
                ),
                html.Div(
                    [
                        html.H3(f"{longest_sleep} min",
                                style={"color": "#333", "fontSize": "2.5em", "margin": "0",
                                       }),
                        html.P("Longest Sleep Duration",
                               style={"color": "#888", "fontSize": "1em", "marginTop": "0.25em",
                                      }),
                    ],
                    style={
                        'margin': '2px',
                        'padding': '6px',
                        'height': '100px',
                        "backgroundColor": "#f9f9f9",
                        "borderLeft": "5px solid #333",
                        "boxShadow": "2px 2px 10px #aaa",
                        "width": "15%",
                        "borderRadius": "8px",
                        "textAlign": "center",
                        'justify-content': 'center',
                    },
                ),
                html.Div(
                    [
                        html.H3(f"{average_number_of_lectures:.2f}",
                                style={"color": "#333", "fontSize": "2.5em", "margin": "0", }),
                        html.P("Average Number of Lectures per Day",
                               style={"color": "#888", "fontSize": "1em", "marginTop": "0.25em", }),
                    ],
                    style={
                        'margin': '2px',
                        'padding': '6px',
                        'height': '100px',
                        "backgroundColor": "#f9f9f9",
                        "borderLeft": "5px solid #333",
                        "boxShadow": "2px 2px 10px #aaa",
                        "width": "15%",
                        "borderRadius": "8px",
                        "textAlign": "center",
                        'justify-content': 'center',
                    },
                ),
                html.Div(
                    [
                        html.H3(f"{total_meeting_count}",
                                style={"color": "#333", "fontSize": "2.5em", "margin": "0"}),
                        html.P("Total Number of Meetings During this Semester",
                               style={"color": "#888", "fontSize": "1em", "marginTop": "0.25em"}),
                    ],
                    style={
                        'margin': '2px',
                        'padding': '6px',
                        'height': '100px',
                        "backgroundColor": "#f9f9f9",
                        "borderLeft": "5px solid #333",
                        "boxShadow": "2px 2px 10px #aaa",
                        "width": "15%",
                        "borderRadius": "8px",
                        "textAlign": "center",
                        'justify-content': 'center',
                    },
                ),
            ],
            style={'margin': '0% 4% 0% 4%', "textAlign": "center", 'display': 'flex', 'justify-content': 'center',
                   'align-items': 'center'},
        ),
    ]

    return chosen_person, div_1_content, div_2_content, div_3_content, div_4_content


@app.callback(
    dash.dependencies.Output("genre_preference", "figure"),
    [
        dash.dependencies.Input("checkboxes1", "value"),
        dash.dependencies.Input("grouping-dropdown", "value"),
        dash.dependencies.Input('buttonBogusia', 'n_clicks'),
        dash.dependencies.Input('buttonWiktoria', 'n_clicks'),
        dash.dependencies.Input('buttonKasia', 'n_clicks')
    ],
)
def update_genre_preference(selected_productivity_values, selected_grouping_value, buttonBogusia_clicks,
                            buttonWiktoria_clicks, buttonKasia_clicks):
    global button_id
    if button_id == 'buttonBogusia':
        history = pd.read_csv("./Data/Bogusia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        songs_info = pd.read_csv("Data/Common/songs_spotify_info.csv")
        songs_info = songs_info[["track_id", "track_genre"]]
        personalData = pd.read_csv("Data/Bogusia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonWiktoria':
        history = pd.read_csv("./Data/Wiktoria/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        songs_info = pd.read_csv("Data/Common/songs_spotify_info.csv")
        songs_info = songs_info[["track_id", "track_genre"]]
        personalData = pd.read_csv("Data/Wiktoria/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonKasia':
        history = pd.read_csv("./Data/Kasia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        songs_info = pd.read_csv("Data/Common/songs_spotify_info.csv")
        songs_info = songs_info[["track_id", "track_genre"]]
        personalData = pd.read_csv("Data/Kasia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date

    data_merged = pd.merge(history, personalData, on="Date")
    data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]
    data = pd.merge(data_merged, songs_info, left_on="spotify_track_uri", right_on="track_id"
                    )

    if selected_grouping_value == "master_metadata_album_artist_name":
        grouping_columns = ["Productivity", selected_grouping_value]
        grouped_genre_preference_data = (
            data_merged.groupby(grouping_columns)["ms_played"]
            .sum()
            .sort_values(ascending=False)
            .reset_index()
        )
        filtered_data = grouped_genre_preference_data[
            grouped_genre_preference_data["Productivity"].isin(
                selected_productivity_values
            )
        ]
        filtered_data = (
            filtered_data.groupby(selected_grouping_value)["ms_played"]
            .sum()
            .sort_values()
            .reset_index()
            .tail(20)
        )
        title = "Top 20 Most Preferred Artists  Depending on Productivity"
        yaxis_label = "Artists"
    elif selected_grouping_value == "master_metadata_track_name":
        grouping_columns = ["Productivity", selected_grouping_value]
        grouped_genre_preference_data = (
            data_merged.groupby(grouping_columns)["ms_played"]
            .sum()
            .sort_values(ascending=False)
            .reset_index()
        )
        filtered_data = grouped_genre_preference_data[
            grouped_genre_preference_data["Productivity"].isin(
                selected_productivity_values
            )
        ]
        filtered_data = (
            filtered_data.groupby(selected_grouping_value)["ms_played"]
            .sum()
            .sort_values()
            .reset_index()
            .tail(20)
        )
        title = "Top 20 Most Preferred Songs  Depending on Productivity"
        yaxis_label = "Song"
    elif selected_grouping_value == "master_metadata_album_album_name":
        grouping_columns = ["Productivity", selected_grouping_value]
        grouped_genre_preference_data = (
            data_merged.groupby(grouping_columns)["ms_played"]
            .sum()
            .sort_values(ascending=False)
            .reset_index()
        )
        filtered_data = grouped_genre_preference_data[
            grouped_genre_preference_data["Productivity"].isin(
                selected_productivity_values
            )
        ]
        filtered_data = (
            filtered_data.groupby(selected_grouping_value)["ms_played"]
            .sum()
            .sort_values()
            .reset_index()
            .tail(20)
        )
        title = "Top 20 Most Preferred Albums Depending on Productivity"
        yaxis_label = "Album"
    else:
        grouping_columns = ["Productivity", selected_grouping_value]
        grouped_genre_preference_data = (
            data.groupby(grouping_columns)["ms_played"]
            .sum()
            .sort_values(ascending=False)
            .reset_index()
        )
        filtered_data = grouped_genre_preference_data[
            grouped_genre_preference_data["Productivity"].isin(
                selected_productivity_values
            )
        ]
        filtered_data = (
            filtered_data.groupby(selected_grouping_value)["ms_played"]
            .sum()
            .sort_values()
            .reset_index()
            .tail(20)
        )
        title = "Top 20 Most Preferred Genres of Music Depending on Productivity"
        yaxis_label = "Genre"

    filtered_data["ms_played"] /= 60000
    try:
        fig = px.bar(
            filtered_data, y=selected_grouping_value, x="ms_played", orientation="h"
        )
    except:
        fig = px.bar(
            filtered_data, y=selected_grouping_value, x="ms_played", orientation="h"
        )
    fig.update_layout(
        title=title,
        xaxis=dict(title="Minutes listened", showgrid=True, gridcolor="white", gridwidth=2, ),
        yaxis=dict(title=yaxis_label),
        font=dict(family="Ubuntu"),
        plot_bgcolor="rgba(0,0,0,0)",
        paper_bgcolor="rgba(0,0,0,0)",
    )
    fig.update_traces(marker=dict(color="#1DB954"))
    return fig


@app.callback(
    dash.dependencies.Output("radar_plot", "figure"),
    [
        dash.dependencies.Input('buttonBogusia', 'n_clicks'),
        dash.dependencies.Input('buttonWiktoria', 'n_clicks'),
        dash.dependencies.Input('buttonKasia', 'n_clicks')
    ],
)
def update_radar_plot(buttonBogusia_clicks, buttonWiktoria_clicks, buttonKasia_clicks):
    ctx = dash.callback_context
    global button_id
    button_id_1 = 'none'
    if ctx.triggered_id:
        button_id_1 = ctx.triggered_id.split('.')[0]
    if buttonBogusia_clicks == 0 and buttonWiktoria_clicks == 0 and buttonKasia_clicks == 0:
        button_id = 'buttonKasia'
    if button_id_1 == 'buttonBogusia':
        button_id = button_id_1
    elif button_id_1 == 'buttonWiktoria':
        button_id = button_id_1
    elif button_id_1 == 'buttonKasia':
        button_id = button_id_1

    if button_id == 'buttonBogusia':
        history = pd.read_csv("./Data/Bogusia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Bogusia/personalDataFull.csv")
        all_data = pd.read_csv("Data/Bogusia/all_data.csv")  # data from DataPreprocessing
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
        data_merged = pd.merge(history, personalData, on="Date")
        data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]
    elif button_id == 'buttonWiktoria':
        history = pd.read_csv("./Data/Wiktoria/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Wiktoria/personalDataFull.csv")
        all_data = pd.read_csv("Data/Wiktoria/all_data.csv")  # data from DataPreprocessing
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
        data_merged = pd.merge(history, personalData, on="Date")
        data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]
    elif button_id == 'buttonKasia':
        history = pd.read_csv("./Data/Kasia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Kasia/personalDataFull.csv")
        all_data = pd.read_csv("Data/Kasia/all_data.csv")  # data from DataPreprocessing
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
        data_merged = pd.merge(history, personalData, on="Date")
        data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]

    grouped = all_data.groupby(['Productivity'])[
        ['acousticness', 'danceability', 'liveness', 'energy', 'valence', 'speechiness', 'loudness']].mean()
    categories = ['acousticness', 'danceability', 'liveness', 'energy', 'valence', 'speechiness', 'loudness']
    productivity_labels = grouped.index.tolist()
    data = grouped.values.tolist()
    radar_plot = go.Figure()

    colors = ['red', '#1E90FF', '#008000']

    for i in range(len(productivity_labels)):
        data[i].append(data[i][0])
        radar_plot.add_trace(go.Scatterpolar(
            r=data[i],
            theta=categories + [categories[0]],
            fill='toself',
            name=productivity_labels[i],
            line=dict(color=colors[i % len(colors)])
        ))

    radar_plot.update_layout(
        polar=dict(
            radialaxis=dict(
                visible=True,
                gridwidth=2,
                range=[0, 1],
            ),
        ),
        showlegend=True,
        plot_bgcolor="rgba(0,0,0,0)",
        paper_bgcolor="rgba(0,0,0,0)",
        title='Median values of music characteristics (scaled from 0 to 1) by productivity',
        font=dict(family='Ubuntu'),
        polar_bgcolor='rgba(255,255,255,0)'
    )
    return radar_plot


# Zakładka 3 - Date

# Funkcje

def czasWminutach(td):
    # To get the minutes information
    return (td / 1000) / 60


def uzupelnicDaty(data1):
    data2 = data1.copy()
    # start_date = pd.to_datetime(min(data2["Date"])).dt.date
    # end_date = pd.to_datetime(max(data2["Date"])).dt.date
    start_date = pd.to_datetime(min(data2["Date"]))
    end_date = pd.to_datetime(max(data2["Date"]))
    data2["Date"] = pd.to_datetime(data2["Date"]).dt.date

    a = pd.date_range(start_date, end_date).difference(data2["Date"])
    x = a.to_frame(name="Date").reset_index(drop=True)
    x['Date'] = pd.to_datetime(x['Date']).dt.date
    dataAllDates = pd.concat([data2, x]).reset_index(drop=True)
    dataAllDates["Date"] = pd.to_datetime(dataAllDates["Date"]).dt.date
    return dataAllDates


def dodacKolumnyCzasow(data1):
    data = data1.copy()
    data["Date"] = pd.to_datetime(data["Date"])
    data['week_number'] = data["Date"].dt.isocalendar().week
    data['day-name'] = data["Date"].apply(lambda xl: xl.day_name())
    data["Date"] = pd.to_datetime(data["Date"]).dt.date
    data['year'] = pd.DatetimeIndex(data["Date"]).year
    data['month'] = pd.DatetimeIndex(data["Date"]).month
    data['day'] = pd.DatetimeIndex(data["Date"]).day

    dataFilled = data.sort_values("Date")
    return dataFilled


@app.callback(
    dash.dependencies.Output("ListeningTimeVsDate", "figure"),
    [
        dash.dependencies.Input("grouping-dropdown2", "value"),
        dash.dependencies.Input("grouping-dropdown3", "value"),
        dash.dependencies.Input('buttonBogusia', 'n_clicks'),
        dash.dependencies.Input('buttonWiktoria', 'n_clicks'),
        dash.dependencies.Input('buttonKasia', 'n_clicks')
    ],
)
def update_ListeningTimeVsDate(selected_grouping_value, selected_date_range, buttonBogusia_clicks,
                               buttonWiktoria_clicks, buttonKasia_clicks):
    global button_id
    if button_id == 'buttonBogusia':
        history = pd.read_csv("./Data/Bogusia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        songs_info = pd.read_csv("Data/Common/songs_spotify_info.csv")
        songs_info = songs_info[["track_id", "track_genre"]]
        personalData = pd.read_csv("Data/Bogusia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonWiktoria':
        history = pd.read_csv("./Data/Wiktoria/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        songs_info = pd.read_csv("Data/Common/songs_spotify_info.csv")
        songs_info = songs_info[["track_id", "track_genre"]]
        personalData = pd.read_csv("Data/Wiktoria/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonKasia':
        history = pd.read_csv("./Data/Kasia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        songs_info = pd.read_csv("Data/Common/songs_spotify_info.csv")
        songs_info = songs_info[["track_id", "track_genre"]]
        personalData = pd.read_csv("Data/Kasia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date

    data_merged = pd.merge(history, personalData, on="Date")
    data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]

    # ### history_with_time_columns
    history_with_time_columns = dodacKolumnyCzasow(history)
    history_with_time_columns["Time(Minutes)"] = history_with_time_columns["ms_played"].apply(czasWminutach).round(5)
    history_with_time_columns = history_with_time_columns.sort_values("Date").reset_index(drop=True)
    # ### data_merged_with_time_columns
    data_merged_with_time_columns = dodacKolumnyCzasow(data_merged)
    data_merged_with_time_columns["Time(Minutes)"] = data_merged_with_time_columns["ms_played"].apply(
        czasWminutach).round(5)
    data_merged_with_time_columns = data_merged_with_time_columns.sort_values("Date").reset_index(drop=True)
    # ### historyAllDates
    historyAllDates = uzupelnicDaty(history_with_time_columns)
    historyAllDates = historyAllDates.sort_values("Date").reset_index(drop=True)
    # ### data_mergedAllDates
    data_mergedAllDates = uzupelnicDaty(data_merged_with_time_columns)
    data_mergedAllDates = data_mergedAllDates.sort_values("Date").reset_index(drop=True)
    day_order = ["Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"]
    if selected_date_range == "full":
        data2 = historyAllDates
    else:
        data2 = data_mergedAllDates
    if selected_grouping_value == "Date":
        grouped_date_time_data = (
            data2.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values(selected_grouping_value)
            .reset_index(drop=True)
        )
        title = "Time spent on Spotify per day"
        xaxis_label = "Date"
    elif selected_grouping_value == "day-name":
        data2['day-name'] = pd.Categorical(data2['day-name'], categories=day_order, ordered=True)
        grouped_date_time_data = (
            data2.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values(selected_grouping_value)
            .reset_index(drop=True)
        )
        title = "Time spent on Spotify per day of the week"
        xaxis_label = "dayname"
    elif selected_grouping_value == "week_number":
        grouped_date_time_data = (
            data2.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values(selected_grouping_value)
            .reset_index(drop=True)
        )
        title = "Time spent on Spotify per the week of the year"
        xaxis_label = "week number"
    elif selected_grouping_value == "month":
        grouped_date_time_data = (
            data2.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values(selected_grouping_value)
            .reset_index(drop=True)
        )
        title = "Time spent on Spotify per the month of the year"
        xaxis_label = "month number"
    else:
        grouped_date_time_data = (
            data2.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values(selected_grouping_value)
            .reset_index(drop=True)
        )
        title = "Time spent on Spotify per year"
        xaxis_label = "year"
    try:
        fig = px.line(
            grouped_date_time_data, x=selected_grouping_value, y="Time(Minutes)"
        )
    except:
        fig = px.line(
            grouped_date_time_data, x=selected_grouping_value, y="Time(Minutes)"
        )

    fig.update_layout(
        title=title,
        yaxis=dict(title="Minutes listened", showgrid=True, gridcolor="white", gridwidth=2, ),
        xaxis=dict(title=xaxis_label),
        font=dict(family="Ubuntu"),
        plot_bgcolor="rgba(0,0,0,0)",
        paper_bgcolor="rgba(0,0,0,0)",
    )
    fig.update_traces(line=dict(color="#1DB954"))
    return fig


@app.callback(
    dash.dependencies.Output("MeanListeningTimeVsTime", "figure"),
    [
        dash.dependencies.Input("checkboxes2", "value"),
        dash.dependencies.Input("grouping-dropdown4", "value"),
        dash.dependencies.Input("grouping-dropdown5", "value"),
        dash.dependencies.Input('buttonBogusia', 'n_clicks'),
        dash.dependencies.Input('buttonWiktoria', 'n_clicks'),
        dash.dependencies.Input('buttonKasia', 'n_clicks')
    ],
)
def update_MeanListeningTimeVsTime(selected_data_values, selected_grouping_value, selected_date_range,
                                   buttonBogusia_clicks, buttonWiktoria_clicks, buttonKasia_clicks):
    global button_id
    if button_id == 'buttonBogusia':
        history = pd.read_csv("./Data/Bogusia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Bogusia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonWiktoria':
        history = pd.read_csv("./Data/Wiktoria/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Wiktoria/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonKasia':
        history = pd.read_csv("./Data/Kasia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Kasia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date

    data_merged = pd.merge(history, personalData, on="Date")
    data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]

    # ### history_with_time_columns
    history_with_time_columns = dodacKolumnyCzasow(history)
    history_with_time_columns["Time(Minutes)"] = history_with_time_columns["ms_played"].apply(czasWminutach).round(5)
    history_with_time_columns = history_with_time_columns.sort_values("Date").reset_index(drop=True)
    # ### data_merged_with_time_columns
    data_merged_with_time_columns = dodacKolumnyCzasow(data_merged)
    data_merged_with_time_columns["Time(Minutes)"] = data_merged_with_time_columns["ms_played"].apply(
        czasWminutach).round(5)
    data_merged_with_time_columns = data_merged_with_time_columns.sort_values("Date").reset_index(drop=True)
    # # ### historyAllDates
    historyAllDates = uzupelnicDaty(history)
    historyAllDates["Time(Minutes)"] = historyAllDates["ms_played"].apply(czasWminutach).round(5)
    historyAllDates = dodacKolumnyCzasow(historyAllDates)
    historyAllDates = historyAllDates.sort_values("Date").reset_index(drop=True)
    # ### data_mergedAllDates
    data_mergedAllDates = uzupelnicDaty(data_merged)
    data_mergedAllDates["Time(Minutes)"] = data_mergedAllDates["ms_played"].apply(czasWminutach).round(5)
    data_mergedAllDates = dodacKolumnyCzasow(data_mergedAllDates)
    data_mergedAllDates = data_mergedAllDates.sort_values("Date").reset_index(drop=True)

    if selected_data_values == ["all"]:
        if selected_date_range == "full":
            data2 = historyAllDates
        else:
            data2 = data_mergedAllDates
    else:
        if selected_date_range == "full":
            data2 = history_with_time_columns
        else:
            data2 = data_merged_with_time_columns

    if selected_grouping_value == "day-name":
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data1 = (
            data2.groupby(grouping_columns, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        grouped_date_time_data = (
            grouped_date_time_data1.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .mean()
        )
        title = "The average amount of time spent on Spotify during different days of the week"
        xaxis_label = "Day of the week"

    elif selected_grouping_value == "week_number":
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data1 = (
            data2.groupby(grouping_columns, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        grouped_date_time_data = (
            grouped_date_time_data1.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .mean()
        )
        title = "The average amount of time spent on Spotify during different weeks of the year"
        xaxis_label = "Week of the year"

    elif selected_grouping_value == "year":
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data1 = (
            data2.groupby(grouping_columns, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        grouped_date_time_data = (
            grouped_date_time_data1.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .mean()
        )
        title = "The average amount of time spent on Spotify during different years"
        xaxis_label = "Year"
    elif selected_grouping_value == "season":
        data2['season'] = data2['Date'].apply(get_season)
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data1 = (
            data2.groupby(grouping_columns, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        grouped_date_time_data = (
            grouped_date_time_data1.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .mean()
        )
        title = "The average amount of time spent on Spotify during different seasons"
        xaxis_label = "Season"
    else:
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data1 = (
            data2.groupby(grouping_columns, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        grouped_date_time_data = (
            grouped_date_time_data1.groupby(selected_grouping_value, as_index=False)["Time(Minutes)"]
            .mean()
        )
        title = "The average amount of time spent on Spotify during different months"
        xaxis_label = "Month of the year"

    if selected_grouping_value == "day-name":
        try:
            fig = px.bar(
                grouped_date_time_data, x='day-name', y='Time(Minutes)',
                category_orders={
                    'day-name': ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']}
            )
        except:
            fig = px.bar(
                grouped_date_time_data, x='day-name', y='Time(Minutes)',
                category_orders={
                    'day-name': ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']}
            )
        fig.update_layout(
            title=title,
            yaxis=dict(title="Minutes listened", showgrid=True, gridcolor="white", gridwidth=2, ),
            xaxis=dict(title=xaxis_label),
            font=dict(family="Ubuntu"),
            plot_bgcolor="rgba(0,0,0,0)",
            paper_bgcolor="rgba(0,0,0,0)",
        )
    elif selected_grouping_value == "season":
        try:
            fig = px.bar(grouped_date_time_data, x='season', y='Time(Minutes)',
                         category_orders={'season': ['Spring', 'Summer', 'Fall', 'Winter']},
                         color='season')
        except:
            fig = px.bar(grouped_date_time_data, x='season', y='Time(Minutes)',
                         category_orders={'season': ['Spring', 'Summer', 'Fall', 'Winter']},
                         color='season')
        fig.update_layout(
            title=title,
            yaxis=dict(title="Minutes listened", showgrid=True, gridcolor="white", gridwidth=2, ),
            xaxis=dict(title=xaxis_label),
            font=dict(family="Ubuntu"),
            plot_bgcolor="rgba(0,0,0,0)",
            paper_bgcolor="rgba(0,0,0,0)",
        )
    else:
        try:
            fig = px.bar(
                grouped_date_time_data, x=selected_grouping_value, y='Time(Minutes)'
            )
        except:
            fig = px.bar(
                grouped_date_time_data, x=selected_grouping_value, y='Time(Minutes)'
            )
        fig.update_layout(
            title=title,
            yaxis=dict(title="Minutes listened", showgrid=True, gridcolor="white", gridwidth=2, ),
            xaxis=dict(title=xaxis_label),
            font=dict(family="Ubuntu"),
            plot_bgcolor="rgba(0,0,0,0)",
            paper_bgcolor="rgba(0,0,0,0)",
        )
    fig.update_traces(marker=dict(color="#1DB954"))
    return fig


@app.callback(
    dash.dependencies.Output("ListeningTimeVsDateBoxPlot", "figure"),
    [
        dash.dependencies.Input("checkboxes3", "value"),
        dash.dependencies.Input("grouping-dropdown7", "value"),
        dash.dependencies.Input("grouping-dropdown6", "value"),
        dash.dependencies.Input('buttonBogusia', 'n_clicks'),
        dash.dependencies.Input('buttonWiktoria', 'n_clicks'),
        dash.dependencies.Input('buttonKasia', 'n_clicks')
    ],
)
def update_ListeningTimeVsDateBoxPlot(selected_data_values, selected_grouping_value, selected_date_range,
                                      buttonBogusia_clicks, buttonWiktoria_clicks, buttonKasia_clicks):
    global button_id
    if button_id == 'buttonBogusia':
        history = pd.read_csv("./Data/Bogusia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Bogusia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonWiktoria':
        history = pd.read_csv("./Data/Wiktoria/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Wiktoria/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonKasia':
        history = pd.read_csv("./Data/Kasia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Kasia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date

    data_merged = pd.merge(history, personalData, on="Date")
    data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]

    # ### history_with_time_columns
    history_with_time_columns = dodacKolumnyCzasow(history)
    history_with_time_columns["Time(Minutes)"] = history_with_time_columns["ms_played"].apply(czasWminutach).round(5)
    history_with_time_columns = history_with_time_columns.sort_values("Date").reset_index(drop=True)
    # ### data_merged_with_time_columns
    data_merged_with_time_columns = dodacKolumnyCzasow(data_merged)
    data_merged_with_time_columns["Time(Minutes)"] = data_merged_with_time_columns["ms_played"].apply(
        czasWminutach).round(5)
    data_merged_with_time_columns = data_merged_with_time_columns.sort_values("Date").reset_index(drop=True)
    # ### historyAllDates
    historyAllDates = uzupelnicDaty(history)
    historyAllDates["Time(Minutes)"] = historyAllDates["ms_played"].apply(czasWminutach).round(5)
    historyAllDates = dodacKolumnyCzasow(historyAllDates)
    historyAllDates = historyAllDates.sort_values("Date").reset_index(drop=True)
    # ### data_mergedAllDates

    data_mergedAllDates = uzupelnicDaty(data_merged)
    data_mergedAllDates["Time(Minutes)"] = data_mergedAllDates["ms_played"].apply(czasWminutach).round(5)
    data_mergedAllDates = dodacKolumnyCzasow(data_mergedAllDates)
    data_mergedAllDates = data_mergedAllDates.sort_values("Date").reset_index(drop=True)

    if selected_data_values == ["all"]:
        if selected_date_range == "full":
            data2 = historyAllDates
        else:
            data2 = data_mergedAllDates
    else:
        if selected_date_range == "full":
            data2 = history_with_time_columns
        else:
            data2 = data_merged_with_time_columns

    if selected_grouping_value == "day-name":
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data = (
            data2.groupby(grouping_columns, as_index=False, dropna=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        title = "The daily amount of time spent on Spotify during different days of the week"
        xaxis_label = "Day of the week"

    elif selected_grouping_value == "season":
        data2['season'] = data2['Date'].apply(get_season)
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data = (
            data2.groupby(grouping_columns, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        title = "The daily amount of time spent on Spotify during different seasons"
        xaxis_label = "Season of the year"

    elif selected_grouping_value == "year":
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data = (
            data2.groupby(grouping_columns, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        title = "The daily amount of time spent on Spotify during different years"
        xaxis_label = "Year"

    else:
        grouping_columns = ["Date", selected_grouping_value]
        grouped_date_time_data = (
            data2.groupby(grouping_columns, as_index=False)["Time(Minutes)"]
            .sum()
            .sort_values("Date")
            .reset_index()
        )
        title = "The daily amount of time spent on Spotify during different months"
        xaxis_label = "Month of the year"

    if selected_grouping_value == "season":
        fig = px.box(grouped_date_time_data, x='season', y='Time(Minutes)',
                     category_orders={'season': ['Spring', 'Summer', 'Fall', 'Winter']}
                     )
        fig.update_layout(
            title=title,
            yaxis=dict(title="Minutes listened", showgrid=True, gridcolor="white", gridwidth=2, ),
            xaxis=dict(title=xaxis_label),
            font=dict(family="Ubuntu"),
            plot_bgcolor="rgba(0,0,0,0)",
            paper_bgcolor="rgba(0,0,0,0)",
        )
    elif selected_grouping_value == "day-name":
        # color_dict = {'Spring': 'green', 'Summer': 'orange', 'Fall': 'red', 'Winter': 'blue'}
        fig = px.box(grouped_date_time_data, x='day-name', y='Time(Minutes)',
                     category_orders={
                         'day-name': ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']}
                     )
        fig.update_layout(
            title=title,
            yaxis=dict(title="Minutes listened", showgrid=True, gridcolor="white", gridwidth=2, ),
            xaxis=dict(title=xaxis_label),
            font=dict(family="Ubuntu"),
            plot_bgcolor="rgba(0,0,0,0)",
            paper_bgcolor="rgba(0,0,0,0)",
        )
    else:
        fig = px.box(
            grouped_date_time_data, x=selected_grouping_value, y="Time(Minutes)"
        )
        fig.update_layout(
            title=title,
            yaxis=dict(title="Minutes listened", showgrid=True, gridcolor="white", gridwidth=2, ),
            xaxis=dict(title=xaxis_label),
            font=dict(family="Ubuntu"),
            plot_bgcolor="rgba(0,0,0,0)",
            paper_bgcolor="rgba(0,0,0,0)",
        )
    fig.update_traces(marker=dict(color="#1DB954"))
    return fig


# Scatter plot

@app.callback(
    dash.dependencies.Output("scatter-plot", "figure"),
    [
        dash.dependencies.Input("x-axis-dropdown", "value"),
        dash.dependencies.Input("color-dropdown", "value"),
        dash.dependencies.Input('buttonBogusia', 'n_clicks'),
        dash.dependencies.Input('buttonWiktoria', 'n_clicks'),
        dash.dependencies.Input('buttonKasia', 'n_clicks')
    ],
)
def update_scatter_plot(x_axis_value, color_value, buttonBogusia_clicks, buttonWiktoria_clicks, buttonKasia_clicks):
    global button_id
    label_dict = {
        "TotalAcademicActivities": "Total Academic Activities",
        "CoffeeCups": "Coffee Cups",
        "AverageEnergyLevel": "Average Energy Level",
        "StepsCount": "Steps Count",
        "NumberOfLecturesToday": "Number Of Lectures Today",
        "WorkingHours": "Working Hours",
        "SleepMinutes": "Sleep Minutes",
    }
    if button_id == 'buttonBogusia':
        history = pd.read_csv("./Data/Bogusia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Bogusia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonWiktoria':
        history = pd.read_csv("./Data/Wiktoria/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Wiktoria/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date
    elif button_id == 'buttonKasia':
        history = pd.read_csv("./Data/Kasia/StreamingHistoryExtended.csv")
        history = history.dropna(subset=['ts'])
        history["Date"] = pd.to_datetime(history["ts"]).dt.date
        personalData = pd.read_csv("Data/Kasia/personalDataFull.csv")
        personalData["Date"] = pd.to_datetime(personalData["Date"]).dt.date

    history_grouped = history.groupby('Date').agg({'ms_played': 'sum'})
    history_grouped['MusicMinutes'] = history_grouped['ms_played'] / 60000
    combined_data = pd.merge(personalData, history_grouped, how='left', on='Date')
    combined_data['MusicMinutes'] = combined_data['MusicMinutes'].fillna(0)
    combined_data['TotalAcademicActivities'] = combined_data['Projects'] + combined_data['Tests'] + combined_data[
        'NumberOfLecturesToday']
    combined_data['AverageEnergyLevel'] = (combined_data['EnergyLevelMorning'] + combined_data[
        'EnergyLevelEvening']) / 2
    data_merged = pd.merge(history, personalData, on="Date")
    data_merged["spotify_track_uri"] = data_merged["spotify_track_uri"].str[14:]

    xaxis_label = label_dict.get(x_axis_value, x_axis_value)
    color_label = label_dict.get(color_value, color_value)

    x_axis_for_plot = label_dict.get(x_axis_value, x_axis_value)
    color_for_plot = label_dict.get(color_value, color_value)

    modified_data = combined_data.copy()

    for original_name, new_name in label_dict.items():
        if original_name in modified_data.columns:
            modified_data = modified_data.rename(columns={original_name: new_name})

    categorical_columns = ['Coffee Cups', "Productivity", "Meeting"]

    for column in categorical_columns:
        modified_data[column] = modified_data[column].sort_values(ascending=True).values

    for column in categorical_columns:
        modified_data[column] = modified_data[column].astype('category')

    for column in reversed(categorical_columns):
        modified_data = modified_data.sort_values(by=column, ascending=True)
    #green_shades = ['lightgreen', 'mediumseagreen', 'forestgreen', 'darkgreen', 'chartreuse', 'mediumspringgreen','olive']
    viridis_shades = ['yellow', 'greenyellow', 'mediumseagreen']
    fig = px.scatter(
        modified_data,
        x=x_axis_for_plot,
        y='MusicMinutes',
        color=color_for_plot,
        color_continuous_scale='Viridis',
        color_discrete_sequence=viridis_shades
    )
    fig.update_layout(
        title=f"Music Listening Time vs {xaxis_label} and {color_label}",
        xaxis=dict(title=xaxis_label, showgrid=True, gridcolor="white", gridwidth=2, ),
        yaxis=dict(title="Music Listening Time (Minutes)"),
        font=dict(family="Ubuntu"),
        legend=dict(title=color_label),
        plot_bgcolor="rgba(0,0,0,0)",
        paper_bgcolor="rgba(0,0,0,0)",
    )
    fig.update_traces(marker={'size': 10, 'line': {'width': 0.5, 'color': 'black'}})
    return fig


if __name__ == '__main__':
    app.run_server(debug=True)
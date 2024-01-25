import dash_bootstrap_components as dbc
import dash_mantine_components as dmc
import pages.components
from dash import html

layout = dbc.Container(
    [
        pages.components.top_layout,
        # Nowy div z gridem
        dbc.Row(
            [
                # Lewa kolumna z tekstem
                dbc.Col(
                    [
                        dmc.Title(
                            "Data and running is a future",
                            color="orange",
                            size="h1",
                            style={"margin": "0 0 10px"},
                        ),
                        dmc.Text(
                            "A cross between data visualization techniques and a passion for running, this project celebrates the combination of the world of numbers and the energy of movement. In it, every meter traveled is transformed into a visual story, and the runner's breath rhythmically intertwines with the pulsing data.",  # noqa: E501
                            align="left",
                            style={"margin": "0 0 10px"},
                        ),
                        dmc.Text(
                            "In this innovative space, data and running are the two pillars of human endeavor - to know and to be active. Here, in a harmony of numbers and steps, we create not just a project, but a narrative of life, discovery and the relentless march forward.",  # noqa: E501
                            align="left",
                        ),
                    ],
                    width=3,
                ),
                # Prawa kolumna z placeholderem na zdjęcie
                dbc.Col(
                    [
                        html.Div(
                            dbc.CardImg(
                                src="assets/runners.jpg",
                                top=True,
                                style={
                                    "width": "90%",
                                    "padding": "0",
                                    "margin": "0",
                                },  # noqa: E501
                            ),
                            style={
                                "width": "80%",
                                "height": "500px",
                                "backgroundColor": "#f0f0f000",
                                "textAlign": "center",
                                "lineHeight": "200px",
                                "padding": "10px",
                                "margin": "0 auto",
                            },
                        )
                    ],
                    width=6,
                ),
            ],
            style={
                "width": "100%",
                "height": "100%",
                "display": "flex",
                "justifyContent": "center",
                "alignItems": "center",
            },
        ),
        # ... można dodać inne komponenty
    ],
    fluid=True,
    style={
        "textAlign": "left",
        "width": "100%",
        "maxWidth": "1200px",
        "margin": "10px auto 50px",
        # "height": "100vh",
    },
)

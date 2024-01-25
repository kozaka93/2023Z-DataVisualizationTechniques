import dash_bootstrap_components as dbc
from dash import dcc, html

layout = dbc.Container(
    [
        dbc.Row(
            dbc.Col(
                html.H1(
                    "👋 Welcome to the top runners dashboard! 🏃",
                    style={"text-align": "center", "margin": "50px 0"},
                )
            )
        ),
        dbc.Row(
            dbc.Container(
                [
                    dcc.Link(
                        dbc.Button("Statistics"),
                        href="/stats",
                    ),
                    dcc.Link(
                        dbc.Button("Map"),
                        href="/map",
                    ),
                    dcc.Link(
                        dbc.Button("About"),
                        href="/about",
                    ),
                ],
                fluid=True,
                style={
                    "textAlign": "center",  # Center-align the buttons
                    "maxWidth": "1200px",
                    "margin": "10px auto 50px",
                    # "backgroundColor": "red"
                },
            ),
        ),
    ]
)

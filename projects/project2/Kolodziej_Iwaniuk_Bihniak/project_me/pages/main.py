import dash_mantine_components as dmc
from dash import dcc

layout = dmc.Container(
    [
        dcc.Link(
            dmc.Button("Home Page", variant="filled", color="black"),
            href="/",
        ),
        dcc.Link(
            dmc.Button("Map", variant="outline"),
            href="/subpage",
        ),
        dcc.Link(
            dmc.Button("About", variant="outline"),
            href="/about",
        ),
    ],
    fluid=True,
    style={
        "textAlign": "left",
        "width": "100%",
        "maxWidth": "1200px",
        "margin": "10px auto 50px",
    },
)

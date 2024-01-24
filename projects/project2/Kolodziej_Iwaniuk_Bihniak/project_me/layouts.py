from dash import dcc


def create_dropdown():
    return dcc.Dropdown(
        id="my-dropdown",
        options=[
            {"label": "Option 1", "value": "OPT1"},
            {"label": "Option 2", "value": "OPT2"},
            {"label": "Option 3", "value": "OPT3"},
        ],
        value="OPT1",
    )

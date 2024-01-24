import dash
import dash_bootstrap_components as dbc
from dash_bootstrap_templates import load_figure_template

load_figure_template("superhero")
external_stylesheets = [dbc.themes.SUPERHERO]

app = dash.Dash(
    __name__,
    suppress_callback_exceptions=True,
    external_stylesheets=external_stylesheets,
)
server = app.server

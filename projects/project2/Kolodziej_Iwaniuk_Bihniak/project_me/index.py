import pages
from app import app
from dash import dcc, html
from dash.dependencies import Input, Output

app.layout = html.Div(
    [dcc.Location(id="url", refresh=False), html.Div(id="page-content")]
)
app.title = "Top Runners"
server=app.server


@app.callback(Output("page-content", "children"), [Input("url", "pathname")])
def display_page(pathname):
    if pathname == "/":
        return pages.stats_layout
    if pathname == "/stats":
        return pages.stats_layout
    elif pathname == "/map":
        return pages.map_layout
    elif pathname == "/about":
        return pages.about_layout
    else:
        return "404"


if __name__ == "__main__":
    app.run_server(debug=True)

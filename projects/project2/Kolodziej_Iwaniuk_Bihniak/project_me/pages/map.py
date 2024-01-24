import dash_bootstrap_components as dbc
import dash_leaflet as dl
import gpxpy
import pages.components
from app import app
from dash import dcc, html
from dash.dependencies import Input, Output


def parse_gpx(file_path):
    with open(file_path, "r") as gpx_file:
        gpx = gpxpy.parse(gpx_file)
        track = gpx.tracks[0]
        segment = track.segments[0]
        # Extracting coordinates and times
        coords = [
            (point.latitude, point.longitude) for point in segment.points
        ]  # noqa: E501
        times = [point.time for point in segment.points]
    return coords, times


route1_coords, route1_times = parse_gpx("project_me/data/routes/test1.gpx")
route2_coords, route2_times = parse_gpx("project_me/data/routes/test2.gpx")
route3_coords, route3_times = parse_gpx("project_me/data/routes/test3.gpx")

routes = {
    "route1": {"coords": route1_coords, "times": route1_times},
    "route2": {"coords": route2_coords, "times": route2_times},
    "route3": {"coords": route3_coords, "times": route3_times},
}


def format_times(times):
    marks = {}
    for i, time in enumerate(times):
        if i % (len(times) // 5) == 0:  # Check if divisible by 60
            marks[i] = {"label": time.strftime("%H:%M:%S")}

    return marks


layout = dbc.Container(
    [
        pages.components.top_layout,
        dbc.Row(
            [
                # Left Column for Map and Slider
                dbc.Col(
                    [
                        dl.Map(
                            [
                                dl.TileLayer(),
                                dl.Polyline(
                                    id="route", positions=route1_coords
                                ),  # noqa: E501
                            ],
                            id="map",
                            style={
                                "width": "100%",
                                "height": "500px",
                            },
                            center=(route1_coords[0][0], route1_coords[0][1]),
                            zoom=14,
                        ),
                    ],
                    className="left-column",
                    style={"padding-right": "20px"},
                ),
                # Right Column for RadioItems and Labels
                dbc.Col(
                    [
                        dbc.Row(
                            [
                                dbc.Col(
                                    html.H4(
                                        "Each of us went for an individual run around Wola to compare the times and routes of our runs in a sporting spirit.",  # noqa: E501
                                        style={
                                            "color": "white",
                                            "textAlign": "left",
                                        },
                                    ),  # noqa: E501
                                ),
                                dbc.Col(
                                    html.H4(
                                        "Matthew, Igor and Nazarii, equipped with smartphones, set off in different directions, exploring the picturesque paths and streets of the district.",  # noqa: E501
                                        style={
                                            "color": "white",
                                            "textAlign": "left",
                                        },
                                    ),
                                ),
                            ]
                        ),
                        dbc.Row(
                            dbc.RadioItems(
                                id="route-selector",
                                value="route1",
                                inline=True,
                                labelStyle={"margin-right": "20px"},
                                options=[
                                    {"label": "Mateusz", "value": "route1"},
                                    {"label": "Igor", "value": "route2"},
                                    {"label": "Nazari", "value": "route3"},
                                ],
                            ),
                            style={"marginTop": "50px"},
                        ),
                        # Label for displaying current time and speed
                        dbc.Row(
                            id="time-speed-label", style={"fontSize": 20}
                        ),  # noqa: E501
                        dcc.Slider(
                            id="time-slider",
                            min=0,
                            max=len(route1_times) - 1,
                            value=0,
                            step=1,
                            updatemode="drag",
                            # marks=None
                            marks=format_times(
                                route1_times
                            ),  # Use the updated function
                        ),
                    ],
                    className="right-column",
                    style={
                        "display": "grid",
                        "gridTemplateRows": "1fr 1fr",
                        "gap": "10px",
                    },
                ),
            ],
            style={
                "display": "grid",
                "gridTemplateColumns": "1fr 1fr",
                "gap": "20px",
                "alignItems": "center",
                "justifyContent": "center",
                "alignContent": "center",
                "height": "100%",
                "width": "80%",
                "margin": "0 auto",
            },
        ),
    ],
    style={"textAlign": "center", "width": "100%", "margin": "0 auto"},
    fluid=True,
)


@app.callback(Output("route", "positions"), [Input("route-selector", "value")])
def update_route(selected_route):
    return routes[selected_route]["coords"]


@app.callback(
    [Output("time-slider", "max"), Output("time-slider", "marks")],
    [Input("route-selector", "value")],
)
def update_slider(selected_route):
    max_val = len(routes[selected_route]["times"]) - 1
    marks = format_times(routes[selected_route]["times"])
    return max_val, marks


@app.callback(
    Output("map", "children"),
    [Input("time-slider", "value"), Input("route-selector", "value")],
)
def update_marker(slider_value, selected_route):
    route_coords = routes[selected_route]["coords"]
    return [
        dl.TileLayer(),
        dl.Polyline(positions=route_coords,color="orange"),
        dl.CircleMarker(center=route_coords[slider_value],radius=12,color="#0f2537",id="marker"),
    ]

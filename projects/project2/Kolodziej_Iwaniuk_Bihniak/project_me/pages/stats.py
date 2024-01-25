import dash_bootstrap_components as dbc
import numpy as np
import pages.components
import pandas as pd
import plotly.express as px
from app import app
from dash import Input, Output, dcc, html

# -- Import and clean data
df = pd.read_csv("project_me/data/run_stats/activities.csv", index_col="Activity ID")

df["Elapsed Time"] = df["Elapsed Time"] / 60
df["Season"] = (
    df["Activity Date"]
    .apply(lambda x: x.split(",")[0].split(" ")[0])
    .replace(
        {
            "Jan": "Winter",
            "Feb": "Winter",
            "Mar": "Spring",
            "Apr": "Spring",
            "May": "Spring",
            "Jun": "Summer",
            "Jul": "Summer",
            "Aug": "Summer",
            "Sep": "Fall",
            "Oct": "Fall",
            "Nov": "Fall",
            "Dec": "Winter",
        }
    )
)
df["Distance Range"] = pd.cut(
    df["Distance"],
    bins=[0, 5, 10, 15, np.inf],
    labels=["0-5", "5-10", "10-15", "15+"],  # noqa: E501
)
# ------------------------------------------------------------------------------
# App layout
layout = dbc.Container(
    [
        pages.components.top_layout,
        dbc.Row(
            dbc.Col(html.H1("Statistics", style={"text-align": "center"}))
        ),  # noqa: E501
        dbc.Row(
            [
                dbc.Col(
                    html.H2(
                        id="whos_the_best_title",
                        children="Who's the best?",
                        style={"text-align": "center"},
                    )
                ),
                dbc.Col(
                    html.H2(
                        id="any_dependencies_title",
                        children="Any correlations?",
                        style={"text-align": "center"},
                    )
                ),
            ]
        ),
        dbc.Row(
            [
                dbc.Col(
                    [
                        html.H4(
                            "Choose category", style={"text-align": "center"}
                        ),  # noqa: E501
                        dbc.Select(
                            id="whos_the_best_dropdown",
                            options=[
                                {"label": "Speed", "value": "Average Speed"},
                                {"label": "Distance", "value": "Distance"},
                                {"label": "Duration", "value": "Elapsed Time"},
                            ],
                            value="Average Speed",
                            style={"width": "40%", "margin": "0 auto"},
                        ),
                    ]
                ),
                dbc.Col(
                    [
                        html.H4("Choose plot", style={"text-align": "center"}),
                        dbc.Select(
                            id="any_dependencies_dropdown",
                            options=[
                                {
                                    "label": "Speed vs Duration",
                                    "value": "Speed vs Duration",
                                },
                                {
                                    "label": "Duration vs Season",
                                    "value": "Duration vs Season",
                                },
                                {
                                    "label": "Length vs Season",
                                    "value": "Length vs Season",
                                },
                            ],
                            value="Speed vs Duration",
                            style={"width": "40%", "margin": "0 auto"},
                        ),
                    ]
                ),
            ]
        ),
        dbc.Row(
            [
                dbc.Col(dcc.Graph(id="whos_the_best", figure={})),
                dbc.Col(dcc.Graph(id="any_dependencies", figure={})),
            ]
        ),
        dbc.Row(
            [
                dbc.Col(
                    [
                        html.H2(
                            id="do_we_run_a_lot_title",
                            children="Do we run a lot?",
                            style={"text-align": "center"},
                        ),
                        html.H5(
                            id="do_we_run_a_lot_subtitle",
                            children="Number of runs across varius distance ranges",  # noqa: E501
                            style={"text-align": "center"},
                        ),
                    ]
                ),
                dbc.Col(
                    [
                        html.H2(
                            id="max_speed_distance_time_title",
                            children="When do we achieve our peak performance?",  # noqa: E501
                            style={"text-align": "center"},
                        ),
                        html.H5(
                            id="max_speed_distance_time_subtitle",
                            children="How is max speed influenced by distance and run duration?",  # noqa: E501
                            style={"text-align": "center"},
                        ),
                    ],
                    width=9,
                ),
            ],
            style={"marginTop": "50px"},
        ),
        dbc.Row(
            [
                dbc.Col(
                    dcc.Graph(
                        id="do_we_run_a_lot",
                        figure=px.bar(
                            data_frame=df.groupby(
                                ["Runner", "Distance Range"]
                            )[  # noqa: E501
                                "Distance Range"
                            ]
                            .value_counts()
                            .reset_index(),  # noqa: E501
                            x="Runner",
                            y="count",
                            color="Distance Range",
                            barmode="stack",
                            labels={"count": "Number of runs"},
                        ).update_layout(
                            legend_title_text="Distance Range (km)",
                            width=325,
                            height=600,
                            legend=dict(
                                orientation="h",
                                yanchor="top",
                                xanchor="left",
                                borderwidth=2,
                                y=-0.2,
                                x=0.1,
                                itemsizing="constant",
                                entrywidth=30,
                            ),
                        ),
                    )
                ),
                dbc.Col(
                    dcc.Graph(
                        id="max_speed_distance_time",
                        figure=px.scatter(
                            df,
                            x="Elapsed Time",
                            y="Distance",
                            color="Runner",
                            size="Max Speed",
                            marginal_x="violin",
                            marginal_y="violin",
                            labels={
                                "Elapsed Time": "Duration (min)",
                                "Distance": "Distance (km)",
                                "Max Speed": "Max Speed (m/s)",
                            },
                        ).update_layout(
                            autosize=False,
                            width=1025,
                            height=600,
                        ),
                    ),
                    width=9,
                ),
            ]
        ),
    ],
    fluid=True,
    style={
        "textAlign": "center",
        "width": "100%",
        "maxWidth": "1400px",
        "margin": "0 auto",
    },
)


# ------------------------------------------------------------------------------
# Connect the Plotly graphs with Dash Components
@app.callback(
    [
        Output(
            component_id="whos_the_best_title", component_property="children"
        ),  # noqa: E501
        Output(component_id="whos_the_best", component_property="figure"),
    ],
    [Input(component_id="whos_the_best_dropdown", component_property="value")],
)
def update_who_is_the_best(option_slctd):
    container = (
        "Who's the fastest?"
        if option_slctd == "Average Speed"
        else "Who runs the longest distance?"
        if option_slctd == "Distance"
        else "Who endures the longest time?"
    )

    fig = px.bar(
        data_frame=df.groupby("Runner")[
            ["Distance", "Elapsed Time", "Average Speed"]
        ].mean(),
        y=option_slctd,
        title=f"The best in {option_slctd} is {df.groupby('Runner')[option_slctd].mean().idxmax()}!",  # noqa: E501
        labels={
            option_slctd: "Average Speed (m/s)"
            if option_slctd == "Average Speed"
            else "Elapsed Time (min)"
            if option_slctd == "Elapsed Time"
            else "Distance (km)",
            "Runner": "Runner",
        },
    ).update_layout(xaxis_categoryorder="total descending")

    return container, fig


@app.callback(
    Output(component_id="any_dependencies", component_property="figure"),
    [
        Input(  # noqa: E501
            component_id="any_dependencies_dropdown",
            component_property="value",  # noqa: E501
        )  # noqa: E501
    ],  # noqa: E501
)
def update_any_dependencies(option_slctd):
    if option_slctd == "Speed vs Duration":
        return px.line(
            data_frame=df.sort_values(by="Elapsed Time"),
            x="Elapsed Time",
            y="Average Speed",
            color="Runner",
            title="Average Speed across distances",
            labels={
                "Elapsed Time": "Duration (min)",
                "Average Speed": "Speed (m/s)",
            },  # noqa: E501
        )
    elif option_slctd == "Duration vs Season":
        return px.bar(
            data_frame=df.groupby(["Runner", "Season"])["Elapsed Time"]
            .mean()
            .reset_index(),
            x="Season",
            y="Elapsed Time",
            color="Runner",
            title="Average Duration across seasons",
            labels={"Elapsed Time": "Duration (min)"},
            barmode="group",
        )
    elif option_slctd == "Length vs Season":
        return px.bar(
            data_frame=df.groupby(["Runner", "Season"])["Distance"]
            .mean()
            .reset_index(),
            x="Season",
            y="Distance",
            color="Runner",
            title="Average Distance across seasons",
            labels={"Distance": "Distance (km)"},
            barmode="group",
        )

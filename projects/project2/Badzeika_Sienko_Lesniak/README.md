# Mapsify

## Google Maps and Spotify data analysis

## Overview
This Shiny app integrates Google Maps and Spotify data to provide a comprehensive analysis of music streaming patterns, travel routes, and district-based music preferences in Warsaw. It offers interactive visualizations and insights, catering to different users' music listening habits. You can access the app using this link https://glebbadzeika.shinyapps.io/R-Shiny-Spotify-Google-Maps/

## Features
- **Map and Data**: Visualize travel routes and streaming data on a map, including custom popups with Spotify song details.
- **Districts of Warsaw**: Explore music preferences across different districts of Warsaw, with data presented both spatially and temporally.
- **Streaming Data Analysis**: Delve into streaming patterns over time, examining genres and audio features of the music listened to by different users.

## Data Processing
The app processes CSV data for each user, including information about their streaming history, genre preferences, top songs, and artists with help of python scripts, that can be found in folder Python. 

## Libraries Used
- Shiny for interactive UI
- Leaflet for mapping
- Plotly and ggplot2 for data visualization
- Other libraries for data manipulation and presentation: dplyr, lubridate, geosphere, sf, tidyr, RColorBrewer, DT, bslib, shinycssloaders, jsonlite, stringr, ggstream, shinythemes, packcircles, ggiraph, stringi

## Usage
1. Select a tab to view the desired visualization.
2. Use the sidebar options to filter data based on date, user, or other parameters.
3. Hover over visualizations for detailed information.

## Installation
To run this app locally, clone the repository and ensure you have R, Python and the above-mentioned libraries installed. Run the app using RStudio or a similar IDE. Before starting the Shiny app, you have to preprocess your data from Google Maps and Spotify with Python scripts



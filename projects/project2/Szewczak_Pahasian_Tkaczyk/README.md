# Me At YouTube

## Introduction
Welcome to "[Me At YouTube](https://milanna.shinyapps.io/MeAtYouTube/)" an R Shiny web application designed to analyze YouTube data for three students: Milanna, Krzysztof, and Michał. This application allows users to explore various aspects of their YouTube video consumption patterns through interactive visualizations.

## Overview
The application consists of four main pages, each providing unique insights into the users' YouTube activities.

## Pages

### 1. Words
- **Description:** The most popular words in the titles of watched videos.
- **Features:**
  - Change the number of displayed words.
  - Select a time range for data analysis.
  - Set a minimum word length for filtering.

### 2. Trending
- **Description:** Most watched channels statistics by date.
- **Features:**
  - Top 5 channels with the largest number of watched videos.
  - Statistics per month (displayed as a bar plot).
  - Share of videos from the top 5 channels in the overall content watched (displayed as a pie plot).
  - Change the time range for data analysis.

### 3. Channels
- **Description:** Table of channels with the number of watched videos.
- **Features:**
  - Choose individuals whose data is displayed using checkboxes.
  - Search for channels in the search line.

### 4. Time
- **Description:** Statistics related to time, including ads watched and videos watched over time.
- **Features:**
  - Percent of ads watched over time by months (line plot).
  - Videos watched over time by months (line plot).
  - Ads watched over time by months (line plot).
  - Films watched by time slot statistics (bar plot).
  - Interactive buttons to change the displayed plots.

## Libraries Used
- `ggwordcloud`: For creating the word cloud plot on the Words page.
- `plotly`: Used for creating interactive plots on the Trending and Time pages.
- `bslib`: For the interface design.
- `shiny`, `ggplot2`, `tidyverse`, `dplyr`, `stringi`, `stringr`, `ggfun`, `thematic`.

## Deployment
The application is deployed and can be accessed through the following link: [Me At YouTube](https://milanna.shinyapps.io/MeAtYouTube/).

## Data Source
The data used for this analysis is sourced from the users' YouTube watch history in JSON format.


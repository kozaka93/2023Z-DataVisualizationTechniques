library(shiny)
library(dplyr)
library(lubridate)
library(geosphere)
library(leaflet)
library(osrm)
library(sf)
library(tidyr)
library(RColorBrewer)
library(DT)
library(bslib)
library(shinycssloaders)
library(jsonlite)
library(stringr)
library(plotly)
library(shinydashboard)
library(bslib)
library(plotly)
library(shinyjs)
library(ggplot2)
library(dplyr)
library(ggstream)
library(shinythemes)
library(shinydashboard)
library(packcircles)
library(ggiraph)
library(stringi)
library(stringr)

pad_legend_title <- function(title, desired_length) {
  str_pad(title, width = desired_length, side = "right", pad = " ")
}

load_and_process_data <- function(person) {
  stream_data <- read.csv(paste0("pliki_csv/streamgraph_", person, ".csv"))
  stream_data$endTime <- as.POSIXct(stream_data$endTime)
  
  genre_data <- read.csv(paste0("pliki_csv/gatunki_", person, ".csv"), encoding = "UTF-8")
  top_data <- read.csv(paste0("pliki_csv/top_10_", person, ".csv"))
  top_genre <- top_data %>% 
    select("genre") %>% 
    head(1)
  
  genre_data <- genre_data %>%
    mutate(msPlayed = as.numeric(msPlayed),
           minutesPlayed = msPlayed / (1000 * 60))
  
  top_time_played <- genre_data %>%
    filter(genre == top_genre[1,]) %>%
    summarise(top_time_played = sum(minutesPlayed))
  
  liczba_gatunkow <- length(unique(genre_data$genre))
  
  set.seed(997)
  artist_data <- read.csv(paste0("pliki_csv/artists_sorted_", person, ".csv"))
  artist_data <- head(artist_data, 100)
  artist_data <- artist_data[sample(nrow(artist_data)), ]
  packing <- circleProgressiveLayout(artist_data$Count, sizetype = 'area')
  artist_data <- cbind(artist_data, packing)
  dat.gg <- circleLayoutVertices(packing, npoints = 70)
  
  return(list(
    top_genre = top_genre,
    stream_data = stream_data,
    genre_data = genre_data,
    top_time_played = top_time_played,
    liczba_gatunkow = liczba_gatunkow,
    artist_data = artist_data,
    dat.gg = dat.gg
  ))
}

martyna <- load_and_process_data("martyna")
zuza <- load_and_process_data("zuza")
gleb <- load_and_process_data("gleb")

custom_popup_style <- "
<style>
  .leaflet-popup-content {
    color: #fff;
    background-color: #333;
    padding: 5px;
    border-radius: 5px;
  }
  .leaflet-popup-content-wrapper{
    background-color: #333;
    width: 150px;
    text-align: center;
  }

</style>
"
custom_popup_style_zuzia<-"
<style>
  .leaflet-popup-content {
    color: #fff;
    background-color: #333;
    padding: 5px;
    border-radius: 5px;
  }
  .leaflet-popup-content-wrapper{
    background-color: #333;
    text-align: center;
  }

</style>
"

df_spotify_maps_zuzia<-readRDS("ZuziaDane/spotify_maps_zuzia.rds")
df_spotify_maps_gleb<-readRDS("ZuziaDane/spotify_maps_gleb.rds")
df_spotify_maps_martyna<-readRDS("ZuziaDane/spotify_maps_martyna.rds")

df<-df_spotify_maps_zuzia
shapefilePath <- "GlebDane/dzielnice_Warszawy/dzielnice_Warszawy.shp"
gleb_districts<-read.csv("GlebDane/songs_with_districts_gleb.csv")
zuzia_districts<-read.csv("GlebDane/songs_with_districts_zuzia.csv")
martyna_districts<-read.csv("GlebDane/songs_with_districts_martyna.csv")

ui <- dashboardPage(
  skin="black",
  dashboardHeader(title = "Mapsify"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map and Data", tabName = "mapAndData", icon = icon("map")),
      menuItem("Districts of Warsaw", tabName = "districtsWarsaw", icon = icon("building")),
      menuItem("Streaming Data Analysis", tabName = "streamingData", icon = icon("stream"))
    )
  ),
  dashboardBody(
    theme = bslib::bs_theme(bootswatch = "cyborg"),
    tabItems(
      tabItem(tabName = "mapAndData",
              sidebarLayout(
                sidebarPanel(
                  dateInput("dayInput", "Select a date:",
                            min = max(min(df_spotify_maps_gleb$startTimestamp),
                                        min(df_spotify_maps_zuzia$startTimestamp),
                                        min(df_spotify_maps_martyna$startTimestamp)),
                            max = min(max(df_spotify_maps_gleb$startTimestamp),
                                      max(df_spotify_maps_zuzia$startTimestamp),
                                      max(df_spotify_maps_martyna$startTimestamp))
                            ),
                  selectInput("personInput", "Select a person:", 
                              choices = c("Hleb", "Zuzia", "Martyna"), multiple = FALSE)
                ),
                mainPanel(
                  fluidRow(
                    valueBoxOutput("km_travelled"),
                    valueBoxOutput("minutes_played_songs"),
                    valueBoxOutput("percent_with_music")),
                  leafletOutput("map"),
                  DTOutput("spotifyTable"),
                  DTOutput("summaryTable")
                )
              )
      ),
      tabItem(tabName = "districtsWarsaw",
              sidebarLayout(
                sidebarPanel(
                  selectInput("personInputGleb", "Select a person:", 
                              choices = c("Zuzia", "Hleb", "Martyna"), multiple = FALSE),
                  dateRangeInput("dateInput", "Select a date:",
                                 start = max(min(gleb_districts$endTime),
                                             min(zuzia_districts$endTime),
                                             min(martyna_districts$endTime)),
                                 end = min(max(gleb_districts$endTime),
                                           max(zuzia_districts$endTime),
                                           max(martyna_districts$endTime)))
                ),
                mainPanel(
                  leafletOutput("map_dzielnice"),
                  plotlyOutput("districtTime")
                )
              )
      ),
      tabItem(tabName = "streamingData",
              fluidRow(
                column(
                  width = 3,
                  style = "background-color: #2E2E2E; padding: 20px;", 
                  selectInput("personInputMartyna", "Select a person:", 
                              choices = c("Zuza", "Hleb", "Martyna"), multiple = FALSE),
                  tags$head(
                    tags$style(HTML('.content-wrapper {background-color: #2E2E2E;}'))
                  )
                ),
                column(
                  width = 9,
                  fluidRow(
                    h3("What genres have we been listening to over time", style = "color: white;"),
                    plotOutput("stream_plot")
                  ),
                  fluidRow(
                    valueBoxOutput("liczba_gatunkow_box"),
                    valueBoxOutput("top_box"),
                    valueBoxOutput("top_time_played_box")
                  ),
                  fluidRow(
                    selectInput("feature", "Choose Audio Feature", choices = c("Danceability", "Liveness", "Energy", "Valence")),
                    girafeOutput("packcircles_plot", width="auto", height = "auto")
                  )
                )
              )
      )
      
    )
  )
)

server <- function(input, output) {
  filteredDateSpotifyMaps<-reactive({
    if(input$personInput == "Hleb"){
      dfSpotifyMaps<-df_spotify_maps_gleb
    }
    if(input$personInput == "Martyna"){
      dfSpotifyMaps<-df_spotify_maps_martyna
    }
    if(input$personInput == "Zuzia"){
      dfSpotifyMaps<-df_spotify_maps_zuzia
    }
    
    dfSpotifyMaps<-dfSpotifyMaps %>% filter(!is.na(startTimestamp), as.Date(startTimestamp) == input$dayInput)
    return(dfSpotifyMaps)
  })

  output$map <- renderLeaflet({
    df<-filteredDateSpotifyMaps()
    
    m <- leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Stadia.AlidadeSmoothDark)
    
    num_routes <- length(df$osrm_routes)
    colors <- colorRampPalette(brewer.pal(min(num_routes, 9), "Set1"))(num_routes)
    
    for (i in 1:num_routes) {
      route_geom <- df$osrm_routes[[i]]
      start_time_formatted <- format(as.POSIXct(df$startTimestamp[i]), format = "%H:%M")
      end_time_formatted <- format(as.POSIXct(df$endTimestamp[i]), format = "%H:%M")
      
      if (!is.na(df$latitude[i]) && !is.na(df$longitude[i]) && df$latitude[i] != 0 && df$longitude[i] != 0) {
        m <- m %>% addMarkers(lng = df$longitude[i], lat = df$latitude[i], 
                              popup = df$startName[i])
      }
      
      if (!is.null(route_geom)) {
        popup_content <- paste("Activity Type: ", df$activityType[i], "<br>",
                               "Start: ", start_time_formatted, "<br>",
                               "End: ", end_time_formatted, "<br>",
                               sep = "")
        
        if (i <= length(df$songs_df)) {
          songs_info <- df$songs_df[[i]]
          if (!is.null(songs_info) && nrow(songs_info) > 0) {
            songs_text <- apply(songs_info, 1, function(row) {
              paste(row['artistName'], "-", row['trackName'])
            })
            songs_popup <- paste("Songs:<br>", paste(songs_text, collapse = "<br>"), sep = "")
            popup_content <- paste(custom_popup_style_zuzia, popup_content, songs_popup, sep = "<br>")
          }
        }
        m <- m %>% addPolylines(data = route_geom, color = colors[i], popup = popup_content)
      }
    }
    m
  })
  
  output$km_travelled <- renderValueBox({
    df <- filteredDateSpotifyMaps()
    totalDistance <- round(sum(df$distance, na.rm = TRUE),2)
    valueBox(
      format(totalDistance, nsmall = 2), 
      "Total Distance Travelled on Selected Day",
      icon = icon("car") 
    )
  })
  output$minutes_played_songs <- renderValueBox({
    df <- filteredDateSpotifyMaps()
    totalMinutesPlayed <- round(sum(df$sum_msPlayed, na.rm = TRUE),2)
    valueBox(
      format(totalMinutesPlayed, nsmall = 2), 
      "Total Minutes Played on Spotify",
      icon = icon("music-note-beamed") 
    )
  })
  output$percent_with_music <- renderValueBox({
    df <- filteredDateSpotifyMaps() 
  
    if (is.null(df) || nrow(df) == 0) {
      avgPercent <- NA
    } else {
      avgPercent <- round(mean(df$percent_music2travel, na.rm = TRUE),2)
    }
    
    valueBox(
      value = ifelse(is.na(avgPercent), "No data", paste0(format(avgPercent, nsmall = 2), "%")),
      subtitle = "Percentage of Travel Time with Music",
      icon = icon("headphones")
    )
  })

  districts <- st_read(shapefilePath)
  
  districts_wgs84 <- st_transform(districts, crs = 4326)
  
  
  filteredDataMostPopularSong <- reactive({
    
    
    if(input$personInputGleb == "Hleb"){
      final_final<-gleb_districts
    }
    if(input$personInputGleb == "Martyna"){
      final_final<-martyna_districts
    }
    if(input$personInputGleb == "Zuzia"){
      final_final<-zuzia_districts
    }
    
    final_final_filtered <- final_final %>%
      
      filter(between(as.Date(endTime), as.Date(input$dateInput[1], format = "%Y-%m-%d"), as.Date(input$dateInput[2], format = "%Y-%m-%d"))) %>%
      select(artistName, trackName, district) %>%
      group_by(district, artistName, trackName) %>%
      summarise(playCount = n(), .groups = 'drop') %>%
      arrange(district, desc(playCount)) %>%
      group_by(district) %>%
      slice(1)
    
    
    return(final_final_filtered)
  })
  
  
  output$map_dzielnice <- renderLeaflet({
    
    songs<-filteredDataMostPopularSong()
    
    map<- merge(districts_wgs84,songs,by.x='nazwa_dzie',by.y='district',
                all.x = TRUE, all.y = FALSE)
    print(map)
    
    leaflet(map) %>% 
      addTiles() %>% 
      addPolygons( fillOpacity = 0.5, smoothFactor = 0.5,
                   
                   popup=ifelse(is.na(map$artistName), "", paste(custom_popup_style,"<b>",map$nazwa_dzie,"</br>",
                                                                 map$artistName,"-</br>",map$trackName,"</b>")),
                   popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
                   col=ifelse(is.na(map$artistName), "grey", "#65aecb")
      )%>% addProviderTiles(providers$Stadia.AlidadeSmoothDark)
  })
  
  filteredDataDistrictsTime <- reactive({
    if(input$personInputGleb == "Hleb"){
      df<-gleb_districts
      
    }
    if(input$personInputGleb == "Martyna"){
      df<-martyna_districts
      
    }
    if(input$personInputGleb == "Zuzia"){
      df<-zuzia_districts
      
    }
    
    df$day <- as.Date(df$endTime,format = "%Y-%m-%d")
    
    result_df <- df %>%
      
      filter(between(as.Date(endTime), as.Date(input$dateInput[1], format = "%Y-%m-%d"), as.Date(input$dateInput[2], format = "%Y-%m-%d"))) %>%
      
      group_by(day, district) %>%
      summarise(total_ms_played = sum(msPlayed)) %>%
      mutate(total_seconds_played = total_ms_played / 1000/60) %>%
      select(day, district, total_seconds_played)%>%ungroup()
    
    return(result_df)
  })
  
  output$districtTime <- renderPlotly({
    start_date <- input$dateInput[1]
    end_date <- input$dateInput[2]
    
    ggplot(filteredDataDistrictsTime(), aes(x = day, y = total_seconds_played, color = district)) +
      geom_line(size = 1) + 
      geom_point() +        
      labs(title = "Interactive Plot", 
           x = "Date", 
           y = "Second Played in each district") +
      theme_minimal() +     
      theme(plot.background = element_rect(fill = "grey18", color = "grey18"),  
            panel.background = element_rect(fill = "grey18"),
            text = element_text(color = "white")
        )
      })
    
    filteredDataStreamGraph <- reactive({
      if(input$personInputMartyna == "Hleb"){
        return(gleb$stream_data)
      }
      if(input$personInputMartyna == "Martyna"){
        return(martyna$stream_data)
      }
      if(input$personInputMartyna == "Zuza"){
        return(zuza$stream_data)
      }
    })
    output$stream_plot <- renderPlot({
      p <- ggplot(filteredDataStreamGraph(), aes(endTime, count, fill = genre)) +
        geom_stream(bw = 0.5, sorting = "inside_out") +
        scale_fill_manual(values = c("#90E2ED", "#31AFD4", "#369CBB", "#406792", "#634490", "#B46F9C", "#EE5973", "#FE4E4E", "#FE7B47", "#FFA72C")) +
        theme_minimal() +
        labs(
          x = "Day",
          y = "Number of plays",
          fill = "genre"
        ) +
        theme(
          plot.background = element_rect(fill = "gray18", color = NA),
          panel.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.background = element_rect(fill='transparent', color = NA),
          legend.box.background = element_rect(fill='transparent', color = NA),
          legend.key = element_rect(fill = 'transparent', color = NA),
          legend.key.width = unit(1, 'cm'),
          rect = element_rect(fill = "transparent"),
          legend.position = "bottom",
          text = element_text(color = "white")
        ) +
        guides(fill = guide_legend(nrow = 2))
      
      p
      
    })
    filteredDataPackcircles <- reactive({
      
      
      if(input$personInputMartyna == "Hleb"){
        return(gleb)
      }
      if(input$personInputMartyna == "Martyna"){
        return(martyna)
      }
      if(input$personInputMartyna == "Zuza"){
        return(zuza)
      }
    })
    output$packcircles_plot <- renderGirafe({
      feature_col <- switch(input$feature,
                            "Danceability" = "Danceability",
                            "Liveness" = "Liveness",
                            "Energy" = "Energy",
                            "Valence" = "Valence")
      
      data_martyna_circles<-filteredDataPackcircles()
      data_martyna_circles$artist_data$text <- paste(data_martyna_circles$artist_data$artistName, "\n", "Liczba odtworzeń: ", data_martyna_circles$artist_data$Count, "\n", paste(input$feature, ":", data_martyna_circles$artist_data[[feature_col]]))
      
      data_martyna_circles$dat.gg[[feature_col]] <- rep(data_martyna_circles$artist_data[[feature_col]], each = 71)
     
      desired_length <- 21
     
      legend_title <- pad_legend_title(input$feature, desired_length)
      
      pp <- ggplot() +
        geom_polygon_interactive(
          data = data_martyna_circles$dat.gg,
          aes(x, y, group = id, fill = data_martyna_circles$dat.gg[[feature_col]],
              tooltip = data_martyna_circles$artist_data$text[id], data_id = id),
          colour = "transparent"
        ) +
        scale_fill_gradient(low = "#90E2ED", high = "#634490", limits = c(0, 1)) +  # Set breaks from 0 to 1
        geom_text(
          data = data_martyna_circles$artist_data,
          aes(x, y, size = Count,
              label = ifelse(stri_width(data_martyna_circles$artist_data$artistName) <= data_martyna_circles$artist_data$radius * 6, data_martyna_circles$artist_data$artistName, "")),
          color = "white"
        ) +
        scale_size_continuous(range = c(0.5, 3)) +
        theme_void() +
        labs(fill = legend_title) +  # Use the padded legend title
        theme(
          plot.background = element_rect(fill = "gray18", color = NA),
          panel.background = element_rect(fill = "gray18", color = NA),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          rect = element_rect(fill = "transparent"),
          legend.position = "right",
          legend.background = element_rect(fill = 'transparent', color = NA),
          legend.box.background = element_rect(fill = 'transparent', color = NA),
          legend.key = element_rect(fill = 'transparent', color = NA),
          legend.text = element_text(color = "white"),
          legend.title = element_text(color = "white")  # Add margins here
        ) +
        guides(size = FALSE) +
        coord_equal()
      
      girafe(code = print(pp),
             options = list(opts_hover(css = "fill:#EE5973;stroke:#EE5973;stroke-width:2px;")))
    })
    output$liczba_gatunkow_box <- renderValueBox({
      valueBox(
        data_box1 <- filteredDataPackcircles(),
        data_box1$liczba_gatunkow,
        "przesłuchanych gatunków",
        icon = icon("list-alt")
      )
    })
    output$liczba_gatunkow_box <- renderValueBox({
      valueBox(
        filteredDataPackcircles()$liczba_gatunkow,
        "przesłuchanych gatunków",
        icon = icon("list-alt")
      )
    })
    output$top_box <- renderValueBox({
      valueBox(
        filteredDataPackcircles()$top_genre[1],
        "Top gatunek",
        icon = icon("star")
      )
    })
    output$top_time_played_box <- renderValueBox({
      valueBox(
        paste(round(filteredDataPackcircles()$top_time_played$top_time_played,2)),
        "przesłuchanych minut ulubionego gatunku",
        icon = icon("clock")
      )
    })
}

shinyApp(ui = ui, server = server)

library(shiny)
library(plotly)
library(dplyr)
library(stringr)
library(wordcloud2)
library(lubridate)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Emojis :)"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      radioButtons("who", "Select Person", c("z", "a", "f" )),
      radioButtons("platform", "Select Platform", c("mg", "ig", "both" ))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("cloud"),
      plotlyOutput("animated")
      )
    )
  )

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #nie wiem jak tu się robi żeby brało plik z repo a nie z komputera
  data <- read.csv("..\\..\\..\\app\\KomunikacJA\\appData\\emoji_merged.csv")
  data <- data %>% mutate(platform = ifelse(platform %in% c("mg", "fb"), "mg", "ig"))
  
  filtered_data <- reactive({
    if (input$platform == "both") {
      return(data %>% filter(name == input$who))
    } else {
      return(data %>% filter(name == input$who, platform == input$platform))
    }
  })
  
  
  observe({
    name_data <- filtered_data()
    # Extract emojis from the content
    emoji_list <- str_extract_all(name_data$emojis, "[\\x{1F600}-\\x{1F64F}\\x{1F300}-\\x{1F5FF}\\x{1F680}-\\x{1F6FF}\\x{1F700}-\\x{1F77F}\\x{1F780}-\\x{1F7FF}\\x{1F800}-\\x{1F8FF}\\x{1F900}-\\x{1F9FF}\\x{1FA00}-\\x{1FA6F}\\x{2600}-\\x{26FF}\\x{2700}-\\x{27BF}]")
    all_emojis <- unlist(emoji_list)
    
    
    # Create a data frame with emoji frequencies
    emoji_freq <- data.frame(table(all_emojis))
    emoji_freq <- emoji_freq %>%  filter (emoji_freq$Freq >= (1/50)*max(emoji_freq$Freq))
    
    emoji_freq <- emoji_freq %>% filter(!(all_emojis %in% c("🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
    
      cloud_plot <- wordcloud2(
        data = emoji_freq,
        color = "goldenrod",
        backgroundColor = "white",
        size = 1.5,
        minRotation = 0,
        maxRotation = 0,
        rotateRatio = 0,
        gridSize = 5,
        shape = "circle",
        shuffle = FALSE
      )
      output$cloud <- renderUI(cloud_plot)
  })
  
  
  
  observe({
    convert_to_vector <- function(emoji_string) {
      if (!is.na(emoji_string)) {
        return(unlist(strsplit(emoji_string, "")))
      } else {
        return(NA)
      }
    }
  # Filter messages containing emojis
  data_with_emojis <- filtered_data()
  
  data_with_emojis <- data_with_emojis %>% 
    mutate(emojis = sapply(emojis, convert_to_vector))
  
  pivoted_data <- data_with_emojis %>%
    select(Timestamp, emojis, name, platform) %>%
    unnest(emojis) %>%
    group_by(Timestamp, emojis, name, platform) %>% 
    summarise(count = n()) %>%
    arrange(emojis, Timestamp, name, platform) %>%
    group_by(emojis) %>%
    mutate(cumulative_count = cumsum(count)) 
  
  # Filter out rows containing unwanted emojis
  filtered_df <- pivoted_data %>% filter(!(emojis %in% c("🏻", "🏼", "🏽", "🏾", "🏿", "♀")))
  
  
  pivoted_data <- filtered_df
  
  pivoted_data <-  pivoted_data %>% filter(!is.na(emojis))
  
  # Select the top 10 emojis based on cumulative_count
  top_10 <- pivoted_data %>%
    group_by(emojis) %>%
    arrange(desc(cumulative_count)) %>%
    slice_head(n = 1) %>%
    arrange(desc(cumulative_count)) %>%
    head(10) %>% 
    pull(emojis)
  
  # Add month_year column
  pivoted_data <- pivoted_data %>%
    mutate(month_year = format(ymd_hms(Timestamp, tz = "UTC"), "%Y-%m"))
  
  # Filter data for selected emojis
  selected_data <- pivoted_data %>%
    filter(emojis %in% top_10)
  
  selected_data <- selected_data %>% select(emojis, month_year, count, name)
  
  # Create a combination of all emojis and months for each sender
  all_combinations <- expand_grid(emojis = unique(selected_data$emojis),
                                  month_year = unique(selected_data$month_year),
                                  name = unique(selected_data$name))
  
  # Merge with selected_data to fill missing combinations with count 0
  complete_data <- left_join(all_combinations, selected_data, by = c("emojis", "month_year", "name")) %>%
    replace_na(list(count = 0))
  
  # Calculate cumulative count for each month
  cumulative_data <- complete_data %>%
    group_by(emojis) %>%
    arrange(emojis, month_year) %>%
    mutate(cumulative_count = cumsum(count))
  
  # For each emoji, keep only the row with the highest cumulative_count in each month
  final_data <- cumulative_data %>%
    group_by(emojis, month_year) %>%
    slice_max(order_by = cumulative_count) %>%
    ungroup()
  
  animated_plot <- plot_ly(final_data, x = ~cumulative_count, y = ~emojis, 
                           type = "bar", frame = ~month_year, 
                           marker = list(color = "blue")) %>%
    layout(title = "Top 10 Most Used Emojis Over Time",
           xaxis = list(title = "Cumulative Count"),
           yaxis = list(title = "Emojis", tickfont = list(size = 10)),
           showlegend = FALSE) %>%
    animation_opts(150, redraw = TRUE) %>%
    animation_slider(currentvalue = 
                       list(prefix = "Month: ", font = list(color="red")))
  
  output$animated <- renderPlotly(animated_plot) })
}

# Run the application 
shinyApp(ui = ui, server = server)
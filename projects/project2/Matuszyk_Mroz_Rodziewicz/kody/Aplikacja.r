library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(jsonlite)
library(stringr)
library(tidyr)
library(shinydashboard)
library(shinycssloaders)

min_year <- 2014
data_loaded <- list(
  dane_kacpra1 = data.table::fread("dane_kacper/heatmap_kacper.csv"),
  dane_alek1 = data.table::fread("dane_alek/heatmap_alek.csv"),
  dane_michal1 =  data.table::fread("dane_michal/heatmap_michal.csv")
)

# UI
ui <- dashboardPage(
  dashboardHeader(title = "Facebook 'JA'"),
  dashboardSidebar(
    selectInput("dane", "Wybierz Zestaw Danych:",
                choices = c("Dane Kacpra", "Dane Alka", "Dane Michała")),
    
    sliderInput("year_range", "Select Year Range", min = min_year, max = 2023, value = c(min_year, 2023), step = 1),
    uiOutput("additionalUI")
  ),
  dashboardBody(
    # Wykresy będą wyświetlane tutaj, poniżej opcji wyboru
    # uiOutput("wykresy"),  # Dynamiczne renderowanie wykresu
    plotOutput("wykres1", width = "100%", height = "400px")%>% withSpinner(color="#5770ff"),  # Adjust width and height as needed
    plotOutput("wykres2", width = "100%", height = "400px") %>% withSpinner(color="#5770ff"),  # Adjust width and height as needed
    plotOutput("wykres3", width = "100%", height = "400px") %>% withSpinner(color="#5770ff"),  # Adjust width and height as needed
    sliderInput("Words", "How many words?", min = 0, max = 50, value = 20, step = 1),
    plotlyOutput("wykres4", width = "100%", height = "400px") %>% withSpinner(color="#5770ff"),  # Adjust width and height as needed
    uiOutput("numbersCheckbox"),  # Checkbox for displaying numbers under output4
    plotOutput("wykres5", width = "100%", height = "400px") %>% withSpinner(color="#5770ff"),  # Full width for the last plot
    plotlyOutput("wykresPlotly") %>% withSpinner(color="#5770ff")
  )
)

# Server
server <- function(input, output, session) {
  min_year <- reactive({
    switch(input$dane,
           "Dane Kacpra" =  2018,
           "Dane Alka" = 2016,
           "Dane Michała" = 2014)
  })
  
  
  
  # # Load data when the application starts
  # observe({
  #   data_loaded$dane_kacpra1 <- 
  #   data_loaded$dane_alek1 <- 
  #   data_loaded$dane_michal1 <-
  # })
  
  
  plot_unique_words_over_years <- function(unique_words, years) {
    # Create a data frame from the vectors
    data <- data.frame(Year = years, UniqueWordsCount = unique_words)
    
    # Generate the plot
    ggplot(data, aes(x = factor(Year), y = UniqueWordsCount)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "white") +
      theme_minimal() +
      labs(title = "Unique Words Count by Year", x = "Year", y = "Unique Words Count") +
      theme(axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
            axis.text.y = element_text(size = 10),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "none")
  }
    
  output$wykres1 <- renderPlot(
    {
      letter_counts <- switch(input$dane,
                             "Dane Kacpra" = data_loaded$dane_kacpra1,
                             "Dane Alka" = data_loaded$dane_alek1,
                             "Dane Michała" = data_loaded$dane_michal1)
      # 
      # dane <- data.table::fread(dane_sciezka)
      # 
      # letter_count_csv_file_path <- dane_sciezka
      # letter_counts <- data.table::fread(letter_count_csv_file_path, stringsAsFactors = FALSE)
      # letter_counts <- data_loaded$dane_kacpra
      
      start_year <- input$year_range[1]
      stop_year <- input$year_range[2]
      start_month <- 1
      stop_month <- 12
      
      
      letter_counts <- letter_counts %>% 
        filter((Year < stop_year & Year > start_year) | 
                 (stop_year == start_year & Month <= stop_month & Month >= start_month) | 
                 (start_year != stop_year & ((Year == start_year & Month > start_month) | (Year == stop_year & Month <= stop_month))))
      
      letter_counts <-  letter_counts %>% group_by(Letter) %>% summarise(count = sum(count))
      letter_counts
      # letter_counts <- letter_counts[order(-letter_counts$Count), ]
      
      letters = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", 
                  "Q", "W", "E", "R", "T", "Y", "U", "I", "O", "P",
                  "A", "S", "D", "F", "G", "H", "J", "K", "L", ";",
                  "Z", "X", "C", "V", "B", "N", "M", ",", ".")
      
      keyboard_layout <- data.frame(
        letter = letters,
        row =    c(4, 4, 4, 4, 4, 4, 4, 4, 4, 4, 
                   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
                   2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                   1, 1, 1, 1, 1, 1, 1, 1, 1),
        col =    c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                   1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                   1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                   1, 2, 3, 4, 5, 6, 7, 8, 9)
      )
      
      keyboard_heatmap_data <- merge(keyboard_layout, letter_counts, by.x = "letter", by.y = "Letter", all.x = TRUE)
      keyboard_heatmap_data
      keyboard_heatmap_data$Count[is.na(keyboard_heatmap_data$count)] <- 0
      ggplot(keyboard_heatmap_data, aes(x = col, y = row, fill = count, label = letter)) +
        geom_tile(color = "gray", size = 0.5) +
        geom_text(size = 5, color = "black", fontface = "bold") +
        scale_fill_gradient(low = "white", high = "skyblue") +
        labs(title = "Keyboard Heatmap of Letter Occurrences") +
        theme_minimal() +
        theme(axis.title = element_text(face = "bold", size = 12),
              plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
              axis.text = element_blank(),
              axis.ticks = element_blank(),
              legend.margin = margin(t = 0, r = 0, b = 0, l = 0))  # Remove margin around the legend
    }
  )
  
  
  output$wykres2 <- renderPlot({
    df_alek_2 <- data.frame(
      year = 2016:2023,
      unique_words = c(1521, 5780, 7235, 9121, 12322, 15870, 19321, 24291)
    )
    
    df_michal_2 <- data.frame(
      year = 2014:2023,
      unique_words = c(1013, 4674, 5949, 6714, 7987, 10468, 17939, 20813, 22006, 27128)
    )
    
    df_kacper_2 <- data.frame(
      year = 2018:2023,
      unique_words = c(8, 913, 6789, 9066, 12105, 19434)
    )
    
    dane <- switch(input$dane,
                   "Dane Kacpra" = df_kacper_2,
                   "Dane Alka" = df_alek_2,
                   "Dane Michała" = df_michal_2)
    
    plot_unique_words_over_years(dane$unique_words, dane$year)
  })
  
  output$wykres3 <-renderPlot({
    csv_file_path2 <- switch(input$dane,
                             "Dane Kacpra" = "dane_kacper/top_50_word_per_month_kacper.csv",
                             "Dane Alka" = "dane_alek/top_50_word_per_month_alek.csv",
                             "Dane Michała" = "dane_michal/top_50_word_per_month_michal.csv")
    
    word_counts <- data.table::fread(csv_file_path2, stringsAsFactors = FALSE) %>% as.data.frame()
    start_year <- input$year_range[1]
    stop_year <- input$year_range[2]
    start_month <- 1
    stop_month <- 12
    
    
    
    word_data_from_date_range <- word_counts %>%
      filter((Year < stop_year & Year > start_year) |
               (stop_year == start_year & Month <= stop_month & Month >= start_month) |
               (start_year != stop_year & ((Year == start_year & Month > start_month) | (Year == stop_year & Month <= stop_month))))
    
    
    
    word_data_from_date_range <- word_data_from_date_range %>% group_by(word) %>% summarise(count = sum(count))
    word_counts <- word_data_from_date_range[order(-word_data_from_date_range$count), ]
    word_counts
    
    
    top_50_words <- head(word_counts, input$Words)
    ggplot(top_50_words, aes(x = reorder(word, count), y = count)) +
      geom_bar(stat = "identity", fill = "skyblue", color = "skyblue", width = 0.8) +
      theme_minimal() +
      coord_flip() +
      labs(title = "Top 50 Words", x = "Word", y = "Count") +
      theme(axis.title = element_text(face = "bold", size = 12),
            plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 10),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.line = element_line(color = "black"),
            legend.position = "none") +
      geom_text(aes(label = count), hjust = -0.2, size = 3, color = "black", fontface = "bold")
  }
        
    
    
    
  )
  
  output$wykres4 <-renderPlotly(
    {
      
      csv_file_path <- switch(input$dane,
                              "Dane Kacpra" = "dane_kacper/hour_reactions_kacper.csv",
                              "Dane Alka" = "dane_alek/hour_reactions_alek.csv",
                              "Dane Michała" = "dane_michal/hour_reactions_michal.csv")
      twoj_imie_nazwisko = switch(input$dane,
                                  "Dane Kacpra" = "Kacper Rodziewicz",
                                  "Dane Alka" = "Alek Mróz",
                                  "Dane Michała" = "Michał Matuszyk")
      
      messages <- data.table::fread(csv_file_path)
      
      reactions_df <- messages %>% 
        filter(!is.na(reactions) & reactions != "") %>% 
        select(reactions) 
      
      df <- data.frame(reactions = reactions_df$reactions, stringsAsFactors = FALSE)
      
      df$reactions <- lapply(df$reactions, function(x) {
        # Usuwamy lub zamieniamy emoji
        x <- gsub("\\\\U[0-9a-fA-F]{5}", "", x)
        fromJSON(str_replace_all(x, "'", "\""))
      })
      
      reactions <- df %>%
        unnest(reactions)
      
      reaction_counts <- reactions %>%
        group_by(reaction) %>%
        summarize(count = n()) %>%
        arrange(desc(count))
      top_reactions <- reaction_counts %>%
        slice_head(n = 5) %>%
        pull(reaction)
      reactions <- reactions %>%
        filter(reaction %in% top_reactions)
      reactions <- reactions[reactions$actor == twoj_imie_nazwisko, ]
      
      
      # Tworzenie wykresu Plotly
      plot_ly(
        reactions,
        labels = ~reaction,
        type = 'pie',
        hole = 0.6,
        hoverinfo = 'label+percent+name',
        textinfo = 'label+percent',
        textposition = 'inside',
        marker = list(colors = 'Viridis')
      )
    }
    
    
  )
  
  output$wykres5 <-renderPlot(
    {
      # Wczytywanie danych z pliku output.txt
      file_path <- switch(input$dane,
                          "Dane Kacpra" = "dane_kacper/number_of_words_kacper.txt",
                          "Dane Alka" = "dane_alek/number_of_words_alek.txt",
                          "Dane Michała" = "dane_michal/number_of_words_michal.txt")
      data <- read.table(file_path, header = FALSE, col.names = c("Length", "Count"))
      
      # Filtrujemy dane tak, aby zawierały tylko długości od 1 do 35
      filtered_data <- data %>% filter(Length >= 1 & Length <= 35)
      # Creating a bar plot with filtered data
      ggplot(filtered_data, aes(x = Length, y = Count)) +
        geom_bar(stat = "identity", fill = "skyblue", color = "white") +  # Adding bars with sky blue fill and white border
        scale_x_continuous(breaks = seq(0, 35, by = 5)) +  # Setting X-axis breaks every 5 units
        theme_minimal() +  # Using a minimal theme
        theme(axis.title = element_text(face = "bold", size = 12),  # Bold axis titles with size 12
              plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # Centered bold plot title with size 16
              axis.text.y = element_text(size = 10),  # Size 10 for Y-axis text
              axis.text.x = element_text(size = 10),  # Size 10 for X-axis text
              panel.grid.major.x = element_line(size = 0.1, color = "grey", linetype = "solid"),  # Adding vertical grid lines
              panel.grid.major.y = element_line(size = 0.1, color = "grey", linetype = "solid"),  # Keeping horizontal grid lines
              panel.border = element_blank(),  # No panel border
              axis.line = element_line(color = "black"),  # Black axis lines
              legend.position = "none") +  # No legend
        labs(title = "Distribution of Message Lengths (1-35 words)", x = "Message Length (in words)", y = "Number of Messages")
    }
          
  )

  output$wykresPlotly <- renderPlotly({

      # Załadowanie danych
      csv_file_path <- switch(input$dane,
                              "Dane Kacpra" = "dane_kacper/hour_reactions_kacper.csv",
                              "Dane Alka" = "dane_alek/hour_reactions_alek.csv",
                              "Dane Michała" = "dane_michal/hour_reactions_michal.csv")

      twoj_imie_nazwisko = switch(input$dane,
                                  "Dane Kacpra" = "Kacper Rodziewicz",
                                  "Dane Alka" = "Alek Mróz",
                                  "Dane Michała" = "Michał Matuszyk")
      messages <- data.table::fread(csv_file_path)

      messages <- messages %>%
        mutate(
          hour = format(as.POSIXct(timestamp_ms / 1000, origin = "1970-01-01", tz = "CET"), format = "%H;%M"),
          year = as.numeric(format(as.POSIXct(timestamp_ms / 1000, origin = "1970-01-01", tz = "CET"), format = "%Y"))
        )

      my_messages <- messages %>%
        filter(sender_name == twoj_imie_nazwisko) %>%
        mutate(category = "My Messages")

      others_messages <- messages %>%
        filter(sender_name != twoj_imie_nazwisko) %>%
        mutate(category = "Others Messages")

      all_messages <- rbind(my_messages, others_messages)

      konwertuj_na_dziesietne <- function(godzina_str) {
        godzina_minuty <- strsplit(godzina_str, ";")[[1]]
        godzina <- as.numeric(godzina_minuty[1])
        minuty <- as.numeric(godzina_minuty[2])
        godzina_dziesietna <- godzina + minuty / 60
        return(godzina_dziesietna)
      }

      all_messages$hour_dziesietne <- sapply(all_messages$hour, konwertuj_na_dziesietne)


      # Tworzenie wykresu "Hours"
      filtered_messages <- reactive({
        filter(all_messages, year >= input$year_range[1] & year <= input$year_range[2])
      })

      gg <- ggplot(filtered_messages(), aes(x = hour_dziesietne, fill = category)) +
        geom_density(alpha = 0.5, size = 0.5) +
        labs(title = "Density Plot of Message Sent Times",
             x = "Hour",
             y = "Density",
             fill = "") +
        theme_minimal() +
        scale_fill_manual(values = c("#00cc66", "#6666ff")) +
        scale_x_continuous(breaks = seq(0, 23, 2))

      ggplotly(gg)
  
  
  
  }
)}

# Uruchomienie aplikacji
shinyApp(ui = ui, server = server)



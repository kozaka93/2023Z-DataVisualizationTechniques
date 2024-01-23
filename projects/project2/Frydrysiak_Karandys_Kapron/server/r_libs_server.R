rLibsServer <- function(input, output, session) {
  
  # LOADING DATA AND HELPERS
  loadRLibsData <- function(person) {
    commonPath <- "data/r-libs/"
    df <- read.delim(paste(commonPath, person, "_r_libs.txt", sep = ""), 
                     sep = ",", row.names = NULL)
    cbind(person = rep(person, times = nrow(df)), df) %>% 
      select(!row.names)
  }
  kubaDF <- loadRLibsData("kuba")
  mateuszDF <- loadRLibsData("mateusz")
  norbertDF <- loadRLibsData("norbert")
  df <- rbind(kubaDF, mateuszDF, norbertDF)
  
  completeImportsDF <- df %>% 
    select(person, Package, Imports) %>% 
    mutate(Imports = str_replace_all(Imports, ",\n", ", ")) %>% 
    separate_longer_delim(Imports, delim = ", ") %>% 
    mutate(Imports = str_replace(Imports, "\\([^\\)]+\\)", "")) %>% 
    na.omit()

  # REACTIVES
  importsDF <- reactive({
    df <- completeImportsDF %>% 
      filter(person == case_when(input$person == "Mateusz" ~ "mateusz",
                                 input$person == "Norbert" ~ "norbert",
                                 input$person == "Kuba" ~ "kuba")) %>% 
      select(!person)
    
    importFrequency <- df %>% 
      group_by(Imports) %>%
      summarise(frequency = n())
    
    df %>% 
      merge(importFrequency, by = 'Imports', all = T) %>% 
      select(Package, Imports, frequency)
  })
  
  mostFrequentlyImported <- reactive({
    importsDF() %>% 
      select(Imports, frequency) %>% 
      unique() %>% 
      slice_max(frequency, n = input$mostFrequentlyImported) %>% 
      pull(Imports)
  })
  
  personDF <- reactive({
    df %>% 
      filter(person == case_when(input$person == "Mateusz" ~ "mateusz",
                                 input$person == "Norbert" ~ "norbert",
                                 input$person == "Kuba" ~ "kuba")) %>% 
      select(!person)
  })
  
  basePackages <- reactive({
    personDF() %>% 
      group_by(Priority) %>% 
      summarise(count = n()) %>% 
      mutate(percentage = count / sum(count)) %>% 
      filter(Priority == 'base') %>% 
      pull(percentage)
  })
  
  # OUTPUTS
  output$rVersion <- renderInfoBox({
    infoBox(
      "R version", 
      personDF() %>% 
        filter(Package == 'base') %>% 
        pull(Version), 
      icon = icon("star")
    )
  })
  
  output$allPackages <- renderInfoBox({
    infoBox(
      "All packages",
      personDF() %>% 
        nrow(),
      icon = icon("hashtag")
    )
  })
  
  output$basePackages <- renderInfoBox({
    infoBox(
      "Base packages",
      paste(round(basePackages() * 100, digits = 2), "%"),
      icon = icon("pen")
    )
  })
  
  output$importsHistogram <- renderPlotly({
    p <- importsDF() %>% 
      select(Imports, frequency) %>% 
      unique() %>% 
      ggplot(
        aes(
          x = frequency
        )
      ) +
      geom_histogram(fill = "#3c8dbc") +
      labs(
        x = "Import frequency of a package",
        y = "No. of packages"
      ) +
      theme_minimal() +
      scale_x_continuous(expand = c(0, 0))
    
    ggplotly(p) %>% 
      config(displayModeBar = F)
  })
  
  output$importsNetwork <- renderSimpleNetwork({
    simpleNetwork(importsDF() %>% filter(Imports %in% mostFrequentlyImported()))
  })
  
}
bashHistoryServer <- function(input, output, session) {
  
  # INITIALIZATION AND HELPERS
  init <- function() {
    lines <- c(paste("Mateusz", str_squish(readLines("data/bash-history/mateusz_bash_history.txt"))),
               paste("NorbertMacos", str_squish(readLines("data/bash-history/norbert_macos.txt"))),
               paste("Kuba", str_squish(readLines("data/bash-history/kuba_bash_history.txt"))))
    lines_norbert_linux= c(paste("NorbertLinux", str_squish(readLines("data/bash-history/norbert_linux.txt"))))
    maxFields_norbert_linux <- max(sapply(lines_norbert_linux, function(x) length(str_split_1(x, pattern = " "))))
    df_norbert_linux <- str_split_fixed(lines_norbert_linux, pattern = " ", n = maxFields_norbert_linux)
    df_norbert_linux <- as.data.frame(df_norbert_linux[,c(-2,-3)])
    maxFields <- max(sapply(lines, function(x) length(str_split_1(x, pattern = " "))))
    df <- str_split_fixed(lines, pattern = " ", n = maxFields)
    df <- as.data.frame(df[,-2])
    df = plyr::rbind.fill(df, df_norbert_linux) #plyr install
    colnames(df) <- c("person", "command", paste("arg", 1:(ncol(df)-2), sep = ""))
    df
  }
  consecutiveCombinations <- function(v, k) {
    result <- sapply(seq(1, length(v) - k + 1), function(i) {
      paste(v[i:(i + k - 1)], collapse = " ")
    })  
    result
  }
  
  df <- init()
  
  # REACTIVES
  personDf <- reactive({
    df %>%
      filter(person == case_when(input$person == "Mateusz" ~ "Mateusz",
                                 input$person == "Norbert(MacOs)" ~ "NorbertMacos",
                                 input$person == "Kuba" ~ "Kuba",
                                 input$person == "Norbert(Linux)" ~ "NorbertLinux"))
  })
  
  personDfLength <- reactive({length(personDf()$command)})
  
  personCommandsUsage <- reactive({
    personDf() %>% 
      mutate(command = ifelse(command == "sudo", arg1, command)) %>% 
      group_by(command) %>%
      summarize(count = n(), frac = round(count / personDfLength() * 100, 2)) %>% 
      mutate(command = fct_reorder(command, frac))
  })
  
  personCommandsSequence <- reactive({
    result <- rle(personDf()$command)
    df <- data.frame(table(unlist(sapply(2:6, function(i) consecutiveCombinations(result$values, k = i)))))
    colnames(df) <- c("sequence", "count")
    df$sequence <- as.character(df$sequence)
    df <- mutate(df, length = sapply(str_split(sequence, pattern = " "), function(v) length(v)))
    df
  })
  
  sudoFraction <- reactive({round(mean(personDf()$command == "sudo") * 100, digits = 2)})
  
  # OUTPUTS
  output$sudoFraction <- renderInfoBox({
    infoBox("Sudo Usage", paste(sudoFraction(), "%"), icon = icon("user-tie"))
  })
  
  output$uniqueCommands <- renderInfoBox({
    infoBox("Unique Commands", length(unique(personDf()$command)), icon = icon("star"))
  })
  
  output$totalCommands <- renderInfoBox({
    infoBox("Total Commands", personDfLength(), icon = icon("hashtag"))
  })
  
  output$commandsUsagePlot <- renderPlotly({
    p <- personCommandsUsage() %>% 
      slice_max(order_by = frac, n = input$hottestCommands, with_ties = FALSE) %>% 
      ggplot(aes(x = command, y = frac)) +
        geom_segment(aes(x = command, xend = command, y = 0, yend = frac), color = "orange") +
        geom_point(color = "orange", size = 4) +
        labs(
          x = "Command",
          y = "Usage fraction"
        ) +
        theme_minimal() +
      scale_y_continuous(
        limits = c(0, 50)
      ) +
        coord_flip() +
        theme(
          panel.grid.major.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks.y = element_blank()
        )
      ggplotly(p) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$commandsUsageTable <- DT::renderDataTable({
   personCommandsUsage() %>% 
      select(command, count, frac) %>% 
      slice_max(order_by = frac, n = input$hottestCommands, with_ties = FALSE) %>% 
      datatable(options = list(dom = "ti",
                               pageLength = 12))
  })
  
  output$commandsSequence <- DT::renderDataTable({
    personCommandsSequence() %>% 
      group_by(length) %>% 
      slice_max(order_by = count, n = 1, with_ties = FALSE) %>% 
      ungroup() %>% 
      datatable(options = list(dom = "ti"))
  })
}
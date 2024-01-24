# TODO dodac opisy osi na wykresach wszystkich np. Nazwa repozytoriów, ilość commitów
gitStatsServer <- function(input, output, session) {
  
  processData <- function(lines, person) {
    idx <- grep("^Repository", lines)
    df_list <- list()
    for (x in 2:length(idx)) {
      df_list[[x - 1]] <- processRepo(lines, idx[x - 1], idx[x], person)
    }
    do.call(rbind, df_list)
  }
  processRepo <- function(lines, start, end, person) {
    repo <- tail(str_split_1(lines[start], pattern = "/"), n = 2)[1]
    df <- str_split_fixed(lines[(start+1):(end-2)], pattern = ", ", n = 4)
    df <- as.data.frame(df)
    colnames(df) <-  c("hash", "author", "date", "message")
    mutate(df, person = person, repo = repo, .before = 1)
  }
  
  init <- function() {
    do.call(rbind, list(
      processData(readLines("data/git-stats/mateusz_git_stats.txt"), "Mateusz"),
      processData(readLines("data/git-stats/kuba_git_stats.txt"), "Kuba"),
      processData(readLines("data/git-stats/norbert_git_stats.txt"), "Norbert")
    ))
  }
  
  df <- init()
  
  personDf <- reactive({
    df %>%
      filter(person == case_when(input$person_w == "vecel" ~ "Mateusz",
                                 input$person_w == "Norbert Frydrysiak" ~ "Norbert",
                                 input$person_w == "kuba-kapron" ~ "Kuba"))
  })
  personDf_only_his_commits = reactive({
    if(input$person_w == "vecel"){
      personDf() %>%filter(author =="vecel" | author =="Mateusz Karandys")
    } else if(input$person_w == "Norbert Frydrysiak"){
      personDf() %>%
        filter(author == "Norbert Frydrysiak" | author=="fantasy2fry")
    } else if(input$person_w == "kuba-kapron"){
      personDf() %>% 
        filter(author == "kuba-kapron")
    }
    
  })
  # every message in new row
  personDf_only_his_commits_messages=reactive({
    personDf_only_his_commits() %>% 
      select(person, message) %>% 
      mutate(message = str_split(message, pattern = " ")) %>%
      unnest(message)
  })
  
  #converting date to date format
  personDf_with_date = reactive({
    personDf_only_his_commits() %>%
      mutate(date = as.Date(date)) %>% 
      mutate(day = weekdays(date))
  })
  personDf_with_date_groupped=reactive({
    personDf_with_date() %>%
      group_by(date) %>%
      summarise(count = n())
  })
  
  output$how_many_repos=renderInfoBox({
    infoBox("Total Repositories", 
            paste0(n_distinct(personDf()$repo)), 
            icon = icon("hashtag"))
  })
  output$total_commits_person=renderInfoBox({
    infoBox("Total Commits By Person", 
            paste0(nrow(personDf_only_his_commits())), 
            icon = icon("user"))
  })
  output$average_commits_per_repo_by_person=renderInfoBox({
    infoBox("Average Commits By Person Per Repo", 
            paste0(round(nrow(personDf_only_his_commits())/n_distinct(personDf()$repo),2)),
            icon = icon("flag"))
  })
  
  output$unique_contributors=renderInfoBox({
    infoBox("Unique Contributors", 
            paste0(n_distinct(personDf()$author)),
            icon = icon("star"))
  })
  
  output$average_total_commits_per_repo=renderInfoBox({
    infoBox("Average Commits Per Repo", 
            paste0(round(x=nrow(personDf())/n_distinct(personDf()$repo),digits=2)),
            icon = icon("paperclip"))
  })
  output$most_popular_day_for_commit=renderInfoBox({
    infoBox("Most Popular Day For Commit", 
            paste0(personDf_with_date() %>% 
                     group_by(day) %>% 
                     summarise(count = n()) %>% 
                     arrange(desc(count)) %>% 
                     slice(1) %>% 
                     pull(day)),
            icon = icon("calendar-days"))
  })
  output$most_popular_contrybutor=renderInfoBox({
    infoBox("Most Popular Contributor", 
            paste0(personDf() %>% 
                     group_by(author) %>% 
                     summarise(count = n()) %>% 
                     arrange(desc(count)) %>% 
                     slice(1) %>% 
                     pull(author)),
            icon = icon("user-check"))
  })
  
  # average number of words per commit
  output$average_number_of_words_per_commit=renderInfoBox({
    infoBox("Average Number Of Words Per Commit", 
            paste0(round(nrow(personDf_only_his_commits_messages())/nrow(personDf_only_his_commits()),2)),
            icon = icon("spell-check"))
  })
  #NAME OF THE REPO WITH THE MOST COMMITS
  output$repo_with_the_most_commits=renderInfoBox({
    infoBox("Repo With The Most Commits", 
            paste0(personDf() %>% 
                     group_by(repo) %>% 
                     summarise(count = n()) %>% 
                     arrange(desc(count)) %>% 
                     slice(1) %>% 
                     pull(repo)),
            icon = icon("house-circle-check"))
  })
  
  output$calendar_heatmap=renderPlot({
    pdff=personDf_with_date_groupped()
    calendarHeat(pdff$date, pdff$count, varname = "Commits", color="w2b")
    
  })
  
  
  output$heatmap <- renderPlotly({
    
    dfhm <- df %>% 
      filter(author == "vecel") %>% 
      mutate(date = as.Date(date)) %>% 
      group_by(date) %>% 
      summarise(count = n())
    
    hm <- data.frame(
      date = seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
    ) %>% 
      left_join(dfhm, by = "date") %>% 
      mutate(count = if_else(is.na(count), 0, count))
      
    
    heatmaply(matrix(hm$count, nrow = 7),
              show_dendrogram = c(FALSE, FALSE))
  })
  output$message_lollipop <- renderPlotly({
    personDf_only_his_commits_messages() %>%
      group_by(message) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(input$number_of_most_used_words) %>%
      plot_ly(y = ~reorder(message, -count), x = ~count, type = 'scatter', mode = 'markers', marker = list(size = 10))%>%
      layout(yaxis = list(title = 'Word in commits'), xaxis = list(title = 'Number of occurrences')) %>% 
      config(displayModeBar = FALSE)
  })
  
  #plotly plot that shows how many commits in each repo
  output$repo_barplot <- renderPlotly({
    personDf() %>%
      group_by(repo) %>%
      summarise(count = n()) %>%
      arrange(desc(count)) %>%
      head(input$number_of_repos) %>% 
      plot_ly(x = ~reorder(repo, -count), y = ~count, type = 'bar', marker = list(color = "#3c8dbc")) %>%
      layout(yaxis = list(title = 'Number of commits', type="log"), xaxis = list(title = 'Repository')) %>% 
      config(displayModeBar = FALSE)
  })
  
}
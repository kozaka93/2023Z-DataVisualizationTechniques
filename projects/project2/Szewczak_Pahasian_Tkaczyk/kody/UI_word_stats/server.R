library(shiny)
library(plotly)
library(ggplot2)
library(tidyverse)
library(dplyr)
library(stringi)
library(stringr)
library(ggwordcloud)
library(bslib)
library(ggfun)
thematic::thematic_shiny(font = "auto")

source("data_loader.R")

data_Milanna <- convert("YThistory/milanna/WHistoryM.json")
data_Milanna_frobind <- data_Milanna %>% mutate(name = "Milanna")
data_Krzysztof <- convert("YThistory/krzysztof/WHistoryK.json")
data_Krzysztof_frobind <-
  data_Krzysztof %>% mutate(name = "Krzysztof")
data_Michal <- convert("YThistory/michal/WHistoryMi.json")
data_Michal_frobind <- data_Michal %>% mutate(name = "Michal")
data_full <- bind_rows(data_Milanna_frobind,
                       data_Krzysztof_frobind,
                       data_Michal_frobind)

get_plot <- function(data, person_factor) {
  ggplot(
    data,
    aes(
      label = word,
      size = number,
      angle = angle,
      color = factor(sample.int(20, nrow(data), replace = TRUE)),
      label_content = sprintf("%s<sub style ='font-size:7.5pt'>(%g)</sub>", word, number)
    )
  ) +
    geom_text_wordcloud_area() +
    scale_size_area(max_size = person_factor) +
    theme_minimal() +
    theme(panel.background = element_rect(fill = 'white', color = 'white')) +
    coord_fixed(ratio = 9 / 16)
}


function(input, output, session) {
  
  person_logo = c("Krzysztof" = "Logo1.png",
                    "Milanna" = "lo2.jpg",
                    "Michal" = "lo3.png")
  
  
  onevent(event="mouseenter", id="wordplot",shinyjs::show("slider1"))
  onevent(event="mouseleave", id="wordplot",shinyjs::hide("slider1"))
  
  onevent(event="mouseenter", id="plot1",{
    if(heights$toogle) return()
    shinyjs::show("input")})
  
  onevent(event="mouseenter", id="plot2",{
    if(!heights$toogle) return()
    shinyjs::show("input2")})
  
  onevent(event="mouseleave", id="plot1",shinyjs::hide("input"))
  onevent(event="mouseleave", id="plot2",shinyjs::hide("input2"))
  
  onevent(event="mouseenter", id="plot3",{
    if(heights$toogle2) return()
    shinyjs::show("inputLine")})
  
  onevent(event="mouseleave", id="plot3",shinyjs::hide("inputLine"))
  
  heights <- reactiveValues(plot1 = "500px", plot2 = "250px", plot3 = "500px", plot4= "250px",
                            toogle = FALSE, toogle2 = FALSE) 
  onclick(id="plot2", {
    heights$toogle <- TRUE
    heights$plot1 <- "250px"
    heights$plot2 <- "500px"
    Sys.sleep(1.5)
    runjs('$("#plot2").insertAfter("#pointer1");
          $("#plot1").insertAfter("#pointer2");
          ')
    plotlyProxy("piechart",session) %>%  plotlyProxyInvoke("relayout", 
                                                           list(autosize = TRUE))
    plotlyProxy("pointPlot",session) %>%  plotlyProxyInvoke("relayout",
                                                            list(autosize = TRUE))
  })
  
  onclick(id="plot1", {
    heights$toogle <- FALSE
    heights$plot1 <- "500px"
    heights$plot2 <- "250px"
    Sys.sleep(1.5)
    runjs('$("#plot1").insertAfter("#pointer1");
          $("#plot2").insertAfter("#pointer2");
          ')
    plotlyProxy("piechart",session) %>%  plotlyProxyInvoke("relayout", 
                                                           list(autosize = TRUE))
    plotlyProxy("pointPlot",session) %>%  plotlyProxyInvoke("relayout",
                                                            list(autosize = TRUE))
  })
  
  onclick(id="plot4", {
    heights$toogle2 <- TRUE
    heights$plot3 <- "250px"
    heights$plot4 <- "500px"
    Sys.sleep(1.5)
    runjs('$("#plot4").insertAfter("#pointer3");
          $("#plot3").insertAfter("#pointer4");
          ')
    plotlyProxy("timeplot",session) %>%  plotlyProxyInvoke("relayout", 
                                                           list(autosize = TRUE))
    plotlyProxy("pointPlot2",session) %>%  plotlyProxyInvoke("relayout",
                                                            list(autosize = TRUE))
  })
  
  onclick(id="plot3", {
    heights$toogle2 <- FALSE
    heights$plot3 <- "500px"
    heights$plot4 <- "250px"
    Sys.sleep(1.5)
    runjs('$("#plot3").insertAfter("#pointer3");
          $("#plot4").insertAfter("#pointer4");
          ')
    plotlyProxy("timeplot",session) %>%  plotlyProxyInvoke("relayout", 
                                                           list(autosize = TRUE))
    plotlyProxy("pointPlot2",session) %>%  plotlyProxyInvoke("relayout",
                                                            list(autosize = TRUE))
  })
  
  output$piechartPlaceholder <- renderUI(
    plotlyOutput("piechart", height = heights$plot2)
  )
  
  output$pointPlotPlaceHolder <- renderUI(
    plotlyOutput("pointPlot", height = heights$plot1)
  )
  
  output$timeplotPlaceholder <- renderUI(
    plotlyOutput("timeplot", height = heights$plot4)
  )
  
  output$pointPlot2PlaceHolder <- renderUI(
    plotlyOutput("pointPlot2", height = heights$plot3)
  )
  
  output$pointPlotTitlePlaceholder <- renderUI({
    if(heights$toogle){
      tags$h6("Most watched channels stattistics by date")
    } else{
    tags$h2("Most watched channels statistics by date")
    }
  })
  
  output$piechartTitlePlaceholder <- renderUI({
    if(!heights$toogle){
      tags$h6("Share of films from top 5 channels in films watched")
    } else{
      tags$h2("Share of films from top 5 channels in films watched")
    }
  })
  
  output$pointPlot2TitlePlaceholder <- renderUI({
    tit = switch(
      input$type,
      "video" = 'Videos watched over time by months',
      "add" = 'Adds watched over time by months',
      "add percent" = 'Percent of adds watched over time by months'
    )
    if(heights$toogle){
      tags$h6(tit)
    } else{
      tags$h2(tit)
    }
  })
  
  output$timeplotTitlePlaceholder <- renderUI({
    if(!heights$toogle2){
      tags$h6(paste0("Films watched by timeslot stats for ", input$radioButton))
    } else{
      tags$h2(paste0("Films watched by timeslot stats for ", input$radioButton))
    }
  })

  output$wordPlot <- renderPlot({
    person_factor = c("Krzysztof" = case_when(input$min_word_length < 7 ~ 40,
                                              TRUE ~ 20 ),
                      "Milanna" = 35,
                      "Michal" = 18)
    
    df <- get_clean_data_Name(data_full) %>%
      filter(
        name == input$radioButton
        & date >= input$time_range_input2[1]
        & date <= input$time_range_input2[2]
      )
    df <- get_word_stats(df)
    df <- df %>%
      filter(nchar(word) >= input$min_word_length) %>%
      head(input$number_of_words) %>%
      mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40)))
    p <- get_plot(df, person_factor[input$radioButton])
    p<- p+theme(panel.background = element_roundrect(fill="white",color="white",r=0.1))
    p
  })
  
  output$person <-
    renderUI({
      div(
        img(
          src = person_logo[input$radioButton],
          width = 20,
          height = 20
        ),
        input$radioButton,
        icon("bell"),
        HTML(
          '<span style="width:65%;display:inline-block"></span>'
        ),
        icon("thumbs-up"),
        "1M",
        icon("thumbs-down"),
        icon("share"),
        icon("save")
      )
    })
  
  output$person2 <-
    renderUI({
      div(
        img(
          src = person_logo[input$radioButton],
          width = 20,
          height = 20
        ),
        input$radioButton,
        icon("bell"),
        HTML(
          '<span style="width:65%;display:inline-block"></span>'
        ),
        icon("thumbs-up"),
        "1M",
        icon("thumbs-down"),
        icon("share"),
        icon("save")
      )
    })
  
  output$person3 <-
    renderUI({
      div(
        img(
          src = person_logo[input$radioButton],
          width = 20,
          height = 20
        ),
        input$radioButton,
        icon("bell"),
        HTML(
          '<span style="width:65%;display:inline-block"></span>'
        ),
        icon("thumbs-up"),
        "1M",
        icon("thumbs-down"),
        icon("share"),
        icon("save")
      )
    })
  
  output$person4 <-
    renderUI({
      div(
        img(
          src = person_logo[input$radioButton],
          width = 20,
          height = 20
        ),
        input$radioButton,
        icon("bell"),
        HTML(
          '<span style="width:65%;display:inline-block"></span>'
        ),
        icon("thumbs-up"),
        "1M",
        icon("thumbs-down"),
        icon("share"),
        icon("save")
      )
    })
  
  output$person5 <-
    renderUI({
      div(
        img(
          src = person_logo[input$radioButton],
          width = 20,
          height = 20
        ),
        input$radioButton,
        icon("bell"),
        HTML(
          '<span style="width:65%;display:inline-block"></span>'
        ),
        icon("thumbs-up"),
        "1M",
        icon("thumbs-down"),
        icon("share"),
        icon("save")
      )
    })
  
  output$table <- renderDataTable({
    tablee <-  get_clean_data_Name(data_full) %>%
      filter(name %in% input$name) %>%
      select(channel) %>%
      group_by(channel) %>%
      summarise(videos_watched = n()) %>% arrange(desc(videos_watched))
    tablee
    
  })
  output$pointPlot <- renderPlotly({
    top5 <-  get_clean_data_Name(data_full) %>%
      filter(
        name == input$radioButton &
          date >= input$time_range_input[1] &
          date <= input$time_range_input[2]
      ) %>%
      select(channel) %>%
      group_by(channel) %>%
      summarise(videos_watched = n()) %>% arrange(desc(videos_watched)) %>%
      head(5)
    tablee <-  get_clean_data_Name(data_full)
    tablee <- tablee %>%
      filter(
        name == input$radioButton,
        channel %in% top5$channel,
        date >= input$time_range_input[1],
        date <= input$time_range_input[2]
      ) %>%
      select(channel, date) %>%
      mutate(year = year(date), month = month(date)) %>%
      group_by(year, month, channel) %>%
      summarise(videos_watched = n()) %>%
      mutate(year_month = paste0(year, "-", sprintf("%02d", month))) %>%
      select(-year, -month)
    
    p <- plot_ly(tablee, x = ~ year_month) %>%
      add_trace(y = ~ videos_watched,
                color = ~ channel) %>%
      layout(
        title = list(text = "", font = list(color = "white")), #'Most watched channels stat by date'
        xaxis = list(title = 'Date', color = "white"),
        yaxis = list(title = 'Videos', color = "white"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(font = list(color = "white"))
      )
    p
    
  })
  output$pointPlot2 <- renderPlotly({
    df_adds_date_count <-  get_adds_Name(data_full) %>%
      filter(name == input$radioButton) %>%
      mutate(year = year(date), month = month(date)) %>%
      group_by(year, month) %>%
      summarise(adds_by_date = n()) %>%
      mutate(date = paste0(year, "-", sprintf("%02d", month))) %>%
      select(-year, -month)
    
    df_fixed_date_count <- data_full %>%
      filter(name == input$radioButton) %>%
      separate_wider_delim(time, "T", names = c("date", "time")) %>%
      mutate(date = as.Date(date)) %>%
      mutate(year = year(date), month = month(date)) %>%
      group_by(year, month) %>%
      summarise(watched_by_date = n()) %>%
      mutate(date = paste0(year, "-", sprintf("%02d", month))) %>%
      select(-year, -month)
    
    
    df_adds_percent <-
      left_join(df_fixed_date_count, df_adds_date_count, by = "date") %>%
      mutate(
        watched_by_date = if_else(is.na(watched_by_date), 0, watched_by_date),
        adds_by_date = if_else(is.na(adds_by_date), 0, adds_by_date),
        adds_percent = 100 * adds_by_date / watched_by_date
      )
    defin = switch(
      input$type,
      "video" = df_adds_percent$watched_by_date,
      "add" = df_adds_percent$adds_by_date,
      "add percent" = df_adds_percent$adds_percent,
    )
    tit = switch(
      input$type,
      "video" = 'Videos watched over time by months',
      "add" = 'Adds watched over time by months',
      "add percent" = 'Percent of adds watched over time by months'
    )
    lab = switch(
      input$type,
      "video" = 'Videos Watched',
      "add" = 'Adds Watched',
      "add percent" = 'Adds Percent'
    )
    plot_ly(
      df_adds_percent,
      x = ~ date,
      y = defin,
      type = 'scatter',
      mode = 'lines',
      color = I("red")
    ) %>%
      layout(
        #title =  list(text = tit, font = list(color = "white")),
        xaxis = list(
          title = 'Date',
          showgrid = FALSE,
          color = "white"
        ),
        yaxis = list(title = lab, color = "white"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(font = list(color = "white"))
      )
  })
  
  
  output$timeplot <- renderPlotly({
    data_time <- data_full %>%
      filter(name == input$radioButton) %>%
      select(time) %>%
      mutate(
        time_slot = case_when(
          substring(time, 12, 13) == "00" ~ "00.00 - 1.59",
          substring(time, 12, 13) == "01" ~ "00.00 - 1.59",
          substring(time, 12, 13) == "02" ~ "02.00 - 3.59",
          substring(time, 12, 13) == "03" ~ "02.00 - 3.59",
          substring(time, 12, 13) == "04" ~ "04.00 - 5.59",
          substring(time, 12, 13) == "05" ~ "04.00 - 5.59",
          substring(time, 12, 13) == "06" ~ "06.00 - 7.59",
          substring(time, 12, 13) == "07" ~ "06.00 - 7.59",
          substring(time, 12, 13) == "08" ~ "08.00 - 9.59",
          substring(time, 12, 13) == "09" ~ "08.00 - 9.59",
          substring(time, 12, 13) == "10" ~ "10.00 - 11.59",
          substring(time, 12, 13) == "11" ~ "10.00 - 11.59",
          substring(time, 12, 13) == "12" ~ "12.00 - 13.59",
          substring(time, 12, 13) == "13" ~ "12.00 - 13.59",
          substring(time, 12, 13) == "14" ~ "14.00 - 15.59",
          substring(time, 12, 13) == "15" ~ "14.00 - 15.59",
          substring(time, 12, 13) == "16" ~ "16.00 - 17.59",
          substring(time, 12, 13) == "17" ~ "16.00 - 17.59",
          substring(time, 12, 13) == "18" ~ "18.00 - 19.59",
          substring(time, 12, 13) == "19" ~ "18.00 - 19.59",
          substring(time, 12, 13) == "20" ~ "20.00 - 21.59",
          substring(time, 12, 13) == "21" ~ "20.00 - 21.59",
          substring(time, 12, 13) == "22" ~ "22.00 - 23.59",
          substring(time, 12, 13) == "23" ~ "22.00 - 23.59"
        )
      ) %>%
      group_by(time_slot) %>%
      summarise(films_in_timeslot = n())
    plot_ly(
      data_time,
      x =  ~ time_slot,
      y =  ~ films_in_timeslot,
      type = "bar",
      marker = list(color = "red")
    ) %>%
      layout(
        title = list(
          font = list(color = "white")
        ),
        xaxis = list(title = "Timeslot", color = "white"),
        yaxis = list(title = "Number of films watched", color = "white"),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(font = list(color = "white"))
      )
    
  })
  
  output$piechart <- renderPlotly({
    tablee <- get_clean_data_Name(data_full)
    tablee <- tablee %>%
      filter(
        name == input$radioButton &
          date <= input$time_range_input3[2] &
          date >= input$time_range_input3[1]
      ) %>%
      select(channel, date) %>%
      #mutate(year = year(date)) %>%
      #filter(as.character(year)==input$range_select) %>%
      group_by(channel) %>%
      summarise(num = n())
    all_year <- tablee %>%
      summarise(suma = sum(num))
    all_year <- as.numeric(all_year$suma)
    
    top5_year <- tablee %>%
      arrange(-num) %>%
      head(5)
    top5_year <- top5_year %>%
      summarise(suma = sum(num))
    top5_year <- as.numeric(top5_year$suma)
    vals <- c(top5_year, all_year)
    laabels <- c("top 5", "other")
    pie_year <- data.frame(vals, laabels)
    p <- plot_ly(
      pie_year,
      labels =  ~ laabels,
      values =  ~ vals,
      type = "pie",
      marker = list(colors = c("white", "red"))
    )
    p <-
      p %>%  layout(
        title = list(
          text = "" ,#paste(
            #"Share of films from top 5 channels in films watched in ",
            #input$range_select
          #)
          #,
          font = list(color = "white")
        ),
        xaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE,
          color = "white"
        ),
        yaxis = list(
          showgrid = FALSE,
          zeroline = FALSE,
          showticklabels = FALSE,
          color = "white"
        ),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        legend = list(font = list(color = "white"))
      )
    p
  })
  
  output$tit =renderText({ switch(
    input$type,
    "video" = 'Videos watched over time by months',
    "add" = 'Adds watched over time by months',
    "add percent" = 'Percent of adds watched over time by months'
  )})
  
  output$tit2 = renderText({
    paste0("Films watched by timeslot stats for ", input$radioButton)
  })
}

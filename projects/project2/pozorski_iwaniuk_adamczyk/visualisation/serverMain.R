serverMain <- function(input, output) {

  
  #####plots####
  
  output$barplot_emoji <- renderPlotly({
    
    #####data_processing####
    emojis_number = input$barplot_n_emoji
    
    filtered_df <- combined_emoji %>% 
      filter(name == input$user,
             is_group %in% input$barplot_group_emoji,
             date >= input$barplot_date_emoji[1],
             date <= input$barplot_date_emoji[2])
    
    
    df <- filtered_df %>% group_by(emoji)%>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>% slice(1:emojis_number) %>%
      mutate(emojis_combined = fct_reorder(emoji,-n))
    
    
    #####plot####
    plot_ly(df,x=~emojis_combined,y=~n,type="bar")%>%
      layout(xaxis = list(title = "",
                          tickfont = list(size = 20),
                          fixedrange = TRUE),
             yaxis = list(title = "Liczba użyć",
                          fixedrange = TRUE)) %>%
      config(displayModeBar = FALSE)
    
    
  })
  
  
  
  output$barplot_count_messages <- renderPlotly({
    
  
    #####data_processing####
    conversations_number <- input$barplot_n_count_messages
    
    filtered_df <- combined_count_messages %>%
      filter(is_group == F,
             df_name == input$user,
             date >= input$barplot_date_count_messages[1],
             date <= input$barplot_date_count_messages[2])
   
    
    df <- filtered_df %>%
      group_by(conversation_id,name) %>%
      summarise(n = sum(`min_messages_is_0`)) %>% 
      summarise(name = paste(name,collapse = ""),n = sum(n)) %>%
      ungroup() %>%
      mutate(name = str_replace_all(name,
                                    as.character(input$user),
                                    "")) %>%
      arrange(desc(n)) %>% 
      slice(1:conversations_number)  
    
    df <- left_join(df,name_gender,by = join_by(name))
    
    
    df <- df %>%
      mutate(
        name = case_when(
          name == "Paweł Pozorski" ~ "Andrzej Tracz",
          name == "Michał Iwaniuk" ~ "Marcin Sosna",
          name == "Krzysiek Adamczyk" ~ "Kacper Świeca",
          TRUE ~ name
        )
      )  
    
    
     df <- df %>% 
      mutate(name = fct_reorder(name,-n))
   
    
    #####plot####
    
    if(length(unique(df$gender))==1){
      if("male" %in% df$gender){
        plot_ly(df,
                x=~name,
                y=~n,
                type = "bar",
                marker = list(color = c("lightblue"))) %>%
          layout(xaxis = list(fixedrange = TRUE, title = ""),
                 yaxis = list(fixedrange = TRUE, title = "Liczba wiadomości")) %>%
          config(displayModeBar = FALSE)
      }
      else{
        plot_ly(df,
                x=~name,
                y=~n,
                type = "bar",
                marker = list(color = c("pink"))) %>%
          layout(xaxis = list(fixedrange = TRUE, title = ""),
                 yaxis = list(fixedrange = TRUE, title = "Liczba wiadomości")) %>%
          config(displayModeBar = FALSE)
      }
    }else{
      
      
      plot_ly(df,
              x=~name,
              y=~n,
              type = "bar",
              color = ~gender,
              colors = c("pink","lightblue")) %>%
        layout(xaxis = list(fixedrange = TRUE, title = ""),
               yaxis = list(fixedrange = TRUE, title = "Liczba wiadomości")) %>%
        config(displayModeBar = FALSE)
    }
    
   
    
    
    
    
  })
  
  output$animated_barplot_count_messages <- renderPlotly({
    
    #####data_processing####
    
    filtered_df <- top_users %>% 
      filter(df_name == input$user)
    range = 1.1*max(filtered_df$n)
    
    top10_names <- filtered_df %>% 
      group_by(name) %>% 
      summarise(n = max(n)) %>%
      arrange(desc(n)) %>%
      slice(1:10) %>%
      pull(name)
    
    filtered_df <- filtered_df %>%
      filter(name %in% top10_names)
    
    
    #reczne usuwanie syfu
    df <- filtered_df %>% filter(n != 3, n!=7, n!=1)
    #
    
    df$date <- factor(df$date)
    df$color <- ifelse(df$gender == "male", "lightblue", "pink")
    
    
    df <- df %>%
      mutate(
        name = case_when(
          name == "Paweł Pozorski" ~ "Andrzej Tracz",
          name == "Michał Iwaniuk" ~ "Marcin Sosna",
          name == "Krzysiek Adamczyk" ~ "Kacper Świeca",
          TRUE ~ name
        )
      )
    
   top10_names <- top10_names %>%
      recode(
        "Paweł Pozorski" = "Andrzej Tracz",
        "Michał Iwaniuk" = "Marcin Sosna",
        "Krzysiek Adamczyk" = "Kacper Świeca"
      )
   
   
   top10_names <- lapply(str_split(top10_names, " "),
                         function(x) {paste(x[1], x[2], sep = "\n") })
   
   df$name <- lapply(str_split(df$name, " "),
                              function(x) {paste(x[1], x[2], sep = "\n") })
   
   df$name <-  factor(df$name ,levels = top10_names)
    
    
    #####plot####
      
    plot_ly(df,
            x=~name,
            y=~n,
            type="bar",
            frame = ~date,
            text = ~n,
            textposition = 'outside',
            marker = list(color = ~color),
            hoverinfo = 'none') %>%
      layout(
        xaxis = list(title = ''), 
        yaxis = list(title = 'Liczba wiadomości',range = c(0, range))) %>%  
      animation_opts(100 ,redraw = T) %>%
      animation_slider(
        currentvalue = list(
          prefix = "",
          font = list(color = "darkred", size = 20)
        )) %>%
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = FALSE)

    
    
    
  })
  
  output$line_count_messages <- renderPlotly({
    
    #####data_processing####
    
    date_start <- input$line_date_count_messages[1]
    date_end <- input$line_date_count_messages[2]
    
    hour_sequence <- seq(as.POSIXct(date_start),as.POSIXct(date_end),by = "1 hour")
    hour_sequence <- as_tibble(hour_sequence) %>% rename(date = value)
    
    
       
    pipi_df <- pipi_count_messages %>% 
      filter(!is_group,
             name == "Paweł Pozorski",
             date >= date_start,
             date <= date_end)
    
    
    pipi_df <- full_join(pipi_df , hour_sequence, by = join_by(date))
    pipi_df$`min_messages_is_0` = ifelse(is.na(pipi_df$`min_messages_is_0`), 0, pipi_df$`min_messages_is_0`)
    
    pipi_df$hour <- paste(format(pipi_df$date, "%H"),":00",sep="")
    pipi_df$hour <- as.factor(pipi_df$hour)
    
    
    pipi_df <-  pipi_df %>% 
      group_by(hour) %>%
      summarise(mean_messages = round(mean(`min_messages_is_0`),digits = 2)) %>%
      mutate(df_name = "Paweł Pozorski")
    
    
    kiddo_df <- kiddo_count_messages %>% 
      filter(!is_group,
             name == "Krzysiek Adamczyk",
             date >= date_start,
             date <= date_end)
    
    
    kiddo_df <- full_join(kiddo_df , hour_sequence, by = join_by(date))
    kiddo_df$`min_messages_is_0` = ifelse(is.na(kiddo_df$`min_messages_is_0`), 0, kiddo_df$`min_messages_is_0`)
    
    kiddo_df$hour <- paste(format(kiddo_df$date, "%H"),":00",sep="")
    kiddo_df$hour <- as.factor(kiddo_df$hour)
    
    
    kiddo_df <-  kiddo_df %>% 
      group_by(hour) %>%
      summarise(mean_messages = round(mean(`min_messages_is_0`),digits = 2)) %>%
      mutate(df_name = "Krzysiek Adamczyk")
    
    
    misiu_df <- misiu_count_messages %>% 
      filter(!is_group,
             name == "Michał Iwaniuk",
             date >= date_start,
             date <= date_end)
    
    
    misiu_df <- full_join(misiu_df , hour_sequence, by = join_by(date))
    misiu_df$`min_messages_is_0` = ifelse(is.na(misiu_df$`min_messages_is_0`), 0, misiu_df$`min_messages_is_0`)
    
    misiu_df$hour <- paste(format(misiu_df$date, "%H"),":00",sep="")
    misiu_df$hour <- as.factor(misiu_df$hour)
    
    
    misiu_df <-  misiu_df %>% 
      group_by(hour) %>%
      summarise(mean_messages = round(mean(`min_messages_is_0`),digits = 2)) %>%
      mutate(df_name = "Michał Iwaniuk")
    
    
    
    
    df <- rbind(pipi_df, misiu_df, kiddo_df)
    
    
    df$df_name <- factor(df$df_name)
    df$label = paste("Godzina: ",df$hour," +-30 minut","\n","Średnia liczba wysłanych wiadomości: ",df$mean_messages,sep="")
    
    max_range = max(df$mean_messages)+1
    
    df <- df %>% mutate(df_name = case_when(df_name == "Paweł Pozorski" ~ "Por",
                                            df_name == "Michał Iwaniuk" ~ "Misiu",
                                            df_name == "Krzysiek Adamczyk" ~ "Kiddo",
                                            T ~ df_name))
    
    
    #####plot####
    plot_ly(df,
            x = ~hour,
            y=~ mean_messages,
            color=~df_name,
            type = "scatter",
            mode = "line",
            text = ~label,
            hoverinfo = "text") %>%
      layout(yaxis = list(title ="Średnia liczba wysłanych wiadomości" ,
                          range= c(0,max_range)),
             xaxis = list(title = "")) 
    
  })
  
  output$line2_count_messages <- renderPlotly({
    
    
    #####data_processing####
    
    pipi_df <- pipi_count_messages %>%
      filter(name=="Paweł Pozorski")%>%
      group_by(round_2week) %>%
      summarise(sum_messages = sum(`min_messages_is_0`)) %>%
      mutate(label = paste("Zakres dat : ",round_2week," +-7 dni","\n","Liczba wysłanych wiadomości: ",sum_messages ,sep="")) %>%
      mutate(df_name = "Paweł Pozorski")
    
    misiu_df <- misiu_count_messages %>%
      filter(name=="Michał Iwaniuk")%>%
      group_by(round_2week) %>%
      summarise(sum_messages = sum(`min_messages_is_0`)) %>%
      mutate(label = paste("Zakres dat : ",round_2week," +-7 dni","\n","Liczba wysłanych wiadomości: ",sum_messages ,sep="")) %>%
      mutate(df_name = "Michał Iwaniuk")
    
    kiddo_df <- kiddo_count_messages %>%
      filter(name=="Krzysiek Adamczyk")%>%
      group_by(round_2week) %>%
      summarise(sum_messages = sum(`min_messages_is_0`)) %>%
      mutate(label = paste("Zakres dat : ",round_2week," +-7 dni","\n","Liczba wysłanych wiadomości: ",sum_messages ,sep="")) %>%
      mutate(df_name = "Krzysiek Adamczyk")
    
        
    df <- rbind(pipi_df,misiu_df,kiddo_df)
    
    df <- df %>% mutate(df_name = case_when(df_name == "Paweł Pozorski" ~ "Por",
                                            df_name == "Michał Iwaniuk" ~ "Misiu",
                                            df_name == "Krzysiek Adamczyk" ~ "Kiddo",
                                            T ~ df_name))
    
    
    #####plot####
    plot_ly(df,
            x=~round_2week,
            y=~sum_messages,
            color=~df_name,
            type="scatter",
            mode="line",
            text=~label,
            hoverinfo="text")%>%
      layout(yaxis = list(title = "Liczba wysłanych wiadomości",range=c(0,max(df$sum_messages)*1.05)),
             xaxis = list(title = ""))
  })
  
  output$line3_count_messages <- renderPlotly({
    
    #####data_processing####
    line3_count_messages$label <- paste("Data: ", line3_count_messages$date, "\n", "Liczba wiadomości: ",line3_count_messages$n_messages, sep="")
    
    df <- line3_count_messages %>% mutate(name = case_when(name == "Paweł Pozorski" ~ "Por",
                                            name == "Michał Iwaniuk" ~ "Misiu",
                                            name == "Krzysiek Adamczyk" ~ "Kiddo",
                                            T ~ name))
    
    #####plot####
    plot_ly(df,
            x = ~date,
            y = ~n_messages,
            color=~name,
            type="scatter",
            mode="line",
            text=~label,
            hoverinfo="text") %>%
      layout(yaxis = list(title = "Łączna liczba wysłanych wiadomości",range=c(0,max(line3_count_messages$n_messages)*1.05)),
             xaxis = list(title=""))
    
  })
  
  
  
  output$heatmap_time_respond <- renderPlotly({
    
    #####data_processing####
    filtered_df <- combined_time_respond %>%    
      filter(name == input$user,
             time_send >= input$heatmap_date_time_respond[1],
             time_send <= input$heatmap_date_time_respond[2])
    
    df <- filtered_df %>%
      group_by(day_send,round_hour_send) %>%
      summarise(mean_delta_min = round(mean(delta_min),digits = 2))%>%
      mutate(label = paste("Dzień:", day_send, "\nGodzina:", round_hour_send, "\nŚredni czas odpowiedzi:", mean_delta_min, "minut"))%>%
      mutate(log_delta = log10(mean_delta_min))
    
    
    
    df1 <- full_join(df, days_hours, by = join_by(day_send==Var1,round_hour_send==Var2 )) %>%
      mutate(label = case_when(
        is.na(label)~"Brak danych",
        T~label))
    
    
    #####plot####
    colors <- c("#FFFFD9", "#EDF8B1", "#C7E9B4", "#7FCDBB", "#41B6C4", "#1D91C0", "#225EA8", "#253494", "#081D58")
    breaks <- c(0, 0.7, 1, 1.18, 1.3, 1.48, 1.78, 1.95, 2.08)
    labels <- c("1 min", "5 min", "10 min", "15 min", "20 min", "30 min", "60 min", "90 min", "120 min")
    
    plot_ly(
      data = df1,
      x = ~round_hour_send,
      y = ~day_send,
      z = ~log_delta,
      text = ~label,
      type = "heatmap",
      colorscale = list(seq(0, 1, length.out = 9), colors),
      zmin = min(breaks),
      zmax = max(breaks),
      colorbar = list(
        tickvals = seq(min(breaks), max(breaks), length.out = 9),
        ticktext = labels,
        title = "Średni czas\nodpowiedzi",
        len = 1
      ),
      hoverinfo = "text") %>%
      layout(
        xaxis = list(title = "Godzina otrzymania wiadomości"),
        yaxis = list(title = "Dzień tygodnia"),
        hoverlabel = list(bgcolor = "white")
      ) %>% 
      layout(xaxis = list(fixedrange = TRUE),
             yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = FALSE)
    
  })
  
  output$line_time_respond <- renderPlotly({
    
    df <- line_time_respond_df %>% mutate(name = case_when(name == "Paweł Pozorski" ~ "Por",
                                                           name == "Michał Iwaniuk" ~ "Misiu",
                                                           name == "Krzysiek Adamczyk" ~ "Kiddo",
                                                           T ~ name))
    
    #####plot####
    plot_ly(df,
            x = ~round_2week,
            y = ~mean_delta,
            type="scatter",
            mode="lines",
            color = ~name,
            text = ~label,
            hoverinfo = "text") %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Średni czas odpowiedzi (min)", range = c(0,max(line_time_respond_df$mean_delta)))
      )
    
  })
  
  
  
  output$barplot_common_strings <- renderPlotly({
    
    #####data_processing####
    
    common_strings_number <- input$barplot_n_common_strings
    
    if(input$barplot_q_common_strings == 1){
      filtered_df <- combined_common_strings_1 %>% 
        filter(name == input$user)
      
    }else if(input$barplot_q_common_strings == 2){
      
      filtered_df <- combined_common_strings_2 %>% 
        filter(name == input$user)
      
    }else{
      
      filtered_df <- combined_common_strings_3 %>% 
        filter(name == input$user)
      
    }
    
    
    
    df <- filtered_df %>% 
      arrange(desc(count))%>%
      slice(1:common_strings_number) %>%
      mutate(sequence_of_strings = fct_reorder(sequence_of_strings, -count))
    
    #####plot####
    
    plot_ly(df,
            x=~sequence_of_strings,
            y=~count,
            type="bar") %>% 
      layout(xaxis = list(fixedrange = TRUE, title = "Sekwencja słów"),
             yaxis = list(fixedrange = TRUE, title = "Liczba użyć")) %>%
      config(displayModeBar = FALSE)
    
    
    
    
  })
  
  
  #####titles####
  
  output$barplot_emoji_title <- renderText({
    
    if(input$user == "Paweł Pozorski"){
      "Najczęściej używane emotki użytkownika Por"
    }else if(input$user == "Michał Iwaniuk"){
      "Najczęściej używane emotki użytkownika Misiu"
    }
    else{
      "Najczęściej używane emotki użytkownika Kiddo"
    }
      
  })
  
  
  
  output$barplot_count_messages_title <- renderText({
    
    if(input$user == "Paweł Pozorski"){
      "Użytkownicy z którymi najczęściej pisał Por (imiona przypadkowe)"
    }else if(input$user == "Michał Iwaniuk"){
      "Użytkownicy z którymi najczęściej pisał Misiu (imiona przypadkowe)"
    }
    else{
      "Użytkownicy z którymi najczęściej pisał Kiddo (imiona przypadkowe)"
    }
    
  })
  
  output$animated_barplot_count_messages_title <- renderText({
    
    if(input$user == "Paweł Pozorski"){
      "Zmiana liczby wiadomości w czasie z użytkownikami z którymi najczęściej pisał Por (imiona przypadkowe)"
    }else if(input$user == "Michał Iwaniuk"){
      "Zmiana liczby wiadomości w czasie z użytkownikami z którymi najczęściej pisał Misiu (imiona przypadkowe)"
    }
    else{
      "Zmiana liczby wiadomości w czasie z użytkownikami z którymi najczęściej pisał Kiddo (imiona przypadkowe)"
    }
    
  })
  
  
  
  output$heatmap_time_respond_title <- renderText({
    
    if(input$user == "Paweł Pozorski"){
      "Średni czas odpowiedzi na wiadomości użytkownika Por w zależności od dnia i godziny"
    }else if(input$user == "Michał Iwaniuk"){
      "Średni czas odpowiedzi na wiadomości użytkownika Misiu w zależności od dnia i godziny"
    }
    else{
      "Średni czas odpowiedzi na wiadomości użytkownika Kiddo w zależności od dnia i godziny"
    }
    
  })
  
  output$line_time_respond_title <- renderText({
    
    "Średni czas odpowiedzi na wiadomości użytkowników w danym przedziale czasowym"
  })
  
  
  
  output$barplot_common_strings_title <- renderText({
    
    if(input$user == "Paweł Pozorski"){
      "Najczęściej używane sekwencje słów użytkownika Por (w bezokolicznikach)"
    }else if(input$user == "Michał Iwaniuk"){
      "Najczęściej używane sekwencje słów użytkownika Misiu (w bezokolicznikach)"
    }
    else{
      "Najczęściej używane sekwencje słów użytkownika Kiddo (w bezokolicznikach)"
    }
    
  })
  
  
  
  #####summarise####
  
  output$summarise_title <- renderText({
    
    if(input$user == "Paweł Pozorski"){
      "Wybrałeś osobę: Por "
    }else if(input$user == "Michał Iwaniuk"){
      "Wybrałeś osobę: Misiu"
    }
    else{
      "Wybrałeś osobę: Kiddo"
    }
    
  })
  
  output$summarise_title_2 <- renderText({
    
    if(input$user == "Paweł Pozorski"){
      "Statystyki osoby: Por "
    }else if(input$user == "Michał Iwaniuk"){
      "Statystyki osoby: Misiu"
    }
    else{
      "Statystyki osoby: Kiddo"
    }
    
  })
  
  output$summarise_gif <- renderImage({
    
    list(src = paste("gifs/",input$user, ".gif",sep=""),
         contentType = 'image/gif',
         height=400)
  },deleteFile = F)
  
  output$summarise_info <-  renderText({
    
    
    user <- input$user
    
    
    emoji <- combined_emoji %>%filter(name==user) %>%
      group_by(emoji)%>%
      summarise(n = n()) %>%
      arrange(desc(n)) %>% slice(1) 
    
    
    most_messages <- top_users %>%
      filter(df_name == user) %>%
      arrange(desc(n)) %>%
      slice(1) %>%
      pull(n)
    
    
    
    
    hour_sequence <- seq(as.POSIXct("2017-01-01"),as.POSIXct("2024-01-01"),by = "1 hour")
    hour_sequence <- as_tibble(hour_sequence) %>% rename(date = value)
    df <- combined_count_messages %>% 
      filter(!is_group,
             name == input$user,
             df_name == input$user)
    df <- full_join(df , hour_sequence, by = join_by(date))
    df$`min_messages_is_0` = ifelse(is.na(df$`min_messages_is_0`), 0, df$`min_messages_is_0`)
    df$hour <- paste(format(df$date, "%H"),":00",sep="")
    df$hour <- as.factor(df$hour)
    
    
    
    
    hour_mes <-  df %>% 
      group_by(hour) %>%
      summarise(mean_messages = round(mean(`min_messages_is_0`),digits = 2)) %>%
      arrange(desc(mean_messages)) %>%
      slice(1)
    
    
    
    
    mean_mes <- df %>% 
      group_by(hour) %>%
      summarise(mean_messages = round(mean(`min_messages_is_0`),digits = 2)) %>%
      pull(mean_messages) %>%
      sum()
    
    
    
    
    all_mes <- line3_count_messages %>%
      filter(name == input$user) %>%
      arrange(desc(n_messages)) %>%
      slice(1) %>%
      pull(n_messages)
    
    
    
    
    mean_res <- line_time_respond_df %>% 
      filter(name == input$user,
             round_2week > "2023-01-01") %>%
      pull(mean_delta)%>%
      mean()
    mean_res <- round(mean_res,digits = 2)
    
    
    
    
    common_word <- combined_common_strings_1 %>%
      filter(name == input$user) %>%
      arrange(desc(count)) %>%
      slice(1) 
    
    
    
    text <- paste("<b>Najczęściej używana emotka:  </b>" , emoji[[1]] ,"<b>,  liczba użyć:  </b>", emoji[[2]] ,"<br><br>",
                  "<b>Największa liczba wiadomości z jednym użytkownikiem:  </b>" , most_messages,"<br><br>",
                  "<b>Godzina największej aktywności:  </b>",hour_mes[[1]],"<br><br>",
                  "<b>Średnia liczba wysłanych wiadomości o ",hour_mes[[1]],":  </b>",hour_mes[[2]],"<br><br>",
                  "<b>Średnia liczba wysłanych wiadomości w ciągu dnia:  </b>",mean_mes,"<br><br>",
                  "<b>Łączna liczba wysłanych wiadomości na Messengerze:  </b>",all_mes,"<br><br>",
                  "<b>Średni czas odpowiedzi na wiadomość w roku 2023:  </b>",mean_res," minut","<br><br>",
                  "<b>Najczęściej używane słowo: </b>\"",common_word[[2]],"\"<b>,  liczba użyć:  </b>",common_word[[3]],
                  sep=""
    )
    
    text <- paste("<div style='font-size: 17px;'>",text,"</div>", sep="")
    text
    
  })
  
}
  
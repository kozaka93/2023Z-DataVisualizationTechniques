# # For testing purposes only
# library(ggplot2)
# library(tidyverse)
# library(shiny)
# library(plotly)
# puuid_Jan <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
# puuid_Bartek <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
# puuid_Mateusz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

df_item_champ <- df_player_match_stats %>%
  inner_join(df_player_match_stats %>% 
               group_by(player_id, position, champion_name) %>%
               count() %>%
               dplyr::filter(n > 1) %>% 
               select(player_id, champion_name),
             by = c('player_id','position','champion_name'))

summoner <- data.frame(name = c("Jan","Bartek","Mateusz"),
                       puuid = c( "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA",
                                  "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA",
                                  "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"))

f_stats_position_reactive <- function(player_name, summoner_position, id1, type, compare = "brak"){
  
  if(is.null(id1)){
    id1 = "All"
  }
  
  if (player_name == "Jan") {
    player_puuid <- puuid_Jan
  } else if (player_name == "Bartek") {
    player_puuid <- puuid_Bartek
  } else if (player_name == "Mateusz") {
    player_puuid <- puuid_Mateusz
  } else {
    stop("Error: Invalid player_puuid.")
  }
  
  df_stats_position <- df_item_champ %>% 
    dplyr::filter(player_id == player_puuid)
  
  if (type == "Density") {
    if (id1 == "All") {
      df_stats_position <- df_stats_position %>%
        dplyr::filter(position == summoner_position) %>% 
        mutate(compare = "All")
    } else if (id1 == "None") {
      df_stats_position <- data.frame() %>%
        mutate(compare = "None")
    } else {
      df_stats_position <- df_stats_position %>%
        dplyr::filter(position == summoner_position) %>%
        mutate(compare = ifelse(champion_name == id1, id1, compare))
      if (compare == "Don't") {
        df_stats_position <- df_stats_position %>% 
          dplyr::filter(champion_name == id1)
      } else if (compare != "All") {
        df_stats_position <- df_stats_position %>% 
          dplyr::filter(champion_name %in% c(id1, compare))
      }
    }
  } else {
    df_stats_position <- df_stats_position %>% 
      dplyr::filter(position == summoner_position)
    if (id1 == "None") {
      df_stats_position <- data.frame()
    } else {
      if (id1 != "All") {
        df_stats_position <- df_stats_position %>% 
          dplyr::filter(champion_name == id1)
      }
      df_stats_position <- df_stats_position %>% 
        arrange(match_start_time) %>% 
        mutate(n = 1:nrow(df_stats_position))
    }
  }
  return(df_stats_position)
}

f_overall_stats_plot <- function(player_name, summoner_position, id1, type, stat, compare = "brak"){
  
  if (stat == "DmgPerDeath") {
    chart_title = "Zadane obrażenia do ilości śmierci"
  } else if (stat=="kill_participation"){
    chart_title = "Procentowy udział w zabójstwach"
  } else if (stat=="MinionsPerMinute") {
    chart_title = "Miniony na minute"
  } else {
    chart_title = "Współczynnik KDA"
  }
  
  df_stats_position <- f_stats_position_reactive(player_name, summoner_position, id1, type, compare)
  if (nrow(df_stats_position) != 0) {
    if (type=="Density") {
      if (stat=="DmgPerDeath") {
        gg <- ggplot(data = df_stats_position %>% mutate(deaths = ifelse(deaths==0,1,deaths)),aes(x = total_damage_dealt_to_champions/deaths)) +
          geom_density(aes(fill = compare),alpha = 0.5)
        
      } else if (stat=="kill_participation") {
        gg <- ggplot(data = df_stats_position,aes(x = kill_participation)) +
          geom_density(aes(fill = compare),alpha = 0.5)
        
      } else  if (stat=="MinionsPerMinute") {
        gg <- ggplot(data = df_stats_position,aes(x = total_minions_killed/(game_length/60))) +
          geom_density(aes(fill = compare),alpha = 0.5)
        
      } else {
        gg <- ggplot(data = df_stats_position %>% mutate(deaths = ifelse(deaths==0,1,deaths)),aes(x = (kills+assists)/deaths)) +
          geom_density(aes(fill = compare),alpha = 0.5)
      }
      gg <- ggplotly(gg + theme(legend.text = element_text(color = "#c8aa6e"),
                                legend.background = element_rect(fill = "#020a13"),
                                plot.margin = margin(t = 30,  
                                                r = 0,  
                                                b = 0,  
                                                l = 0)) +
                       scale_fill_manual(values = c("#0ac8b9","#005a82")) +
                       scale_x_continuous(expand = c(0, 0), limits = c(0, NA),) + 
                       scale_y_continuous(expand = c(0, 0), limits = c(0, NA)),
                     hovertemplate = "<i>xd</i>") %>%
        layout(legend=list(title=list(text='Comparison:',font = list(color = "#c8aa6e")))) %>%
        config(staticPlot = TRUE)
      ##882a2e
      #values = c("#0ac8b9","#005a82"))) 
    } else if (type=="Chronologically") {
      if(stat=="DmgPerDeath"){
        gg <- plot_ly(data = df_stats_position %>% mutate(deaths = ifelse(deaths==0,1,deaths)),
                      x = ~n,
                      y = ~total_damage_dealt_to_champions/deaths,
                      type = 'scatter',
                      mode = 'lines+markers',
                      text = ~champion_name,
                      hovertemplate = paste('<i>Value</i>: %{y:.2f}',
                                            '<br><b>Match</b>: %{x:.0f}',
                                            '<br><b>Champ</b>: <b>%{text}</b>','<extra></extra>'),
            showlegend = FALSE
          ) 
      } else if (stat=="kill_participation") {
        gg <- plot_ly(data = df_stats_position,
                      x = ~n,
                      y = ~kill_participation,
                      type = 'scatter',
                      mode = 'lines+markers',
                      text = ~champion_name,
                      hovertemplate = paste('<i>Value</i>: %{y:.2f}',
                                            '<br><b>Match</b>: %{x:.0f}',
                                            '<br><b>Champ</b>: <b>%{text}</b>','<extra></extra>'),
                      showlegend = FALSE
        ) 
      } else if (stat=="kill_participation") {
        gg <- plot_ly(data = df_stats_position,
                      x = ~n,
                      y = ~total_minions_killed/(game_length/60),
                      type = 'scatter',
                      mode = 'lines+markers',
                      text = ~champion_name,
                      hovertemplate = paste('<i>Value</i>: %{y:.2f}',
                                            '<br><b>Match</b>: %{x:.0f}',
                                            '<br><b>Champ</b>: <b>%{text}</b>','<extra></extra>'),
                      showlegend = FALSE
        ) 
      } else {
        gg <- plot_ly(data = df_stats_position %>% mutate(deaths = ifelse(deaths==0,1,deaths)),
                      x = ~n,
                      y = ~(kills + assists)/deaths,
                      type = 'scatter',
                      mode = 'lines+markers',
                      text = ~champion_name,
                      hovertemplate = paste('<i>Value</i>: %{y:.2f}',
                                            '<br><b>Match</b>: %{x:.0f}',
                                            '<br><b>Champ</b>: <b>%{text}</b>','<extra></extra>'),
                      showlegend = FALSE
        ) 
      }
    }
  } else {
    gg <- ggplot(data = data.frame(brak = "brak")) + geom_text(aes(x = 1,y = 1,label = brak))
    gg <- ggplotly(gg)
  }
  
  gg <- gg  %>%
    layout(
      title = list(text =  chart_title),
      font = list(
        size = 12,
        color = "#c8aa6e"
      ),
      xaxis = list(title = "",tickfont = list(color = "#5b5a56"),color = "#c8aa6e", showgrid = TRUE, gridcolor = "#c8aa6e35", zeroline = TRUE, showline = TRUE,rangemode="tozero"),
      yaxis = list(title = "",tickfont = list(color = "#5b5a56"),color = "#c8aa6e", showgrid = TRUE, gridcolor = "#c8aa6e35", zeroline = TRUE, showline = TRUE,rangemode="tozero"),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "rgba(0,0,0,0)"
      ) %>%
    config(displayModeBar = FALSE)

  return(gg)
}
# # For testing purposes only
# library(plotly)
# library(dplyr)
# df_player_match_stats <- read.csv("./db/playerMatchStats.csv")
# puuid_Jan <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
# puuid_Bartek <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
# puuid_Mateusz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

f_plot_champions <- function(player, stat, champ_amount = 5){
    

    # Player names and colors
    if(player == "Jan"){
        player_puuid <- puuid_Jan
        color <- "#4F6F52"
    } else if (player == "Bartek"){
        player_puuid <- puuid_Bartek
        color <- "#f6bb45"
    } else if (player == "Mateusz"){
        player_puuid <- puuid_Mateusz
        color <- "#005a82"
    } else {
        stop("Error: Invalid player_puuid.")
    }

    # Filter data
    player_data <- df_player_match_stats %>%
        dplyr::filter(player_id == player_puuid)

    # Filter out ${champ_amount} champions that were played the most
    top_champions <- player_data %>%
        group_by(champion_name) %>%
        summarise(count = n()) %>%
        arrange(desc(count)) %>%
        head(champ_amount) %>%
        pull(champion_name)

    player_data <- player_data %>%
        filter(champion_name %in% top_champions)

    # Prepare stat to plot and set y axis title
    if (stat == "Zabójstwa") {
        y_axis_title <- "Ilość zabójstw"
        stats <- player_data %>% 
            group_by(champion_name) %>% 
            summarise(target = sum(kills)) 
    } else if (stat == "Śmierci") {
        y_axis_title <- "Ilość śmierci"
        stats <- player_data %>% 
            group_by(champion_name) %>% 
            summarise(target = sum(deaths))
    } else if (stat == "Współczynnik KDA"){
        y_axis_title <- "Współczynnik KDA"
        stats <- player_data %>%
            group_by(champion_name) %>%
            mutate(deaths = ifelse(deaths == 0, 1, deaths)) %>%
            summarise(target = (sum(kills) + sum(assists)) / sum(deaths))
    } else if (stat == "Współczynnik zwycięstw"){
        y_axis_title <- "Procent wygranych gier"
        stats <- player_data %>%
            group_by(champion_name) %>%
            summarise(target = sum(win)/n())
    } else if (stat == "Liczba gier"){
        y_axis_title <- "Ilość rozegranych gier"
        stats <- player_data %>%
            group_by(champion_name) %>%
            summarise(target = n())
    } else {
        stop("Error: Invalid stat.")
    }

    # Plot
    plot_champions <- stats %>%
        plot_ly(
            x = ~factor(champion_name), 
            y = ~target, 
            type = "bar",
            marker = list(color = paste(color, "88", sep=""), 
            line = list(color = color, width = 2))) %>%
    layout(
        xaxis = list(title = "Nazwa championa", color = "#c8aa6e"),
        yaxis = list(title = y_axis_title, color = "#c8aa6e", showgrid = TRUE, gridcolor = "#c8aa6e35", zeroline = TRUE, showline = TRUE),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "rgba(0,0,0,0)",
        font = list(
            size = 14,
            color = "#c8aa6e"
        )
    ) %>%
    config(displayModeBar = FALSE)

    return(plot_champions)
}

# f_plot_champions("Mateusz", "kda")

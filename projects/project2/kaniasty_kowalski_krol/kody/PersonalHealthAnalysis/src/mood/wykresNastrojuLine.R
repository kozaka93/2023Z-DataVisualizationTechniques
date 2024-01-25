library(dplyr)
library(ggplot2)


moodLine <- function(startDate, endDate, user) {
  
  if(user == "Hubert") {
    daylio <- read.csv("./data/mood/daylio-export-H2.csv", header = TRUE)
  }
  else if(user == "Mateusz") {
    daylio <- read.csv("./data/mood/daylio-export-M.csv", header = TRUE)
  }
  else if(user == "Adam") {
    daylio  <- read.csv("./data/mood/daylio-export-A.csv", header = TRUE, sep = ";")
  }

# Mapping moods to integers:
map_mood <- data.frame(
  mood = c("wspaniale", "dobrze", "tak sobie", "źle", "okropnie"),
  value = c(5.0, 4.0, 3.0, 2.0, 1.0)
)

daylio_moods <- merge(daylio, map_mood, by = "mood")

# Filter dates:
daylio_moods <- daylio_moods %>%
  filter(as.Date(full_date) >= startDate & as.Date(full_date) <= endDate)


# Divide data into two sets: duplicated dates and unique dates
daylio_moods %>%
  group_by(full_date) %>%
  summarise(n = n()) %>%
  filter(n > 1) %>%
  select(full_date) -> duplicated_dates

daylio_moods_dup <- daylio_moods %>%
  filter(full_date %in% simplify2array(duplicated_dates)) %>%
  group_by(full_date) %>%
  summarise(maxm = max(value), minm = min(value), meanm = mean(value))

daylio_moods_sin <- daylio_moods %>%
  filter(!(full_date %in% simplify2array(duplicated_dates)))

daylio_moods_dup_line <- daylio_moods_sin %>%
  merge(daylio_moods_dup, by = "full_date") %>%
  select(-c(minm, maxm, value)) %>%
  rename(value = meanm)

daylio_moods_line <- rbind(daylio_moods_dup_line, daylio_moods_sin)

daylio_moods_line <- daylio_moods_line %>%
  mutate(full_date = as.Date(full_date)) %>%
  arrange(full_date)

# Line plot od mood in time with gradient depending on Mood
# ggthemr::ggthemr(palette = 'earth')
# daylio_moods_line %>%
#   ggplot(aes(x = as.Date(full_date))) +
#   geom_line(aes(y = value, color = value), size = 2) +
#   theme(
#     plot.title = element_text(size = 20, hjust = 0.5),
#     axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5, size = 14),
#     axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 14),
#     axis.title = element_text(size = 16),
#     panel.grid.major.x = element_blank(),
#     legend.position = "none",
#     panel.background = element_rect(fill = "white")
#   ) + 
#   labs(
#     title = "Mood change in time",
#     x = "Date",
#     y = "Mood"
#   ) + 
#   scale_x_date(
#     date_breaks = "4 days",
#     date_labels = "%d %b"
#   ) + 
#   scale_y_continuous(
#     breaks = c(1.0, 2.0, 3.0, 4.0, 5.0),
#     labels = c("terrible", "bad", "so-so", "good", "fantastic"),
#     limits = c(1,5.5)
#   ) +
#   scale_color_gradientn(
#     colours = c("red", "orange", "yellow", "green", "#008000"),
#     values = c(0, 0.25, 0.5, 0.75, 1)
#   ) -> mood_change_line

  map_moods_list <- list("terrible", "bad", "so-so", "good", "fantastic")
  names(map_moods_list) <- c(1.0, 2.0, 3.0, 4.0, 5.0)
 # Plotly
  mood_change_line_plotly <- daylio_moods_line %>%
  plot_ly(type = "scatter", mode = "markers+lines") %>%
    add_trace(
      x = ~as.Date(full_date),
      y = ~value,
      line = list(width = 3, color = "rgba(256, 256, 256, 0.7)", shape = "spline"),
      marker = list(
        size = 16,
        color = ~value,
        colorscale = list(
          c(0, 0.25, 0.5, 0.75, 1),
          c("red", "orange", "yellow", "green", "#008000")
          ),
        showscale = F
      ),
      hoverinfo = "text",
      text = ~paste("<b>Date: </b>", full_date, "<br><b>Mood:</b> ", map_moods_list[value])
    ) %>%
    layout(
      title = list(text = paste0("Mood change in time for ", user), font = list(color = "#f7f7f7")),
      xaxis = list(
        title = "Date",
        tickformat = "%d %b",
        tickangle = -30,
        color = "#f7f7f7",
        gridcolor = 'rgba(247, 247, 247, 0.5)',
        zerolinecolor = 'rgba(247, 247, 247, 0.5)'
      ),
      yaxis = list(
        title = "Mood",
        tickvals = c(1.0, 2.0, 3.0, 4.0, 5.0),
        ticktext = c("terrible", "bad", "so-so", "good", "fantastic"),
        range = c(1, 5.5),
        color = "#f7f7f7",
        gridcolor = 'rgba(247, 247, 247, 0.5)',
        zerolinecolor = 'rgba(247, 247, 247, 0.5)'
      ),
      plot_bgcolor = "rgba(0,0,0,0)",
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(autoexpand = TRUE,
                                l = 30,
                                r = 20,
                                t = 50,
                                b = 50),
      showlegend = FALSE
    )
  
  

return(mood_change_line_plotly)
}

library(dbplyr)
library(ggplot2)
library(plotly)



moodLollipop <- function(startDate, endDate, user) {
  
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
  mood = c("okropnie", "źle", "tak sobie", "dobrze", "wspaniale"),
  value = c(1,2,3,4,5)
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
  filter(!full_date %in% simplify2array(duplicated_dates))


# Change of mood in time (lollipop plot)
daylio_moods %>%
  ggplot(aes(x = as.Date(full_date))) +
  geom_linerange(data = daylio_moods_dup, aes(x = as.Date(full_date), ymin = minm, ymax = meanm, color = minm), size = 1.5) +
  geom_linerange(data = daylio_moods_dup, aes(x = as.Date(full_date), ymin = meanm, ymax = maxm, color = maxm), size = 1.5) + 
  geom_point(data = daylio_moods, aes(x = as.Date(full_date), y = value, color = value), size = 6) + 
  theme(
    plot.title = element_text(size = 17, hjust = 0.5, color = "#f7f7f7"),
    axis.text.x = element_text(angle = 30, hjust = 0.5, vjust = 0.5, size = 11, color = "#f7f7f7"),
    axis.text.y = element_text(angle = 0, hjust = 0.5, vjust = 0.5, size = 11, color = "#f7f7f7"),
    axis.title = element_text(size = 12, color = "#f7f7f7"),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    panel.background = element_rect(fill = "rgba(0,0,0,0)"),
    plot.background = element_rect(fill = "rgba(0,0,0,0)")
  ) + 
  labs(
    title = paste0("Mood change in time for ", user),
    x = "Date",
    y = "Mood"
  ) + 
  scale_x_date(
    date_breaks = "4 days",
    date_labels = "%d %b"
  ) + 
  scale_y_continuous(
    breaks = c(1, 2, 3, 4, 5),
    labels = c("terrible", "bad", "so-so", "good", "fantastic"),
    limits = c(1,5.5)
  ) + 
  scale_color_gradientn(
    colours = c("#FF0000", "#FFA500", "#FFFF00", "#00FF00", "#008000")
  ) -> mood_change_lollipop


  map_moods_list <- list("terrible", "bad", "so-so", "good", "fantastic")
  names(map_moods_list) <- c(1.0, 2.0, 3.0, 4.0, 5.0)
  
  mood_change_lollipop_plotly <- ggplotly(mood_change_lollipop) %>%
    style(
      hovertext = paste0(
        "<b>Date:</b> ", daylio_moods$full_date, "<br>",
        "<b>Mood:</b> ", map_moods_list[daylio_moods$value]
      )
    )
  mood_change_lollipop_plotly

  return(mood_change_lollipop_plotly)
}

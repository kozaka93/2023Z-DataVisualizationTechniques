library(lubridate)

gymSpider <- function(data, all_data){
  
  num_days <- as.numeric(difftime(max(data$start_time), min(data$start_time), units = "days")) + 1
  num_days_all_data <- as.numeric(difftime(max(all_data$start_time), min(all_data$start_time), units = "days")) + 1
  
  muscle_groups <- data %>%
    group_by(muscle_group) %>%
    summarize(exercises_count = sum(reps, na.rm = TRUE)) %>% 
    mutate(exercises_count = exercises_count / num_days)
  
  
  p <- plot_ly() %>%
    add_trace(
      name = "Selected period",
      data = muscle_groups,
      type = 'scatterpolar',
      r = ~exercises_count,
      theta = ~muscle_group,
      mode = 'lines',
      fill = 'toself',
      fillcolor="#93d79b",
      opacity = 1,
      line = list(color = "#00A14F", width = 3),
      marker = list(
        size = 5,
        color = '#00A14F',
        line = list(
          color = '#00A14F',
          width = 2
        )
        ),
      text = ~paste(muscle_group, ": ", exercises_count),
      hoverinfo = 'text',
      hoveron = "points"
    ) %>%
    layout(
      polar = list(
        radialaxis = list(
          visible = T,
          color = '#f7f7f7'
        ),
        angularaxis = list(
          color = '#f7f7f7'
        ),
        bgcolor = 'rgba(0,0,0,0)'),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      showlegend = T
    )
  
  return(p)
}
library(plotly)
library(dplyr)
library(RColorBrewer)

weightDistribution <- function(data, input) {
  plotData <- data
  
  pastel_palette <- c('#FAEDCB', '#C9E4DE', '#C6DEF1', '#DBCDF0', '#F2C6DE', '#F7D9C4')
  
  color_mapping <- setNames(pastel_palette, unique(plotData$muscle_group))
  
  p <- plot_ly()
  
  # Iterate over each muscle group and add a violin plot for each
  for (muscle_group in unique(plotData$muscle_group)) {
    group_data <- plotData[plotData$muscle_group == muscle_group, ]
    
    if ("weight" %in% input$variables) {
      p <- p %>% add_trace(data = group_data, x = ~muscle_group, y = ~weight_kg, 
                           type = 'violin', name = muscle_group,
                           line = list(color = color_mapping[muscle_group]),
                           fillcolor = pastel_palette[muscle_group]
      )
    }
    
    if ("reps" %in% input$variables) {
      p <- p %>% add_trace(data = group_data, x = ~muscle_group, y = ~reps, 
                           type = 'violin', name = muscle_group,
                           line = list(color = color_mapping[muscle_group]))
    }
  }
  
  p <- p %>% layout(
    plot_bgcolor = 'rgba(0,0,0,0)',
    paper_bgcolor = 'rgba(0,0,0,0)',
    hoverlabel = list(bgcolor = 'rgba(0,0,0,0)', font = list(color = 'white')),
    yaxis = list(
      title = 'Value', 
      color = '#f7f7f7',
      gridcolor = 'rgba(247, 247, 247, 0.5)',
      zerolinecolor = 'rgba(247, 247, 247, 0.5)'
    ), 
    xaxis = list(
      title = 'Muscle Group', 
      color = '#f7f7f7',
      gridcolor = 'rgba(247, 247, 247, 0.5)',
      zerolinecolor = 'rgba(247, 247, 247, 0.5)'
    ),
    legend = list(title = list(text = 'Muscle Group'), orientation = 'v', x = 1, y = 1)
  )
  
  return(p)
}

library(plotly)
library(dplyr)

data <- data.frame(
  Player = c('James Harden','Russell Westbrook','LeBron James','Devin Booker','Damian Lillard','D\'Angelo Russell',
             'Kyrie Irving','Jure Holiday','DeMar DeRozan','Mike Conley','Trea Young','Nikola Jokic','De\'Aaron Fox',
             'Chris Paul','Ben Simmons','Kyle Lowry','Jeff Teague','Efrid Payton','Rajon Rondo','Draymond Green'),
  Team = c('HOU', 'HOU', 'LAL', 'PHX', 'POR','MIN',
           'BKN','NOP','SAS','MEM','ATL','DEN','SAC',
           'OKC','PHI','TOR','TOT','NYK','LAL','GSW'),
  Sht = c(22.3,22.4,19.4,18.3,20.4,18.4,
          20.8,16.5,15.4,12,17.6,14.7,19.1,
          12.6,10.1,13.8,8,9.7,6.8,7.3),
  Ast = c(7.5,7,10.2,6.5,8,6.3,
          6,6.7,5.6,4.4,9.3,7,6.8,
          6.7,8,7.5,5.2,7.2,5,8.9)
)

plot <- plot_ly(
  data = data,
  x = ~Sht,
  y = ~Ast,
  color = ~Team,
  colors = "Set1",
  type = "scatter",
  mode = "markers",
  #text = paste0('Player: ', data$Player, '<br>Team: ', data$Team),
  #hoverinfo = 'x+y+text'
  hoverinfo ="text", 
  text = ~paste("</br><b>Player:</b>", data$Player, 
                "</br><b>Team:</b> ", data$Team, 
                "</br><b>Shots/Game:</b> ", data$Sht,
                "</br><b>Assists/Game:</b> ", data$Ast)
) %>% 
  layout(title = list(text = "NBA 2019/20 Regular Season Shots per Game vs Assists per Game",
                      xanchor = "center", yanchor = "top",
                      font = list(size = 20)),
         xaxis = list(title = list(text = "Shots/Game", font = list(size = 12))),
         yaxis = list(title = list(text = "Assists/Game", font = list(size = 12))),
         legend = list(x = 1, y = 1),
         margin = list(l = 50, r = 50, b = 50, t = 80),
         plot_bgcolor = "grey85",
         paper_bgcolor = "grey85"
  )

plot

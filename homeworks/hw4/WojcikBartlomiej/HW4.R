library(plotly)

data_text <- "Rk,Squad,MP,W,D,L,GF,GA,GD,Pts,Pts/MP,xG,xGA,xGD,xGD/90,Attendance,Top Team Scorer,Goalkeeper,Notes
0,1,Manchester City,38,28,5,5,94,33,+61,89,2.34,77.7,31.3,+46.4,+1.22,\"53,249\",Erling Haaland - 36,Ederson,→ Champions League via league finish
1,2,Arsenal,38,26,6,6,88,43,+45,84,2.21,71.4,42.0,+29.5,+0.78,\"60,191\",\"Martin Ødegaard, Martinelli - 15\",Aaron Ramsdale,→ Champions League via league finish
2,3,Manchester Utd,38,23,6,9,58,43,+15,75,1.97,67.7,50.4,+17.3,+0.45,\"73,671\",Marcus Rashford - 17,David de Gea,→ Champions League via league finish
3,4,Newcastle Utd,38,19,14,5,68,33,+35,71,1.87,71.9,39.6,+32.4,+0.85,\"52,127\",Callum Wilson - 18,Nick Pope,→ Champions League via league finish
4,5,Liverpool,38,19,10,9,75,47,+28,67,1.76,72.4,50.8,+21.6,+0.57,\"53,163\",Mohamed Salah - 19,Alisson,→ Europa League via league finish
5,6,Brighton,38,18,8,12,72,53,+19,62,1.63,73.4,50.3,+23.2,+0.61,\"31,477\",Alexis Mac Allister - 10,Robert Sánchez,→ Europa League via league finish
6,7,Aston Villa,38,18,7,13,51,46,+5,61,1.61,50.3,52.6,-2.3,-0.06,\"39,485\",Ollie Watkins - 15,Emiliano Martínez,→ Europa Conference League via league finish
7,8,Tottenham,38,18,6,14,70,63,+7,60,1.58,57.1,49.7,+7.4,+0.19,\"61,585\",Harry Kane - 30,Hugo Lloris,
8,9,Brentford,38,15,14,9,58,46,+12,59,1.55,55.9,48.9,+7.0,+0.18,\"17,078\",Ivan Toney - 20,David Raya,
9,10,Fulham,38,15,7,16,55,53,+2,52,1.37,46.2,63.8,-17.6,-0.46,\"23,746\",Aleksandar Mitrović - 14,Bernd Leno,
10,11,Crystal Palace,38,11,12,15,40,49,-9,45,1.18,39.3,48.1,-8.8,-0.23,\"24,952\",Eberechi Eze - 10,Vicente Guaita,
11,12,Chelsea,38,11,11,16,38,47,-9,44,1.16,49.4,52.5,-3.0,-0.08,\"40,002\",Kai Havertz - 7,Kepa Arrizabalaga,
12,13,Wolves,38,11,8,19,31,58,-27,41,1.08,36.9,59.5,-22.6,-0.60,\"31,482\",\"Rúben Neves, Daniel Podence - 6\",José Sá,
13,14,West Ham,38,11,7,20,42,55,-13,40,1.05,49.2,53.0,-3.9,-0.10,\"62,462\",\"Saïd Benrahma, Jarrod Bowen - 6\",Łukasz Fabiański,
14,15,Bournemouth,38,11,6,21,37,71,-34,39,1.03,38.6,63.9,-25.3,-0.67,\"10,362\",Philip Billing - 7,Neto,
15,16,Nott'ham Forest,38,9,11,18,38,68,-30,38,1.00,39.3,64.2,-24.9,-0.66,\"29,188\",Taiwo Awoniyi - 10,Dean Henderson,
16,17,Everton,38,8,12,18,34,57,-23,36,0.95,45.2,65.7,-20.5,-0.54,\"37,180\",Dwight McNeil - 7,Jordan Pickford,
17,18,Leicester City,38,9,7,22,51,68,-17,34,0.89,50.6,63.4,-12.8,-0.34,\"30,193\",Harvey Barnes - 13,Danny Ward,Relegated
18,19,Leeds United,38,7,10,21,48,78,-30,31,0.82,47.4,67.2,-19.8,-0.52,\"34,626\",Rodrigo - 13,Illan Meslier,Relegated
19,20,Southampton,38,6,7,25,36,73,-37,25,0.66,37.7,60.9,-23.2,-0.61,\"30,440\",James Ward-Prowse - 9,Gavin Bazunu,Relegated"


data <- read.table(text = data_text, sep = ",", header = TRUE, quote = "\"")

data$Squad <- factor(data$Squad, levels = data$Squad)

plot_ly(data, x = ~Squad, y = ~GF, type = 'bar', name = 'Goals For',
        hovertemplate = 'Team: %{x}<br>Goals For: %{y}<br>Top Scorer: %{customdata}<extra></extra>',
        customdata = ~Top.Team.Scorer, marker = list(color = 'green')) %>%
  add_trace(y = ~GA, name = 'Goals Against', 
            hovertemplate = 'Team: %{x}<br>Goals Against: %{y}<br>Goalkeeper: %{customdata}<extra></extra>',
            customdata = ~Goalkeeper, marker = list(color = 'red')) %>%
  layout(title = list(text = 'Goals For and Against for Each Team', font = list(size = 20)),
         xaxis = list(title = 'Team', tickangle = 45, tickmode = 'array', tickvals = ~Squad,
                      titlefont = list(size = 16)),
         yaxis = list(title = 'Number of Goals', range = c(0, max(data$GF, data$GA) + 5),
                      titlefont = list(size = 16)),
         barmode = 'group',
         showlegend = TRUE, legend = list(x = 0.85, y = 1.1),
         margin = list(l = 50, r = 50, b = 100, t = 100))

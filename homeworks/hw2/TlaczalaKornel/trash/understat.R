# install.packages(worldfootballR)
library(worldfootballR)

team_urls <- understat_team_meta(team_name = c("Liverpool", "Manchester_City", "Tottenham"))
understat_results <- understat_league_match_results(league = "EPL", season_start_year = 2023)

# Aż szkoda że w tej pracy nie mam jak wykorzystać tych danych"
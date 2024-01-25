# This script is used to update the matches.csv file with new matches for a given player.

# API calls
ids <- get_match_ids(api_key, puuid, 100)
match_list_unfiltered <- get_matches(api_key, ids, 1, 20)

# Filters
match_list <- filter_matches(match_list_unfiltered, 1)

if(length(match_list) == 0){
  stop("No matches found")
}

existing_data <- read.csv("./db/matches.csv")

for (i in 1:length(match_list)) {
  match_id <- match_list[[i]]$metadata$matchId
  game_creation <- as.character(as.POSIXct(match_list[[i]]$info$gameCreation/1000,origin = "1970-01-01", tz = "UTC"))
  game_mode <- match_list[[i]]$info$gameMode
  map_id <- match_list[[i]]$info$mapId
  paricipants <- match_list[[i]]$metadata$participants

  new_record <- data.frame(
      match_id = match_id, 
      game_creation = game_creation, 
      game_mode = game_mode,
      map_id = map_id,
      paricipants = paricipants
  )

  if (!any(duplicated(rbind(existing_data, new_record)))) {
    existing_data <- rbind(existing_data, new_record)
  }
}

write.csv(existing_data, "./db/matches.csv", row.names = FALSE)
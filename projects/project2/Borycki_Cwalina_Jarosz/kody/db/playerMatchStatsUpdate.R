# This script is used to update the playerMatchStats.csv file with new matches for items_created given player.
library(tidyverse)

# API calls
ids <- get_match_ids(api_key, puuid, 100)
match_list_unfiltered <- get_matches(api_key, ids, 1, 20)

# Filters
match_list <- filter_matches(match_list_unfiltered, 1)

if(length(match_list) == 0){
  stop("No matches found")
}

existing_data <- read.csv("./db/playerMatchStats.csv")
source("./db/items.R")

for (i in 1:length(match_list)) {
    player_idx <- which(match_list[[i]]$metadata$participants == puuid)
  match_id <- match_list[[i]]$metadata$matchId
  match_start_time <-  match_list[[i]]$info$gameCreation
  kills <- match_list[[i]]$info$participants$kills[[player_idx]]
  deaths <- match_list[[i]]$info$participants$deaths[[player_idx]]
  assists <- match_list[[i]]$info$participants$assists[[player_idx]]
  win <- match_list[[i]]$info$participants$win[[player_idx]]
  total_damage_dealt_to_champions <- match_list[[i]]$info$participants$totalDamageDealtToChampions[[player_idx]]
  total_damage_taken <- match_list[[i]]$info$participants$totalDamageTaken[[player_idx]]
  gold_earned <- match_list[[i]]$info$participants$goldEarned[[player_idx]]
  champion_name <- match_list[[i]]$info$participants$championName[[player_idx]]
  champ_level <- match_list[[i]]$info$participants$champLevel[[player_idx]]
  vision_score <- match_list[[i]]$info$participants$visionScore[[player_idx]]
  nexus_kills <- match_list[[i]]$info$participants$nexusKills[[player_idx]]
  turret_kills <- match_list[[i]]$info$participants$turretKills[[player_idx]]
  inhibitor_kills <- match_list[[i]]$info$participants$inhibitorKills[[player_idx]]
  items_created <- c(match_list[[i]]$info$participants$item0[[player_idx]],
                  match_list[[i]]$info$participants$item1[[player_idx]],
                  match_list[[i]]$info$participants$item2[[player_idx]],
                  match_list[[i]]$info$participants$item3[[player_idx]],
                  match_list[[i]]$info$participants$item4[[player_idx]],
                  match_list[[i]]$info$participants$item5[[player_idx]],
                  match_list[[i]]$info$participants$item6[[player_idx]])
   if_mythic_item <- items_created[items_created %in% mythic_items_id$item_id]
   mythic_item <- ifelse(length(if_mythic_item)==0,NA,if_mythic_item)
   position <- match_list[[i]]$info$participants$individualPosition[[player_idx]]
   kill_participation <-  match_list[[i]]$info$participants$challenges$killParticipation[[player_idx]]
   total_minions_killed <- match_list[[i]]$info$participants$totalMinionsKilled[[player_idx]]
   game_length <-  match_list[[i]]$info$participants$challenges$gameLength[[player_idx]]

   if(!is.null(kill_participation)){
     
     new_record <- data.frame(
       player_id = puuid,
       match_id = match_id,
       match_start_time = match_start_time,
       kills = kills,
       deaths = deaths,
       assists = assists,
       win = win,
       total_damage_dealt_to_champions = total_damage_dealt_to_champions,
       total_damage_taken = total_damage_taken,
       gold_earned = gold_earned,
       champ_level = champ_level,
       champion_name = champion_name,
       vision_score = vision_score,
       nexus_kills = nexus_kills,
       turret_kills = turret_kills,
       inhibitor_kills = inhibitor_kills,
       mythic_item = mythic_item,
       position = position,
       kill_participation = kill_participation,
       total_minions_killed = total_minions_killed,
       game_length = game_length
     )
     
     if (!any(duplicated(rbind(existing_data, new_record)))) {
       existing_data <- rbind(existing_data, new_record)
     }
   }
}

write.csv(existing_data, "./db/playerMatchStats.csv", row.names = FALSE)

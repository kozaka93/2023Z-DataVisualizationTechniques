# Functions for getting data from Riot Games API

library(httr)
library(jsonlite)
library(dplyr)

keys_file <- file("./keys.txt", "r")

puuid <- readLines(keys_file, n = 1)
api_key <- readLines(keys_file, n = 1)

close(keys_file)

riot_api_request <- function(api_url, api_key) {
  response <- GET(api_url, add_headers("X-Riot-Token" = api_key))
  if (http_status(response)$category == "Success") {
    json_content <- content(response, "text", encoding = "UTF-8")
    r_list <- fromJSON(json_content)
    return(r_list)
  } else {
    stop("Error: Unable to retrieve data from the Riot Games API.")
  }
}


get_match_ids <- function(api_key, puuid, n = 100) {
  ids <-
    riot_api_request(
      paste(
        'https://europe.api.riotgames.com/lol/match/v5/matches/by-puuid/',
        puuid,
        '/ids?count=',
        as.character(n),
        sep = ''
      ),
      api_key
    )
  return(ids)
}

get_matches <- function(api_key,
                        ids,
                        min = 1,
                        max = length(ids)) {
  match_list <- list()
  for (i in min:max) {
    match_list[[i - min + 1]] <-
      riot_api_request(
        paste(
          'https://europe.api.riotgames.com/lol/match/v5/matches/',
          ids[[i]],
          sep = ''
        ),
        api_key
      )
    
  }
  return(match_list)
}


get_matches_timelines <- function(api_key,
                                  ids,
                                  min = 1,
                                  max = length(ids)) {
  match_timeline_list <- list()
  for (i in min:max) {
    match_timeline_list[[i - min + 1]] <-
      riot_api_request(
        paste(
          'https://europe.api.riotgames.com/lol/match/v5/matches/',
          ids[[i]],
          '/timeline',
          sep = ''
        ),
        api_key
      )
    
  }
  return(match_timeline_list)
}

filter_matches <- function(match_list, type) {
  # type 1=summoners rift,2=ARAM
  filtered <- list()
  c <- 1
  if (type == 1) {
    for (i in 1:length(match_list)) {
      if (match_list[[i]]$info$mapId == 11) {
        filtered[[c]] <- match_list[[i]]
        c <- c + 1
      }
    }
  } else if (type == 2) {
    for (i in 1:length(match_list)) {
      if (match_list[[i]]$info$mapId %in% c(12, 13)) {
        filtered[[c]] <- match_list[[i]]
        c <- c + 1
      }
    }
  } else{
    print("Podałeś zły typ ziomeczku patrz w opisie funkcji z farcikiem")
  }
  return(filtered)
}

filter_match_timelines <- function(match_timeline_list, filtered_matches) {
    # funkcja filtruje timeline zgodnie z typem gier filtered matches - aram, rift
    filtered_timelines <- list()
    c <- 1
    for (i in 1:length(filtered_matches)) {
      for (j in 1:length(match_timeline_list)) {
        if (filtered_matches[[i]]$metadata$matchId == match_timeline_list[[j]]$metadata$matchId) {
          filtered_timelines[[c]] <- match_timeline_list[[j]]
          c <- c + 1
        }
      }
    }
    return(filtered_timelines)
}
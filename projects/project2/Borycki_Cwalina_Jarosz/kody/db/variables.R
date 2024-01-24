
df_participant_events <- read.csv("./db/participantEvents.csv")
df_matches <- read.csv("./db/matches.csv")
df_participant_frames <- read.csv("./db/participantFrames.csv")
df_player_match_stats <- read.csv("./db/playerMatchStats.csv")

puuid_Jan <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
puuid_Bartek <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
puuid_Mateusz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"

summoner <- data.frame(name = c("Jan","Bartek","Mateusz"),
                       puuid = c(puuid_Jan, puuid_Bartek, puuid_Mateusz))
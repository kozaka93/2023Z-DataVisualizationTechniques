# # For testing purposes only
# library(httr)
# library(ggplot2)
# library(tidyverse)
# library(shiny)
# library(networkD3)
# source("./db/items.R")
# df_player_match_stats <- read.csv("./db/playerMatchStats.csv")
# puuid_Jan <- "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA" 
# puuid_Bartek <- "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA"
# puuid_Mateusz <- "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"


f_items_sankey_graph  <-  function(player){

  if (player == "Jan") {
    player_puuid <- puuid_Jan
  } else if (player == "Bartek") {
    player_puuid <- puuid_Bartek
  } else if (player == "Mateusz") {
    player_puuid <- puuid_Mateusz
  } else {
    stop("Error: Invalid player_puuid.")
  }

  myStats <- df_player_match_stats %>% 
    dplyr::filter(player_puuid == player_id, !is.na(mythic_item)) %>% 
    select(champion_name,mythic_item)

  GroupChampItemTemp <- left_join(myStats,champion_stats %>% select(name,type),join_by('champion_name'=='name'))
  GroupChampItem <- left_join(GroupChampItemTemp,mythic_items_id %>% select(item_id,item_name),join_by('mythic_item'=='item_id'))
  
  champ_type_link <- GroupChampItem %>%
    select(champion_name,type) %>% 
    group_by(champion_name,type) %>% 
    count()

  champ_item_link <-  GroupChampItem %>% 
    select(champion_name,item_name) %>% 
    group_by(champion_name,item_name) %>% 
    count()
  
  links <- data.frame(
    source=c(champ_type_link$type, champ_item_link$champion_name), 
    target=c(champ_type_link$champion_name, champ_item_link$item_name), 
    value=c(champ_type_link$n, champ_item_link$n)
  )
  
  nodesTemp <- left_join(data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  ),champion_stats %>% select(name,type),join_by('name'=='name')) %>%
    mutate(type = ifelse(is.na(type) & name %in% unique(champion_stats$type),
                         name,type)) 
  nodes <- left_join(nodesTemp,mythic_items_id %>% select(item_name,recomanded),join_by('name'=='item_name')) %>%
    mutate(type = ifelse(is.na(type),recomanded,type))
  
  #LEGEND
  links_legend <- data.frame(
    source=c('Class','Champion'), 
    target=c('Champion','Item'), 
    value=c(1,1)
  )
  
  nodes_legend <-  data.frame(
    name=c('Class','Champion','Item'), 
    type=c('Example','Example','Example'),
    recomanded=c(NA,NA,NA)
  )
  
  links <- rbind(links_legend,links)
  nodes <- rbind(nodes_legend,nodes)
  
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1

  #kolejność kolorów ,posortować
  tekst_color <- 'd3.scaleOrdinal() .domain(["Fighter","Mage","Slayer","Tank","Marksman","Specialist", "Controller","Link","Example"]) .range(["#E18417","#7C17E1","#E41D1D","#00BF3B","#004AAD","#03F6FF","#EDCC23","#5b5a56","#5b5a56"])'
  
  links <- links %>% mutate(type = "Link")
  
  # Make the Network
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", 
                     sinksRight=FALSE, NodeGroup = "type",
                     LinkGroup = 'type',units = "times",
                     colourScale = JS(tekst_color))
    return(p)
}



# f_items_sankey_graph("Jan")

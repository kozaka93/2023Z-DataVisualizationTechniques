library(httr)
library(ggplot2)
library(tidyverse)

#ITEMY
url_items <- "https://ddragon.leagueoflegends.com/cdn/13.24.1/data/en_US/item.json?fbclid=IwAR2_R3caRPfpTpAd8-5RpbvDjcGW9CJLIfP5yjsQ2xN_B9uS5ay2HYB2U4Q"
items <- content(GET(url_items), "parsed")

#TYPY CHAMPIONÓW
champion_stats <- read.csv("./db/championsStats.csv", sep = ';')
champion_stats <- champion_stats %>%
  select(champion_name, class, playstyle) %>%
  rename(name = champion_name, type = class, playstyle = playstyle) %>%
  mutate(
    name = case_when(
      name == "S<e9>raphine" ~ "Seraphine",
      name == "Wukong" ~ "MonkeyKing",
      TRUE ~ name
    ),
    type = case_when(
      type == "Mage Artillery" ~ "Mage",
      type == "Controllerer" ~ "Controller",
      TRUE ~ type
    )
  ) %>% mutate(name = str_replace(name,' ',''))

new_champions <- data.frame(
  name = c('Nilah', 'Akshan', 'Vex', 'Zeri', 'RenataGlasc', 'Bel\'Veth', 'K\'Sante', 'Milio', 'Naafiri', 'Briar', 'Hwei'),
  type = c('Slayer', 'Slayer', 'Mage', 'Marksman', 'Controller', 'Slayer', 'Tank', 'Controller', 'Slayer', 'Fighter', 'Mage'),
  playstyle = c('Skirmisher', 'Assassin', 'Burst', 'Marksman', 'Enchanter', 'Skirmisher', 'Warden', 'Enchanter', 'Assassin', 'Diver', 'Artillery')
)

champion_stats <- bind_rows(champion_stats, new_champions)

class2Color <- data.frame(class = c("Fighter","Mage","Slayer","Tank","Marksman","Specialist", "Controller"),
                          clolr = c("#E18417","#7C17E1","#E41D1D","#00BF3B","#004AAD","#03F6FF","#EDCC23"))

mythic_items <- c( "Crown of the Shattered Queen",
                  "Divine Sunderer",
                  "Duskblade of Draktharr",
                  "Echoes of Helia",
                  "Eclipse",
                  "Evenshroud",
                  "Everfrost",
                  "Galeforce",
                  "Goredrinker",
                  "Guinsoo's Rageblade",
                  "Heartsteel",
                  "Hextech Rocketbelt",
                  "Iceborn Gauntlet",
                  "Infinity Edge",
                  "Jak'Sho, The Protean",
                  "Liandry's Anguish",
                  "Locket of the Iron Solari",
                  "Luden's Tempest",
                  "Moonstone Renewer",
                  "Navori Quickblades",
                  "Night Harvester",
                  "Radiant Virtue",
                  "Riftmaker",
                  "Rod of Ages",
                  "Shurelya's Battlesong",
                  "Stridebreaker",
                  "Trinity Force",
                  "Youmuu's Ghostblade")

df_mythic_items <- data.frame(
  mythic_item_name = mythic_items,
  recomanded = c("Mage",
                 "Fighter",
                 "Slayer",
                 "Controller",
                 "Slayer",
                 "Controller",
                 "Mage",
                 "Marksman",
                 "Fighter",
                 "Marksman",
                 "Tank",
                 "Mage",
                 "Tank",
                 "Marksman",
                 "Tank",
                 "Mage",
                 "Controller",
                 "Mage",
                 "Controller",
                 "Marksman",
                 "Mage",
                 "Controller",
                 "Mage",
                 "Mage",
                 "Controller",
                 "Fighter",
                 "Fighter",
                 "Slayer")
)

#Lista Itemów na NORMALU
items_normal <- data.frame(item_id = numeric(),
                          item_name = character())

for (item in items$data) {
  if (item$maps$'11') {
    items_normal <- rbind(items_normal, data.frame(item_id = as.numeric(gsub('.png',"",item$image$full)),
                                                 item_name = item$name))
  }
}

#LISTA LEGEND I MITHIC NA NORMALU
mythic_items_id <- left_join(items_normal %>% filter(item_name %in% mythic_items) %>% select(item_id,item_name),df_mythic_items,join_by('item_name'=='mythic_item_name'))
#boots <- c(3006,3009,3020,3047,3111,3117,3158)


summoner <- data.frame(name = c("Jan","Bartek","Mateusz"),
                       puuid = c( "zwlLeN31xQwaocZE1bEC_i4Y91Rr6-VDrwrkPCi2G-SX889BGKzpT3IdtxhhdxncCX9cMjTgnoekAA",
                                  "sGIXvsl6UBP_Xsn8GJuJONeVj6H5ScomqSMsNMC6dI-E6A3mRDu1aPZb83rzHw6-_ExYKI_8W2xDTA",
                                  "n_Qfzo6Yhpupwck98rbPTHI23QyxqF17iUwCkgz_6WApNw39aFp5bhbq93pFvLICoBGCviFqQvEQag"))

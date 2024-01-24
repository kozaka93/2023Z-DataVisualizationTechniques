library(dplyr)
library(r2d3)



activGraph <- function(user) {
  
  # if(user == "Hubert") {
  #   daylioH <- read.csv("./data/mood/daylio-export-H2.csv", header = TRUE)
  #   
  #   activCountAH <- table(unlist(strsplit(as.character(daylioH$activities), " \\| ")))
  #   activTableAH <- as.data.frame.table(activCountAH)
  #   colnames(activTableAH) <- c("activity", "frequency")
  #   
  #   activities <- c("ćwiczenie", "spacer", "filmy i tv", "czytanie", "sport", "impreza", "pianino", "relaks",
  #                   "wieczór z drugą połówką", "praca", "zakupy", "sprzątanie", "gotowanie", "pranie", "mycie naczyń",
  #                   "śmieci", "nauka", "zajęcia", "projekt grupowy", "pielęgnacja skóry")
  #   # Translated into english:
  #   activitiesEN <- c("exercise", "walk", "movies", "reading", "sport", "party", "piano", "relaxation",
  #                     "randez-vous", "work", "shopping", "cleaning", "cooking", "laundry", "dishes",
  #                     "taking out trash", "learning", "classes", "group project", "skin care")
  #   
  #   # Change activities into english
  #   activTableAH %>%
  #     filter(activity %in% activities) %>%
  #     mutate(activity = activitiesEN[match(activity, activities)]) -> activitiesTableAH
  #   
  #   r2d3(
  #     data = activitiesTableAH,
  #     script = "./www/mood/activitiesGraph.js",
  #     d3_version = 4
  #   ) -> d3_activ_graphH
  #   
  #   
  #   return(d3_activ_graphH)
  # }
  # else if(user == "Mateusz") {
  #   daylioM <- read.csv("./data/mood/daylio-export-M.csv", header = TRUE)
  #   activCountAM <- table(unlist(strsplit(as.character(daylioM$activities), " \\| ")))
  #   activTableAM <- as.data.frame.table(activCountAM)
  #   colnames(activTableAM) <- c("activity", "frequency")
  #   
  #   activities <- c("ćwiczenie", "spacer", "filmy i tv", "czytanie", "sport", "impreza", "pianino", "relaks",
  #                   "wieczór z drugą połówką", "praca", "zakupy", "sprzątanie", "gotowanie", "pranie", "mycie naczyń",
  #                   "śmieci", "nauka", "zajęcia", "projekt grupowy", "pielęgnacja skóry")
  #   # Translated into english:
  #   activitiesEN <- c("exercise", "walk", "movies", "reading", "sport", "party", "piano", "relaxation",
  #                     "randez-vous", "work", "shopping", "cleaning", "cooking", "laundry", "dishes",
  #                     "taking out trash", "learning", "classes", "group project", "skin care")
  #   
  #   # Change activities into english
  #   activTableAM %>%
  #     filter(activity %in% activities) %>%
  #     mutate(activity = activitiesEN[match(activity, activities)]) -> activitiesTableAM
  #   
  #   r2d3(
  #     data = activitiesTableAM,
  #     script = "./www/mood/activitiesGraph.js",
  #     d3_version = 4
  #   ) -> d3_activ_graphM
  #   
  #   
  #   return(d3_activ_graphM)
  # }
  # else if(user == "Adam") {
  #   daylioA  <- read.csv("./data/mood/daylio-export-A.csv", header = TRUE)
  #   
  #   activCountAA <- table(unlist(strsplit(as.character(daylioA$activities), " \\| ")))
  #   activTableAA <- as.data.frame.table(activCountAA)
  #   colnames(activTableAA) <- c("activity", "frequency")
  #   
  #   activities <- c("ćwiczenie", "spacer", "filmy i tv", "czytanie", "sport", "impreza", "pianino", "relaks",
  #                   "wieczór z drugą połówką", "praca", "zakupy", "sprzątanie", "gotowanie", "pranie", "mycie naczyń",
  #                   "śmieci", "nauka", "zajęcia", "projekt grupowy", "pielęgnacja skóry")
  #   # Translated into english:
  #   activitiesEN <- c("exercise", "walk", "movies", "reading", "sport", "party", "piano", "relaxation",
  #                     "randez-vous", "work", "shopping", "cleaning", "cooking", "laundry", "dishes",
  #                     "taking out trash", "learning", "classes", "group project", "skin care")
  #   
  #   # Change activities into english
  #   activTableAA %>%
  #     filter(activity %in% activities) %>%
  #     mutate(activity = activitiesEN[match(activity, activities)]) -> activitiesTableAA
  #   
  #   r2d3(
  #     data = activitiesTableAA,
  #     script = "./www/mood/activitiesGraph.js",
  #     d3_version = 4
  #   ) -> d3_activ_graphA
  #   
  #   
  #   return(d3_activ_graphA)
  # }
  
  
    daylioH <- read.csv("./data/mood/daylio-export-H2.csv", header = TRUE)

    activCountAH <- table(unlist(strsplit(as.character(daylioH$activities), " \\| ")))
    activTableAH <- as.data.frame.table(activCountAH)
    colnames(activTableAH) <- c("activity", "frequency")

    activities <- c("ćwiczenie", "spacer", "filmy i tv", "czytanie", "sport", "impreza", "pianino", "relaks",
                    "wieczór z drugą połówką", "praca", "zakupy", "sprzątanie", "gotowanie", "pranie", "mycie naczyń",
                    "śmieci", "nauka", "zajęcia", "projekt grupowy", "pielęgnacja skóry")
    # Translated into english:
    activitiesEN <- c("exercise", "walk", "movies", "reading", "sport", "party", "piano", "relaxation",
                      "randez-vous", "work", "shopping", "cleaning", "cooking", "laundry", "dishes",
                      "taking out trash", "learning", "classes", "group project", "skin care")

    # Change activities into english
    activTableAH %>%
      filter(activity %in% activities) %>%
      mutate(activity = activitiesEN[match(activity, activities)]) -> activitiesTableAH

    r2d3(
      data = activitiesTableAH,
      script = "./www/mood/activitiesGraph.js",
      d3_version = 4
    ) -> d3_activ_graphH


    return(d3_activ_graphH)
  
  
  
}
library(dplyr)
library(r2d3)


emotionsGraph <- function(user) {
  
  # if(user == "Hubert") {
  #   daylioH <- read.csv("./data/mood/daylio-export-H2.csv", header = TRUE)
  #   
  #   activCountH <- table(unlist(strsplit(as.character(daylioH$activities), " \\| ")))
  #   activTableH <- as.data.frame.table(activCountH)
  #   colnames(activTableH) <- c("emotion", "frequency")
  #   
  #   emotions <- c("szczęście", "podniecenie", "wdzięczność", "relaks", "zadowolenie", "zmęczenie", "niepewność", "nuda", "niepokój",
  #                 "złość", "stres", "smutek", "desperacja", "ulga", "zainteresowanie", "duma", "spełnienie", "euforia", "obrzydzenie", 
  #                 "zazdrość", "nostalgia")
  #   # Translated into english:
  #   emotionsEN <- c("happiness", "excitement", "gratitude", "relaxation", "satisfaction", "tiredness", "uncertainty", "boredom", "anxiety",
  #                   "anger", "stress", "sadness", "desperation", "relief", "interest", "pride", "fulfillment", "euphoria", "disgust",
  #                   "jealousy", "nostalgia")
  #   
  #   # Change emotions to english
  #   activTableH %>%
  #     filter(emotion %in% emotions) %>%
  #     mutate(emotion = emotionsEN[match(emotion, emotions)]) -> emotionsTableH
  #   
  #   r2d3(
  #     data = emotionsTableH,
  #     script = "./www/mood/emotionsGraph.js",
  #     d3_version = 4
  #   ) -> d3_emotions_graphH
  #   
  #   return(d3_emotions_graphH)
  # }
  # else if(user == "Mateusz") {
  #   
  #   daylioM <- read.csv("./data/mood/daylio-export-M.csv", header = TRUE)
  #   
  #   activCountM <- table(unlist(strsplit(as.character(daylioM$activities), " \\| ")))
  #   activTableM <- as.data.frame.table(activCountM)
  #   colnames(activTableM) <- c("emotion", "frequency")
  #   
  #   emotions <- c("szczęście", "podniecenie", "wdzięczność", "relaks", "zadowolenie", "zmęczenie", "niepewność", "nuda", "niepokój",
  #                 "złość", "stres", "smutek", "desperacja", "ulga", "zainteresowanie", "duma", "spełnienie", "euforia", "obrzydzenie", 
  #                 "zazdrość", "nostalgia")
  #   # Translated into english:
  #   emotionsEN <- c("happiness", "excitement", "gratitude", "relaxation", "satisfaction", "tiredness", "uncertainty", "boredom", "anxiety",
  #                   "anger", "stress", "sadness", "desperation", "relief", "interest", "pride", "fulfillment", "euphoria", "disgust",
  #                   "jealousy", "nostalgia")
  #   
  #   # Change emotions to english
  #   activTableM %>%
  #     filter(emotion %in% emotions) %>%
  #     mutate(emotion = emotionsEN[match(emotion, emotions)]) -> emotionsTableM
  #   
  #   r2d3(
  #     data = emotionsTableM,
  #     script = "./www/mood/emotionsGraph.js",
  #     d3_version = 4
  #   ) -> d3_emotions_graphM
  #   
  #   return(d3_emotions_graphM)
  # }
  # else if(user == "Adam") {
  #   daylioA  <- read.csv("./data/mood/daylio-export-A.csv", header = TRUE)
  #   
  #   activCountA <- table(unlist(strsplit(as.character(daylioA$activities), " \\| ")))
  #   activTableA <- as.data.frame.table(activCountA)
  #   colnames(activTableA) <- c("emotion", "frequency")
  #   
  #   emotions <- c("szczęście", "podniecenie", "wdzięczność", "relaks", "zadowolenie", "zmęczenie", "niepewność", "nuda", "niepokój",
  #                 "złość", "stres", "smutek", "desperacja", "ulga", "zainteresowanie", "duma", "spełnienie", "euforia", "obrzydzenie", 
  #                 "zazdrość", "nostalgia")
  #   # Translated into english:
  #   emotionsEN <- c("happiness", "excitement", "gratitude", "relaxation", "satisfaction", "tiredness", "uncertainty", "boredom", "anxiety",
  #                   "anger", "stress", "sadness", "desperation", "relief", "interest", "pride", "fulfillment", "euphoria", "disgust",
  #                   "jealousy", "nostalgia")
  #   
  #   # Change emotions to english
  #   activTableA %>%
  #     filter(emotion %in% emotions) %>%
  #     mutate(emotion = emotionsEN[match(emotion, emotions)]) -> emotionsTableA
  #   
  #   r2d3(
  #     data = emotionsTableA,
  #     script = "./www/mood/emotionsGraph.js",
  #     d3_version = 4
  #   ) -> d3_emotions_graphA
  #   
  #   return(d3_emotions_graphA)
  # }
  
  
  daylioH <- read.csv("./data/mood/daylio-export-H2.csv", header = TRUE)

    activCountH <- table(unlist(strsplit(as.character(daylioH$activities), " \\| ")))
    activTableH <- as.data.frame.table(activCountH)
    colnames(activTableH) <- c("emotion", "frequency")

    emotions <- c("szczęście", "podniecenie", "wdzięczność", "relaks", "zadowolenie", "zmęczenie", "niepewność", "nuda", "niepokój",
                  "złość", "stres", "smutek", "desperacja", "ulga", "zainteresowanie", "duma", "spełnienie", "euforia", "obrzydzenie",
                  "zazdrość", "nostalgia")
    # Translated into english:
    emotionsEN <- c("happiness", "excitement", "gratitude", "relaxation", "satisfaction", "tiredness", "uncertainty", "boredom", "anxiety",
                    "anger", "stress", "sadness", "desperation", "relief", "interest", "pride", "fulfillment", "euphoria", "disgust",
                    "jealousy", "nostalgia")

    # Change emotions to english
    activTableH %>%
      filter(emotion %in% emotions) %>%
      mutate(emotion = emotionsEN[match(emotion, emotions)]) -> emotionsTableH

    r2d3(
      data = emotionsTableH,
      script = "./www/mood/emotionsGraph.js",
      d3_version = 4
    ) -> d3_emotions_graphH

    return(d3_emotions_graphH)
  
  
  }
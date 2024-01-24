gitStatsUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(width=8,box(
        title="",
        width=NULL,
        status = "success",
        plotOutput(ns("calendar_heatmap")) %>% withSpinner()
      ),
       box(
        title="Most used words in commits",
        width=NULL,
        solidHeader = TRUE,
        status="success",
        sliderInput(
          ns("number_of_most_used_words"),
          label = "Select number of most used words in commits",
          min = 3,
          max = 18,
          value = 6,
          step = 1),
        plotlyOutput(ns("message_lollipop")) %>% withSpinner()
      ), 
      box(
        title="Repositories and their number of commits",
        width=NULL,
        solidHeader = TRUE,
        status="danger",
        sliderInput(
          ns("number_of_repos"),
          label = "Select number of repositories to show",
          min = 3,
          max = 10,
          value = 5,
          step = 1),
        plotlyOutput(ns("repo_barplot")) %>% withSpinner()
      
      )
      ),
      column(width = 4,
             box(
                title="Input",
                width=NULL,
                solidHeader = TRUE,
                status = "warning", 
                selectInput(
                  ns("person_w"), 
                  label = "Choose person",
                  choices = c("Mateusz" = "vecel", "Kuba"="kuba-kapron", "Norbert"="Norbert Frydrysiak")
                )),
             box(
               title="Git Repositories on our Computers Information",
               width=NULL,
               solidHeader = TRUE,
               status = "primary", 
               infoBoxOutput(ns("how_many_repos"), width = NULL) %>% withSpinner(),
               infoBoxOutput(ns("total_commits_person"), width = NULL) %>% withSpinner(),
               #infoBoxOutput(ns("average_commits_per_repo_by_person"), width = NULL) %>% withSpinner(),
               infoBoxOutput(ns("average_total_commits_per_repo"), width = NULL) %>% withSpinner(),
               infoBoxOutput(ns("unique_contributors"), width = NULL) %>% withSpinner(),
               infoBoxOutput(ns("most_popular_day_for_commit"), width = NULL) %>% withSpinner(),
               infoBoxOutput(ns("most_popular_contrybutor"), width = NULL) %>% withSpinner(),
               infoBoxOutput(ns("average_number_of_words_per_commit"), width = NULL) %>% withSpinner(),
               infoBoxOutput(ns("repo_with_the_most_commits"),width = NULL) %>% withSpinner()
             ),
             box(
               title="Git Enthusiasts",
               width=NULL,
               includeMarkdown(
                 "- **Norbert, Kuba, and Mateusz** are currently users of Git and GitHub.
- **Norbert** also uses Bitbucket alongside GitHub.
- They primarily employ Git for coursework at their university, focusing on subjects such as *Object-Oriented Programming in Java* and *Data Visualization Techniques*.
- The trio has developed a strong liking for this version control system, seamlessly integrating it with their enthusiasm for working in the terminal and using Linux.
- They plan to continue leveraging Git in future projects and incorporate it into their professional endeavors.
"
               )
             )
             )
      )
    
  )
  
}
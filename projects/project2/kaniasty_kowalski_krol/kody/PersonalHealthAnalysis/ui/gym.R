# ui/gym.R

library(r2d3)
library(plotly)

gymUI <- fluidRow(
  column(12, 
         h2("Gym performance analysis"))
)

gymUI <- tagList(gymUI, 
                 fluidRow(
                   box(
                     title = "Stats",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 6,
                     class = 'box-calc',
                     fluidRow(
                       box(
                         title = "Total reps",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = FALSE,
                         width = 6,
                         class = 'box-calc-item',
                         verbatimTextOutput("total_reps")
                       ),
                       box(
                         title = "Workouts' duration [h]",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = FALSE,
                         width = 6,
                         verbatimTextOutput("total_duration")
                       ),  
                     ),
                     fluidRow(
                       box(
                         title = "Number of workouts",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = FALSE,
                         width = 6,
                         verbatimTextOutput("number_of_workouts")
                       ),
                       box(
                         title = "Workouts' volume [T]",
                         status = "primary",
                         solidHeader = TRUE,
                         collapsible = FALSE,
                         width = 6,
                         verbatimTextOutput("total_volume")
                       )
                     )
                   ),
                   box(
                     title = "Weight distribution per set for each muscle group",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 6,
                     plotlyOutput("gym_weight_distribution")
                   ),
                 ))

gymUI <- tagList(gymUI,
                 fluidRow(
                   box(
                    title = "Reps count for each muscle group",
                    status = "primary",
                    solidHeader = TRUE,
                    collapsible = FALSE,
                    width = 12,
                    class = "row2 tall-box",
                    fluidRow(
                      column(6,
                             imageOutput("gym_muscles")
                      ),
                      column(6,
                             class = "spider",
                             plotlyOutput("gym_spider")
                      )
                    )
                    )
  )
)

gymUI <- tagList(gymUI,
                 fluidRow(
                   box(
                     title = "Total volume of workouts over weeks",
                     status = "primary",
                     solidHeader = TRUE,
                     collapsible = FALSE,
                     width = 12,
                   column(12, d3Output("stacked_progress")),
                   )
                 ))

gymUI
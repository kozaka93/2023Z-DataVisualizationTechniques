pythonLibsUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fluidRow(
      column(4,
             box(
               title = "Python libraries",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               includeMarkdown("
- **Norbert, Kuba, and Mateusz** extensively use Python across various aspects of their work, including:
  - Algebra
  - Data scraping from the internet
  - Creating plots and charts
  - Working with dataframes
  - Algorithm development
  - Graph visualization

- They encountered Python in courses such as:
  - Data Visualization Techniques
  - Fundamentals of Programming and Data Processing
  - Structured Data Processing

- The trio has a strong affinity for Python, particularly enjoying its usage through Jupyter Notebooks and IDEs like *Data Spell* or *Visual Studio Code*. They express a keen interest in further developing their Python skills, with future plans to explore machine learning using Python's rich array of libraries.")
             )
      ),
      column(4,
             box(
               title="Input",
               status="warning",
               solidHeader = TRUE,
               width = NULL,
               selectInput(ns("person_python"), 
                           "Select the person for the python packages to analyze:",
                           c("Norbert",
                             "Mateusz",
                             "Kuba"),
                           selected = "Norbert"),
               numericInput(ns("ile_python"), "How many common prefixes of packages to show:",  min = 3,
                            max = 12,
                            value = 6,
                            step = 1),
               numericInput(ns("prefixy_python"), "The length of the prefix:",  min = 1,
                            max = 10,
                            value = 3,
                            step = 1)
             ),
             box(
               title = "Python packages prefixes plot",
               status = "success",
               solidHeader = TRUE,
               width = NULL,
               plotlyOutput(ns("plot22"))
             )),
      column(4,
             box(
               title = "How many python packages do we have installed?",
               status = "primary",
               solidHeader = TRUE,
               width = NULL,
               plotlyOutput(ns("plot_how_many_packages"))
             )
      ),
    ),
    fluidRow(
      box(
        title = "Graph of most frequent dependecies",
        width = NULL,
        status = "primary",
        solidHeader = TRUE,
        sliderInput(
          ns("numberOfDependencies"),
          label = "Select number of dependencies",
          min = 1,
          max = 12,
          value = 6,
          step = 1
        ),
        simpleNetworkOutput(ns("dependenciesNetwork")) %>% withSpinner()
      )
    )
)
  
}
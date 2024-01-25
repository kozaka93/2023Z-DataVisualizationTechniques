generate_nutrition_ui <- function() {

  foodUI <- fluidPage(
    titlePanel("Analysis of diet and nutrition"),
  
    sidebarLayout(  

      mainPanel(

        box(
          title = "Daily calorie intake",
          status = "warning",
          width = 12,
          solidHeader = TRUE,
          collapsible = FALSE,
          plotlyOutput("caloriePlot")
        ),
        box(
          title = "Macroelement intake",
          status = "warning",
          width = 12,
          solidHeader = TRUE,
          collapsible = FALSE,
          plotlyOutput("foodPlot")
        )
        ),
      sidebarPanel(
        
        box(
          title = "Are you eating well?",
          status = "warning",
          width = 12,
          solidHeader = TRUE,
          collapsible = FALSE,
          HTML(paste('<span style="font-size: 20px;">As a popular saying goes, <strong>"You are what you eat"</strong>. 
            Indeed, what we eat and how we eat may have greater impact on our wellbeing than most people think
             - which is why we have decided to track our daily meals as well. <br> <br>
            
            <strong>Polar chart on the left presents total energetic value</strong> of meals on each day. 
            Each day is a separate slice, and amount of consumed kcal on each day is proportional to area of slice.
            Do not be alarmed by very low values, as they were most probably caused by incomplete tracking. 
            However, nutrition is not just about counting calories, and so the <strong>second plot shows daily intake of crucial macroelements</strong>:
            carbohydrates, protein, as well as fats and sugar.')),
        )
      )
    )
  )
  
  return(foodUI)
}

aboutUI <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    column(
      width = 12,
      box(
        title = "",
        width = NULL,
        status = "primary",
        includeMarkdown(readLines("README.md"))
      )
    )
    
  )
}
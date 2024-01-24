intro_columns <- function(image_path, title, description, github_link, linkedin_link) {
  column(
    width = 12,
    img(src = image_path, class = "img-circle", align = 'center'),
    h3(title),
    p(description, align = 'center'),
    a(icon("github"), href = github_link, target = "_blank", style = "font-size: 50px;"),
    a(icon("linkedin"), href = linkedin_link, target = "_blank", style = "font-size: 50px;")
  )
}

# Ta funkcja jest konieczna, aby uniknąć printowania "TRUE" z boku kolumn


libraries_used <- function() {
  HTML('<h2>Libraries used:</h2>
                            <div class="col-md-2">
                              <ul class="text-left">
                                <li><strong>shiny</strong></li>
                                <li>grid</li>
                                <li>r2d3</li>
                              </ul>
                            </div>
                      
                            <div class="col-md-2">
                              <ul class="text-left">
                                <li>chron</li>
                                <li>htmltools</li>
                                <li>RColorBrewer</li>
                              </ul>
                            </div>
                      
                            <div class="col-md-2">
                              <ul class="text-left">
                                <li>dbplyr</li>
                                <li>jsonlite</li>
                                <li>readr</li>
                              </ul>
                            </div>
                            
                            <div class="col-md-2">
                              <ul class="text-left">
                                <li>dplyr</li>
                                <li>lubridate</li>
                                <li>shinydashboard</li>
                              </ul>
                            </div>
                            
                            <div class="col-md-2">
                              <ul class="text-left">
                                <li>ggplot2</li>
                                <li>magick</li>
                                <li>shinyjqui</li>
                              </ul>
                            </div>
                            
                            <div class="col-md-2">
                              <ul class="text-left">
                                <li>ggthemr</li>
                                <li>plotly</li>
                                <li>tidyr</li>
                              </ul>
                            </div>')
}

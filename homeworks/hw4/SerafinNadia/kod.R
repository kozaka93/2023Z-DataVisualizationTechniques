# source
# https://www.fastfoodmenuprices.com/how-much-big-mac-costs-states-cities/

# importing library plotly
library(plotly)

# data
data <- data.frame(code = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                            "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                            "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                            "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                            "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY"),
                   state = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", 
                             "Colorado", "Connecticut", "Delaware", "Florida", "Georgia", 
                             "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", 
                             "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", 
                             "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", 
                             "Nebraska", "Nevada", "New Hampshire", "New Jersey", 
                             "New Mexico", "New York", "North Carolina", "North Dakota", 
                             "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "Rhode Island", 
                             "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", 
                             "Vermont", "Virginia", "Washington", "West Virginia", 
                             "Wisconsin", "Wyoming"),
                   big_mac_price = c(4.70, 6.50, 5.59, 5.49, 5.89, 5.69, 6.09, 5.49, 4.29, 
                                     5.15, 5.31, 5.39, 6.09, 4.40, 4.69, 4.49, 4.69, 4.69, 
                                     6.29, 5.19, 7.09, 4.79, 4.80, 5.40, 5.69, 5.19, 4.29, 
                                     4.49, 6.29, 5.49, 4.99, 5.29, 4.19, 5.39, 4.29, 4.69, 
                                     5.00, 4.29, 5.99, 4.89, 4.54, 5.09, 5.36, 4.39, 6.29, 
                                     4.99, 4.49, 4.79, 4.59, 4.19))

# adding descriptions
data$desc <- with(data, paste(state, "<br>"))

# defining boarder lines
l <- list(color = toRGB("white"), 
          width = 2)

# map projection
g <- list(scope = "usa",
          projection = list(type = "albers usa"),
          showlakes = TRUE,
          lakecolor = toRGB("white"))

# initialising graphic plot
fig <- plot_geo(data,
                locationmode = "USA-states")

# adding traces
fig <- fig %>% 
  add_trace(z = ~big_mac_price,
            text = ~desc,
            locations = ~code,
            color = ~big_mac_price,
            colors = "Reds",
            marker = list(line = list(color = "black", width = 1)),
            colorbar = list(title = list(text = "Big Mac Price in USD", 
                                         font = list(family = "Times New Roman")), 
                            tickfont = list(family = "Times New Roman"),
                            len = 0.3,
                            x = 1,
                            y = 0.9,
                            thickness = 15))


# setting layout of the plot
fig <- fig %>% 
  layout(title = list(text = "Big Mac Price by State",
                      font = list(family = "Times New Roman", size = 20, color = "black"),
                      y = 0.9),
         geo = g)

# displaying
fig

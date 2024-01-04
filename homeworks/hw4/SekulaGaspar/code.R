### TWD - HW 4 ###

### libraries 

library(dplyr)
library(plotly)

### data frame

df <- read.csv("C:/my_repos/data_visualization_techniques/homework_4/data.csv")

### modify data frame 

df$OBS_VALUE <- as.numeric(df$OBS_VALUE)

df <- df %>% 
  select("sex", "geo", "OBS_VALUE", "OBS_FLAG") %>% 
  mutate( geo = ifelse(geo == "European Union - 27 countries (from 2020)", "EU", geo),
          OBS_FLAG = case_when(OBS_FLAG == "d" ~ "Different definition (see LFS methodology).",
                               OBS_FLAG == "u" ~ "Low reliability data for women in this country.",
                               TRUE ~ "")) 
df_ <- df

df <- df %>%  arrange(desc(OBS_VALUE)) %>% 
  filter(!is.na(OBS_VALUE) & geo != "EU")

df_ <-  df_ %>%  arrange(desc(OBS_VALUE)) %>% 
  filter(!is.na(OBS_VALUE) & geo == "EU")

df <- rbind(df_, df)

df$geo <- factor(df$geo, levels = rev(unique(df$geo)))


### plot

plot_ly(data = df,
        x = ~geo,
        y = ~OBS_VALUE,
        type = "bar",
        color = ~sex,
        legendgroup = ~sex,
        text = ~OBS_FLAG,
        colors = c("darkred", "navy"))  %>% 
  layout(
    title = "Employed people with an ICT education by sex, 2022",
    xaxis = list(
      title = "country",
      tickangle = 45,  
      categoryorder = "total",  
      categoryarray = df$geo  
    ),
    yaxis = list(title = "percentage"),
    
    updatemenus = updatemenus <- list(
      list(
        buttons = list(
          list(method = "restyle",
               args = list("visible", list(TRUE,TRUE)),
               label = "Both"),
          list(method = "restyle",
               args = list("visible", list(FALSE, TRUE)),
               label = "Males"),
          list(method = "restyle",
               args = list("visible", list(TRUE, FALSE)),
               label = "Females")
          
        ),
        direction = "down",
        showactive = TRUE,
        x = 0.1,
        xanchor = "left",
        y = 1.15,
        yanchor = "top"
      )
    )
  ) 

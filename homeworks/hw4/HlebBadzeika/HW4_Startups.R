unicorns_data <- read.csv("~/Downloads/unicorns till sep 2022.csv")

library(plotly)

unicorns_data$Valuation...B. <- gsub("\\$", "", unicorns_data$Valuation...B.)
unicorns_data$Valuation...B. <- gsub(",", "", unicorns_data$Valuation...B.)
unicorns_data$Valuation...B. <- as.numeric(unicorns_data$Valuation...B.)


high_value_startups <- subset(unicorns_data, Valuation...B. > 10)

industry_valuation <- high_value_startups %>%
  group_by(Industry) %>%
  summarise(TotalValuation = sum(Valuation...B.)) %>%
  arrange(desc(TotalValuation))


high_value_startups$Industry <- factor(high_value_startups$Industry,
                                       levels = industry_valuation$Industry)

country_valuation <- high_value_startups %>%
  group_by(Country) %>%
  summarise(TotalValuation = sum(Valuation...B.)) %>%
  arrange(TotalValuation)


high_value_startups$Country <- factor(high_value_startups$Country,
                                       levels = country_valuation$Country)



high_value_startups$Jittered_Industry <- jitter(as.numeric(as.factor(high_value_startups$Industry)))
high_value_startups$Jittered_Country <- jitter(as.numeric(as.factor(high_value_startups$Country)))



fig <- plot_ly(high_value_startups,
               x = ~Jittered_Industry,
               y = ~Jittered_Country,
               size = ~Valuation...B.,
               color = ~Industry,
               text = ~paste(Company, ":", Valuation...B., "B$",", ",Country,", ",City.,"\nInvestors: ",Investors),
               hoverinfo = "text",
               type = 'scatter',
               mode = 'markers',
               showlegend=FALSE)


fig <- fig %>% layout(title = "Valuation of Startups by Industry and Country",
                      xaxis = list(title = "Industry", tickmode = "array", tickvals = jitter(as.numeric(as.factor(unique(high_value_startups$Industry)))), ticktext = unique(high_value_startups$Industry)),
                      yaxis = list(title = "Country", tickmode = "array", tickvals = jitter(as.numeric(as.factor(unique(high_value_startups$Country)))), ticktext = unique(high_value_startups$Country)))


fig

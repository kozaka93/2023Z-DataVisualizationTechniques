sluparob <- function(df,wyb)
{
  if(wyb=="kaczor")
  {  
    ggplot(df, aes(x = Month, fill = Type)) +
      geom_bar(position = position_dodge()) +
      coord_flip() +
      labs(title = "Filmy i reklamy a kwestia polska", x = "Miesiąc", y = "Liczba obserwacji", fill = "Typ") +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm")
      ) -> p
  }
  else
  {
    ggplot(df, aes(x = WeekDay, fill = Type)) +
      geom_bar(position = position_dodge()) +
      coord_flip() +
      labs(title = "Filmy i reklamy a kwestia polska", x = "Dzień tygodnia", y = "Liczba obserwacji", fill = "Typ") +
      scale_fill_brewer(palette = "Pastel1") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 11),
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14),
        title = element_text(size = 16, face = "bold"),
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(1, 1, 1, 1, "cm")
      ) -> p
  }
  ggplotly(p)
}
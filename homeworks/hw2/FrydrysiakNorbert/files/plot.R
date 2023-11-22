library(ggplot2)

# Read in data

df = data.frame(
  Partia=c("Prawo i Sprawiedliwość", "Koalicja Obywatelska", "Trzecia Droga", "Lewica", "Konfederacja"
           , "Mniejszość Niemiecka")
  , Wynik=c(200, 163, 55, 30, 12,1)
)

ggplot(df, aes(x=reorder(Partia,Wynik, decreasing=TRUE), y=Wynik)) + 
  geom_col()+
  labs(title="Podział mandatów w Sejmie 2023", x="komitet wyborczy", y="mandaty poselskie", 
       subtitle = "Exit Poll na podstawie danych IPSOS")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))+
  geom_text(aes(label=Wynik), vjust=-0.3, size=3.5)+
  scale_y_continuous(expand = expansion(mult = c(0, 0.1)))->plot

plot

ggsave("plot.png", plot, width=10, height=6, units="in", dpi=300)

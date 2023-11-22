library(dplyr)
library(stringr)
library(ggplot2)
library(rnaturalearth)
library(scales)
library(svglite)

# Folder "code" powinien być obecnym katalogiem roboczym.
# Jeśli getwd() nie zwraca ".../Kukla_Matuszyk_Pergala/code",
# to należy użyć: setwd(".../Kukla_Matuszyk_Pergala/code").
# Wtedy skrypt będzie poprawnie czerpał dane względem obecnego katalogu roboczego.
# Ścieżka jest wg konwencji Windowsa. Na Liunksie wystarczy zmienić forward slashe "/"
# na backshlashe "\" w ścieżkach do ramek danych. Dla ułatwienia życia te miejsca 
# będą oznaczane poprzez komentarz: "# TUTAJ JEST ŚCIEŻKA DO RAMKI DANYCH".


# Przygotowanie danych ----------------------------------------------------------
# df <- read.csv("List of Countries by Sugarcane Production.csv")
# world <- ne_countries(scale = "small", returnclass = "sf")

# Podział na kraje
world <- ne_countries(scale = "small", returnclass = "sf")
df <- read.csv("../data/List of Countries by Sugarcane Production.csv") %>% # # TUTAJ JEST ŚCIEŻKA DO RAMKI DANYCH
  mutate(production = as.numeric(str_replace_all(Production..Tons., "\\.", ""))) %>%
  group_by(Country) %>%
  summarise(Srednia_produkcja = mean(production))
df <- left_join(world, df, by = c("sovereignt" = "Country"))


# Rysowanie wykresu -----------------------------------------------------------
ggplot(data = df) + geom_sf(aes(fill = Srednia_produkcja)) + 
  #labs(title = "Mean amount of sugarcane produced per country") + # bez tytułu w wykresie, aby dodać tytuły w Canva o jednolitym stylu czcionki dla wszystkich wykresów
  theme(plot.background = element_rect(fill = "transparent", colour=NA),
        axis.ticks.x=element_blank(),
        axis.text.x=element_blank(),
        panel.background = element_rect(fill = "transparent",
                                        colour = "transparent",
                                        linewidth = 0.5,
                                        linetype = "solid"),
        legend.background = element_rect(fill="transparent",
                                         size=0.5, linetype="solid", 
                                         colour ="transparent"), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.text = element_text(color="white"),
        title = element_text(color="white", 
                            face="bold"),
        legend.key = element_blank())+
  guides(fill = guide_legend(title="Sugarcane production\nin tons per year",
                             reverse = TRUE)) +
  scale_x_continuous(labels = comma) +
  scale_fill_gradient2(labels = comma,
                       low= "#9000ff", high="#ff009d", mid = "#b802f0",
                       midpoint = 360000000,
                       guide="colorbar", na.value="#5f3e70")


# Zapisywanie wykresu ----------------------------------------------------------
# ggsave("C:/Users/Sebastian/Desktop/TWZ_PRO_1/cheropleth_produkcja_cukru/wykres.svg",
#        scale = 2,
#        bg = "transparent")


# Testowanie kolorów / tworzenie legendy ---------------------------------------
# Legenda na mapce ma szarą ramkę wokół kwadratów z kolorami, której nie można się pozbć.
# Ta ramka nie występuje na np. wykresie kolumnowym.
# Stąd pomysł, aby zrobić legendę na wykresie kolumnowym, i ją przekleić w Canva.
sample_data <- data.frame(name = c("600,000,000", "400,000,000" ,"200,000,000", "NA"),
  value = c(5, 4, 3, 2))
sample_data$name <- factor(sample_data$name,
                           levels = c("600,000,000", "400,000,000" ,"200,000,000", "NA"))
ggplot(sample_data, aes(x = name, y = value, fill = name)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#ff009d", "#b802f0", "#9000ff", "#5f3e70")) +
  guides(fill = guide_legend(title="Mean sugarcane production\nin tons per year")) +
  theme(
    plot.background = element_rect(fill = "transparent", colour=NA),
    panel.background = element_rect(fill = "transparent",
                                    colour = "transparent",
                                    linewidth = 0.5,
                                    linetype = "solid"),
    legend.background = element_rect(fill="transparent",
                                     size=0.5, linetype="solid",
                                     colour ="transparent"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title = element_text(color="white"),
    axis.text = element_text(color="white"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white"),
    legend.key = element_blank())

# ggsave("C:/Users/Sebastian/Desktop/TWZ_PRO_1/cheropleth_produkcja_cukru/kolory_z_legenda.svg",
#        bg = "transparent")

# Wczytanie potrzebnych modulow
library(dplyr)
library(ggplot2)
library(ggthemes)                                                                                     # Uzywany do theme_economist()

# Stworzenie ramki danych
df <- data.frame(
  Value = c(39, 42, 43, 44, 44, 44, 45, 46, 46, 47, 47, 48, 49, 49, 49, 49, 50, 51, 50),
  Month_Year = c("Oct 23", "Nov 23", "Dec 23", "Jan 24", "Feb 24", "Mar 24", "Apr 24", "May 24", "Jun 24", "Jul 24", "Aug 24", "Sep 24", "Oct 24", "Nov 24", "Dec 24", "Jan 25", "Feb 25", "Mar 25", "Apr 25")
)                                                                                                     # Wartosci odczytane z oryginalnego wykresu


# Modyfikacja ramki danych
df$Month_Year <- factor(df$Month_Year, levels = unique(df$Month_Year))                                # Aby kolejnosc sie nie zmienila

df$future <- ifelse(as.Date(paste("01", df$Month_Year, sep = " "), 
                            format = "%d %b %y") > as.Date("01 Nov 23", format = "%d %b %y"), "Yes", "No")    # Dodaje kolumne, aby stwierdzic czy sa to dane, czy przewidywania


# Tworzenie wykresu
wykres_poprawiony <- ggplot(data = df) +                                                              # Tworze pusty wykres z danymi z df
  geom_point(aes(x = as.numeric(factor(Month_Year)), y = Value, color = factor(future)), size = 3) +  # Dodaje punkty na wykres
  geom_line(aes(x = as.numeric(factor(Month_Year)), y = Value, ), size = 1) +                         # Dodaje linie, aby lepiej bylo widac zmiany
  scale_x_continuous(breaks = as.numeric(factor(df$Month_Year)), labels = df$Month_Year) +            # Dodaje os x
  scale_y_continuous(limits = c(0, 51)) +                                                             # Dodaje os y, ktora zaczyna sie od 0 a nie od 35 jak w oryginalne POPRAWA
  xlab("Month and Year") +                                                                            # Dodanie opisu osi x - brakowalo go w oryginale POPRAWA
  ylab("Price of milk (cents/kg)")+                                                                   # Dodanie opisu osi y - brakowalo go w oryginale POPRAWA
  guides(color = guide_legend(title = "Prediction")) +                                                # Dodanie legendy
  theme_economist() +                                                                                 # Wedlug mnie jeden z najladniejszych them'ow
  theme(axis.text.y = element_text(size = 12),                                                        # Zwiekszenie rozmiaru elementow osi y
        axis.title.x = element_text(size = 20,                                                        # Zwiekszenie rozmiaru tytulu osi x
                                    margin = margin(t = 0.5, unit = "line")),                                                     # Dodanie marginesu, wedlug mnie lepiej tak wyglada
        axis.title.y =  element_text(size = 20, margin = margin(r = 0.5, unit = "line")),             # Zwiekszenie rozmiaru czcionki tytulu na osi y
        axis.text.x = element_text(angle = 45, hjust = 0, size = 15),                                 # Obrocenie (wedlug mnie lepiej wyglada) i zwiekszenie tekstu na osi x
        plot.margin = margin(b = 0.5, l = 0.5, r = 0.5, t = 0.5, unit = "line")                       # Dodanie marginesu do calego wykresu
  )

# Zapisanie wykresu
ggsave("wykres_poprawiony.png", plot = wykres_poprawiony, width = 12, height = 6, dpi = 800)     # Ustawiam proporcje jak w oryginale, zwiekszylem dpi (wg mnie lepiej wyglada z wieksza), zapisuje
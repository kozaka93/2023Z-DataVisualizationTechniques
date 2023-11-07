library(ggplot2)
library(forcats)
library(stringr)

df <- data.frame(
  odpowiedz = c("Te cenne drukuję i przechowuję", "Nagrywam je na dysk", "Chcę, ale nie mam czasu/nie pamiętam", "Nie obawiam się awarii"),
  frakcja = c(0.417, 0.372, 0.191, 0.020)
)

ggplot(df, aes(x = fct_reorder(odpowiedz, -frakcja), y = frakcja)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = scales::percent(frakcja)), vjust = -0.3, size = 3.5) +
  labs(x = "Odpowiedź", y = "Frakcja") +
  ggtitle("Czy zapobiegasz utracie cennych zdjęć cyfrowych?") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold")) +
  theme(axis.title.x = element_text(size = 12, face = "bold")) +
  theme(axis.title.y = element_text(size = 12, face = "bold"))

#ggsave("czy_zapobiegasz_utracie_cennych_zdjec_cyfrowych.png")



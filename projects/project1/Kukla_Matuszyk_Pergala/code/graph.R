library(tidyr)
library(dplyr)
library(stringr)
library(igraph)
library(ggplot2)
library(svglite)

# Folder "code" powinien być obecnym katalogiem roboczym.
# Jeśli getwd() nie zwraca ".../Kukla_Matuszyk_Pergala/code",
# to należy użyć: setwd(".../Kukla_Matuszyk_Pergala/code").
# Wtedy skrypt będzie poprawnie czerpał dane względem obecnego katalogu roboczego.
# Ścieżka jest wg konwencji Windowsa. Na Liunksie wystarczy zmienić forward slashe "/"
# na backshlashe "\" w ścieżkach do ramek danych. Dla ułatwienia życia te miejsca 
# będą oznaczane poprzez komentarz: "# TUTAJ JEST ŚCIEŻKA DO RAMKI DANYCH".


# Przygotowanie danych ------------------------------------------------------------------
df_przepisy <- read.csv('../data/RAW_recipes.csv')  # ramka danych niedodana z powodu dużego rozmiaru

# wybieram id przepisów i składniki z ramki danych
df_skladniki <- df_przepisy %>%
  select(id, ingredients)

# wybór tych id, gdzie występuje cukier
df_skladniki <- df_skladniki %>%
  filter(str_detect(ingredients, "'sugar'"))

# stworzenie ramki danych z id przepisu zawierajacym cukier i sformatowaną listą składników
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")

# rozdzielenie składników z zachowaniem id przepisu
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')

# ilość składników łączonych z cukrem (top 10)
df_laczenie_z_cukrem <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(14)
# dodaję do siebie: "eggs"+"egg", "flour"+"all-purpose flour", "vanilla"+"vanilla extract"
# bo te produkty należą do tej samej kategorii
# mąka jest też obecna w przepisach pod innymi nazwami niż "flour" i "flour"+"all-purpose flour", 
# ale ta liczba jest względnie mała i można ją pominąć
df_laczenie_z_cukrem <- df_laczenie_z_cukrem %>%
  filter(ingredients %in% c("sugar", "salt", "butter", "baking powder",
                            "water", "milk", "baking soda","cinnamon")) %>%
  add_row(ingredients = "flour", count = sum(df_laczenie_z_cukrem$count[5], df_laczenie_z_cukrem$count[11])) %>%
  add_row(ingredients = "eggs", count = sum(df_laczenie_z_cukrem$count[4], df_laczenie_z_cukrem$count[12])) %>%
  add_row(ingredients = "vanilla", count = sum(df_laczenie_z_cukrem$count[9], df_laczenie_z_cukrem$count[14])) %>%
  arrange(desc(count))
top_10_skladnikow_laczonych_z_cukrem <- df_laczenie_z_cukrem %>%
  select(ingredients)
df_laczenie_z_cukrem <- df_laczenie_z_cukrem %>%
  filter(ingredients != "sugar")

# ilość składników łączonych z solą
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'salt'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_sola <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("eggs", "flour", "butter", "vanilla",
                         "baking powder","water", "milk", "baking soda","cinnamon"))

# ilość składników łączonych z jajkami
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'eggs'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_jajkami <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("flour", "butter", "vanilla",
                            "baking powder","water", "milk", "baking soda","cinnamon"))

# ilość składników łączonych z mąką
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'flour'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_maka <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("butter", "vanilla",
                            "baking powder","water", "milk", "baking soda","cinnamon"))

# ilość składników łączonych z masłem
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'butter'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_maslem <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("vanilla",
                            "baking powder","water", "milk", "baking soda","cinnamon"))

# ilość składników łączonych z wanilią
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'vanilla'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_wanilia <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("baking powder","water", "milk", "baking soda","cinnamon"))

# ilość składników łączonych z proszkiem do pieczenia
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'baking powder'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_proszkiem_do_pieczenia <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("water", "milk", "baking soda","cinnamon"))

# ilość składników łączonych z wodą
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'water'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_woda <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("milk", "baking soda","cinnamon"))

# ilość składników łączonych z mlekiem
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'milk'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_mlekiem <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("baking soda","cinnamon"))

# ilość składników łączonych z sodą oczyszczoną
df_skladniki <- df_przepisy %>%
  select(id, ingredients) %>%
  filter(str_detect(ingredients, "'baking soda'"))
df_skladniki <- bind_cols(df_skladniki$id, as.data.frame(str_replace_all(df_skladniki$ingredients, "\\[|\\]|'", '')))
colnames(df_skladniki) <- c("id", "ingredients")
df_skladniki <- df_skladniki %>% 
  separate_rows(ingredients, sep = ', ')
df_laczenie_z_soda_oczyszczona <- df_skladniki %>%
  group_by(ingredients) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  filter(ingredients %in% c("cinnamon"))


# Tworzenie wykresu --------------------------------------------------------------------
# aby wypisywało się lepiej:  quick-fix
df_laczenie_z_cukrem$ingredients[6] <- "baking\npowder"
df_laczenie_z_sola$ingredients[5] <- "baking\npowder"
df_laczenie_z_jajkami$ingredients [3] <- "baking\npowder"
df_laczenie_z_maka$ingredients[2] <- "baking\npowder"
df_laczenie_z_maslem$ingredients [2] <- "baking\npowder"
df_laczenie_z_wanilia$ingredients[2] <- "baking\npowder"
top_10_skladnikow_laczonych_z_cukrem[7,] <- "baking\npowder"
top_10_skladnikow_laczonych_z_cukrem[10,] <- "baking\nsoda"
df_laczenie_z_cukrem$ingredients[9] <- "baking\nsoda"
df_laczenie_z_sola$ingredients[7] <- "baking\nsoda"
df_laczenie_z_jajkami$ingredients [5] <- "baking\nsoda"
df_laczenie_z_maka$ingredients[4] <- "baking\nsoda"
df_laczenie_z_maslem$ingredients [5] <- "baking\nsoda"
df_laczenie_z_wanilia$ingredients[1] <- "baking\nsoda"
df_laczenie_z_proszkiem_do_pieczenia$ingredients[1] <- "baking\nsoda"
df_laczenie_z_woda$ingredients[3] <- "baking\nsoda"
df_laczenie_z_mlekiem$ingredients[2] <- "baking\nsoda"
# krawędzie
links <- data.frame(
  source = c(rep("sugar", 10), rep("salt", 9), rep("eggs", 8), rep("flour", 7), rep("butter", 6), 
             rep("vanilla", 5), rep("baking\npowder", 4), rep("water", 3), rep("milk", 2), rep("baking\nsoda", 1)),
  target = c(df_laczenie_z_cukrem$ingredients, df_laczenie_z_sola$ingredients,
             df_laczenie_z_jajkami$ingredients, df_laczenie_z_maka$ingredients,
             df_laczenie_z_maslem$ingredients, df_laczenie_z_wanilia$ingredients,
             df_laczenie_z_proszkiem_do_pieczenia$ingredients, df_laczenie_z_woda$ingredients,
             df_laczenie_z_mlekiem$ingredients, df_laczenie_z_soda_oczyszczona$ingredients),
  importance = c(df_laczenie_z_cukrem$count, df_laczenie_z_sola$count,
                 df_laczenie_z_jajkami$count, df_laczenie_z_maka$count,
                 df_laczenie_z_maslem$count, df_laczenie_z_wanilia$count,
                 df_laczenie_z_proszkiem_do_pieczenia$count, df_laczenie_z_woda$count,
                 df_laczenie_z_mlekiem$count, df_laczenie_z_soda_oczyszczona$count)
)
# wierzchołki
nodes <- data.frame(
  name = top_10_skladnikow_laczonych_z_cukrem$ingredients,
  carc = top_10_skladnikow_laczonych_z_cukrem$ingredients
)
# tworzenie obiektu 'network'
network <- graph_from_data_frame(d=links, vertices=nodes, directed=F) 
# ustalanie kolorów:
kolory_wierzcholkow <- c("white","#f7e6f7","#f2d285","#f7f3df",
                         "#fcea9f","#ffea00","#dbdbdb","#36aff5",
                         "#e6f2ff","#ddeded","#854d00")
# kolory_krawedzi <- c(rep("white", 10),rep("#4ddafa", 9),rep("#4ddafa", 8),rep("#4ddafa", 7),
#                      rep("#4ddafa", 6),rep("#4ddafa", 5),rep("#4ddafa", 4),rep("#4ddafa", 3),
#                      rep("#4ddafa", 2),rep("#4ddafa", 1))
kolory_krawedzi <- c(rep("#ff009d", 10),rep("#b802f0", 45))
kolory_ramek_wierzcholkow <- c("#9000ff",rep("#5f3e70", 10))
# kolor tła
par(bg = "black")
# par(bg="transparent") # w paczce igraph nie jest obsługiwana przezroczystość ;(
# rysowanie wykresu
graf <- plot(network, vertex.color=kolory_wierzcholkow, vertex.label.cex=0.38,
     vertex.frame.color=kolory_ramek_wierzcholkow, edge.color=kolory_krawedzi, vertex.label.color="black",
     edge.width=(E(network)$importance)^2/70000000, edge.curved=0.1, #E(network)$importance/3500
     layout=layout.circle,
     # layout=layout.fruchterman.reingold
     )

# dodanie legendy (lepiej jest dodać ją ręcznie w Canva, łatwiej jest ją wtedy prawidłowo umiejscowić,
# podobnie tytuł lepiej jest dodać w Canva, aby był taki sam rozmiar i styl czcionki dla każdego wykresu)
# title("How often are ingredients used together\nwith sugar in recipes?",cex.main=1,col.main="white")
# text(-0.3, 0.9999,"10 ingredients used most often with sugar",col="white", cex=1.2)
# text(-0.9,-0.99,"Line width corresponds with\nnumber of times ingredients\nwere used together.",col="white", cex=0.5)
# legend("bottomleft", legend=top_10_skladnikow_laczonych_z_cukrem$ingredients,
#        col = kolory_wierzcholkow) , bty = "n", pch=20 , pt.cex = 3, cex = 1.5, text.col=kolory_wierzcholkow , horiz = FALSE, inset = c(0.1, 0.1))


# Zapisywanie wykresu (najlepiej jest wyeksportować do .svg z okna "Plots") -----------------------------
# write_graph(graf,
#             "C:/Users/Sebastian/Desktop/TWZ_PRO_1/graf_jak_czesto_cukier_jest_laczony_z_innymi_produktami/wykresik.png",
#             "edgelist")

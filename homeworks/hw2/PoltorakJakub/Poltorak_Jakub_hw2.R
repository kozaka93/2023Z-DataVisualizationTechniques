library(ggplot2)
library(dplyr)
library(forcats)
library(magick)
library(ggtext)

df<-data.frame(miasto=c("Barcin","Gąsawa","Janowiec","Łabiszyn","Rogowo","Żnin","Szubin","Kcynia","województwo","kraj"),
               frekwencja=c(56.47,60.45,53.17,54.76,57.77,56.89,56.83,52.29,56.75,57.54)) 
df %>% mutate(name=fct_reorder(miasto,desc(frekwencja))) ->df

urls<-c("https://upload.wikimedia.org/wikipedia/commons/2/27/POL_Barcin_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/c/cd/POL_gmina_G%C4%85sawa_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/b/b4/POL_gmina_Janowiec_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/6/6c/POL_%C5%81abiszyn_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/e/ed/POL_Rogowo_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/d/d7/POL_%C5%BBnin_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/a/af/POL_Szubin_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/c/cf/POL_Kcynia_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/7/79/POL_wojew%C3%B3dztwo_kujawsko-pomorskie_COA.svg",
         "https://upload.wikimedia.org/wikipedia/commons/c/c9/Herb_Polski.svg")


lapply(urls,function(x) download.file(x,destfile = fs::path_file(x)))
fn<- fs::path_file(urls)

output_paths <- gsub("\\.svg", ".png", fn)

for (i in seq_along(fn)) {
  testimage <- image_read_svg(fn[i],width=1000)
  image_write(testimage, path = output_paths[i], format = "png")
}
df %>% mutate(label=output_paths) ->df

df$label <- paste0("<img src='", df$label, "' width = '15'/>")

df$PołożenieGeograficzne<-c("Ziemia Pałucka","Ziemia Pałucka","Ziemia Pałucka",
                            "Ziemia Pałucka","Ziemia Pałucka","Ziemia Pałucka",
                            "Ziemia Pałucka","Ziemia Pałucka",
                            "woj. Kujawsko-pomorskie","Polska")
df %>% 
  ggplot(aes(x=name,y=frekwencja,fill=PołożenieGeograficzne,label=frekwencja)) + 
  geom_bar(stat="identity",width=0.6) +geom_text(size = 3, position = position_stack(vjust = 0.95))+ 
  geom_richtext(aes(label = label),label.colour = NA, fill = NA, label.padding = unit(c(0, 0.25, -.3, 0.25), "lines"))+
  theme_light()+ scale_fill_manual("Położenie geograficzne",values=c("woj. Kujawsko-pomorskie"="#fb8500","Polska"="#f42b00","Ziemia Pałucka"="#00b2ca"))+ 
  labs(title="Frekwencja w wyborach parlamentarnych 2023 do godziny 17.00 na Ziemii Pałuckiej",y="frekwencja [%]",x="Region") -> plot

plot

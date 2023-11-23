aisles <- read.csv("archive/aisles.csv")
departments <- read.csv("archive/departments.csv")
order_products_prior <- read.csv("archive/order_products__prior.csv")
order_products_train <- read.csv("archive/order_products__train.csv")
orders <- read.csv("archive/orders.csv")
products <- read.csv("archive/products.csv")
zam_num <- order_products_prior %>% 
  left_join(products, by='product_id')
 zam_num <- zam_num %>% select(order_id,aisle_id)

zam_num %>% 
  group_by(order_id) %>% 
  summarise(PSY=(aisle_id==40), KOTY=(aisle_id==41)) %>% 
  ungroup() %>% 
  filter(PSY & !KOTY) -> tak_nie
x <- tak_nie %>% pull(order_id)
zam_num %>% filter(order_id %in% x) -> psiezam
psiezam %>% 
  group_by(order_id) %>% 
  summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
  ungroup() %>% 
  filter(ds | do | sd | sdc| stc) -> psiealko
zam_num %>% 
  group_by(order_id) %>% 
  summarise(PSY=(aisle_id==40), KOTY=(aisle_id==41)) %>% 
  ungroup() %>% 
  filter(!PSY & KOTY) -> nie_tak
x <- nie_tak %>% pull(order_id)
zam_num %>% filter(order_id %in% x) -> kociezam
kociezam %>% 
  group_by(order_id) %>% 
  summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
  ungroup() %>% 
  filter(ds | do | sd | sdc| stc) -> kociealko
zam_num %>% 
  group_by(order_id) %>% 
  summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
  ungroup() %>% 
  filter(ds | do | sd | sdc| stc) -> alko
psy <- length(unique(psiezam %>% pull(order_id)))
psyal <- length(unique(psiealko %>% pull(order_id)))
koty <- length(unique(kociezam %>% pull(order_id)))
kotyal <- length(unique(kociealko %>% pull(order_id)))
ordery <- length(unique(zam_num %>% pull(order_id)))
orderyal <- length(unique(alko %>% pull(order_id)))
ogolli <- as.data.frame(zam_num %>% pull(order_id) %>% table)
ogolli <- ogolli %>% rename(order_id=".")
psyli <- as.data.frame(psiezam %>% pull(order_id) %>% table)
psyli <- psyli %>% rename(order_id=".")
kotyli <- as.data.frame(kociezam %>% pull(order_id) %>% table)
kotyli <- kotyli %>% rename(order_id=".")
mniejszy %>% 
  group_by(order_id) %>% 
  summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
  ungroup() %>% 
  filter(ds | do | sd | sdc| stc) -> alkomniejsordery <- length(unique(zam_num %>% pull(order_id)))
unimniej <- length(unique(mniejszy %>% pull(order_id)))
unialkomniej <- length(unique(alkomniejszy %>% pull(order_id)))
wektor_prawdy <- c()
for (i in 1:15)
{
  coto %>% 
    filter(number_of_products<=i) %>% 
    pull(order_id) -> filtr
  zam_num %>% 
    filter(order_id %in% filtr) -> ponadi
    ponadi %>% 
    group_by(order_id) %>% 
    summarise(ds=(aisle_id==27), do=(aisle_id==28), sd=(aisle_id==62), sdc=(aisle_id==124), stc=(aisle_id==134)) %>% 
    ungroup() %>% 
    filter(ds | do | sd | sdc| stc) %>% 
    pull(order_id) -> alkordery_nieunikalne
    length(unique(alkordery_nieunikalne)) -> alkol
    length(unique(ponadi$order_id)) -> wszystkol
    wektor_prawdy <- c(wektor_prawdy, alkol/wszystkol)
}
ramka_prawdy %>% 
  ggplot(aes(x=licz, y=wektor_prawdy, fill=licz)) + geom_bar(stat='identity') -> plotka
plotka + labs(title='Udział zamówień z alkoholem w zależności od liczby produktów w zamówieniu', subtitle='w liczbie wszystkich zamówień', x= 'liczba produktów w zamówieniu', y='udział') + coord_cartesian(xlim=c(1,15) ,ylim=c(0.023,0.0325)) 
kociatabela <- as.data.frame(table(kociezam %>% pull(order_id))) %>% rename(order_id="Var1")
psiatabela <- as.data.frame(table(psiezam %>% pull(order_id))) %>% rename(order_id="Var1")
tabela <- as.data.frame(table(zam_num %>% pull(order_id))) %>% rename(order_id="Var1")
srkota <- mean(kociatabela %>% pull(Freq))
srpsa <- mean(psiatabela %>% pull(Freq))
sr <- mean(tabela %>% pull(Freq))
wier <- c("psy", "koty", "całość")
war <- c(srkota, srpsa, sr)
war<- round(war,digits=2)
ram <- cbind(wier,war)
ram <- as.data.frame(ram)
ram %>% ggplot(ram, mapping = aes(wier,as.numeric(war), fill=wier))+geom_bar(stat='identity') + scale_y_continuous() + coord_cartesian(ylim=c(8,15)) + ggtitle("Średnia liczba produktów w zamówieniu w zależności od koszyka") + xlab("Koszyk") + ylab("Średnia") 
war <- c(0.063, 0.071, 0.026)
ram %>% ggplot(mapping=aes(x=wier, y=as.numeric(war)
                           ))+geom_bar(stat='identity', fill="gold2", width=0.5, color="white") +scale_y_continuous(labels = scales::percent_format(scale = 100), expand=expansion(mult=c(0,0.05))) +
labs(title="Share of orders containing alcohol in the general number of orders",
                          subtitle="in some weird baskets", x="Basket", y="Share")  + dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
                            theme(
                            panel.background = element_rect(fill='transparent', color=NA),
                            plot.background = element_rect(fill='transparent', color=NA),
                            #legend.background = element_rect(fill='transparent'),
                            #legend.box.background = element_rect(fill='transparent'),
                            #plot.title=element_text(color="red"),
                            #plot.subtitle = element_text(color="red"),
                            panel.grid.minor=element_line(color="#393e46"),
                            panel.grid.major=element_line(color="#393e46"),
                            axis.text = element_text(size = 12), 
                            axis.title = element_text(size = 16),
                            plot.title = element_text(family = "Fira Sans Condensed", size = 22, margin = margin(10, 0, 30, 0))
                          ) -> czornyj
  
p <- barplot(height=as.numeric(ram$war), names.arg = ram$wier, col="#3abb5f", font.axis=2, col.axis="red", ) 
ramka_prawdy %>% ggplot(aes(x=licz, y=as.numeric(wektor_prawdy)))+geom_line(color="gold2", linewidth=1.5, aes(colour="white"))+
  scale_y_continuous(labels = scales::percent_format(scale = 100), expand=expansion(mult=c(0,0.05)))+
  coord_cartesian(ylim=c(0,0.031))+
  dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) +
  labs(title="Share of orders containing alcohol", subtitle = "by number of products in order", x="Number of products in order", y="Share")+
  theme(
    panel.background = element_rect(fill='transparent', color=NA),
    plot.background = element_rect(fill='transparent', color=NA),
    panel.grid.minor=element_line(color="#393e46"),
    panel.grid.major=element_line(color="#393e46"),
    axis.text = element_text(size = 12), 
    axis.title = element_text(size = 16),
    plot.title = element_text(family = "Fira Sans Condensed", size = 22, margin = margin(10, 0, 30, 0))
  ) -> liniowy

                                                                                                              
ggsave("beztla.png", plot=czornyj, bg="transparent")
ggsave("pozmianachslup.png", plot=czornyj, bg="transparent")
ggsave("zmianlinod0.png", plot=liniowy, bg="transparent")

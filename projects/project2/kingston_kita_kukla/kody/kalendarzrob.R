robkalendarz <- function(df, pocz, kon)
{
  df$Month_num <- case_when(df$Month == 'Jul' ~ '07',
                            df$Month == 'Aug' ~ '08',
                            df$Month == 'Sep' ~ '09',
                            df$Month == 'Oct' ~ '10',
                            df$Month == 'Nov' ~ '11',
                            df$Month == 'Dec' ~ '12')
  df$daycor <- case_when(df$Day == 1 ~ '01',
                         df$Day == 2 ~ '02',
                         df$Day == 3 ~ '03',
                         df$Day == 4 ~ '04',
                         df$Day == 5 ~ '05',
                         df$Day == 6 ~ '06',
                         df$Day == 7 ~ '07',
                         df$Day == 8 ~ '08',
                         df$Day == 9 ~ '09',
                         TRUE ~ as.character(df$Day))
  reference_date <- as.Date("1970-01-01")
  df$data <- paste(as.character(df$Year),df$Month_num, df$daycor, sep="-")
  df$liczba_dni <- as.numeric(difftime(df$data, reference_date, units = "days"))
  df$liczba_dni <-as.integer(floor(df$liczba_dni))
  df %>% filter(between(liczba_dni,pocz,kon)) -> df
  
  df$liczba_dni_mniej <- df$liczba_dni-pocz+1
  
  df %>% filter(Type=="Video") %>% group_by(liczba_dni_mniej) %>% summarise(n=n()) -> df_inn
  df_inn$log <- log(df_inn$liczba_dni_mniej)
  df_inn %>% arrange(liczba_dni_mniej) -> df_inn
  k <- data.frame(1:(kon-pocz+1))
  names(k) <- 'liczba_dni_mniej'
  k <- k %>% left_join(df_inn,by='liczba_dni_mniej')
  k$n <- if_else(is.na(k$n),0,k$n)
  k$log <- if_else(is.na(k$log),0,k$log)
  p  <- as.character(as.Date(pocz, origin="1970-01-01"))
  kk <- as.character(as.Date(kon, origin="1970-01-01"))
  calendR(title="Heatmapa oglądania na kalendarzu nędzny referendarzu",from = p,to = kk,special.days = k$n, special.col = "red",
          low.col = "white",gradient=TRUE,ncol = 3, legend.pos='right',text.size = 12,
          legend.title = "Tytuł")
}


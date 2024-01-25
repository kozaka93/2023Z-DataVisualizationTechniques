zawez_kalendarzem <- function(df, pocz, kon)
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
  
}
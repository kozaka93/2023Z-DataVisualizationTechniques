  df_milosz = read.csv('./CSV-ki/watch_history/milosz_history_weekday.csv')
  df_michal = read.csv('./CSV-ki/watch_history/michal_history_weekday.csv')
  df_antek = read.csv('./CSV-ki/watch_history/antek_history_weekday.csv')
  months_ordered <- c("Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  df_milosz$Month <- factor(df_milosz$Month, levels = months_ordered)
  df_michal$Month <- factor(df_michal$Month, levels = months_ordered)
  df_antek$Month <- factor(df_antek$Month, levels = months_ordered)
  df_milosz$FullHour = df_milosz$Hour + df_milosz$Minute / 60
  df_michal$FullHour = df_michal$Hour + df_michal$Minute / 60
  df_antek$FullHour = df_antek$Hour + df_antek$Minute / 60
  assign("df_milosz", df_milosz, envir = .GlobalEnv)
  assign("df_michal", df_michal, envir = .GlobalEnv)
  assign("df_antek", df_antek, envir = .GlobalEnv)

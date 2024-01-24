df1 <- df_words1_mil
df2 <- df_milosz
output$wordcloud_plot <- renderPlot(generate_wordcloud(df1$word, df1$n, 25))
output$plotlajek <- renderPlotly(plotlajek(df1))
output$plot2 <- renderPlotly(boxarob(df2,"kaczor"))
output$kalendarz <- renderPlot(kalendarzrob(df2))
output$plot4 <- renderPlotly(sluparob(df2, "kaczor"))
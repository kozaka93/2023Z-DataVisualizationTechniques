options(scipen = 999)
# setwd("~/Documents/informatyczne/iadstudia/twd/linux_me_project")
systemPackagesServer <- function(input, output, session) {
  init <- function(){
    lines <- c(paste("mat", str_squish(readLines("data/system-packages/mateusz_system_packages.txt"))),
               paste("normacos", str_squish(readLines("data/system-packages/norbert_system_packages_macos.txt"))),
               paste("norlinux", str_squish(readLines("data/system-packages/norbert_system_packages.txt"))),
               paste("kuba", str_squish(readLines("data/system-packages/kuba_system_packages.txt"))))
    lines = as.data.frame(str_split_fixed(lines, " ", n=2))
    colnames(lines) <- c("user", "packages")
    df = lines %>% 
      mutate(packages = str_extract(packages, '^[^\\/\t ]+')) %>% 
      mutate(user=as.factor(user))
    df
  }
  
  df <- init()
  
  update_df <- reactive({
    
    df %>%
      filter(user %in% input$kogo_komendy)}
  )
  
  plot_first_update_df <- reactive({
    update_df() %>% 
      group_by(user) %>%
      summarize(count = n()) }
  )
  first_3_letters_df = reactive({
    df %>% 
      mutate(packages=str_sub(packages,1,input$prefixy)) %>% 
      group_by(user, packages) %>%
      summarize(count = n()) %>% #sortowanie po count
      arrange(desc(count)) %>% 
      filter(user==input$player) %>% arrange(desc(count)) %>% head(input$ile)}
  )
  
  first_plot_df_people=reactive({
    df %>% 
      group_by(user) %>%
      summarize(count = n()) %>% 
      mutate(user=case_when(user=="mat" ~ "Mateusz",
                            user=="normacos" ~ "Norbert Macos",
                            user=="norlinux" ~ "Norbert Linux",
                            user=="kuba" ~ "Kuba"))
  })
  
  output$test=renderText({
    paste(input$kogo_komendy)
  })
  
  output$plot1=renderPlotly({
  #  p=ggplot(plot_first_update_df(), aes(x=user, y=count))+
  #    geom_bar(stat = "identity", fill = "skyblue")+
  #   geom_text(aes(label=count), vjust=-0.3, size=3)+
  #    theme_minimal()+
  #    scale_y_log10()
      #geom_text(aes(label=packages))
    p=plot_ly(first_plot_df_people(),
              x=~user,
              y=~count, 
              type='bar',
              marker = list(color='#00a65a')) %>% layout(yaxis = list(title="Number of Packages",type = "log"),
                                     xaxis=list(title="User"))
    
    #p
    p %>% 
    config( displayModeBar = FALSE)
  
  })
  
  output$plot2=renderPlotly({
    p=ggplot(first_3_letters_df(), aes(x=packages, y=count))+
      geom_bar(stat = "identity", fill = "#00a65a")+
      theme_minimal()+labs(x="Package Prefix", y="Number of packages")
    #p
    ggplotly(p) %>% 
    config(displayModeBar = FALSE)
    
  })
}
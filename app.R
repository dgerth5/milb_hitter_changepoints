library(baseballr)
library(tidyverse)
library(ocp)
library(zoo)
library(readxl)
library(shiny)

fangraphs_id2024 <- read_excel("fangraphs_id2024.xlsx")
just_hitters <- fangraphs_id2024 %>% filter(Hitter == 1)

get_lvls <- function(id){
  levels <- unique(fg_milb_batter_game_logs(id, 2024)$Level)
  gsub("[()]", "", levels) 
}

cp_plot_fn <- function(id, name_for_plot, window, lvl, type){
  
  gl <- fg_milb_batter_game_logs(id, 2024) # only using 2024 for this analysis
  
  if (type == "BA") {
    
    gl_sort <- gl %>%
      filter(Level == paste0("(",lvl,")")) %>%
      arrange(Date) %>%
      mutate(roll_ab = rollsum(AB, window, fill = NA, align = "right"),
             roll_h = rollsum(H, window, fill = NA, align = "right"),
             roll_per = roll_h / roll_ab) %>%
      select(Date, roll_ab, roll_h, roll_per) %>%
      drop_na()
    
  } else if (type == "SLG") {
    
    gl_sort <- gl %>%
      filter(Level == paste0("(",lvl,")")) %>%
      arrange(Date) %>%
      mutate(roll_ab = rollsum(AB, window, fill = NA, align = "right"),
             roll_xbh = rollsum(`2B`, window, fill = NA, align = "right")*2 + rollsum(`3B`, window, fill = NA, align = "right")*3 + rollsum(HR, window, fill = NA, align = "right")*4 + (rollsum(H, window, fill = NA, align = "right") - rollsum(`2B`, window, fill = NA, align = "right") -  rollsum(`3B`, window, fill = NA, align = "right") - rollsum(HR, window, fill = NA, align = "right")),
             roll_per = roll_xbh / roll_ab) %>%
      select(Date, roll_ab, roll_xbh, roll_per) %>%
      drop_na()
    
  } else if (type == "OBP") {
    
    gl_sort <- gl %>%
      filter(Level == paste0("(",lvl,")")) %>%
      arrange(Date) %>%
      mutate(roll_denom = rollsum(AB, window, fill = NA, align = "right") + rollsum(BB, window, fill = NA, align = "right") + rollsum(SF, window, fill = NA, align = "right") + rollsum(HBP, window, fill = NA, align = "right"),
             roll_num = rollsum(BB, window, fill = NA, align = "right") + rollsum(H, window, fill = NA, align = "right") + rollsum(HBP, window, fill = NA, align = "right"), 
             roll_per = roll_num / roll_denom) %>%
      select(Date, roll_denom, roll_num, roll_per) %>%
      drop_na()
    
  } else if (type == "BB") {
    
    gl_sort <- gl %>%
      filter(Level == paste0("(",lvl,")")) %>%
      arrange(Date) %>%
      mutate(roll_pa = rollsum(PA, window, fill = NA, align = "right"),
             roll_bb = rollsum(BB, window, fill = NA, align = "right"),
             roll_per = roll_bb / roll_pa) %>%
      select(Date, roll_pa, roll_bb, roll_per) %>%
      drop_na()
    
  } else if (type == "K") {
    
    gl_sort <- gl %>%
      filter(Level == paste0("(",lvl,")")) %>%
      arrange(Date) %>%
      mutate(roll_pa = rollsum(PA, window, fill = NA, align = "right"),
             roll_k = rollsum(SO, window, fill = NA, align = "right"),
             roll_per = roll_k / roll_pa) %>%
      select(Date, roll_pa, roll_k, roll_per) %>%
      drop_na()
    
  } else if (type == "wRC") {
    
    gl_sort <- gl %>%
      filter(Level == paste0("(",lvl,")")) %>%
      arrange(Date) %>%
      mutate(roll_pa = rollsum(PA, window, fill = NA, align = "right"),
             roll_wRC = rollsum(wRC, window, fill = NA, align = "right"),
             roll_per = roll_wRC / roll_pa) %>%
      select(Date, roll_pa, roll_wRC, roll_per) %>%
      drop_na()
    
  } else {
    
    print ("Input type BA, OBP, SLG, K, BB, wRC.")
  }
  
  cp <- onlineCPD(gl_sort$roll_per)
  
  changepoint_indices <- unlist(cp$changepoint_lists["colmaxes"][[1]])
  changepoint_dates <- as.Date(gl_sort$Date[changepoint_indices])
  
  header <- paste0(" Rolling ", window, " Game ", type, "% with Changepoints")
  header_wrc <- paste0(" Rolling ", window, " Game ", type, " with Changepoints")
  
  y_lab <- paste0(type, "%")
  
  if (type == "wRC"){
    
    ggplot(gl_sort, aes(x = as.Date(Date), y = roll_per)) + 
      geom_line() + 
      geom_vline(xintercept = changepoint_dates[-1], color = "red", linetype = "dashed") +
      labs(title = paste0(name_for_plot, header_wrc), x = "Date", y = "wRC") + 
      theme_minimal() + 
      theme(plot.title = element_text(size = 25, face = "bold")) 
    
  } else if (type %in% c("BA","SLG","OBP")){
    
    ggplot(gl_sort, aes(x = as.Date(Date), y = roll_per)) + 
      geom_line() + 
      geom_vline(xintercept = changepoint_dates[-1], color = "red", linetype = "dashed") +
      labs(title = paste0(name_for_plot, header_wrc), x = "Date", y = type) + 
      scale_y_continuous(labels = scales::label_number(accuracy = 0.001)) +
      theme_minimal() + 
      theme(plot.title = element_text(size = 25, face = "bold"))
    
  } else {
    
    ggplot(gl_sort, aes(x = as.Date(Date), y = roll_per)) + 
      geom_line() + 
      geom_vline(xintercept = changepoint_dates[-1], color = "red", linetype = "dashed") +
      scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
      labs(title = paste0(name_for_plot, header), x = "Date", y = y_lab) + 
      theme_minimal() +
      theme(plot.title = element_text(size = 25, face = "bold")) 
  }
  
  
}



ui <- fluidPage(
  titlePanel("2024 MiLB Hitter Changepoints"),
  
  fluidRow(
    column(3, selectInput("player_name", "Choose Player:", choices = sort(unique(just_hitters$Name)))),
    column(3, uiOutput("level_ui")),
    column(2, selectInput("type", "Choose Stat Type:", choices = c("BA", "OBP", "SLG", "BB", "K", "wRC"))),
    column(2, numericInput("window", "Window Size:", value = 15, min = 1))
  ),
  
  hr(), # horizontal divider
  
  fluidRow(
    column(12, plotOutput("cp_plot"))
  )
)

server <- function(input, output, session) {
  player_id <- reactive({
    just_hitters %>% filter(Name == input$player_name) %>% select(PlayerId)
  })
  
  output$level_ui <- renderUI({
    levels <- get_lvls(player_id())
    selectInput("level", "Choose Level:", choices = levels)
  })
  
  output$cp_plot <- renderPlot({
    req(input$player_name, input$level, input$type, input$window)
    id <- player_id()
    name_for_plot <- input$player_name
    window <- input$window
    lvl <- input$level
    type <- input$type
    cp_plot_fn(id, name_for_plot, window, lvl, type)
  })
}


shinyApp(ui = ui, server = server)

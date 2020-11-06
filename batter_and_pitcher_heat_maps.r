library(shiny)
library(tidyverse)
library(mgcv)
library(readr)
library(lubridate)

# -----UI Portion------

#all_data_17_to_20 <- read_csv("all_data_17_to_20.csv")

all_data_17_to_20 <- read_csv("all_data_2020.csv")


all_data_17_to_20%>%
  filter(game_year == 2020)->filtered_data

#all_data_2020%>%
#  filter(year(game_date) == 2020)->all_data_2020

filtered_data$player_name

filtered_data%>%
  arrange(player_name)->filtered_data

# all_data_2020%>%
#   select(player_name, home_team, inning_topbot)%>%
#   filter(inning_topbot == "Bot")->home_teams
# 
# all_data_2020%>%
#   select(player_name, away_team, inning_topbot)%>%
#   filter(inning_topbot == "Top")->away_teams

type <- "Hit Probability Heat Map"


# -----Constructing the strikezone------

topKzone <- 3.5
botKzone <- 1.6
inKzone <- -0.95
outKzone <- 0.95
kZone <- data.frame(
  x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
  y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
)

# -----UI Portion------

ui <-  fluidPage(    
  
  # Give the page a title
  titlePanel("Heat Maps"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("player", "Select a player:", 
                  choices = unique(filtered_data$player_name)),
      hr(),
      selectInput("player2", "Select a player:", 
                  choices = unique(filtered_data$player_name)),
      hr(),
      helpText("Data from 2020 Statcast from baseballr package")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("myPlot"),
      plotOutput("myPlot2"),
    )
  )
)

# -----Server Portion------

server <- (function(input, output) {
  
  output$myPlot <- renderPlot({
    
    #print(input$player)

    filtered_data%>%
      filter(player_name == input$player & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R")%>%
      mutate(Hit = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0))%>%
      select(events, plate_x, plate_z, Hit)%>%
      rename(X = plate_x, Z = plate_z)->heat_map_data
    
    fit <- gam(Hit ~ s(X, Z), family=binomial, data=heat_map_data)
    
    # find predicted probabilities over a 50 x 50 grid
    x <- seq(-1.5, 1.5, length.out=50)
    y <- seq(0.5, 5, length.out=50)
    data.predict <- data.frame(X = c(outer(x, y * 0 + 1)),
                               Z = c(outer(x * 0 + 1, y)))
    lp <- predict(fit, data.predict)
    data.predict$Probability <- exp(lp) / (1 + exp(lp))

    # produce heat map using ggplot
    ggplot(kZone, aes(x, y)) +
      geom_tile(data=data.predict, 
                aes(x=X, y=Z, fill= Probability)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") +
      coord_fixed() +
      ggtitle(paste(input$player, type))

  })
  
  # Fill in the spot we created for a plot
  output$myPlot2 <- renderPlot({
    
    #print(input$player2)
    
    filtered_data%>%
      filter(player_name == input$player2 & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R")%>%
      mutate(Hit = ifelse(events %in% c("single", "double", "triple", "home_run"), 1, 0))%>%
      select(events, plate_x, plate_z, Hit)%>%
      rename(X = plate_x, Z = plate_z)->heat_map_data
    
    fit <- gam(Hit ~ s(X, Z), family=binomial, data=heat_map_data)
    
    # find predicted probabilities over a 50 x 50 grid
    x <- seq(-1.5, 1.5, length.out=50)
    y <- seq(0.5, 5, length.out=50)
    data.predict <- data.frame(X = c(outer(x, y * 0 + 1)),
                               Z = c(outer(x * 0 + 1, y)))
    lp <- predict(fit, data.predict)
    data.predict$Probability <- exp(lp) / (1 + exp(lp))
    
    # produce heat map using ggplot
    ggplot(kZone, aes(x, y)) +
      geom_tile(data=data.predict, 
                aes(x=X, y=Z, fill= Probability)) +
      scale_fill_distiller(palette = "Spectral") +
      geom_path(lwd=1.5, col="black") +
      coord_fixed() +
      ggtitle(paste(input$player2, type))
    
  })
  
})

# -----Combine UI and Server Portion------

shinyApp(ui = ui, server = server)
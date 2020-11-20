library(shiny) # essential to create shiny app
library(baseballr) # to utilize scrape_savant_leaderboards function and others
library(readr) # to utilize read_csv function
library(tidyverse) # essential to use pipe (%>%) and more
library(mgcv) # to utilize gam function

# Reading in relevant data
all_events_data_17_to_20 <- read_csv("relevant_all_events_data_17_to_20.csv")

# Scraping in baseball savant data from 2017 to 2020 to get Pitcher Names
statcast_scraper_2017_pitchers <- scrape_savant_leaderboards(leaderboard = "expected_statistics", year = 2017, min_pa = 1, position = 1, player_type = "pitcher")%>%
  mutate(primary_position = 1)%>%
  mutate(pitcher_name = paste(first_name, last_name, sep = " "))

statcast_scraper_2018_pitchers <- scrape_savant_leaderboards(leaderboard = "expected_statistics", year = 2018, min_pa = 1, position = 1, player_type = "pitcher")%>%
  mutate(primary_position = 1)%>%
  mutate(pitcher_name = paste(first_name, last_name, sep = " "))

statcast_scraper_2019_pitchers <- scrape_savant_leaderboards(leaderboard = "expected_statistics", year = 2019, min_pa = 1, position = 1, player_type = "pitcher")%>%
  mutate(primary_position = 1)%>%
  mutate(pitcher_name = paste(first_name, last_name, sep = " "))

statcast_scraper_2020_pitchers <- scrape_savant_leaderboards(leaderboard = "expected_statistics", year = 2020, min_pa = 1, position = 1, player_type = "pitcher")%>%
  mutate(primary_position = 1)%>%
  mutate(pitcher_name = paste(first_name, last_name, sep = " "))

statcast_all_pitchers <- rbind(statcast_scraper_2017_pitchers, statcast_scraper_2018_pitchers, statcast_scraper_2019_pitchers, statcast_scraper_2020_pitchers)%>%
  rename(game_year = year, pitcher = player_id)%>%
  select(pitcher_name, pitcher, game_year)

# Joining pitcher names to our main data
all_events_data_17_to_20 <- left_join(all_events_data_17_to_20, statcast_all_pitchers, by = c("pitcher", "game_year"))

all_events_data_17_to_20 <- na.omit(all_events_data_17_to_20)

# Filtering past 3 seasons worth of data for heat maps
all_events_data_17_to_20%>%
  filter(game_year == 2020 | game_year == 2019 | game_year == 2018)->all_heat_map_data

all_events_data_17_to_20%>%
  filter(game_year == 2020)%>%
  group_by(player_name, batter)%>%
  tally()%>%
  filter(n >= 40)%>%
  arrange(player_name)->filtered_data_hitters

all_events_data_17_to_20%>%
  filter(game_year == 2020)%>%
  group_by(pitcher_name, pitcher)%>%
  tally()%>%
  filter(n >= 40)%>%
  arrange(pitcher_name)->filtered_data_pitchers

type <- "Hard Hit Probability Heat Map"

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
  
  # Descriptive text
  h4("View is from the catcher's perspective."),
  
  h6("Per MLB, hard hit means BIP with Exit Velo greater than or equal to 95 MPH."),
  
  h6("Credit to Jim Albert for his work on constructing heat maps and for inspiring this project."),
  
  br(),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      # Batter data
      selectInput("batter", "Select a Batter:", 
                  choices = unique(filtered_data_hitters$player_name)),
      
      radioButtons(inputId = "ptype_bat", label = "Choose a Pitch Type",
                   choices = c("All" = "all_pitches_bat", "Fastballs" = "fb_bat", "Non-Fastballs" = "non_fb_bat")),
      hr(),
      
      radioButtons(inputId = "phanded", label = "Choose a Pitcher Handedness",
                   choices = c("All" = "all_pitchers", "R" = "RHP", "L" = "LHP")),
      hr(),
      
      # Pitcher data
      selectInput("pitcher", "Select a Pitcher:", 
                  choices = unique(filtered_data_pitchers$pitcher_name)),
      hr(),
      
      radioButtons(inputId = "ptype_pit", label = "Choose a Pitch Type",
                   choices = c("All" = "all_pitches_pit", "Fastballs" = "fb_pit", "Non-Fastballs" = "non_fb_pit")),
      hr(),
      
      radioButtons(inputId = "bhanded", label = "Chose a Batter Handedness",
                   choices = c("All" = "all_batters", "R" = "RHH", "L" = "LHH")),
      
      helpText("Player Population: Min 40 BIP"),
      helpText("Data Sample: Last 500 BIP")
    ),
    
    mainPanel(
      plotOutput("myPlot"),
      plotOutput("myPlot2")
    )
  )
)

# -----Server Portion------

server <- (function(input, output) {
  
  # Plot for batters
  output$myPlot <- renderPlot({
    
    {
      
      if(input$ptype_bat == "all_pitches_bat" & input$phanded == "all_pitchers"){
        
        all_heat_map_data%>%
          filter(player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R")%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      if(input$ptype_bat == "all_pitches_bat" & input$phanded == "RHP"){
        
        all_heat_map_data%>%
          filter(player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & p_throws == "R")%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      if(input$ptype_bat == "all_pitches_bat" & input$phanded == "LHP"){
        
        all_heat_map_data%>%
          filter(player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & p_throws == "L")%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      if(input$ptype_bat == "fb_bat" & input$phanded == "all_pitchers"){
        
        all_heat_map_data%>%
          filter((player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FF") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SI") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FC"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      if(input$ptype_bat == "fb_bat" & input$phanded == "RHP"){
        
        all_heat_map_data%>%
          filter((player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FF" & p_throws == "R") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SI" & p_throws == "R") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FC" & p_throws == "R"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      if(input$ptype_bat == "fb_bat" & input$phanded == "LHP"){
        
        all_heat_map_data%>%
          filter((player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FF" & p_throws == "L") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SI" & p_throws == "L") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FC" & p_throws == "L"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      if(input$ptype_bat == "non_fb_bat" & input$phanded == "all_pitchers"){
        
        all_heat_map_data%>%
          filter((player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CU") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SL") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CH") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "KC") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FS") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FO") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "EP"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      if(input$ptype_bat == "non_fb_bat" & input$phanded == "RHP"){
        
        all_heat_map_data%>%
          filter((player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CU" & p_throws == "R") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SL" & p_throws == "R") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CH" & p_throws == "R") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "KC" & p_throws == "R") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FS" & p_throws == "R") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FO" & p_throws == "R") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "EP" & p_throws == "R"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      if(input$ptype_bat == "non_fb_bat" & input$phanded == "LHP"){
        
        all_heat_map_data%>%
          filter((player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CU" & p_throws == "L") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SL" & p_throws == "L") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CH" & p_throws == "L") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "KC" & p_throws == "L") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FS" & p_throws == "L") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FO" & p_throws == "L") |
                   (player_name == input$batter & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "EP" & p_throws == "L"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data
      }
      
      # Generalized Additive Model
      # Predicting hard hits based on plate_x and plate_z location of pitch
      
      fit <- gam(hard_hit ~ s(X, Z), family=binomial, data=heat_map_data)
      
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
        ggtitle(paste(input$batter, type))
      
    }
    
  })
  
  # Plot for pitchers
  output$myPlot2 <- renderPlot({
    
    {
      
      if(input$ptype_pit == "all_pitches_pit" & input$bhanded == "all_batters"){
        
        all_heat_map_data%>%
          filter(pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R")%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      if(input$ptype_pit == "all_pitches_pit" & input$bhanded == "RHH"){
        
        all_heat_map_data%>%
          filter(pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & stand == "R")%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      if(input$ptype_pit == "all_pitches_pit" & input$bhanded == "LHH"){
        
        all_heat_map_data%>%
          filter(pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & stand == "L")%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      if(input$ptype_pit == "fb_pit" & input$bhanded == "all_batters"){
        
        all_heat_map_data%>%
          filter((pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FF") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SI") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FC"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      if(input$ptype_pit == "fb_pit" & input$bhanded == "RHH"){
        
        all_heat_map_data%>%
          filter((pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FF" & stand == "R") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SI" & stand == "R") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FC" & stand == "R"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      if(input$ptype_pit == "fb_pit" & input$bhanded == "LHH"){
        
        all_heat_map_data%>%
          filter((pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FF" & stand == "L") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SI" & stand == "L") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FC" & stand == "L"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      if(input$ptype_pit == "non_fb_pit" & input$bhanded == "all_batters"){
        
        all_heat_map_data%>%
          filter((pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CU") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SL") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CH") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "KC") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FS") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FO") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "EP"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      if(input$ptype_pit == "non_fb_pit" & input$bhanded == "RHH"){
        
        all_heat_map_data%>%
          filter((pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CU" & stand == "R") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SL" & stand == "R") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CH" & stand == "R") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "KC" & stand == "R") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FS" & stand == "R") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FO" & stand == "R") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "EP" & stand == "R"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      if(input$ptype_pit == "non_fb_pit" & input$bhanded == "LHH"){
        
        all_heat_map_data%>%
          filter((pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CU" & stand == "L") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "SL" & stand == "L") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "CH" & stand == "L") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "KC" & stand == "L") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FS" & stand == "L") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "FO" & stand == "L") |
                   (pitcher_name == input$pitcher & events != "null" & events != "walk" & events != "hit_by_pitch" & game_type == "R" & pitch_type == "EP" & stand == "L"))%>%
          mutate(hard_hit = ifelse(launch_speed >= 95, 1, 0))%>%
          select(game_date, events, plate_x, plate_z, hard_hit)%>%
          rename(X = plate_x, Z = plate_z)%>%
          arrange(desc(game_date))%>%
          head(500)->heat_map_data_pitchers
      }
      
      # Generalized Additive Model
      # Predicting hard hits based on plate_x and plate_z location of pitch
      
      fit <- gam(hard_hit ~ s(X, Z), family=binomial, data=heat_map_data_pitchers)
      
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
        ggtitle(paste(input$pitcher, type))
      
    }
    
  })
  
})

# -----Combine UI and Server Portion------

shinyApp(ui = ui, server = server)

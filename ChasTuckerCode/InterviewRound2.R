# Charles Tucker
# Interview 

# Install Libraries
library(tidyverse)
library(ggplot2)

# Fix This when needed
setwd("~/PersonalCode/nfl/nflData")

# Load in data For the Regular Season

pbp <- read_csv(file = "play_by_play_data/regular_season/reg_pbp_2019.csv")


team_list = tibble(t_name = character(), total_yards = numeric(), carries = numeric(), yards_per_carry = double())
ny_giants <- filter(pbp, posteam == "NYG")

team_rush <- filter(ny_giants, play_type == "run")

# find team total rush yards
ty <- sum(team_rush$yards_gained)

# find team carries
tc <- as.numeric(count(team_rush)$n)

# find team years per carries
ypc <- as.double(ty) / as.double(tc)

team_list <- add_row(team_list, t_name = "NYG", total_yards = ty, carries = tc, yards_per_carry = ypc)





# Get Teams Rushing Years,Carries, and yards per carry for the 2019 regular season
teams <- unique(pull(pbp, home_team))
team_list = tibble(t_name = character(), total_yards = numeric(), carries = numeric(), yards_per_carry = double())

for (t in teams) {
  team <- filter(pbp, posteam == t)
  team_rush <- filter(team, play_type == "run")
  
  # find team total rush yards
  ty <- sum(team_rush$yards_gained)
  
  # find team carries
  tc <- as.numeric(count(team_rush)$n)
  
  # find team years per carries
  ypc <- as.double(ty) / as.double(tc)
  
  team_list <- add_row(team_list, t_name = t, total_yards = ty, carries = tc, yards_per_carry = ypc)
  
}






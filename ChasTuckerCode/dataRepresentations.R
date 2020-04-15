# Charles Tucker
# Coding Review With Ty Siam

#addition of data manipulation library
library(tidyverse)

# Seting the correct working directory
setwd("~/PersonalCode/nfl/nflData")

# Retreiving player data
p_stats <- read_csv(file = "legacy_data/season_player_stats/season_passing_df.csv")

# Filtering the data to People who were a part of at least 50 Drives that season
p_stats_filtered <- filter(p_stats, Drives> 50)

#Getting unique Individual Names
names <- select(p_stats_filtered, Player_Name)
names <- levels(as.factor(names$Player_Name))


#Initial Variables for loop
i <- 0
par(mfrow=c(3,2))
colors <- c(rgb(.8,0,0), rgb(0,.8,0))

# Looping through all Names in the data set which had over 50 Drives per Season
for (n in names) {
  if (i == 6){
    par(mfrow=c(3,2))
    i <- 0
  }
  player <- filter(p_stats_filtered, Player_Name == n)
  comp_attp_seas <- select(player, Season, Attempts, Completions)
  
  #Extracting and Setting data for graph
  comp_attp<- c(comp_attp_seas$Attempts, comp_attp_seas$Completions)
  m <- matrix(data=comp_attp, ncol = 2)
  m <- t(m)
  
  #Making the graph
  title <- paste(n , "Completions/Attempts Graph")
  barplot(m, main = title , ylab = "Total Count", beside = TRUE, col = colors,
          names.arg = c(comp_attp_seas$Season), ylim = range(0,700))
  
  # Keeping track of graphs on one page.
  i <- i+1
}






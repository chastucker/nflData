#Charles Tucker
# https://overthecap.com/position/running-back/2017/

library(tidyverse)
library(ggplot2)

# Seting the correct working directory
setwd("~/PersonalCode/nfl/nflData")

#reading in the data
ps_rush <- read_csv(file = "legacy_data/season_player_stats/season_rushing_df.csv")
ps_rec <- read_csv(file = "legacy_data/season_player_stats/season_receiving_df.csv")
salary <- read_csv("ChasTuckerCode/PlayerSalaries.csv")

ps_rush_2017 <- filter(ps_rush, Season == 2017)
ps_rec_2017 <- filter(ps_rec, Season == 2017)
options(scipen = 5)



#########################################################################################################################
#Rushing Stats Compared to Cash Spent

# Rushing Yards Per Carry V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rush$Yards_per_Car)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Yards Per Carry" = val, "Name" = names)

# Plotting Yards Per Carry
ypc_plot <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + 
  ggtitle("Yards Per Carry vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Yards Per Carry") + ylab("Cash Spent") +
  xlim(NA, 7) +
  geom_label(
    label=df$Name, 
    nudge_x = .3, nudge_y = 150000, 
  ) 
ypc_plot


# Rushing Carries V Cash Spent
sal <- c()
val <- c()
names <- c

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rush$Carries)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Carries" = val, "Name" = names)

# Plotting Cairries VS Cash Spent
carries_plot <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Carries vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Carries") + ylab("Cash Spent") +
  xlim(NA, 350) +
  geom_label(
    label=df$Name, 
    nudge_x = .3, nudge_y = 150000, 
  ) 
carries_plot

# Rushing Drives V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rush$Drives)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Drives" = val, "Name" = names)

# Plotting Drives V Cash Spent
drives_plot <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Rushing Drives vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Rushing Drives") + ylab("Cash Spent") +
  xlim(NA, 160) +
  geom_label(
    label=df$Name, 
    nudge_x = .3, nudge_y = 150000, 
  ) 
drives_plot

# Rushing Success Rate V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rush$Success_Rate )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Rushing Success_Rate " = val, "Name" = names)

# Plotting Rushing Success Rate V Cash Spent
suc_rate_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Rushing Success Rate vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Rushing Success Rate") + ylab("Cash Spent") +
  xlim(NA, 0.5) +
  geom_label(
    label=df$Name, 
    nudge_x = .025, nudge_y = 150000, 
  ) 
suc_rate_plot


# Rushing TDs
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rush$TDs  )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "TDs  " = val, "Name" = names)

# Plotting Rushing TDs V Cash Spent
td_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Rushing TDs vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Rushing TDs") + ylab("Cash Spent") +
  xlim(NA, 12) +
  geom_label(
    label=df$Name, 
    nudge_x = .5, nudge_y = 150000, 
  ) 
td_plot


# Rushing Win Success Rate V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rush$Win_Success_Rate)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "TDs  " = val, "Name" = names)

# Plotting Rushing Win Success Rate V Cash Spent
ws_rate_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Rushing Win Success Rate vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Rushing Win Success Rate") + ylab("Cash Spent") +
  xlim(NA, .525) +
  geom_label(
    label=df$Name, 
    nudge_x = .02, nudge_y = 150000, 
  ) 
ws_rate_plot


# Rushing Total Clutch V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rush$Total_Clutch_EPA )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Total Clutch Expected Point Average" = val, "Name" = names)

# Plotting Rushing Total Clutch EPA Rate V Cash Spent
t_clutch_rate_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Rushing: Total Clutch Expected Point Average\n vs \nCash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Rushing: Total Clutch Expected Point Average") + ylab("Cash Spent") +
  xlim(NA, 3) +
  geom_label(
    label=df$Name, 
    nudge_x = .08, nudge_y = 150000, 
  ) 
t_clutch_rate_plot


# Rushing Total Win Porbability Average
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rush$Total_WPA)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Rushing_Total_WPA" = val, "Name" = names)

# Plotting Rushing Total Win Probability Average V Cash Spent
wpa_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Rushing Total Win Probability Average From All Carries\n vs \nCash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Total_WPA") + ylab("Cash Spent") +
  xlim(NA, 1) +
  geom_label(
    label=df$Name, 
    nudge_x = .12, nudge_y = 150000, 
  ) 
wpa_plot


###############################################################################################################
# Receiving Stats V Cash Spent


# Receiving Targets V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rec$Targets)
  print(val)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Targets" = val, "Name" = names)

# Plotting Receiving Targets V Cash Spent
target_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() +
  ggtitle("Targets vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Targets") + ylab("Cash Spent") +
  xlim(NA, 125) +
  geom_label(
    label=df$Name, 
    nudge_x = 6, nudge_y = 150000, 
  ) 
target_plot


# Total Receiving Yards V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rec$Total_Yards )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Total Receiving Yards " = val, "Name" = names)

# Plotting Receiving Total Yards V Cash Spent
ty_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Total Receiving Yards vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Total Receiving Yards") + ylab("Cash Spent") +
  xlim(NA, 900) +
  geom_label(
    label=df$Name, 
    nudge_x = 30, nudge_y = 150000, 
  ) 
ty_plot



# Receiving Sucess Rate V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rec$Win_Success_Rate  )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Win_Success_Rate  " = val, "Name" = names)

# Plotting Receiving Receiving Sucess Rate V Cash Spent
ws_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Percentage of Targets with Positive EPA \n(EPA = expected points added from all targets)\n vs \nCash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Win_Success_Rate  ") + ylab("Cash Spent") +
  xlim(NA, .7) +
  geom_label(
    label=df$Name, 
    nudge_x = .03, nudge_y = 150000, 
  ) 
ws_plot


# Receiving Total Clutch V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rec$Total_Clutch_EPA   )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Total_Clutch_EPA   " = val, "Name" = names)

# Plotting Receiving Total Clutch V Cash Spent
clutch_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Total EPA From Targets Weighted By Each Play's WPA\n(WPA= win probability from all targets)\n vs \nCash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Receiving Clutch") + ylab("Cash Spent") +
  xlim(NA, 4.3) +
  geom_label(
    label=df$Name, 
    nudge_x = .3, nudge_y = 150000, 
  ) 
clutch_plot


# Recieving Success Rate V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rec$Success_Rate)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Success_Rate" = val, "Name" = names)

# Plotting Recieving Success Rate V Cash Spent
suc_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Recieving Success Rate vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Recieving Success Rate") + ylab("Cash Spent") +
  xlim(NA, .7) +
  geom_label(
    label=df$Name, 
    nudge_x = .03, nudge_y = 150000, 
  ) 
suc_plot

# Receiving TDs V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  val <- c(val, player_rec$TDs)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "TDs" = val, "Name" = names)

# Plotting Receiving TDs V Cash Spent
td_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Receiving TDs vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("TDs") + ylab("Cash Spent") +
  xlim(NA, 7) +
  geom_label(
    label=df$Name, 
    nudge_x = .4, nudge_y = 150000, 
  ) 
td_plot

# TDs per Target V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  var1 <- filter(ps_rec_2017, Player_Name == n)
  var2 <- filter(salary, Player_Name == n)
  sal <- c(sal, var2$Cash_Spent)
  val <- c(val, var1$TD_per_Targets)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "TD_per_Targets" = val, "Name" = names)

# Plotting TDs per Target V Cash Spent
td_per_targ_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("TD per Targets vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("TD per Targets") + ylab("Cash Spent") +
  xlim(NA, 0.09) +
  geom_label(
    label=df$Name, 
    nudge_x = .0075, nudge_y = 150000, 
  ) 
td_per_targ_plot

#####################################################################################################
#rushing and receiving

#Total Yards V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  num <- player_rec$Total_Yards  + player_rush$Total_Yards
  val <- c(val, num )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Total_Yards" = val, "Name" = names)

# Plotting Total Yards V Cash Spent
tot_yards_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Total Yards vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Total_Yards") + ylab("Cash Spent") +
  xlim(NA, 2200) +
  geom_label(
    label=df$Name, 
    nudge_x = 150, nudge_y = 150000, 
  ) 
tot_yards_plot




# Targets and Carries V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  num <- player_rec$Targets + player_rush$Carries 
  val <- c(val, num )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Targets and Carries" = val, "Name" = names)

# Plotting Targets and Carries V Cash Spent
tar_and_car_yards_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Targets and Carries vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Targets and Carries") + ylab("Cash Spent") +
  xlim(NA, 500) +
  geom_label(
    label=df$Name, 
    nudge_x = 45, nudge_y = 150000, 
  ) 
tar_and_car_yards_plot


# Success Rates V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  num <- ((player_rec$Success_Rate * player_rec$Targets) + (player_rush$Success_Rate * player_rush$Carries))/(player_rec$Targets + player_rush$Carries)  
  val <- c(val, num)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Success_Rate" = val, "Name" = names)

# Plotting Success Rates V Cash Spent
tar_and_car_yards_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Total Success_Rate vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Total Success_Rate ") + ylab("Cash Spent") +
  xlim(NA, .55) +
  geom_label(
    label=df$Name, 
    nudge_x = .025, nudge_y = 150000, 
  ) 
tar_and_car_yards_plot




# Clutch V Cash Spent
sal <- c()
val <- c()
names <- c

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  num <- player_rec$Total_Clutch_EPA  + player_rush$Total_Clutch_EPA  
  val <- c(val, num )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "Total_Clutch_EPA" = val, "Name" = names)

# Plotting Clutch V Cash Spent
tar_and_car_yards_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("Clutch vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("Clutch") + ylab("Cash Spent") +
  xlim(NA, 7) +
  geom_label(
    label=df$Name, 
    nudge_x = .5, nudge_y = 150000, 
  ) 
tar_and_car_yards_plot


# EPA per Carry and EPA per Catch V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  num <- player_rec$EPA_per_Target + player_rush$EPA_per_Car  
  val <- c(val, num )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "EPA_per_Car and EPA_per_Target " = val, "Name" = names)

# Plotting EPA per Carry and EPA per Catch V Cash Spent
EPA_per_target_and_catch_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("EPA per Carry and EPA per Catch\n vs \nCash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("EPA per Carry and EPA per Catch ") + ylab("Cash Spent") +
  xlim(NA, .8) +
  geom_label(
    label=df$Name, 
    nudge_x = .13, nudge_y = 150000, 
  ) 
EPA_per_target_and_catch_plot


# Total TD V Cash Spent
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  num <- player_rec$TDs  + player_rush$TDs   
  val <- c(val, num )
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "TDs" = val, "Name" = names)

# Plotting Total TD V Cash Spent
TDs_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("TDs vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("TDs") + ylab("Cash Spent") +
  xlim(NA, 15) +
  geom_label(
    label=df$Name, 
    nudge_x = 1.6, nudge_y = 150000, 
  ) 
TDs_plot


#TD per target per Carry
sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_rec <- filter(ps_rec_2017, Player_Name == n)
  player_sal <- filter(salary, Player_Name == n)
  player_rush <- filter(ps_rush_2017, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  num <- ((player_rec$TD_per_Targets  * player_rec$Targets) + (player_rush$TD_per_Car * player_rush$Carries))/(player_rec$Targets + player_rush$Carries)  
  val <- c(val, num)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "TD_per_Targets and Carry" = val, "Name" = names)

# Plotting TD per target per Carry
tar_and_car_tds_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("TD per Targets and Carry\n vs \nCash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("TD_per_Targets and Carry ") + ylab("Cash Spent") +
  xlim(NA, .1) +
  geom_label(
    label=df$Name, 
    nudge_x = .010, nudge_y = 150000, 
  ) 
tar_and_car_tds_plot



###############################################################################################################################
# Made Up Metric
my_norm <- function(x, minn, maxx) {
  return ((x-minn)/(maxx-minn))
}

# My metric that I made up
chas_metric <- function(player) {
  rec <- filter(ps_rec_2017, Player_Name == player)
  rush <- filter(ps_rush_2017, Player_Name == player)
  tar <- my_norm(rec$Targets, min(ps_rec_2017$Targets), max(ps_rec_2017$Targets) )
  car <- my_norm(x = rush$Carries, minn = min(ps_rush_2017$Carries), maxx = max(ps_rush_2017$Carries) )
  total_epa <- my_norm(x = rush$Total_EPA  + rec$Total_EPA , 
                       minn = min(ps_rec_2017$Total_EPA ) + min(ps_rush_2017$Total_EPA ), 
                       maxx = max(ps_rec_2017$Total_EPA ) + max(ps_rush_2017$Total_EPA ))
  clutch <- my_norm(x = rush$Total_Clutch_EPA + rec$Total_Clutch_EPA, 
                    minn = min(ps_rec_2017$Total_Clutch_EPA) + min(ps_rush_2017$Total_Clutch_EPA), 
                    maxx = max(ps_rec_2017$Total_Clutch_EPA) + max(ps_rush_2017$Total_Clutch_EPA))
  
  return (.25*tar) + (.25*car) +  (.25*total_epa) + (.25*clutch)
} 


sal <- c()
val <- c()
names <- c()

# Placing Data into Correct Data Frame format
for (n in salary$Player_Name) {
  player_sal <- filter(salary, Player_Name == n)
  sal <- c(sal, player_sal$Cash_Spent)
  num <- chas_metric(n)
  val <- c(val, num)
  names <- c(names, n)
}
M <- matrix(c(val,sal) ,nrow=14, ncol= 2)
df <- data.frame("Cash Spent" =  sal, "My_Factor" = val, "Name" = names)

# Plotting my_Metric V Cash Spent
tar_and_car_tds_plot  <- ggplot(df, aes(x=val, y=sal)) +
  geom_point() + # Show dots
  ggtitle("My_Factor vs Cash Spent") + theme(plot.title = element_text(hjust = 0.5)) +
  theme(plot.margin=unit(c(1,1,1.5,1.2),"cm")) +
  xlab("My_Factor") + ylab("Cash Spent") +
  xlim(NA, .175) +
  geom_label(
    label=df$Name, 
    nudge_x = .015, nudge_y = 150000, 
  ) 
tar_and_car_tds_plot




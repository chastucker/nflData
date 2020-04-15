# Charles Tucker
# Coding Review With Ty Siam

#library for easier data manipulation
library(tidyverse)

# Seting the correct working directory
setwd("~/PersonalCode/nfl/nflData")

#reading in the data
ps_rush <- read_csv(file = "legacy_data/season_player_stats/season_rushing_df.csv")

ps_rush_filt <- filter(ps_rush, Drives>50)

#Top ten guys with the most yards for the season
top_ten_rush <- filter(ps_rush , Player_Name=="A.Peterson" | 
                         Player_Name == "C.Johnson"        |
                         Player_Name == "D.Murray" |
                         Player_Name == "E.Elliott" |
                         Player_Name == "A.Morris" |
                         Player_Name == "L.McCoy" |
                         Player_Name == "A.Foster" |
                         Player_Name == "M.Jones-Drew" |
                         Player_Name == "M.Lynch" |
                         Player_Name == "J.Charles")

# Filtering the seasons of productivity
filtered_top_ten <- filter(top_ten_rush, Drives>50)

#Features
  # yards per carry, carries, all the nfl stuff
  # college yards, carries, yards per carry, games played, 40 dash, bench, weight, height, 



#Collge and Combine Numbers

#A.Peterson College: https://www.sports-reference.com/cfb/players/adrian-peterson-1.html
#A.Peterson Combine: https://nflcombineresults.com/playerpage.php?i=7759

a_peterson_nfl <- filter(filtered_top_ten, Player_Name=="A.Peterson")
names <- c("Name", "40_dash_tot", "40_dash_10", "40_dash_speed", "Verticle", "Shuttle", "FR_Yds_per_Car", "SO_Yds_per_Car", "JR_Yds_per_Car")
val <- c(4.41, 1.54, 18.55, 38.5, 4.40, 1925/339, 1104/221, 1012/188)

top_col_nflComb <- read_csv("ChasTuckerCode/TopTenCollegeNFLCombine.csv")

#C.Johnson College: https://www.sports-reference.com/cfb/players/chris-johnson-1.html
#C.Johnson Combine: http://nflcombineresults.com/playerpage.php?i=7983

#D.Murray College: https://www.sports-reference.com/cfb/players/demarco-murray-1.html
#D.Murray Combine: https://nflcombineresults.com/playerpage.php?f=DeMarco&l=Murray&i=6402

#E.Elliott College: https://www.sports-reference.com/cfb/players/ezekiel-elliott-1.html
#E.Elliott Combine: https://nflcombineresults.com/playerpage.php?f=Ezekiel&l=Elliott&i=21982

#A.Morris College: https://www.sports-reference.com/cfb/players/alfred-morris-1.html
#A.Morris Combine: https://nflcombineresults.com/playerpage.php?f=Alfred&l=Morris&i=9774

#L.McCoy College: https://www.sports-reference.com/cfb/players/lesean-mccoy-1.html
#L.McCoy Combine: https://nflcombineresults.com/playerpage.php?f=LeSean&l=McCoy&i=29256

#A.Foster College: https://www.sports-reference.com/cfb/players/arian-foster-1.html
#A.Foster Combine: https://nflcombineresults.com/playerpage.php?f=Arian&l=Foster&i=8203

#M.Jones-Drew College: https://www.sports-reference.com/cfb/players/maurice-drew-1.html
#M.Jones-Drew Combine: https://nflcombineresults.com/playerpage.php?f=Maurice&l=Jones-Drew&i=29513

#M.Lynch College: https://www.sports-reference.com/cfb/players/marshawn-lynch-1.html
#M.Lynch Combine: https://nflcombineresults.com/playerpage.php?f=Marshawn&l=Lynch&i=7715

#J.Charles College: https://www.sports-reference.com/cfb/players/jamaal-charles-1.html
#J.Charles Combine: https://nflcombineresults.com/playerpage.php?f=Jamaal&l=Charles&i=7896


ps_rush_filtered <- filter(ps_rush, Drives>50)
mean(ps_rush_filtered$Yards_per_Car)

#look at these people
# plot each of them with their stats of college / nfl
# try to find out what makes them great

R.Mathews
R.Hillman
J.Starks
M.Goodson
G.Bernard
S.Ridley
K.Dixon
L.Blount
P.Hillis

# Find good, bad, and average running back based on data base
# find college stats and salaries and compare them
#




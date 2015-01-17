#
# NFL Ranking using the ELO ALgorithm
#
# 1/13/'15
#
print("Clearing Workspace ...")
rm(list = ls(all = TRUE))
#install.packages("dplyr")
require(dplyr)
data <- read.csv("~/Big Data//NFL/years_2014_games_games.csv",as.is=TRUE)
#
data$PtsW <- as.numeric(data$PtsW)
data$PtsL <- as.numeric(data$PtsL)
data$YdsW <- as.numeric(data$YdsW)
data$YdsL <- as.numeric(data$YdsL)
data$TOW <- as.numeric(data$TOW)
data$TOL <- as.numeric(data$TOL)
# Delete heading rows
rows_to_delete <- which(data$Week=='Week')
data1 <- data_frame()
data1 <- data[-rows_to_delete,]
#
rankings <- data_frame()
teams <- data1 %>% distinct(Winner.tie) %>% select(Winner.tie)
rankings <- bind_rows(rankings,teams)
#print(data)
#class(rankings)
#print(rankings)
rankings <- rankings[-33,]
class(data1)
for (i in 2:24) {
  rankings[,i] <- 0
}
colnames(rankings) <- c("Team","Week.0","Week.1","Week.2","Week.3","Week.4","Week.5","Week.6",
                        "Week.7","Week.8","Week.9","Week.10","Week.11","Week.12","Week.13",
                        "Week.14","Week.15","Week.16","Week.17","Week.18","Week.19","Week.20",
                        "Week.21","Week.22")
rankings$Week.0 <- 1500

# Iterate for each week of play
week.no <- 1
k_factor <- 400.0
week.data <- data1 [data1$Week == week.no,]
#apply(week.data,1,elo,week.no)
# Iterate for all games
for (j in 1:17) {
  week.no <- j
  k_factor <- 400.0
  week.data <- data1 [data1$Week == week.no,]
  # Wildcard & Division
  week.no <- 18
  week.data <- data1[258:265,]
  #week.no <- 19
  #week.data <- data1[258:261,]
  for (i in 1:nrow(week.data)) {
    winner <- week.data[i,"Winner.tie"]
    loser <- week.data[i,"Loser.tie"]
    old.rank.w <- rankings[rankings$Team == winner,week.no+1]
    old.rank.w <- old.rank.w[[1]]
    old.rank.l <- rankings[rankings$Team == loser,week.no+1]
    old.rank.l <- old.rank.l[[1]]
    s_ij_w <- (week.data$PtsW[i] + 1.0) / (week.data$PtsW[i] + week.data$PtsL[i] + 2)
    s_ij_l <- (week.data$PtsL[i] + 1.0) / (week.data$PtsW[i] + week.data$PtsL[i] + 2)
    #
    d_ij_w <- old.rank.w - old.rank.l
    d_ij_l <- old.rank.l - old.rank.w
    #
    mu_ij_w <- 1 / (1 + 10 ^ ((-1 * d_ij_w)/400))
    new.rank.w <- round(old.rank.w + (k_factor * (s_ij_w - mu_ij_w)))
    #
    mu_ij_l <- 1 / (1 + 10 ^ ((-1 * d_ij_l)/400))
    new.rank.l <- round(old.rank.l + (k_factor * (s_ij_l - mu_ij_l)))
    #
    print (new.rank.w)
    print(new.rank.l)
    rankings[rankings$Team == winner,week.no+2] <- new.rank.w
    rankings[rankings$Team == loser,week.no+2] <- new.rank.l
  } 
  # if team didn't play, carry forward early ratings
  # not needed for wildcard, division et al
  for (i in 1:nrow(rankings)) {
    if (is.na(rankings[i,week.no+2])) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }
    if (rankings[i,week.no+2] < 1) {
      rankings[i,week.no+2] <- rankings[i,week.no+1]
    }  
  }
}
# week #1 ranking
rankings %>% select(Team,Week.1) %>% arrange(-Week.1)
# week #17 ranking
rankings %>% select(Team,Week.17) %>% arrange(-Week.17)
#
# week #18 ranking
rankings %>% select(Team,Week.18) %>% filter (Week.18 > 0) %>% arrange(-Week.18)
Team Week.18
# 1      Seattle Seahawks    1740
# 2     Green Bay Packers    1661
# 3     Carolina Panthers    1640
# 4    Indianapolis Colts    1629
# 5        Dallas Cowboys    1609
# 6         Detroit Lions    1590
# 7   Pittsburgh Steelers    1517
# 8        Denver Broncos    1504
# 9     Arizona Cardinals    1504
# 10 New England Patriots    1463
# 11   Cincinnati Bengals    1441
# 12     Baltimore Ravens    1434
#
# Pr(A) = 1 / (10^(-ELODIFF/400) + 1) http://fivethirtyeight.com/datalab/introducing-nfl-elo-ratings/

# Superbowl XLIX Week 20
# Packers vs Seahawks
# Colts vs Patriots

Pr_Seahawks <- 1 / (10 ^(-(1740-1661)/400)+1) # 0.61
Pr_Packers <- 1 / (10 ^(-(1661-1740)/400)+1) # 0.39
#
Pr_Colts <- 1 / (10 ^(-(1629-1463)/400)+1) # 0.72
Pr_Patriots <- 1 / (10 ^(-(1463-1629)/400)+1) # 0.28




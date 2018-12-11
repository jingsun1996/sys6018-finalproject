# SYS 6018 - Final Project
# Exploratary Data Analysis
# March Machine Learning Mania 2017
# Jing Sun (js6mj), Runhao Zhao (rz6dg), Luke Kang   (sk5be)

library(ggplot2)
library(plyr)
library(dplyr)
library(caret)
library(rpart.plot)
setwd("~/Desktop/uva/fall18/sys6018/sys6018-finalproject/")

# -------------------------------------------------------+
# Data Preprocessing                                     |
# -------------------------------------------------------+
GameStats = read.csv('RegularSeasonDetailedResults.csv')
TourneyResults = read.csv('TourneyDetailedResults.csv')
team = read.csv("Teams.csv")
Slots = read.csv('TourneySlots.csv')
Seeds = read.csv('TourneySeeds.csv')


## each row in GameStats contains data for both the winning and losing team
## will construct a new dataframe 'FullGame' to extract game stats for each team in each game
# winning teams
Winners = GameStats[,c(1,3:4,9:21)]
colnames(Winners)[2:16] = c("Team","Score","Fgm","Fga","Tpm","Tpa",
                            "Ftm","Fta","OR","DR","Ast",
                            "TO","St","Bl","PF")
Winners$Win = 1  # add a dummy variable to record if the team won this game

# losing team
Losers = GameStats[,c(1,5:6,22:34)]
colnames(Losers)[2:16] = c("Team","Score","Fgm","Fga","Tpm","Tpa",
                           "Ftm","Fta","OR","DR","Ast",
                           "TO","St","Bl","PF")
Losers$Win = 0
FullGame = rbind(Winners,Losers)


## then we construct 'AnnualSummaries' to record the mean game stats
## per team per season using FullGame
AnnualSummaries = FullGame %>% 
  group_by(Season, Team) %>%
  summarise(WinPct = mean(Win), PPG = mean(Score),
            Fgm = mean(Fgm), Fga = mean(Fga),
            Tpm = mean(Tpm), Tpa = mean(Tpa),
            Ftm = mean(Ftm), Fta = mean(Fta),
            OR = mean(OR), DR = mean(DR),
            Ast = mean(Ast), TO = mean(TO),
            St = mean(St), Bl = mean(Bl),
            PF = mean(PF))

# -------------------------------------------------------+
# Exploratory data analysis                              |
# -------------------------------------------------------+
# calculate frequency
rs_loc_freq <- as.data.frame(count(GameStats$Wloc))
# calculate percentage and round to 2sf
rs_loc_freq['percentage'] <- 100 / sum(rs_loc_freq$freq) * rs_loc_freq$freq
rs_loc_freq['percentage_r'] <- signif(rs_loc_freq$percentage, digits = 3)
# calculate center of each segment for label
rs_loc_freq['pos'] <- cumsum(rs_loc_freq$percentage) - rs_loc_freq$percentage/2
# set levels for segments so can order to match labels
rs_loc_freq$x <- factor(rs_loc_freq$x, levels = rev(rs_loc_freq$x))

#create a pie chart see  what percentage of games were won at each location (H:Home, A:Away or N:Neutral)
p <- ggplot(rs_loc_freq, aes(x="", y=percentage_r, fill=x)) +
  geom_bar(stat = "identity") 

pie <- p + coord_polar("y") + 
  ggtitle("Season: Percentage of games won at each location") + 
  scale_fill_brewer("Blues") +
  guides(fill=guide_legend(title='Location')) + 
  geom_text(aes(y = pos, label = percentage_r), size=4)
#display the plot
pie

#plot out the top 20 teams with the highest win percent 
# calculate win frequency
rs_team_Wfreq <- as.data.frame(count(GameStats$Wteam))
rs_team_Wfreq$x <- as.factor(rs_team_Wfreq$x)

# calculate loss frequency
rs_team_Lfreq <- as.data.frame(count(GameStats$Lteam))
rs_team_Lfreq$x <- as.factor(rs_team_Lfreq$x)

# rename to a common name so can be merged with teams
colnames(rs_team_Wfreq)[1] <- "Team_Id"
colnames(rs_team_Lfreq)[1] <- "Team_Id"

# merge data frame with teams so team names are shown as well as Team Id
rs_team_Wfreq <- merge(rs_team_Wfreq, team, by="Team_Id")

win_frequency <- merge(rs_team_Wfreq, rs_team_Lfreq, by="Team_Id")

#rename columns for clarity
colnames(win_frequency)[2] <- "Win"
colnames(win_frequency)[4] <- "Lose"

# calculate win percentage
win_frequency["Percentage"] <- (100 / (win_frequency$Win + win_frequency$Lose)) * win_frequency$Win

# team name as factor
win_frequency$Team_Id <- as.factor(win_frequency$Team_Id)

win_frequency$Team_Id <- factor(win_frequency$Team_Id, levels = win_frequency$Team_Id[order(win_frequency$Percentage)])

#display the plot 
top20 <-win_frequency[order(win_frequency$Percentage,decreasing = TRUE),][1:20,]
ggplot(top20, aes(x=reorder(Team_Name, Percentage), y=Percentage,fill=Team_Name)) +
  ylim(0, 100) +
  ggtitle("Season: Percentage of games won") +
  geom_bar(stat='identity') +
  xlab("Team Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")

# Frequency of overtime periods
rs_ot_freq <- as.data.frame(count(GameStats$Numot))
rs_ot_freq$x <- as.factor(rs_ot_freq$x)
ggplot(rs_ot_freq, aes(x=x, y=freq)) +
  ggtitle("Season: Number of Overtime Periods") +
  geom_bar(stat='identity',fill="orange") +
  xlab("Overtime Periods") + 
  ylab('Frequency') +
  geom_text(aes(label=freq), vjust=-0.3, size=3.5)


#top20 teams with the higest average points scored every game
# extract Wteam and Wscore 

Wscores <- as.data.frame(GameStats$Wteam)
colnames(Wscores)[1] <- "Team_Id"
Wscores['Score'] <- GameStats$Wscore

Lscores <- as.data.frame(GameStats$Lteam)
colnames(Lscores)[1] <- "Team_Id"
Lscores['Score'] <- GameStats$Lscore

# calculate team with highest score
AllScores <- rbind(Wscores, Lscores)

Agg_Scores <- aggregate(Score ~ Team_Id, data = AllScores,  FUN = "mean")

Agg_Scores <- merge(Agg_Scores, team, by = "Team_Id")
Agg_Scores <- Agg_Scores[order(Agg_Scores$Score, decreasing = TRUE),][1:20,]
# team name as factor


# display the plot
ggplot(Agg_Scores, aes(x=reorder(Team_Name,-Score), y=Score, fill=Team_Name)) +
  ylim(0, 100) +
  ggtitle("Season: Average Points Scored Over All Appearences (Win or Lose)") +
  geom_bar(stat='identity') +
  xlab("Team Name") + 
  ylab('Average Points Scored')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")

# Tournament Analysis
### Win percentage for all teams
# calculate win frequency
t_team_Wfreq <- as.data.frame(count(TourneyResults$Wteam))
t_team_Wfreq$x <- as.factor(t_team_Wfreq$x)

# calculate loss frequency
t_team_Lfreq <- as.data.frame(count(TourneyResults$Lteam))
t_team_Lfreq$x <- as.factor(t_team_Lfreq$x)

# rename to a common name so can be merged with teams
colnames(t_team_Wfreq)[1] <- "Team_Id"
colnames(t_team_Lfreq)[1] <- "Team_Id"

# merge data frame with teams so team names are shown as well as Team Id
t_team_Wfreq <- merge(t_team_Wfreq, team, by="Team_Id")

t_win_frequency <- merge(t_team_Wfreq, t_team_Lfreq, by="Team_Id")

#rename columns for clarity
colnames(t_win_frequency)[2] <- "Win"
colnames(t_win_frequency)[4] <- "Lose"

# calculate win percentage
t_win_frequency["Percentage"] <- (100 / (t_win_frequency$Win + t_win_frequency$Lose)) * t_win_frequency$Win

t_win_frequency <- t_win_frequency[order(t_win_frequency$Percentage, decreasing = TRUE),][1:20,]

# plot
ggplot(t_win_frequency, aes(x=reorder(Team_Name,-Percentage), y=Percentage,fill=Team_Name)) +
  ylim(0, 100) +
  ggtitle("Tournament: Percentage of games won") +
  geom_bar(stat='identity') +
  xlab("Team Name") + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")

##top 20 teams with the highest avearge points scored per game
# extract Wteam and Wscore 

tWscores <- as.data.frame(TourneyResults$Wteam)
colnames(tWscores)[1] <- "Team_Id"
tWscores['Score'] <- TourneyResults$Wscore


tLscores <- as.data.frame(TourneyResults$Lteam)
colnames(tLscores)[1] <- "Team_Id"
tLscores['Score'] <- TourneyResults$Lscore


# calculate team with highest score
tAllScores <- rbind(tWscores, tLscores)

tAgg_Scores <- aggregate(Score ~ Team_Id, data = tAllScores,  FUN = "mean")

tAgg_Scores <- merge(tAgg_Scores, team, by = "Team_Id")

tAgg_Scores <- tAgg_Scores[order(tAgg_Scores$Score,decreasing = TRUE),][1:20,]
# plot
ggplot(tAgg_Scores, aes(x=reorder(Team_Name,-Score), y=Score,fill=Team_Name))+
  ylim(0, 100) +
  ggtitle("Tournament: Average Points Scored Over All Appearences (Win or Lose)") +
  geom_bar(stat='identity') +
  xlab("Team Name") + 
  ylab('Average Points Scored') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),legend.position="none")

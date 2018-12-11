# SYS 6018 - Final Project
# March Machine Learning Mania 2017
# Jing Sun (js6mj), Runhao Zhao (rz6dg), Luke Kang   (sk5be)

library(ggplot2)
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
# Train and Test sets                                    |
# -------------------------------------------------------+
## train: 2003 - 2013 data
## test: 2014 - 2016 data

# now we combine TourneyResults with AnnualSummaries to construct train and test sets
# need to extract AnnualSummaries for each team in each tournament games

# here we will construct two dataframes:
# 1. WinningTraining/Test: winning team as Team1, with dummy variable Win==1
# 2. LoseTraining/Test: losing team first, with dummy variable Win==0

# Win will be our response variable

## training
TrainingResults = filter(TourneyResults, Season <= 2013)
WinningTraining = as.data.frame(matrix(0, ncol = 33, nrow = nrow(TrainingResults)))
colnames(WinningTraining)[1:3] = c("Season","Team1","Team2")
colnames(WinningTraining)[4:18] = c("T1WinPct","T1PPG","T1Fgm","T1Fga","T1Tpm",
                                    "T1Tpa","T1Ftm","T1Fta","T1OR","T1DR",
                                    "T1Ast","T1TO","T1St","T1Bl","T1PF")
colnames(WinningTraining)[19:33] = c("T2WinPct","T2PPG","T2Fgm","T2Fga","T2Tpm",
                                     "T2Tpa","T2Ftm","T2Fta","T2OR","T2DR",
                                     "T2Ast","T2TO","T2St","T2Bl","T2PF")
WinningTraining[,1:3] = TrainingResults[,c(1,3,5)]   # fill in Season, Team1(Win), Team2(Lose)
for (i in 1:nrow(TrainingResults)){
  WinningTraining[i,4:18] =
    AnnualSummaries[which(AnnualSummaries$Season==TrainingResults[i,"Season"]
                          &AnnualSummaries$Team ==TrainingResults[i,"Wteam"]),3:17]
  WinningTraining[i,19:33] =
    AnnualSummaries[which(AnnualSummaries$Season==TrainingResults[i,"Season"]
                          &AnnualSummaries$Team ==TrainingResults[i,"Lteam"]),3:17]
}  # for each tournament game, extract each team's annual game stats from AnnualSummaries
WinningTraining$Win = 1  # add the dummy variable Win, 1 means Team 1 won


LoseTraining = as.data.frame(matrix(0, ncol = 33, nrow = nrow(TrainingResults)))
colnames(LoseTraining)[1:3] = c("Season","Team1","Team2")
colnames(LoseTraining)[4:18] = c("T1WinPct","T1PPG","T1Fgm","T1Fga","T1Tpm",
                                 "T1Tpa","T1Ftm","T1Fta","T1OR","T1DR",
                                 "T1Ast","T1TO","T1St","T1Bl","T1PF")
colnames(LoseTraining)[19:33] = c("T2WinPct","T2PPG","T2Fgm","T2Fga","T2Tpm",
                                  "T2Tpa","T2Ftm","T2Fta","T2OR","T2DR",
                                  "T2Ast","T2TO","T2St","T2Bl","T2PF")
LoseTraining[,1:3] = TrainingResults[,c(1,5,3)]     # fill in Season, Team1(Lose), Team2(Win)
for (i in 1:nrow(TrainingResults)){
  LoseTraining[i,4:18] =
    AnnualSummaries[which(AnnualSummaries$Season==TrainingResults[i,"Season"]
                          &AnnualSummaries$Team ==TrainingResults[i,"Lteam"]),3:17]
  LoseTraining[i,19:33] =
    AnnualSummaries[which(AnnualSummaries$Season==TrainingResults[i,"Season"]
                          &AnnualSummaries$Team ==TrainingResults[i,"Wteam"]),3:17]
}   # for each tournament game, extract each team's annual game stats from AnnualSummaries
LoseTraining$Win = 0  # add the dummy variable Win, 0 means Team 2 lost
TrainingData = rbind(WinningTraining,LoseTraining)


## test
# repeat same process as above to create test set
TestResults = filter(TourneyResults, Season > 2013)
WinningTest = as.data.frame(matrix(0, ncol = 33, nrow = nrow(TestResults)))
colnames(WinningTest)[1:3] = c("Season","Team1","Team2")
colnames(WinningTest)[4:18] = c("T1WinPct","T1PPG","T1Fgm","T1Fga","T1Tpm",
                                "T1Tpa","T1Ftm","T1Fta","T1OR","T1DR",
                                "T1Ast","T1TO","T1St","T1Bl","T1PF")
colnames(WinningTest)[19:33] = c("T2WinPct","T2PPG","T2Fgm","T2Fga","T2Tpm",
                                 "T2Tpa","T2Ftm","T2Fta","T2OR","T2DR",
                                 "T2Ast","T2TO","T2St","T2Bl","T2PF")
WinningTest[,1:3] = TestResults[,c(1,3,5)]

for (i in 1:nrow(TestResults)){
  WinningTest[i,4:18] =
    AnnualSummaries[which(AnnualSummaries$Season==TestResults[i,"Season"]
                          &AnnualSummaries$Team ==TestResults[i,"Wteam"]),3:17]
  WinningTest[i,19:33] =
    AnnualSummaries[which(AnnualSummaries$Season==TestResults[i,"Season"]
                          &AnnualSummaries$Team ==TestResults[i,"Lteam"]),3:17]
}
WinningTest$Win = 1

LoseTest = as.data.frame(matrix(0, ncol = 33, nrow = nrow(TestResults)))
colnames(LoseTest)[1:3] = c("Season","Team1","Team2")
colnames(LoseTest)[4:18] = c("T1WinPct","T1PPG","T1Fgm","T1Fga","T1Tpm",
                             "T1Tpa","T1Ftm","T1Fta","T1OR","T1DR",
                             "T1Ast","T1TO","T1St","T1Bl","T1PF")
colnames(LoseTest)[19:33] = c("T2WinPct","T2PPG","T2Fgm","T2Fga","T2Tpm",
                              "T2Tpa","T2Ftm","T2Fta","T2OR","T2DR",
                              "T2Ast","T2TO","T2St","T2Bl","T2PF")
LoseTest[,1:3] = TestResults[,c(1,5,3)]

for (i in 1:nrow(TestResults)){
  LoseTest[i,4:18] =
    AnnualSummaries[which(AnnualSummaries$Season==TestResults[i,"Season"]
                          &AnnualSummaries$Team ==TestResults[i,"Lteam"]),3:17]
  LoseTest[i,19:33] =
    AnnualSummaries[which(AnnualSummaries$Season==TestResults[i,"Season"]
                          &AnnualSummaries$Team ==TestResults[i,"Wteam"]),3:17]
}
LoseTest$Win = 0
TestData = rbind(WinningTest,LoseTest)



# -------------------------------------------------------+
# Logistic Regression                                    |
# -------------------------------------------------------+
## 10-fold repeated cross validation, repeat 3 times
train = subset(TrainingData, select = -c(T1Ftm,T2Ftm))
test = subset(TestData, select = -c(T1Ftm,T2Ftm))

trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
logistic <- train(as.factor(Win) ~ ., data = train, method="glm", trControl=trctrl)
summary(logistic)
## AIC: 1715.7
test_pred = predict(logistic, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
## Accuracy = 0.6965

# remove variables that are not significant
logistic2 <- train(as.factor(Win) ~ .-Team1-Team2-T1Fgm-T1Tpa-T1DR-T1Ast-T1PF-T2Fgm-T2Tpa-T2DR
                   -T2Ast-T2St-T2PF, data = train, method="glm", 
                   trControl=trctrl)
summary(logistic2)
## AIC: 1701.1
test_pred = predict(logistic2, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
## Accuracy : 0.6791

# further remove variables that are not significant in logistic2
logistic3 <- train(as.factor(Win) ~ .-Team1-Team2-T1Fgm-T1Tpa-T1DR-T1Ast-T1PF-T2Fgm-T2Tpa-T2DR
                   -T2Ast-T2St-T2PF-Season, data = train, method="glm", trControl=trctrl)
summary(logistic3)
## AIC: 1699.1
test_pred = predict(logistic3, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
## Accuracy : 0.6791

# will use logistic as our final logistic model
# smallest AIC, highest Accuracy



# -------------------------------------------------------+
# Decision Tree                                          |
# -------------------------------------------------------+
# Train decision tree using information gain criteria:
dtree_fit_information = train(as.factor(Win) ~ ., data = train, method = "rpart",
                              parms = list(split = "information"),
                              trControl=trctrl, tuneLength = 10)
prp(dtree_fit_information$finalModel, box.palette = "Reds", tweak = 1)
test_pred = predict(dtree_fit_information, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
#            Reference
# Prediction   0   1
#          0 146  91
#          1  55 110
# Accuracy = 0.6368
# most important variable are T1WinPct, followed by T2WinPct, T2PF, T2Fgm, etc. 


# Train decision tree using gini criteria: 
dtree_fit_gini = train(as.factor(Win) ~., data = train, method = "rpart",
                       parms = list(split = "gini"),
                       trControl=trctrl, tuneLength = 10)
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1)
test_pred_gini = predict(dtree_fit_gini, newdata = test)
confusionMatrix(test_pred_gini, as.factor(test$Win))
#            Reference
# Prediction   0   1
#          0 137  89
#          1  64 112
# Accuracy = 0.6194
# most important variable are also T1WinPct, followed by T2WinPct.

## we will use information criteria decision tree for final predictions
## since it gives a higher accuracy



# -------------------------------------------------------+
# Random Forest                                          |
# -------------------------------------------------------+
## iterated through a lot of mtry values (2,5,10,15,...) to find bestTune
## this would take around 10 minutes
## will examine around the bestTune mtry number to find the best mtry

# rf_fit = train(as.factor(Win) ~. , data=train, method="rf",
#                trControl=trctrl, tuneLength = 10)
# rf_fit
# plot(rf_fit)
# rf_fit$bestTune
# best tune gives mtry = 5, will manually readjust mtry after initial run
# Accuracy m = 5: 0.6245842

rf_fit_4 = train(as.factor(Win) ~. , data=train, method="rf",
                 trControl=trctrl, tuneGrid = expand.grid(mtry = 4))
rf_fit_4$results
# Accuracy m = 4: 0.6229623

rf_fit_6 = train(as.factor(Win) ~. , data=train, method="rf",
                  trControl=trctrl, tuneGrid = expand.grid(mtry = 6))
rf_fit_6$results
# Accuracy m = 6: 0.6219669

## mtry = 5 gives the best accuracy, will use rf_fit_5 for final predictions
rf_fit_5 = train(as.factor(Win) ~. , data=train, method="rf",
                 trControl=trctrl, tuneGrid = expand.grid(mtry = 5))
Importance = varImp(rf_fit_5)
plot(Importance)
test_pred = predict(rf_fit_5, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
## the top two important variables agree with decision tree outputs
## which are T1WinPct and T2WinPct



# -------------------------------------------------------+
# Support Vector Machine                                 |
# -------------------------------------------------------+
library(e1071)
## svm with linear kernel
# use tunegrid to find best cost
svm.linear = train(as.factor(Win) ~., data=train, method="svmLinear",
                   trControl=trctrl, tuneGrid=data.frame(.C=c(.25, .5, 1, 5, 10, 15, 20)))
# C      Accuracy   Kappa    
#  0.25  0.6601314  0.3202825
#  0.50  0.6627004  0.3254226
#  1.00  0.6608323  0.3216953
#  5.00  0.6580138  0.3160513
# 10.00  0.6566152  0.3132520
# 15.00  0.6566200  0.3132601
# 20.00  0.6563886  0.3127963

## cost=0.5 gives best result
svm.linear=svm(as.factor(Win) ~., data=train, kernel="linear", cost=0.5)
test_pred = predict(svm.linear, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
#           Reference
# Prediction   0   1
#          0 137  63
#          1  64 138
# Accuracy : 0.6841



## svm with polynomial kernel
svm.poly.model = train(as.factor(Win) ~., data=train, method="svmPoly",trControl=trctrl)
svm.poly.model$bestTune

svm.poly=svm(as.factor(Win) ~., data=train, kernel="polynomial", cost=0.25, scale=0.1, degree=1)
test_pred = predict(svm.poly, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
#           Reference
# Prediction   0   1
#          0 134  67
#          1  67 134
# Accuracy : 0.6667


## svm with radial kernel

# find the best tune
svm.radial=train(as.factor(Win) ~., data=train, method="svmRadial",
                 trControl=trctrl, tuneGrid=data.frame(.C=c(.25,.5,1,5,10,15),
                                                       .sigma=.005))
# C      Accuracy   Kappa    
#  0.25  0.6514834  0.3029228
#  0.50  0.6542922  0.3085281
#  1.00  0.6580219  0.3159644
#  5.00  0.6563969  0.3127271
# 10.00  0.6489179  0.2977916
# 15.00  0.6419393  0.2838421

# build the model using svm with radial kernel
svm.radial=svm(as.factor(Win) ~., data=train, kernel="radial", gamma = 0.005, cost = 1)
test_pred = predict(svm.radial, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
#           Reference
# Prediction   0   1
#          0 132  69
#          1  69 132
# Accuracy : 0.6567


# -------------------------------------------------------+
# Predict Winner function                                |
# -------------------------------------------------------+
# examine seeds distribution for each year before predictions
# create prediction for each round

PredictWinners = function(thisModel,Year){
  mycolnames = c("Season","Team1","Team2","T1WinPct","T1PPG","T1Fgm","T1Fga",
                 "T1Tpm","T1Tpa","T1Ftm","T1Fta","T1OR","T1DR","T1Ast",
                 "T1TO","T1St","T1Bl","T1PF","T2WinPct","T2PPG","T2Fgm",
                 "T2Fga","T2Tpm","T2Tpa","T2Ftm","T2Fta","T2OR",
                 "T2DR","T2Ast","T2TO","T2St","T2Bl","T2PF" )
  # extract slots (slot number, strong seed number, weak seed number) for current Year
  TheseSlots = filter(Slots,Season==Year) 
  # extract seeds (seed number, team number) for current Year
  TheseSeeds = filter(Seeds,Season==Year)  
  TheseSlots$Prediction = 0    # Initiate to store predictions
  
  #Round 1
  Round1Games = as.data.frame(matrix(0, ncol = 33, nrow = 32))  # initiate round 1 with 32 rows
  colnames(Round1Games)[1:33] = mycolnames
  Round1Games$Season = Year
  for (i in 1:32) {
    # get Team1 number by matching strong seed number to team number
    Round1Games[i,"Team1"] =
      TheseSeeds[which(TheseSeeds$Seed == as.character(TheseSlots$Strongseed[i])),3]
    # get Team2 number by matching strong seed number to team number
    Round1Games[i,"Team2"] =
      TheseSeeds[which(TheseSeeds$Seed == as.character(TheseSlots$Weakseed[i])),3]
    # fill in annual game stats for team1 from table 'AnnualSummaries'
    Round1Games[i,4:18] =
      AnnualSummaries[which(AnnualSummaries$Season==Round1Games[i,"Season"] &
                              AnnualSummaries$Team==Round1Games[i,"Team1"]),3:17]
    # fill in annual game stats for team1 from table 'AnnualSummaries'
    Round1Games[i,19:33] =
      AnnualSummaries[which(AnnualSummaries$Season==Round1Games[i,"Season"] &
                              AnnualSummaries$Team ==Round1Games[i,"Team2"]),3:17]
  }
  
  # Create predictions on round 1
  pred = predict(thisModel, Round1Games)
  Round1Pred = data.frame(Slot = Slots[1:32,"Slot"],PredictedWinner = 0)
  # store winning team number in PredictedWinner
  for (i in 1:32) {
    if (pred[i] == 1) {
      Round1Pred[i,"PredictedWinner"] = Round1Games[i,"Team1"]
    }
    else {
      Round1Pred[i,"PredictedWinner"] = Round1Games[i,"Team2"]
    }
  }
  TheseSlots$Prediction[1:32] = Round1Pred[,"PredictedWinner"]  # store predictions in TheseSlots
  
  
  ## Round 2
  # Use the predictions in TheseSlots to construct round 2
  Round2Games = as.data.frame(matrix(0, ncol = 33, nrow = 16))
  colnames(Round2Games)[1:33] = mycolnames
  Round2Games$Season = Year
  for (i in 1:16) {
    # start from the 33th game (round 2)
    # fill Team1 as the strong seed team number that won in round 1
    Round2Games[i,"Team1"] =
      Round1Pred[which(Round1Pred$Slot == as.character(TheseSlots$Strongseed[i+32])),
                 "PredictedWinner"]
    # fill Team2 as the weak seed team number that won in round 1
    Round2Games[i,"Team2"] =
      Round1Pred[which(Round1Pred$Slot == as.character(TheseSlots$Weakseed[i+32])),
                 "PredictedWinner"]
    Round2Games[i,4:18] =
      AnnualSummaries[which(AnnualSummaries$Season==Round2Games[i,"Season"] &
                              AnnualSummaries$Team ==Round2Games[i,"Team1"]),3:17]
    Round2Games[i,19:33] =
      AnnualSummaries[which(AnnualSummaries$Season==Round2Games[i,"Season"] &
                              AnnualSummaries$Team ==Round2Games[i,"Team2"]),3:17]
  }
  
  # Create predictions on round 2
  pred = predict(thisModel, Round2Games)
  Round2Pred = data.frame(Slot = Slots[33:48,"Slot"],PredictedWinner = 0)
  for (i in 1:16) {
    if (pred[i] == 1) {
      Round2Pred[i,"PredictedWinner"] = Round2Games[i,"Team1"]
    }
    else {
      Round2Pred[i,"PredictedWinner"] = Round2Games[i,"Team2"]
    }
  }
  TheseSlots$Prediction[33:48] = Round2Pred[,"PredictedWinner"]
  
  
  ## Round 3
  # Use the predictions in TheseSlots to construct round 3
  Round3Games = as.data.frame(matrix(0, ncol = 33, nrow = 8))
  colnames(Round3Games)[1:33] = mycolnames
  Round3Games$Season = Year
  # start from the 49th game (round 2)
  # fill Team1 as the strong seed team number that won in round 2
  for (i in 1:8) {
    Round3Games[i,"Team1"] =
      Round2Pred[which(Round2Pred$Slot == as.character(TheseSlots$Strongseed[i+48])),
                 "PredictedWinner"]
    # fill Team2 as the weak seed team number that won in round 2
    Round3Games[i,"Team2"] =
      Round2Pred[which(Round2Pred$Slot == as.character(TheseSlots$Weakseed[i+48])),
                 "PredictedWinner"]
    Round3Games[i,4:18] =
      AnnualSummaries[which(AnnualSummaries$Season==Round3Games[i,"Season"] &
                              AnnualSummaries$Team ==Round3Games[i,"Team1"]),3:17]
    Round3Games[i,19:33] =
      AnnualSummaries[which(AnnualSummaries$Season==Round3Games[i,"Season"] &
                              AnnualSummaries$Team ==Round3Games[i,"Team2"]),3:17]
  }
  # Create predictions on round 3
  pred = predict(thisModel, Round3Games)
  Round3Pred = data.frame(Slot = Slots[49:56,"Slot"],PredictedWinner = 0)
  for (i in 1:8) {
    
    if (pred[i] == 1){
      Round3Pred[i,"PredictedWinner"] = Round3Games[i,"Team1"]
    }
    else {
      Round3Pred[i,"PredictedWinner"] = Round3Games[i,"Team2"]
    }
  }
  TheseSlots$Prediction[49:56] = Round3Pred[,"PredictedWinner"]
  
  
  ## Round 4
  # Use the predictions in TheseSlots to construct round 4
  Round4Games = as.data.frame(matrix(0, ncol = 33, nrow = 4))
  colnames(Round4Games)[1:33] = mycolnames
  Round4Games$Season = Year
  # start from the 57th game (round 3)
  # fill Team1 as the strong seed team number that won in round 3
  for (i in 1:4) {
    Round4Games[i,"Team1"] =
      Round3Pred[which(Round3Pred$Slot == as.character(TheseSlots$Strongseed[i+56])),
                 "PredictedWinner"]
    # fill Team2 as the weak seed team number that won in round 3
    Round4Games[i,"Team2"] =
      Round3Pred[which(Round3Pred$Slot == as.character(TheseSlots$Weakseed[i+56])),
                 "PredictedWinner"]
    Round4Games[i,4:18] =
      AnnualSummaries[which(AnnualSummaries$Season==Round4Games[i,"Season"] &
                              AnnualSummaries$Team ==Round4Games[i,"Team1"]),3:17]
    Round4Games[i,19:33] =
      AnnualSummaries[which(AnnualSummaries$Season==Round4Games[i,"Season"] &
                              AnnualSummaries$Team ==Round4Games[i,"Team2"]),3:17]
  }
  
  # Create predictions on round 4
  pred = predict(thisModel, Round4Games)
  Round4Pred = data.frame(Slot = Slots[57:60,"Slot"],PredictedWinner = 0)
  for (i in 1:4) {
    if (pred[i] == 1) {
      Round4Pred[i,"PredictedWinner"] = Round4Games[i,"Team1"]
    }
    else {
      Round4Pred[i,"PredictedWinner"] = Round4Games[i,"Team2"]
    }
  }
  TheseSlots$Prediction[57:60] = Round4Pred[,"PredictedWinner"]
  
  
  ## Round 5
  # Use the predictions in TheseSlots to construct round 4
  Round5Games = as.data.frame(matrix(0, ncol = 33, nrow = 2))
  colnames(Round5Games)[1:33] = mycolnames
  Round5Games$Season = Year
  # start from the 61 th game (round )
  # fill Team1 as the strong seed team number that won in round 4
  for (i in 1:2){
    Round5Games[i,"Team1"] =
      Round4Pred[which(Round4Pred$Slot == as.character(TheseSlots$Strongseed[i+60])),
                 "PredictedWinner"]
    # fill Team1 as the week seed team number that won in round 4
    Round5Games[i,"Team2"] =
      Round4Pred[which(Round4Pred$Slot == as.character(TheseSlots$Weakseed[i+60])),
                 "PredictedWinner"]
    Round5Games[i,4:18] =
      AnnualSummaries[which(AnnualSummaries$Season==Round4Games[i,"Season"] &
                              AnnualSummaries$Team ==Round4Games[i,"Team1"]),3:17]
    Round5Games[i,19:33] =
      AnnualSummaries[which(AnnualSummaries$Season==Round4Games[i,"Season"] &
                              AnnualSummaries$Team ==Round4Games[i,"Team2"]),3:17]
  }
  
  # Create predictions on round 5
  pred = predict(thisModel, Round5Games)
  Round5Pred = data.frame(Slot = Slots[61:62,"Slot"],PredictedWinner = 0)
  for (i in 1:2) {
    if (pred[i] == 1) {
      Round5Pred[i,"PredictedWinner"] = Round5Games[i,"Team1"]
    }
    else {
      Round5Pred[i,"PredictedWinner"] = Round5Games[i,"Team2"]
    }
  }
  TheseSlots$Prediction[61:62] = Round5Pred[,"PredictedWinner"]
  
  
  ## Round 6
  # Use the predictions in TheseSlots to construct round 5
  Round6Games = as.data.frame(matrix(0, ncol = 33, nrow = 1))
  colnames(Round6Games)[1:33] = mycolnames
  Round6Games$Season = Year
  # start from the 63 th game (round )
  # fill Team1 as the strong seed team number that won in round 5
  for (i in 1:1) {
    Round6Games[i,"Team1"] =
      Round5Pred[which(Round5Pred$Slot == as.character(TheseSlots$Strongseed[i+62])),
                 "PredictedWinner"]
    # fill Team1 as the week seed team number that won in round 5
    Round6Games[i,"Team2"] =
      Round5Pred[which(Round5Pred$Slot == as.character(TheseSlots$Weakseed[i+62])),
                 "PredictedWinner"]
    Round6Games[i,4:18] =
      AnnualSummaries[which(AnnualSummaries$Season==Round6Games[i,"Season"] &
                              AnnualSummaries$Team ==Round6Games[i,"Team1"]),3:17]
    Round6Games[i,19:33] =
      AnnualSummaries[which(AnnualSummaries$Season==Round6Games[i,"Season"] &
                              AnnualSummaries$Team ==Round6Games[i,"Team2"]),3:17]
  }
  
  # Create predictions on round 6
  pred = predict(thisModel, Round4Games)
  Round6Pred = data.frame(Slot = Slots[63,"Slot"],PredictedWinner = 0)
  for (i in 1:1) {
    if (pred[i] == 1) {
      Round6Pred[i,"PredictedWinner"] = Round6Games[i,"Team1"]
    }
    else {
      Round6Pred[i,"PredictedWinner"] = Round6Games[i,"Team2"]
    }
  }
  TheseSlots$Prediction[63] = Round6Pred[,"PredictedWinner"]
  
  
  TheseResults = filter(TourneyResults, Season==Year)
  TheseSlots$Actual = 0
  for (i in 1:63) {
    TheseSlots[i,"Actual"] =
      TheseResults[which(as.character(TheseSlots[i,"Slot"])==TheseResults$Slot),
                   "Wteam"]
  }
  
  ## creat a list of round numbers
  Rounds = c(rep(1,32), rep(2,16), rep(3,8), rep(4,4), rep(5,2), 6)
  
  ## construct a dataframe 'Results'
  ## round number, Predicted winning team number, Actual winning team number
  Results = data.frame(Round = Rounds,
                       Predicted = TheseSlots$Prediction,
                       Winner = TheseSlots$Actual)
  return(Results)
}



# -------------------------------------------------------+
# Score Calculation function                             |
# -------------------------------------------------------+

CalculateScore = function(predict_col, actual_col) {
  score = 0   # initialize score
  
  ## add 1 point per correct round 1 prediction
  for (i in 1:32) {
    if (predict_col[i] == actual_col[i]) {
      score = score+1
    }
  }
  
  ## add 2 point per correct round 2 prediction
  for (i in 33:48) {
    if (predict_col[i] == actual_col[i]) {
      score = score+2
    }
  } 
  
  ## add 4 point per correct round 3 prediction
  for (i in 49:56) {
    if (predict_col[i] == actual_col[i]) {
      score = score+4
    }
  }
  
  ## add 8 point per correct round 4 prediction
  for (i in 57:60) {
    if (predict_col[i] == actual_col[i]) {
      score = score+8
    }
  }
  
  ## add 16 point per correct round 5 prediction
  for (i in 61:62) {
    if (predict_col[i] == actual_col[i]) {
      score = score+16
    }
  }
  
  ## add 32 point per correct round 6 prediction
  for (i in 63) {
    if (predict_col[i] == actual_col[i]) {
      score = score+32
    }
  }
  return(score)
}



# -------------------------------------------------------+
# Make Predictions and Calculate Score                   |
# -------------------------------------------------------+
## make predictions
# logistic regression
Prediction_2014_logi = PredictWinners(logistic, 2014)
Prediction_2015_logi = PredictWinners(logistic, 2015)
Prediction_2016_logi = PredictWinners(logistic, 2016)

# decision tree (Information gain)
Prediction_2014_dtree = PredictWinners(dtree_fit_information, 2014)
Prediction_2015_dtree = PredictWinners(dtree_fit_information, 2015)
Prediction_2016_dtree = PredictWinners(dtree_fit_information, 2016)

# Random forest
Prediction_2014_rf = PredictWinners(rf_fit_5, 2014)
Prediction_2015_rf = PredictWinners(rf_fit_5, 2015)
Prediction_2016_rf = PredictWinners(rf_fit_5, 2016)

# svm with linear kernel
Prediction_2014_svm = PredictWinners(svm.linear, 2014)
Prediction_2015_svm = PredictWinners(svm.linear, 2015)
Prediction_2016_svm = PredictWinners(svm.linear, 2016)

# svm with polynomial kernel
Prediction_2014_svmpoly = PredictWinners(svm.poly, 2014)
Prediction_2015_svmpoly = PredictWinners(svm.poly, 2015)
Prediction_2016_svmpoly = PredictWinners(svm.poly, 2016)

# svm with radial kernel
Prediction_2014_svmrad = PredictWinners(svm.radial, 2014)
Prediction_2015_svmrad = PredictWinners(svm.radial, 2015)
Prediction_2016_svmrad = PredictWinners(svm.radial, 2016)




## calculate scores

# logistic regression
score_2014_logi = CalculateScore(Prediction_2014_logi$Predicted, Prediction_2014_logi$Winner)
score_2015_logi = CalculateScore(Prediction_2015_logi$Predicted, Prediction_2015_logi$Winner)
score_2016_logi = CalculateScore(Prediction_2016_logi$Predicted, Prediction_2016_logi$Winner)

# decison tree(based on information gain)
score_2014_dtree = CalculateScore(Prediction_2014_dtree$Predicted, Prediction_2014_dtree$Winner)
score_2015_dtree = CalculateScore(Prediction_2015_dtree$Predicted, Prediction_2015_dtree$Winner)
score_2016_dtree = CalculateScore(Prediction_2016_dtree$Predicted, Prediction_2016_dtree$Winner)

# random forest
score_2014_rf = CalculateScore(Prediction_2014_rf$Predicted, Prediction_2014_rf$Winner)
score_2015_rf = CalculateScore(Prediction_2015_rf$Predicted, Prediction_2015_rf$Winner)
score_2016_rf = CalculateScore(Prediction_2016_rf$Predicted, Prediction_2016_rf$Winner)

# svm with linear kernel
score_2014_svm = CalculateScore(Prediction_2014_svm$Predicted, Prediction_2014_svm$Winner)
score_2015_svm = CalculateScore(Prediction_2015_svm$Predicted, Prediction_2015_svm$Winner)
score_2016_svm = CalculateScore(Prediction_2016_svm$Predicted, Prediction_2016_svm$Winner)

# svm with polynomial kernel
score_2014_svmpoly = CalculateScore(Prediction_2014_svmpoly$Predicted, Prediction_2014_svmpoly$Winner)
score_2015_svmpoly = CalculateScore(Prediction_2015_svmpoly$Predicted, Prediction_2015_svmpoly$Winner)
score_2016_svmpoly = CalculateScore(Prediction_2016_svmpoly$Predicted, Prediction_2016_svmpoly$Winner)

# svm with radial kernel
score_2014_svmrad = CalculateScore(Prediction_2014_svmrad$Predicted, Prediction_2014_svmrad$Winner)
score_2015_svmrad = CalculateScore(Prediction_2015_svmrad$Predicted, Prediction_2015_svmrad$Winner)
score_2016_svmrad = CalculateScore(Prediction_2016_svmrad$Predicted, Prediction_2016_svmrad$Winner)

# combine the results 
Year = c(2014,2015,2016)
Score_logi = c(score_2014_logi,score_2015_logi,score_2016_logi)
Score_dtree = c(score_2014_dtree,score_2015_dtree,score_2016_dtree)
Score_rf = c(score_2014_rf,score_2015_rf,score_2016_rf)
Score_svm = c(score_2014_svm,score_2015_svm,score_2016_svm)
Score_svmpoly = c(score_2014_svmpoly,score_2015_svmpoly,score_2016_svmpoly)
Score_svmradial = c(score_2014_svmrad,score_2015_svmrad,score_2016_svmrad)

# display the model performances
dt <- data.frame(Year,Score_logi,Score_dtree,Score_rf,Score_svm,Score_svmpoly,Score_svmradial)
dt

#   Year Score_logi Score_dtree Score_rf Score_svm Score_svmpoly Score_svmradial
# 1 2014         44          38       50        44            52              47
# 2 2015        107          62      126       104            94              86
# 3 2016         95          45       67        95            89              80




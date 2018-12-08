# SYS 6018 - Final Project
# March Machine Learning Mania 2017
# Jing Sun (js6mj), Runhao Zhao (rz6dg), Luke Kang   (sk5be)

library(ggplot2)
library(dplyr)
library(caret)
library(kableExtra)
library(rpart.plot)
setwd("~/Desktop/UVA/fall18/sys6018/final project")

# -------------------------------------------------------+
# Data Preprocessing                                     |
# -------------------------------------------------------+
GameStats = read.csv('RegularSeasonDetailedResults.csv')
TourneyResults = read.csv('TourneyDetailedResults.csv')
Slots = read.csv('TourneySlots.csv')
Seeds = read.csv('TourneySeeds.csv')


## Since each row in GameStats contains data for both the winning team and 
## the losing team, we need to construct a new data table FullGame to extract 
## the game stats for each team in each game. 
Winners = GameStats[,c(1,3:4,9:21)]
colnames(Winners)[2:16] = c("Team","Score","Fgm","Fga","Tpm","Tpa",
                            "Ftm","Fta","OR","DR","Ast",
                            "TO","St","Bl","PF")
Winners$Win = 1

Losers = GameStats[,c(1,5:6,22:34)]
colnames(Losers)[2:16] = c("Team","Score","Fgm","Fga","Tpm","Tpa",
                           "Ftm","Fta","OR","DR","Ast",
                           "TO","St","Bl","PF")
Losers$Win = 0
FullGame = rbind(Winners,Losers)

## We then create a summary table named AnnualSummaries of average stats 
## per team per year by grouping the data in FullGame by Season and then by Team.
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
## We then construct our training data using 2003 - 2013 tournament victories
## and use 2014 - 2016 as test data.

# Construct Training Data merging games played with stats above
TrainingResults = filter(TourneyResults, Season <= 2013)
WinningTraining = as.data.frame(matrix(0, ncol = 33, nrow = nrow(TrainingResults)))
colnames(WinningTraining)[1:3] = c("Season","Team1","Team2")
colnames(WinningTraining)[4:18] = c("T1WinPct","T1PPG","T1Fgm","T1Fga","T1Tpm",
                                    "T1Tpa","T1Ftm","T1Fta","T1OR","T1DR",
                                    "T1Ast","T1TO","T1St","T1Bl","T1PF")
colnames(WinningTraining)[19:33] = c("T2WinPct","T2PPG","T2Fgm","T2Fga","T2Tpm",
                                     "T2Tpa","T2Ftm","T2Fta","T2OR","T2DR",
                                     "T2Ast","T2TO","T2St","T2Bl","T2PF")
WinningTraining[,1:3] = TrainingResults[,c(1,3,5)]

for (i in 1:nrow(TrainingResults)){
  WinningTraining[i,4:18] =
    AnnualSummaries[which(AnnualSummaries$Season==TrainingResults[i,"Season"]
                          &AnnualSummaries$Team ==TrainingResults[i,"Wteam"]),3:17]
  WinningTraining[i,19:33] =
    AnnualSummaries[which(AnnualSummaries$Season==TrainingResults[i,"Season"]
                          &AnnualSummaries$Team ==TrainingResults[i,"Lteam"]),3:17]
}
WinningTraining$Win = 1


# Now use a similar logic as above, changing indices as necessary, to create
# a dataframe 'LoseTraining' with the losing team of each game as Team 1.
LoseTraining = as.data.frame(matrix(0, ncol = 33, nrow = nrow(TrainingResults)))
colnames(LoseTraining)[1:3] = c("Season","Team1","Team2")
colnames(LoseTraining)[4:18] = c("T1WinPct","T1PPG","T1Fgm","T1Fga","T1Tpm",
                                 "T1Tpa","T1Ftm","T1Fta","T1OR","T1DR",
                                 "T1Ast","T1TO","T1St","T1Bl","T1PF")
colnames(LoseTraining)[19:33] = c("T2WinPct","T2PPG","T2Fgm","T2Fga","T2Tpm",
                                  "T2Tpa","T2Ftm","T2Fta","T2OR","T2DR",
                                  "T2Ast","T2TO","T2St","T2Bl","T2PF")
LoseTraining[,1:3] = TrainingResults[,c(1,5,3)]

for (i in 1:nrow(TrainingResults)){
  LoseTraining[i,4:18] =
    AnnualSummaries[which(AnnualSummaries$Season==TrainingResults[i,"Season"]
                          &AnnualSummaries$Team ==TrainingResults[i,"Lteam"]),3:17]
  LoseTraining[i,19:33] =
    AnnualSummaries[which(AnnualSummaries$Season==TrainingResults[i,"Season"]
                          &AnnualSummaries$Team ==TrainingResults[i,"Wteam"]),3:17]
}
LoseTraining$Win = 0
TrainingData = rbind(WinningTraining,LoseTraining)


# construct Test Set
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
## 10-fold repeated cross validation
train = subset(TrainingData, select = -c(T1Ftm,T2Ftm))
test = subset(TestData, select = -c(T1Ftm,T2Ftm))

train_control <- trainControl(method="repeatedcv", number=10, repeats=5)
logistic <- train(as.factor(Win) ~ ., data = train, method="glm", trControl=train_control)
summary(logistic)
logistic
## Accuracy = 0.6616808.


# -------------------------------------------------------+
# Decision Tree                                          |
# -------------------------------------------------------+
## information gain criteria
# Train a simple decision tree
# Train decision tree using information gain criteria:
trctrl = trainControl(method = "repeatedcv", number = 10, repeats = 3)
dtree_fit_information = train(as.factor(Win) ~ ., data = train, method = "rpart",
                              parms = list(split = "information"),
                              trControl=trctrl, tuneLength = 10)
prp(dtree_fit_information$finalModel, box.palette = "Reds", tweak = 1)

test_pred = predict(dtree_fit_information, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
# Accuracy = 0.6393
# most important variable are T1WinPct, followed by T2WinPct, T2PF, T2Fgm, etc. 

## gini criteria
dtree_fit_gini = train(as.factor(Win) ~., data = train, method = "rpart",
                       parms = list(split = "gini"),
                       trControl=trctrl, tuneLength = 10)
prp(dtree_fit_gini$finalModel, box.palette = "Reds", tweak = 1)
test_pred_gini = predict(dtree_fit_gini, newdata = test)
confusionMatrix(test_pred_gini, as.factor(test$Win))
# Accuracy = 0.6393
# most important variable are also T1WinPct, followed by T2WinPct.



# -------------------------------------------------------+
# Random Forest                                          |
# -------------------------------------------------------+
# rf_fit = train(as.factor(Win) ~. , data=train, method="rf",
#                trControl=trctrl, tuneLength = 10)
# rf_fit
# plot(rf_fit)
# rf_fit$bestTune
# best tune gives mtry = 5, will manually readjust mtry after initial run
# Accuracy m = 5: 0.6121813

rf_fit_4 = train(as.factor(Win) ~. , data=train, method="rf",
                 trControl=trctrl, tuneGrid = expand.grid(mtry = 4))
rf_fit_4$results
# Accuracy m = 4: 0.6267171

# rf_fit_6 = train(as.factor(Win) ~. , data=train, method="rf",
#                  trControl=trctrl, tuneGrid = expand.grid(mtry = 6))
# rf_fit_6$results
# Accuracy m = 6: 0.6231822

## mtry = 4 gives the best accuracy
Importance = varImp(rf_fit_4)
plot(Importance)



# -------------------------------------------------------+
# Support Vector Machine                                 |
# -------------------------------------------------------+
library(e1071)
svm.linear=svm(as.factor(Win) ~., data=train, kernel="linear", cost=20)
test_pred = predict(svm.linear, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
# Accuracy = 0.699

svm.poly=svm(as.factor(Win) ~., data=train, kernel="polynomial", cost=10)
test_pred = predict(svm.poly, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
# Accuracy = 0.5423

svm.radial=svm(as.factor(Win) ~., data=train, kernel="radial", gamma = 0.005, cost = 10)
test_pred = predict(svm.radial, newdata = test)
confusionMatrix(test_pred, as.factor(test$Win))
# Accuracy = 0.6667


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
  TheseSlots = filter(Slots,Season==Year)
  TheseSeeds = filter(Seeds,Season==Year)
  
  TheseSlots$Prediction = 0        # Initiate to store predictions
  
  #Round 1
  Round1Games = as.data.frame(matrix(0, ncol = 33, nrow = 32))
  colnames(Round1Games)[1:33] = mycolnames
  Round1Games$Season = Year
  for (i in 1:32) {
    Round1Games[i,"Team1"] =
      TheseSeeds[which(TheseSeeds$Seed == as.character(TheseSlots$Strongseed[i])),3]
    Round1Games[i,"Team2"] =
      TheseSeeds[which(TheseSeeds$Seed == as.character(TheseSlots$Weakseed[i])),3]
    Round1Games[i,4:18] =
      AnnualSummaries[which(AnnualSummaries$Season==Round1Games[i,"Season"] &
                              AnnualSummaries$Team==Round1Games[i,"Team1"]),3:17]
    Round1Games[i,19:33] =
      AnnualSummaries[which(AnnualSummaries$Season==Round1Games[i,"Season"] &
                              AnnualSummaries$Team ==Round1Games[i,"Team2"]),3:17]
  }
  
  # Create predictions on round 1
  pred = predict(thisModel, Round1Games)
  Round1Pred = data.frame(Slot = Slots[1:32,"Slot"],PredictedWinner = 0)
  for (i in 1:32) {
    if (pred[i] == 1) {
      Round1Pred[i,"PredictedWinner"] = Round1Games[i,"Team1"]
    }
    else {
      Round1Pred[i,"PredictedWinner"] = Round1Games[i,"Team2"]
    }
  }
  TheseSlots$Prediction[1:32] = Round1Pred[,"PredictedWinner"]
  
  
  ## Round 2
  # Use the predicted classes to construct round 2
  Round2Games = as.data.frame(matrix(0, ncol = 33, nrow = 16))
  colnames(Round2Games)[1:33] = mycolnames
  Round2Games$Season = Year
  for (i in 1:16) {
    Round2Games[i,"Team1"] =
      Round1Pred[which(Round1Pred$Slot == as.character(TheseSlots$Strongseed[i+32])),
                 "PredictedWinner"]
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
  
  # Round 3
  Round3Games = as.data.frame(matrix(0, ncol = 33, nrow = 8))
  colnames(Round3Games)[1:33] = mycolnames
  Round3Games$Season = Year
  for (i in 1:8) {
    Round3Games[i,"Team1"] =
      Round2Pred[which(Round2Pred$Slot == as.character(TheseSlots$Strongseed[i+48])),
                 "PredictedWinner"]
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
  
  # Round 4
  Round4Games = as.data.frame(matrix(0, ncol = 33, nrow = 4))
  colnames(Round4Games)[1:33] = mycolnames
  Round4Games$Season = Year
  for (i in 1:4) {
    Round4Games[i,"Team1"] =
      Round3Pred[which(Round3Pred$Slot == as.character(TheseSlots$Strongseed[i+56])),
                 "PredictedWinner"]
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
  
  
  # Round 5
  Round5Games = as.data.frame(matrix(0, ncol = 33, nrow = 2))
  colnames(Round5Games)[1:33] = mycolnames
  Round5Games$Season = Year
  for (i in 1:2){
    Round5Games[i,"Team1"] =
      Round4Pred[which(Round4Pred$Slot == as.character(TheseSlots$Strongseed[i+60])),
                 "PredictedWinner"]
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
  
  
  # Round 6
  Round6Games = as.data.frame(matrix(0, ncol = 33, nrow = 1))
  colnames(Round6Games)[1:33] = mycolnames
  Round6Games$Season = Year
  for (i in 1:1) {
    Round6Games[i,"Team1"] =
      Round5Pred[which(Round5Pred$Slot == as.character(TheseSlots$Strongseed[i+62])),
                 "PredictedWinner"]
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
  
  Rounds = 0
  for (i in 1:32) {
    Rounds[i] = 1
  }
  for (i in 33:48) {
    Rounds[i] = 2
  }
  for (i in 49:56) {
    Rounds[i] = 3
  }
  for (i in 57:60) {
    Rounds[i] = 4
  }
  for (i in 61:62) {
    Rounds[i] = 5
  }
  for (i in 63) {
    Rounds[i] = 6
  }
  
  Results = data.frame(Round = Rounds,
                       Predicted = TheseSlots$Prediction,
                       Winner = TheseSlots$Actual)
  return(Results)
}



# -------------------------------------------------------+
# Score Calculation function                             |
# -------------------------------------------------------+

CalculateScore = function(predict_col, actual_col) {
  score = 0
  for (i in 1:32) {
    if (predict_col[i] == actual_col[i]) {
      score = score+1
    }
  }
  for (i in 33:48) {
    if (predict_col[i] == actual_col[i]) {
      score = score+2
    }
  } 
  for (i in 49:56) {
    if (predict_col[i] == actual_col[i]) {
      score = score+4
    }
  }
  for (i in 57:60) {
    if (predict_col[i] == actual_col[i]) {
      score = score+8
    }
  }
  for (i in 61:62) {
    if (predict_col[i] == actual_col[i]) {
      score = score+16
    }
  }
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
Prediction_2014_logi = PredictWinners(logistic, 2014)
Prediction_2015_logi = PredictWinners(logistic, 2015)
Prediction_2016_logi = PredictWinners(logistic, 2016)

Prediction_2014_dtree = PredictWinners(dtree_fit_information, 2014)
Prediction_2015_dtree = PredictWinners(dtree_fit_information, 2015)
Prediction_2016_dtree = PredictWinners(dtree_fit_information, 2016)

Prediction_2014_rf = PredictWinners(rf_fit_4, 2014)
Prediction_2015_rf = PredictWinners(rf_fit_4, 2015)
Prediction_2016_rf = PredictWinners(rf_fit_4, 2016)

Prediction_2014_svm = PredictWinners(svm.linear, 2014)
Prediction_2015_svm = PredictWinners(svm.linear, 2015)
Prediction_2016_svm = PredictWinners(svm.linear, 2016)

Prediction_2014_svmpoly = PredictWinners(svm.poly, 2014)
Prediction_2015_svmpoly = PredictWinners(svm.poly, 2015)
Prediction_2016_svmpoly = PredictWinners(svm.poly, 2016)

Prediction_2014_svmrad = PredictWinners(svm.radial, 2014)
Prediction_2015_svmrad = PredictWinners(svm.radial, 2015)
Prediction_2016_svmrad = PredictWinners(svm.radial, 2016)




## calculate scores
score_2014_logi = CalculateScore(Prediction_2014_logi$Predicted, Prediction_2014_logi$Winner)
score_2015_logi = CalculateScore(Prediction_2015_logi$Predicted, Prediction_2015_logi$Winner)
score_2016_logi = CalculateScore(Prediction_2016_logi$Predicted, Prediction_2016_logi$Winner)

score_2014_dtree = CalculateScore(Prediction_2014_dtree$Predicted, Prediction_2014_dtree$Winner)
score_2015_dtree = CalculateScore(Prediction_2015_dtree$Predicted, Prediction_2015_dtree$Winner)
score_2016_dtree = CalculateScore(Prediction_2016_dtree$Predicted, Prediction_2016_dtree$Winner)

score_2014_rf = CalculateScore(Prediction_2014_rf$Predicted, Prediction_2014_rf$Winner)
score_2015_rf = CalculateScore(Prediction_2015_rf$Predicted, Prediction_2015_rf$Winner)
score_2016_rf = CalculateScore(Prediction_2016_rf$Predicted, Prediction_2016_rf$Winner)

score_2014_svm = CalculateScore(Prediction_2014_svm$Predicted, Prediction_2014_svm$Winner)
score_2015_svm = CalculateScore(Prediction_2015_svm$Predicted, Prediction_2015_svm$Winner)
score_2016_svm = CalculateScore(Prediction_2016_svm$Predicted, Prediction_2016_svm$Winner)

score_2014_svmpoly = CalculateScore(Prediction_2014_svmpoly$Predicted, Prediction_2014_svmpoly$Winner)
score_2015_svmpoly = CalculateScore(Prediction_2015_svmpoly$Predicted, Prediction_2015_svmpoly$Winner)
score_2016_svmpoly = CalculateScore(Prediction_2016_svmpoly$Predicted, Prediction_2016_svmpoly$Winner)

score_2014_svmrad = CalculateScore(Prediction_2014_svmrad$Predicted, Prediction_2014_svmrad$Winner)
score_2015_svmrad = CalculateScore(Prediction_2015_svmrad$Predicted, Prediction_2015_svmrad$Winner)
score_2016_svmrad = CalculateScore(Prediction_2016_svmrad$Predicted, Prediction_2016_svmrad$Winner)


Year = c(2014,2015,2016)
Score_logi = c(score_2014_logi,score_2015_logi,score_2016_logi)
Score_dtree = c(score_2014_dtree,score_2015_dtree,score_2016_dtree)
Score_rf = c(score_2014_rf,score_2015_rf,score_2016_rf)
Score_svm = c(score_2014_svm,score_2015_svm,score_2016_svm)
Score_svmpoly = c(score_2014_svmpoly,score_2015_svmpoly,score_2016_svmpoly)
Score_svmradial = c(score_2014_svmrad,score_2015_svmrad,score_2016_svmrad)

dt <- data.frame(Year,Score_logi,Score_dtree,Score_rf,Score_svm,Score_svmpoly,Score_svmradial)
dt
#   Year Score_logi Score_dtree Score_rf Score_svm Score_svmpoly
# 1 2014         44          40       37        44            43
# 2 2015        107          63       76       103            61
# 3 2016         95          48       68        96            14



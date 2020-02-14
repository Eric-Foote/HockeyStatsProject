# This library allows us to run logistic regression models
library(glmnet)
# This line reads all of the csv files into R that we are going to use as our data to create the models
Dataset<-(lapply(list.files(), read.csv))
# This line reads in the 2017-2018 data
TrainingSet <- (lapply(list.files(), read.csv))
# These lines clean the dataset
for(i in 1:270){
  Dataset[[i]]$X <- NULL
  Dataset[[i]]$gamesPlayed <- NULL
}
for(i in 1:31){
  TrainingSet[[i]]$X <- NULL
  TrainingSet[[i]]$gamesPlayed <- NULL
}
wins <- NULL
losses <- NULL
ot <- NULL
pts <- NULL
ptPctg <- NULL
goalsPerGame <- NULL
goalsAgainstPerGame <- NULL
evGAARatio <- NULL
powerPlayPercentage <- NULL
powerPlayGoals <- NULL
powerPlayGoalsAgainst <- NULL 
powerPlayOpportunities <- NULL
penaltyKillPercentage <- NULL
shotsPerGame <- NULL
shotsAllowed <- NULL
winScoreFirst <- NULL
winOppScoreFirst <- NULL
winLeadFirstPer <- NULL
winLeadSecondPer <- NULL
winOutshootOpp <- NULL
winOutshotByOpp <- NULL
faceOffsTaken <- NULL
faceOffsWon <- NULL
faceOffsLost <- NULL
faceOffWinPercentage <- NULL
shootingPctg <- NULL
savePctg <- NULL

for(i in 1:270){
  wins[i] <- Dataset[[i]]$wins
  losses[i] <- Dataset[[i]]$losses
  ot[i] <- Dataset[[i]]$ot
  pts[i] <- Dataset[[i]]$pts
  ptPctg[i] <- Dataset[[i]]$ptPctg
  goalsPerGame[i] <- Dataset[[i]]$goalsPerGame
  goalsAgainstPerGame[i] <- Dataset[[i]]$goalsAgainstPerGame
  evGAARatio[i] <- Dataset[[i]]$evGGARatio
  powerPlayPercentage[i] <- Dataset[[i]]$powerPlayPercentage
  powerPlayGoals[i] <- Dataset[[i]]$powerPlayGoals
  powerPlayGoalsAgainst[i] <- Dataset[[i]]$powerPlayGoalsAgainst
  powerPlayOpportunities[i] <- Dataset[[i]]$powerPlayOpportunities
  penaltyKillPercentage[i] <- Dataset[[i]]$penaltyKillPercentage
  shotsPerGame[i] <- Dataset[[i]]$shotsPerGame
  shotsAllowed[i] <- Dataset[[i]]$shotsAllowed
  winScoreFirst[i] <- Dataset[[i]]$winScoreFirst
  winOppScoreFirst[i] <- Dataset[[i]]$winOppScoreFirst
  winLeadFirstPer[i] <- Dataset[[i]]$winLeadFirstPer
  winLeadSecondPer[i] <- Dataset[[i]]$winLeadSecondPer
  winOutshootOpp[i] <-  Dataset[[i]]$winOutshootOpp
  winOutshotByOpp[i] <- Dataset[[i]]$winOutshotByOpp
  faceOffsTaken[i] <- Dataset[[i]]$faceOffsTaken
  faceOffsWon[i] <- Dataset[[i]]$faceOffsWon
  faceOffsLost[i] <- Dataset[[i]]$faceOffsLost
  faceOffWinPercentage[i] <- Dataset[[i]]$faceOffWinPercentage
  shootingPctg[i] <- Dataset[[i]]$shootingPctg
  savePctg[i] <-  Dataset[[i]]$savePctg
}

Dataset <- cbind(wins,losses ,ot , pts , ptPctg ,goalsPerGame , goalsAgainstPerGame ,evGAARatio ,powerPlayPercentage , powerPlayGoals ,powerPlayGoalsAgainst , powerPlayOpportunities ,penaltyKillPercentage ,shotsPerGame ,shotsAllowed ,winScoreFirst ,winOppScoreFirst ,winLeadFirstPer ,winLeadSecondPer ,winOutshootOpp ,winOutshotByOpp ,faceOffsTaken , faceOffsWon ,faceOffsLost ,faceOffWinPercentage ,shootingPctg ,savePctg )

wins <- NULL
losses <- NULL
ot <- NULL
pts <- NULL
ptPctg <- NULL
goalsPerGame <- NULL
goalsAgainstPerGame <- NULL
evGAARatio <- NULL
powerPlayPercentage <- NULL
powerPlayGoals <- NULL
powerPlayGoalsAgainst <- NULL 
powerPlayOpportunities <- NULL
penaltyKillPercentage <- NULL
shotsPerGame <- NULL
shotsAllowed <- NULL
winScoreFirst <- NULL
winOppScoreFirst <- NULL
winLeadFirstPer <- NULL
winLeadSecondPer <- NULL
winOutshootOpp <- NULL
winOutshotByOpp <- NULL
faceOffsTaken <- NULL
faceOffsWon <- NULL
faceOffsLost <- NULL
faceOffWinPercentage <- NULL
shootingPctg <- NULL
savePctg <- NULL

# These lines clean the prediction data
for(i in 1:31){
  wins[i] <- TrainingSet[[i]]$wins
  losses[i] <- TrainingSet[[i]]$losses
  ot[i] <- TrainingSet[[i]]$ot
  pts[i] <- TrainingSet[[i]]$pts
  ptPctg[i] <- TrainingSet[[i]]$ptPctg
  goalsPerGame[i] <- TrainingSet[[i]]$goalsPerGame
  goalsAgainstPerGame[i] <- TrainingSet[[i]]$goalsAgainstPerGame
  evGAARatio[i] <- TrainingSet[[i]]$evGGARatio
  powerPlayPercentage[i] <- TrainingSet[[i]]$powerPlayPercentage
  powerPlayGoals[i] <- TrainingSet[[i]]$powerPlayGoals
  powerPlayGoalsAgainst[i] <- TrainingSet[[i]]$powerPlayGoalsAgainst
  powerPlayOpportunities[i] <- TrainingSet[[i]]$powerPlayOpportunities
  penaltyKillPercentage[i] <- TrainingSet[[i]]$penaltyKillPercentage
  shotsPerGame[i] <- TrainingSet[[i]]$shotsPerGame
  shotsAllowed[i] <- TrainingSet[[i]]$shotsAllowed
  winScoreFirst[i] <- TrainingSet[[i]]$winScoreFirst
  winOppScoreFirst[i] <- TrainingSet[[i]]$winOppScoreFirst
  winLeadFirstPer[i] <- TrainingSet[[i]]$winLeadFirstPer
  winLeadSecondPer[i] <- TrainingSet[[i]]$winLeadSecondPer
  winOutshootOpp[i] <-  TrainingSet[[i]]$winOutshootOpp
  winOutshotByOpp[i] <- TrainingSet[[i]]$winOutshotByOpp
  faceOffsTaken[i] <- TrainingSet[[i]]$faceOffsTaken
  faceOffsWon[i] <- TrainingSet[[i]]$faceOffsWon
  faceOffsLost[i] <- TrainingSet[[i]]$faceOffsLost
  faceOffWinPercentage[i] <- TrainingSet[[i]]$faceOffWinPercentage
  shootingPctg[i] <- TrainingSet[[i]]$shootingPctg
  savePctg[i] <-  TrainingSet[[i]]$savePctg
}
TrainingSet <- cbind(wins,losses ,ot , pts , ptPctg ,goalsPerGame , goalsAgainstPerGame ,evGAARatio ,powerPlayPercentage , powerPlayGoals ,powerPlayGoalsAgainst , powerPlayOpportunities ,penaltyKillPercentage ,shotsPerGame ,shotsAllowed ,winScoreFirst ,winOppScoreFirst ,winLeadFirstPer ,winLeadSecondPer ,winOutshootOpp ,winOutshotByOpp ,faceOffsTaken , faceOffsWon ,faceOffsLost ,faceOffWinPercentage ,shootingPctg ,savePctg )

#This creates the correlation matrix
correlation.matrix <- cor(Dataset)
#This creates the heatmap
heatmap(correlation.matrix,Rowv = NA,Colv = NA, main = "Correlation Matrix",col = cm.colors(256))

Dataset <- data.frame(Dataset)
#This removes all the highly correlated variables
Dataset$wins <- NULL
Dataset$losses <- NULL
Dataset$ot <- NULL
Dataset$faceOffsLost <- NULL
Dataset$ptPctg <- NULL

#This creates a matrix out of our dataset to be used in the ridge regression 
modelmatrix <- model.matrix(Dataset$pts ~. -1,data = Dataset)

#This cleans the training set
TrainingSet <- data.frame(TrainingSet)

TrainingSet$wins <- NULL
TrainingSet$losses <- NULL
TrainingSet$ot <- NULL
TrainingSet$faceOffsLost <- NULL
TrainingSet$ptPctg <- NULL

newmodelmatrix <- model.matrix(TrainingSet$pts ~. -1,data = TrainingSet)

y <- Dataset$pts
newy <- TrainingSet$pts

# Linear Regression Section

null.model <- lm(pts ~ 1, data = Dataset)
full.model <- lm(pts ~., data = Dataset)
#This runs our AIC variable selection
AIC.model <- step(full.model, scope = list(lower = null.model, upper = full.model), direction = "both")
#This creates the ridge regession model the key is the alpha=0 
ridge.regression.model <- cv.glmnet(modelmatrix,y,alpha = 0)
#This function creates all of our plots
plot(AIC.model)

#This creates our predictions
ridge.prediction <- predict.cv.glmnet(ridge.regression.model, newx = newmodelmatrix)
AIC.prediction <- predict(AIC.model, TrainingSet)

plot(ridge.regression.model)

summary(full.model)

#These variables come from the p-values in the full model
pvalue.model <- lm(pts ~ winScoreFirst + winOppScoreFirst + winOutshootOpp + winOutshotByOpp, data = Dataset)

plot(pvalue.model)

pvalue.prediction <- predict(pvalue.model, TrainingSet)

AIC.error <- abs(newy - AIC.prediction)
pvalue.error <- abs(newy - pvalue.prediction)
Ridge.error <- abs(newy - ridge.prediction)

boxplot(AIC.error)
boxplot(pvalue.error)
boxplot(Ridge.error)

#Logistic Regression Section

#This variable was hardcoded

madePlayoffs <- c(1,1,0,1,0,1,1,1,1,0,0,1,1,1,0,0,0,0,1,1,1,1,1,1,0,0,1,0,0,1,1,0,0,0,0,0,1,1,0,0,0,0,1,0,1,0,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,1,0,0,1,0,0,0,0,1,0,0,0,1,0,0,1,1,0,0,0,0,1,0,1,0,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,1,0,0,0,1,1,1,1,0,1,0,1,0,0,0,0,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,1,0,1,1,1,1,1,1,0,1,0,0,0,0,0,0,0,0,0,0,1,1,0,1,1,0,1,1,1,1,1,1,1,0,1,0,1,0,1,0,1,1,1,1,1,1,1,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,0,0,1,1,1,1,1,0,0,0,1,0,1,1,1,0,0,0,0,0,0,0,0,0,1,0,1,1,1,1,0,1,0,0,1,1,1,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0)

Dataset <- data.frame(cbind(Dataset,madePlayoffs))

logistic.full.model.playoffs <- glm(madePlayoffs ~ ., data = Dataset, family = "binomial")
logistic.null.model.playoffs <- glm(madePlayoffs ~ 1, data = Dataset, family = "binomial")

madePlayoffs <- c(1,0,1,0,0,0,0,1,1,0,0,0,0,1,1,0,1,1,0,0,0,1,1,1,0,1,1,0,1,1,1)

TrainingSet <- data.frame(cbind(TrainingSet,madePlayoffs))

logistic.full.model.playoff.prediction <- predict.glm(logistic.full.model.playoffs, TrainingSet, type = "response")


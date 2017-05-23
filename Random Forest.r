####################      Random Forest     ###############################
###########################################################################
###########################################################################
###########################################################################

T <- read.csv("titanic3.csv",header=TRUE)

############create data with the n below variables#############
TI <- T[,c(1,2,4,5,6,7)]

##########show dimensions of the data$$$$$$$$$$$
dim(TI)
TI <- na.omit(TI)
dim(TI)


################### infor for a decision tree example################
table(T$sex,T$survived)
TMales <- T[which(T$sex==0),]
TMalesGT9yrs <- TMales[which(TMales$age > 9),]
TMalesLE9yrs <- TMales[which(TMales$age <= 9),]
sum(TMalesGT9yrs$survived)
sum(TMalesLE9yrs$survived)

################ install caret #######################
install.packages("caret", dependencies = TRUE)
install.packages("randomForest")
library(caret)
library(randomForest)

############# Random selection$$$$$$$$$$$$
set.seed(1711973)

######## Split data into training and test sets##
######### Create Set to test results#############
holdout <- sample(1:nrow(TI), 200, replace=F)

###########Training set#####################
TI.train <- TI[-holdout, ]   # Training set of 1109 rows

##########Test Set ########################
TI.test <-  TI[holdout, ]    # Test set of 200 rows

#############Create Table ###################
T1 <- table(TI$pclass,TI$survived)

############Chi Squared##################
chisq.test(T1)

T2 <- table(TI$sex,TI$survived)
T2
chisq.test(T2)

T3 <- table(TI$sibsp,TI$survived)
T3
chisq.test(T3)

T4 <- table(TI$parch,TI$survived)
T4
chisq.test(T4)

########### Set a random seed (so you will get the same results as me)############
set.seed(42)
############ Train the model using a "random forest" algorithm##################
m1 <- train(survived ~ pclass + sex + age + sibsp +  parch,                       
                          data = TI.train, 
                          method = "rf",trControl = trainControl(method = "cv", # Use cross-validation
                                                   number = 4),importance=TRUE) 
########## Use 4 folds for cross-validation##############3

############### Training Set ###############
predictionTrain <- predict(m1, TI.train) 
PredYTrain <- round( predictionTrain,0)  
TBL <- table(TI.train$survived,PredYTrain)   
accuracyTrain <- sum(diag(TBL))/sum(TBL)
accuracyTrain

############# Prediction Set ###############     
predictionTest <- predict(m1, TI.test) 
PredYTest <- round( predictionTest,0)
TBL <- table(TI.test$survived,PredYTest)   
accuracy <- sum(diag(TBL))/sum(TBL)
accuracy

###########################################################################
###########################################################################
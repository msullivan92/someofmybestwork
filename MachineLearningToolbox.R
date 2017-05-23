
####################          Machine Learning Toolbox Script        #############################



#########################         Read In Data and Packages      #################################
##################################################################################################

#########################             Linear Modeling            #################################
##################################################################################################


# Shuffle row indices: rows
rows=sample(nrow(diamonds))

# Randomly order data
diamonds=diamonds[rows,]

# Determine row to split on: split
split.percent=0.8
split=round(nrow(diamonds)*split.percent)

# Create train
train=diamonds[1:split,]

# Create test
test=diamonds[(split+1):nrow(diamonds),]

# Fit lm model on train: model
model=lm(price~.,data=train)

# Predict on test: p
p=predict(model,test)

# Compute errors: error
error=test$price-p

# Calculate RMSE
sqrt(mean(error^2))

# Fit lm model using 10-fold CV: model
model <- train(
  price~ ., diamonds,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 10,
    verboseIter = TRUE
  )
)

# Print model to console
model

# Fit lm model using 5-fold CV: model
model <- train(
  medv~., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    verboseIter = TRUE
  )
)

# Print model to console
model

# Fit lm model using 5 x 5-fold CV: model
model <- train(
  medv ~ ., Boston,
  method = "lm",
  trControl = trainControl(
    method = "cv", number = 5,
    repeats = 5, verboseIter = TRUE
  )
)

# Print model to console
model

# Predict on full Boston dataset
predict(model,Boston)



#########################               Logit Rally              #################################
##################################################################################################

# Shuffle row indices: rows
rows = sample(nrow(Sonar))

# Randomly order data: Sonar
Sonar = Sonar[rows,]

# Identify row to split on: split
split <- round(nrow(Sonar) * 0.60)

# Create train
train = Sonar[1:split,]

# Create test
test = Sonar[(split+1):nrow(Sonar),]

# Fit glm model: model
model = glm(Class~.,family="binomial",data=train)

# Predict on test: p
p=predict(model,test,type="response")

# Calculate class probabilities: p_class
p_class=ifelse(p>0.50,"M","R")

# Create confusion matrix
confusionMatrix(p_class,test[["Class"]])

# Apply threshold of 0.9: p_class
p_class=ifelse(p>0.9,"M","R")

# Create confusion matrix
confusionMatrix(p_class,test[["Class"]])

# Apply threshold of 0.10: p_class
p_class=ifelse(p>0.1,"M","R")

# Create confusion matrix
confusionMatrix(p_class,test[["Class"]])

# Predict on test: p
p <- predict(model, test, type = "response")

# Make ROC curve
colAUC(p, test[["Class"]], plotROC = TRUE)

# Create trainControl object: myControl
myControl <- trainControl(
  method = "cv",
  number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Train glm with custom trainControl: model
model=train(Class~.,method="glm",family="binomial",trControl=myControl,data=Sonar)


# Print model to console
model



#########################             Random Forest              #################################
##################################################################################################


# Fit random forest: model
model <- train(
  quality~.,
  tuneLength = 1,
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model

# Fit random forest: model
model <- train(
  quality~.,
  tuneLength = 3,
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model

# Plot model
plot(model)

# Fit random forest: model
model <- train(
  quality~.,
  tuneGrid = data.frame(mtry=c(2,3,7)),
  data = wine, method = "ranger",
  trControl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
)

# Print model to console
model

# Plot model
plot(model)

# Create custom trainControl: myControl
myControl <- trainControl(
  method = "cv", number = 10,
  summaryFunction = twoClassSummary,
  classProbs = TRUE, # IMPORTANT!
  verboseIter = TRUE
)

# Fit glmnet model: model
model <- train(
  y~., 
  method = "glmnet",
  trControl = myControl
  ,data=overfit)

# Print model to console
model

# Print maximum ROC statistic
max(model[["results"]][["ROC"]])

# Train glmnet with custom trainControl and tuning: model
model <- train(
  y~., data=overfit,
  tuneGrid = expand.grid(alpha=0:1,lambda=seq(0.0001,1,length=20)),
  method = "glmnet",
  trControl = myControl
)

# Print model to console
model

# Print maximum ROC statistic
max(model[["results"]][["ROC"]])
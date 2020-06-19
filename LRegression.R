############ GLM Logistic Regression  ############

# Set the working directory
setwd("/Users/singhh/Downloads/CSCI48900/titanic")

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)

str(train)

# Convert the variables
train$Survived <- as.factor(as.character(train$Survived))
train$Pclass <- as.factor(as.character(train$Pclass))
train$Sex <- as.factor(as.character(train$Sex))
train$Embarked <- as.factor(as.character(train$Embarked))

# Convert the variables
test$Pclass <- as.factor(as.character(test$Pclass))
test$Sex <- as.factor(as.character(test$Sex))
test$Embarked <- as.factor(as.character(test$Embarked))

# Get summary
summary(train)

# Make a subset with missing age
missing <- subset(train, is.na(Age))
summary(missing)

library(ggplot2)

# Visualize the data
ggplot() + 
  geom_histogram(data = train, mapping = aes(x = Fare), fill = "blue", bins = 5) +
  geom_histogram(data = missing, mapping = aes(x = Fare), fill = "red", bins = 5) + 
  facet_grid(.~ Pclass)

table(missing$Pclass, missing$Sex)

library(mice)
imputed <- complete(mice(train,m=5,maxit=50,meth='pmm',seed=500))

# Visualize the data
ggplot() +
  geom_histogram(data = imputed, mapping = aes(x = Age), fill = "red", bins = 10) +
  geom_histogram(data = train, mapping = aes(x = Age), fill = "black", bins = 10, alpha = 0.4) +
  facet_grid(. ~ Sex)

# Baseline model for prediction
table(train$Survived)

ggplot() +
  geom_histogram(data = imputed, mapping = aes(x = Survived, fill = factor(Sex)), alpha = 0.8, stat="count")
ggplot() +
  geom_histogram(data = imputed, mapping = aes(x = Age, fill = factor(Survived)), bins = 10) +
  facet_grid(. ~ Sex)
ggplot() +
  geom_histogram(data = imputed, mapping = aes(x = Pclass, fill = factor(Survived)), stat="count") +
  facet_grid(. ~ Sex)
ggplot() +
  geom_histogram(data = imputed, mapping = aes(x = SibSp, fill = factor(Survived)), stat="count") +
  facet_grid(. ~ Sex)
ggplot() +
  geom_histogram(data = imputed, mapping = aes(x = Parch, fill = factor(Survived)), stat="count") +
  facet_grid(. ~ Sex)
ggplot() +
  geom_histogram(data = imputed, mapping = aes(x = Embarked, fill = factor(Survived)), stat="count") +
  facet_grid(. ~ Sex)

# Build the logistic regression model
imputed <- subset(imputed, select = -c(PassengerId))
model1 <- glm(Survived ~ Sex + Pclass + Embarked + SibSp, data = imputed, family=binomial)
predict1 <- predict(model1, type="response")

a <- table(imputed$Survived, predict1 >= 0.5)

TP <- a[2,2] 
TN <- a[1,1] 
FP <- a[1,2] 
FN <- a[2,1] 

sensitivity <- TP/(TP+FN)
specificity <- TN/(TN+FN)
accuracy <- (TN + TP)/(TN + TP + FP + FN)

# Final prediction
prediction <- predict(model1, newdata=test, type = "response")
KaggleSubmission5 <- data.frame(PassengerID = test$PassengerId, Survived = round(prediction, 0))
write.csv(KaggleSubmission5, file = 'KaggleSubmission5.csv', row.names = FALSE)



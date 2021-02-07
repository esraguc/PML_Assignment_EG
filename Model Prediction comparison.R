#Training and testing data have been downloaded on local hardrive
library(caret)
library(lattice)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(corrplot)
library(randomForest)
library(rattle)
set.seed(123)
#Load the raw data
train_dat<- read.csv("pml-training.csv")[,-1]
test_dat<- read.csv("pml-testing.csv")[,-1]
dim(train_dat)
dim(test_dat)
#There are 159 variable in this data set however some variables have a lot of NA, some has almost 0 variation 
#also first 5 variables wont be used in training so these are removed
Zerovar<- nearZeroVar(train_dat)
training<- train_dat[,-Zerovar]
testing<- test_dat[,-Zerovar]
NAVal <- sapply(training, function(x) mean(is.na(x))) > 0.95
training <- training[, NAVal == "FALSE"]
testing <- testing[, NAVal== "FALSE"]
training <- training[,-c(1:5)]
testing <- testing[,-c(1:5)]
dim(training)
dim(testing)
#variable is reduced to 53

#Preparation of training and testing dataset before testing the models
inTraining<- createDataPartition(y=training$classe, p=0.7, list=FALSE)
train_final<- training[inTraining, ]
test_final<- training[-inTraining, ]
# Decision Tree Modeling #I classified the data argument in as.factor so data and reference factors will be in the same number of levels
mod_DT <- train(classe ~ .,data=train_final, method = "rpart")
predict_train_DT <- predict(mod_DT, train_final)
ConfM_train_DT <- confusionMatrix(predict_train_DT,as.factor(train_final$classe))
predict_test_DT <- predict(mod_DT, test_final)
ConfM_test_DT <- confusionMatrix(predict_test_DT,as.factor(test_final$classe))
print(ConfM_test_DT)
print(summary(mod_DT))
# Decisiont Tree modeling accuracy is 0.4997
# Random Forest, data processing takes very long, I got this tip from Coursera Forum, data needs to be tuned using tuneGrid
mod_RF <- train(classe ~ .,data=train_final, method = "rf", trControl=trainControl(method="none"), tuneGrid=data.frame(mtry=7))
predict_train_RF <- predict(mod_RF, train_final)
ConfM_train_RF <- confusionMatrix(predict_train_RF,as.factor(train_final$classe))
predict_test_RF <- predict(mod_RF, test_final)
ConfM_test_RF <- confusionMatrix(predict_test_RF,as.factor(test_final$classe))
print(ConfM_test_RF)
print(summary(mod_RF))


#When these two methods are compared Random Forest model predicts with higher accuracy overall 0.9944 .

print(predict(mod_RF, newdata=testing))




install.packages('data.table')
install.packages('speedglm')
library(data.table)
library(dplyr)
library(corrplot)
library(speedglm)
library(ROCR)

rm(list = ls())

#SetWorking Directory
setwd(path)
getwd()

#Load Data
train = fread('D:/Data Science/Miscellaneous/Santander Customer Transaction Prediction/train.csv')
test = fread('D:/Data Science/Miscellaneous/Santander Customer Transaction Prediction/test.csv')

#Check Structure of the dataset
glimpse(train)
glimpse(test)

#Check if ID column has unique value for each observation
train$ID_code %>% unique %>% length
test$ID_code %>% unique %>% length

#Convert data type of target variable from numeric to factor
train$target = as.factor(train$target)
str(train)

#Missing Value Analysis
MissingValue_Train = data.frame(apply(train,2,function(f){sum(is.na(f))}))
unique(MissingValue$apply.train..2..function.f...)
MissingValue_Test = data.frame(apply(test,2,function(f){sum(is.na(f))}))

#Correlation Analysis
cormat <- cor(train[,-c(1,2)])
summary(cormat[upper.tri(cormat)])

##Modelling
##Logistic Regression
trainData = as.data.frame(scale(train[,-c(1,2)]))
testData = as.data.frame(scale(test[,-1]))
target = getElement(train,"target")

LogisticModel = glm(target ~., data = trainData, family = "binomial")
summary(LogisticModel)
Test_Prediction = predict(LogisticModel, newdata = testData, type = "response")

table(ActualValue = train$target, PredictedValue = Test_Prediction > 0.5)

#Plotting ROC Curve
ROCRPred = prediction(Test_Prediction, train$target)
ROCRPerf = performance(prediction.obj = ROCRPred, "tpr", "fpr")

plot(ROCRPerf, colorize = TRUE, print.cutoff.at = seq(0.1, by = 0.1))

#Writing into the submission file.
Test_Prediction_Modified = ifelse(Test_Prediction > 0.5,1,0)
submission = read.csv('sample_submission.csv')
submission$target = Test_Prediction_Modified
write.csv(submission, file="finalSubmission.csv", row.names=F)



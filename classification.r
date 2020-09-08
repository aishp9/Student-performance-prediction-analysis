##setwd(apfiles)
##################### Classification Decision Tree #############################
##reading the data
source("st-utils.R")
#maths = readStudentdata()
maths<-read.csv("stud_math.csv")
maths = factorizeStudentdata(maths)
findMissing(maths)
attach(maths)
str(maths)
summary(maths)

#create training and validation data from given data
library(caTools)
set.seed(123)
split <- sample.split(maths, SplitRatio = 2/3)
train_data <- subset(maths, split == TRUE)
test_data <- subset(maths, split == FALSE)

#Building Decision tree
library(rpart)
library(rpart.plot)
fit <- rpart(train_data$schoolsup ~ . -G1 -G2 -G3, data = train_data, method = 'class')
fit
summary(fit)
rpart.plot(fit, extra = 106)
train_tab = predict(fit,train_data, type = 'class')
mat_train = table(train_tab,train_data$schoolsup)
mat_train
accuracy_Train <- sum(diag(mat_train)) / sum(mat_train)
accuracy_Train

#Predicting on Test data
predict_unseen = predict(fit, test_data, type = 'class')
table_mat <- table(test_data$schoolsup, predict_unseen)
table_mat
accuracy_Test <- sum(diag(table_mat)) / sum(table_mat)
accuracy_Test
#Pruned Tree
fit_pruned = prune.rpart(fit, cp = 0.05)
fit_pruned
rpart.plot(fit_pruned, extra = 106)
train_tab_p = predict(fit_pruned,train_data, type = 'class')
mat_train_p = table(train_tab_p,train_data$schoolsup)
mat_train_p
accuracy_Train_p <- sum(diag(mat_train_p)) / sum(mat_train_p)
accuracy_Train_p

#Predicting on Test data
predict_unseen_p = predict(fit_pruned, test_data, type = 'class')
table_mat_p <- table(test_data$schoolsup, predict_unseen_p)
table_mat_p
accuracy_Test_p <- sum(diag(table_mat_p)) / sum(table_mat_p)
accuracy_Test_p

#########################   Random Forest   ##############################

library(randomForest)
set.seed(10)
RFModel <- randomForest(train_data$schoolsup ~ . -G1 -G2 -G3, data=train_data, importance=TRUE, ntree=5000)
RFModel
Mat_RF = RFModel$confusion
Mat_RF
Conf_Mat_RF = Mat_RF[,-3]
accuracy_RF = sum(diag(Conf_Mat_RF)) / sum(Conf_Mat_RF)
accuracy_RF
#varImpPlot(RFModel)
library(caret)
library(e1071)
RFImp =varImp(RFModel)
RFImp
varImpPlot(RFModel)

RFPrediction = predict(RFModel, test_data)
RFConfMat = confusionMatrix(RFPrediction, test_data[,"schoolsup"])
RFConfMat
RF_Test_mat = RFConfMat$table
accuracy_RF_test = sum(diag(RF_Test_mat)) / sum(RF_Test_mat)
accuracy_RF_test

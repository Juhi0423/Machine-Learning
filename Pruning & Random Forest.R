#  We Use the IBM HR Analytics data set.
library(data.table)
library(rpart)
library(rattle)
ibm <- fread("E:/Machine Learning/CLASSES/Lecture 06 - Pruning and Random Forest/HR Analytics.csv")
ibm_train <- ibm[sample(seq(1,nrow(ibm)),0.7*nrow(ibm)),]
ibm_test <- ibm[sample(seq(1,nrow(ibm)),0.3*nrow(ibm)),]
str(ibm)
dim(ibm_train)
dim(ibm_test)
# Here we are doing the Classification so output variable should be factor

ibm$Attrition <- as.factor(ibm$Attrition)
ibm_test$Attrition <- as.factor(ibm_test$Attrition)
ibm_train$Attrition <- as.factor(ibm_train$Attrition)

model = rpart(Attrition ~ Gender + MonthlyIncome + OverTime,data = ibm_train)
fancyRpartPlot(model)
# ** The Output column should always be factor to get the exact probability for each class
pred_class <- predict(model,ibm_test,type = "class")

# Accuracy Manually done
sum(ibm_test$Attrition == pred_class)/length(pred_class)*100
# Using Metric Package
library(Metrics) # For Accuracy Function
accuracy(ibm_test$Attrition,pred_class)

#======== Confusuion Matrix
library(caret)
# Confusion Matrix Manually
table(ibm_test$Attrition,pred_class)

# confusuionMatrix
# First Argument should always be Predicted Values
# Second Argument should always be the Actual Values
cm <- confusionMatrix(pred_class, ibm_test$Attrition,positive = "1")
cm

# Calculating Manually
TN <- sum(ibm_test$Attrition == pred_class & pred_class == 0)
TP <- sum(ibm_test$Attrition == pred_class & pred_class == 1)
FN <- sum(ibm_test$Attrition != pred_class & pred_class == 0)
FP <- sum(ibm_test$Attrition != pred_class & pred_class == 1)


# Accuracy Manually

acc = (TN + TP)/(TP+FP+TN+FN)
sens = (TP)/(TP+FN)

#==============rpart.control
# cp = complexity parameter
# cp = -1 grows the tree to full possible extent which is not optimal
model <- rpart(Attrition~JobRole,data = ibm,control = rpart.control(cp = -1))
fancyRpartPlot(model)
printcp(model)


model <- rpart(Attrition~JobRole + MonthlyIncome + OverTime,data = ibm)
printcp(model)

model = rpart(Attrition ~ Gender + MonthlyIncome + OverTime,data = ibm_train)
printcp(model)
#Overfitting model is aligining the model too accurate for only the training data
#The less the cp , the model will align more to training set
#So the accuracy of training data will be around 100,but for test the model does not perform well.
# cp will be based on the number of terminal nodes at each level.
#minsplit = minimum number of samples required to further split the node into branch
#minbucket = minimum number of samples required in either of branches after splitting
# Generally minbucket = round(minsplit/3)


#============= Ensemble Methods




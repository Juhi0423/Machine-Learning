##=========== Performance Benchmarking
View(ibm)
library(data.table)
library(rpart)
library(rattle)
library(randomForest)
library(caret)

ibm <- fread("E:/Machine Learning/CLASSES/Lecture 06 - Pruning and Random Forest/HR Analytics.csv",stringsAsFactors = T)
set.seed(100)
ibm_train <- ibm[sample(seq(1,nrow(ibm)),0.7*nrow(ibm)),]
ibm_test <- ibm[sample(seq(1,nrow(ibm)),0.3*nrow(ibm)),]

ibm_test$Attrition <- as.factor(ibm_test$Attrition)
ibm_train$Attrition <- as.factor(ibm_train$Attrition)

rand.model <- randomForest(Attrition ~ .,ibm_train)
pred.val <- predict(rand.model,ibm_test,type = 'prob')

# Now validating our model
# We can predict the class directly but to evaluate we use probability values.

## Problem Statement : How accurately we are able o correctly classify the employees who 
## leave the Organization.
ibm_test$pred_class <- ifelse(pred.val[,2] > 0.5,1,0)
ibm_test$pred_class <- as.factor(ibm_test$pred_class)
rand.cm <- confusionMatrix(ibm_test$pred_class,ibm_test$Attrition,positive = "1")
rand.cm

## No Information Rate
# The % Accuracy that can be acquired by predicting the values without building model
# Example : Consider the % of 0's & 1's in Attrition Values
table(ibm_train$Attrition)/nrow(ibm_train)
# Therefore, Predicting all values to be 0's will give me 83% Accuracy


#=============== Fine Tuning the Parameters
# Changing the Theshold Probabaility for classifying the Classes.
# if probability of a row becoming 1 is greater than 0.4 then classify as 1
ibm_test$pred_class0.4 <- ifelse(pred.val[,2] > 0.4,1,0)
ibm_test$pred_class0.4 <- as.factor(ibm_test$pred_class0.4)
rand.cm0.4 <- confusionMatrix(ibm_test$pred_class0.4,ibm_test$Attrition,positive = "1")
rand.cm0.4

# if probability of a row becoming 1 is greater than 0.3 then classify as 1
ibm_test$pred_class0.3 <- ifelse(pred.val[,2] > 0.3,1,0)
ibm_test$pred_class0.3 <- as.factor(ibm_test$pred_class0.3)
rand.cm0.3 <- confusionMatrix(ibm_test$pred_class0.3,ibm_test$Attrition,positive = "1")
rand.cm0.3


#============== ROC Curves
library(ROCR)
library(pROC)
roc_rf <- roc(ibm_test$Attrition,pred.val[,2])
plot(roc_rf)

## These are the different threshold probabilities checked by software for drawing curve
roc_rf$thresholds

## Senesitivity for the different probabilities
roc_rf$sensitivities
## Specificities for the different probabilities
roc_rf$specificities


#================ KNN Algorithm

dummy_obj <- dummyVars(~.,data = ibm %>% select(-Over18))
ibm_knn <- data.frame(predict(dummy_obj,ibm))
View(ibm_knn)

# Normalize or Scaling the Data.
library(BBmisc)
library(class)
View(ibm_knn_norm)
ibm_knn_norm <- normalize(ibm_knn,method = "range",range = c(0,1))
ibm_train <- ibm_knn_norm[sample(seq(1,nrow(ibm_knn_norm)),0.7*nrow(ibm_knn_norm)),]
ibm_test <- ibm_knn_norm[sample(seq(1,nrow(ibm_knn_norm)),0.3*nrow(ibm_knn_norm)),]

knn.prob <- knn(ibm_train %>% select(-Attrition)
                        ,ibm_test %>% select(-Attrition),
                        cl = as.factor(ibm_train$Attrition),k = 8,prob = TRUE)

class <- as.vector(knn.prob)
prob <- attributes(knn.prob)[[3]]
comp_class <- 1-as.numeric(class)
comp_prob <- 1-prob
df = data.frame(matrix(ncol = 2,nrow = nrow(ibm_test)))
names(df) <- c("0","1")
df$`0` <- ifelse(class == 0,prob,comp_prob)
df$`1` <- ifelse(class == 1,prob,comp_prob)
df
#===== ROC Curve

roc_knn <- roc(ibm_test$Attrition,df$`1`)
plot(roc_knn,col = "blue")
lines(roc_rf,col = "red")
# When we Observe these two curves between 2 Models
# Random Forest has higher area than KNN
# So we conclude that Random Forest is the best model

#====== Area under AOC Curve
auc(roc_rf)
auc(roc_knn)

#** Finding the Optimal Probability Value
#=== Random Forest
# Wrong Value ---------- Not Working Correctly
pred_obj <- prediction(pred.val[,2],as.factor(ibm_test$Attrition))
cost.pred <- performance(pred_obj,'cost')
y.val <- cost.pred@y.values[[1]]
x.val <- pred_obj@cutoffs[[1]]
x.val <- x.val[!x.val %in% c(Inf,-Inf)]
y.val <- y.val[!y.val %in% c(Inf,-Inf)]
x.val[which.min(y.val)]

pred_obj@cutoffs[[1]]
which.min(cost.pred@y.values[[1]])


#======== Probability Callibration

histogram(pred.val[,2])
# From the above graph we can clearly say that the most of probabilities 
# are present betweeen 0 & 0.3, This is un-even distributed probabilities
# So we need to perform the Probability Callibration


# For this Data set  Callibration is not required as Sensitivity & Specificity are close to 1
mushroom <- read.csv("E:/Machine Learning/CLASSES/Lecture 11 - Performance Benchmarking/mushroom_full.csv")
mushroom_train <- mushroom[sample(1:nrow(mushroom),0.7*nrow(mushroom)),]
mushroom_test <- mushroom[sample(1:nrow(mushroom),0.3*nrow(mushroom)),]

mush_rf <- randomForest(class~.,data = mushroom_train)
mush_pred <- predict(mush_rf,mushroom_test,type = "class")
mush_pred.probs <- predict(mush_rf,mushroom_test,type = "prob")
confusionMatrix(mush_pred,mushroom_test$class,positive = "POISONOUS")


# Performing Callibration
mushroom_test$pred.prob <- mush_pred.probs[,2]

# bin 0.1 - 0.2
mush_subset = mushroom_test %>% filter(pred.prob > 0.1 & pred.prob < 0.2)
nrow(mush_subset %>% filter(class == "POISONOUS"))/nrow(mush_subset)

# bin 0.2 - 0.3
mush_subset = mushroom_test %>% filter(pred.prob > 0.2 & pred.prob < 0.3)
nrow(mush_subset %>% filter(class == "POISONOUS"))/nrow(mush_subset)

# bin 0.3 - 0.4
mush_subset = mushroom_test %>% filter(pred.prob > 0.3 & pred.prob < 0.4)
nrow(mush_subset %>% filter(class == "POISONOUS"))/nrow(mush_subset)

#======= Updating Bins Automatically
start_bin = 0
x_vals <- c()
y_vals <- c()
for(i in seq(0,1,0.05))
{
  start_bin = i
  end_bin  = i + 0.05
  x_vals <- c(x_vals,(start_bin + end_bin)/2)
  df_subset = mushroom_test %>% filter(pred.prob > start_bin & pred.prob <= end_bin)
  curr_y = nrow(df_subset %>% filter(class == "POISONOUS"))/nrow(df_subset)
  y_vals <- c(y_vals,curr_y)
}
plot(x_vals,y_vals,type = "b")


##======== Callibration for HR Dataset

ibm_test$pred_prob <- pred.val[,2]
start_bin = 0
x_vals <- c()
y_vals <- c()
for(i in seq(0,1,0.05))
{
  start_bin = i
  end_bin  = i + 0.05
  x_vals <- c(x_vals,(start_bin + end_bin)/2)
  df_subset = ibm_test %>% filter(pred_prob > start_bin & pred_prob <= end_bin)
  curr_y = nrow(df_subset %>% filter(Attrition == 1))/nrow(df_subset)
  y_vals <- c(y_vals,curr_y)
}
plot(x_vals,y_vals,type = "b")

## Based on the Above Graph, The Graph is not aligned or near to 45 Degree.
## So we need to perform the Probability Callibration on the Dataset
## Performing Callibration
ibm <- fread("E:/Machine Learning/CLASSES/Lecture 06 - Pruning and Random Forest/HR Analytics.csv",stringsAsFactors = T)
set.seed(100)
ibm_train <- ibm[sample(seq(1,nrow(ibm)),0.7*nrow(ibm)),]
ibm_test <- ibm[sample(seq(1,nrow(ibm)),0.3*nrow(ibm)),]

ibm_test$Attrition <- as.factor(ibm_test$Attrition)
ibm_train$Attrition <- as.factor(ibm_train$Attrition)

rand.model <- randomForest(Attrition ~ .,ibm_train)
train_probs <- predict(rand.model,ibm_train,type = 'prob')
train_probs <- as.data.frame(train_probs)
print(train_probs)
train_probs$class <- ibm_train$Attrition

## Callibration using Logistic Regeression
calib_model <- glm(class ~ `1`,data = train_probs,family = binomial)
calib_model

test_prob <- predict(rand.model,ibm_test,type = 'prob')
test_prob <- as.data.frame(test_prob)
names(test_prob) <- c("0","1")
test_prob$new_prob_1 <- predict(calib_model,test_prob,type = "response")


test_prob$pred_class <- as.factor(ifelse(test_prob$`1` > 0.5,1,0))
test_prob$pred_class_new <- as.factor(ifelse(test_prob$new_prob_1 > 0.5,1,0))

##  Confusion 
confusionMatrix(test_prob$pred_class,ibm_test$Attrition,positive = "1")
confusionMatrix(test_prob$pred_class_new,ibm_test$Attrition,positive = "1")

## Updated Threshold

library(ROCR)
pred_obj <- prediction(test_prob[,'new_prob_1'],as.factor(ibm_test$Attrition))
class(pred_obj)
cost.perf <- performance(pred_obj,'cost')
class(cost.perf)
threshold <- pred_obj@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]

test_prob$pred_class_new2 <- ifelse(test_prob$new_prob_1 > threshold,1,0)
test_prob$pred_class_new2 <- as.factor(test_prob$pred_class_new2)
confusionMatrix(test_prob$pred_class_new2,ibm_test$Attrition,positive = "1")


## Summary

m2 <- roc(ibm_test$Attrition,test_prob$new_prob_1)
m1 <- roc(ibm_test$Attrition,test_prob$`1`)
plot(m1)
lines(m2,col = 'red')
#========= Parameters of Confusion Matrix
# Precision
# Recall
# F1 Score
# Kappa












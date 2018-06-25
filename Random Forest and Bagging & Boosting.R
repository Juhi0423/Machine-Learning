library(randomForest)
library(caret)

ibm <- fread("E:/Machine Learning/CLASSES/Lecture 06 - Pruning and Random Forest/HR Analytics.csv",stringsAsFactors = T)
ibm_train <- ibm[sample(seq(1,nrow(ibm)),0.7*nrow(ibm)),]
ibm_test <- ibm[sample(seq(1,nrow(ibm)),0.3*nrow(ibm)),]
ibm_train$Attrition <- as.factor(ibm_train$Attrition)
ibm_test$Attrition <- as.factor(ibm_test$Attrition)

# Normal Decision Tree
model <- rpart(Attrition~.,data = ibm_train)
pred <- predict(model,ibm_test,type = "class")
cm <- confusionMatrix(pred,ibm_test$Attrition,positive = '1')
cm


# Random Forest Implementation using package.
# mtry =  no.of predictors at a time that is randomly selected
# ntree = no.of decision trees we grow.
mtry = round(sqrt(length(colnames(ibm_train))-1))
model.rf <- randomForest(Attrition~.,data = ibm_train,ntree = 400,mtry = mtry)
pred <- predict(model.rf,ibm_test)
cm <-confusionMatrix(pred,ibm_test$Attrition,positive = "1")
acc <- cm$overall['Accuracy']*100
sens <- cm$byClass['Sensitivity']*100


#========== Building Many Models Automatically
mtry = round(sqrt(length(colnames(ibm_train))-1))
result <- data.frame(matrix(nrow = 0,ncol = 3),stringsAsFactors = F)
for(i in 10:400)
{
  model.rf <- randomForest(Attrition~.,data = ibm_train,ntree = i,mtry = mtry)
  pred <- predict(model.rf,ibm_test)
  cm <-confusionMatrix(pred,ibm_test$Attrition,positive = "1")
  acc <- cm$overall['Accuracy']*100
  sens <- cm$byClass['Sensitivity']*100
  res <- data.frame(i,acc,sens,stringsAsFactors = F)
  result <- rbind(result,res)
}

View(result)
plot(result$i,result$acc,type = "l")
plot(result$i,result$sens,type = "l")
plot(result$acc,result$sens)

which(result$acc == max(result$acc))
which(result$sens == max(result$sens))

#================ Implementing Random Forest Manually 

# Removing the Output Column and finding the Colnames
input_pred <- colnames(ibm_train %>% select(-Attrition))
mtry <- round(sqrt(length(input_pred)))
ntree = 10
result <- data.frame(matrix(nrow = nrow(ibm_test),ncol = 0),stringsAsFactors = F)
for(i in 1:10)
{
  # Each Iteration we select 6 randomly picked input columns
  samp_pred <- input_pred[sample(1:length(input_pred),mtry)]
  samp_index <- sample(seq(1,nrow(ibm_train)),0.6*nrow(ibm_train))
  samp_data <- subset(ibm_train,select = c(samp_pred,'Attrition'))
  samp_data <- samp_data[samp_index,]
  curr_model <- rpart(Attrition~.,data = samp_data)
  result[,paste0("Tree:",i)] <- predict(curr_model,ibm_test,type = "class")
}
View(result)
result$Count_0 <- rowSums(result == 0)
result$Count_1 <- rowSums(result == 1)
result$pred <- ifelse(result$Count_0 > result$Count_1,0,1)
result$actual <- ibm_test$Attrition
sum(result$actual == result$pred)/length(result$actual)

#=============== Out of Bag Error
# Mostly used when the entire data sample is very less number.
# We dont split the data into Test and Train Dataset
# Instead we pass the entire data to the model.
# Each tree built is a Bag here.
# If a particular row, row 1: is present in n bags and absent in m bags
# We consider those m bags and pass this row as testing data for those bags
# we can calculate the error based on the predictions made on this test data.
ibm$Attrition <- as.factor(ibm$Attrition)
input_pred <- colnames(ibm_train %>% select(-Attrition))
mtry <- round(sqrt(length(input_pred)))
ntree = 10
result <- data.frame(matrix(nrow = 0.4*nrow(ibm),ncol = 0),stringsAsFactors = F)
seq <- seq(1,nrow(ibm))
for(i in 1:10)
{
  # Each Iteration we select 6 randomly picked input columns
  samp_pred <- input_pred[sample(1:length(input_pred),mtry)]
  samp_index <- sample(seq(1,nrow(ibm)),0.6*nrow(ibm))
  non_samp_index <- seq[!seq %in% samp_index]
  non_samp_data <- ibm[non_samp_index,]
  samp_data <- subset(ibm,select = c(samp_pred,'Attrition'))
  samp_data <- samp_data[samp_index,]
  curr_model <- rpart(Attrition~.,data = samp_data)
  result[,paste0("Tree:",i)] <- predict(curr_model,non_samp_data,type = "class")
  result[,paste0("Actual:",i)] <- non_samp_data$Attrition
}
View(result)
result$Count_0 <- rowSums(result == 0)
result$Count_1 <- rowSums(result == 1)
result$pred <- ifelse(result$Count_0 > result$Count_1,0,1)
result$actual <- ibm$Attrition
sum(result$actual == result$pred)/length(result$actual)

#================= Boosting
# Each of values mis-classified in the model-1 have high weight in the Model - 2
# This is a sequential process where model - 2 depends upon output of model-1
library(adabag)
model <- boosting(Attrition ~ .,data = ibm_train)
pred_obj <- predict(model,ibm_test)
pred_class <- pred_obj$class
pred_class <- as.factor(pred_class)
confusionMatrix(pred_class,ibm_test$Attrition,positive = "1")


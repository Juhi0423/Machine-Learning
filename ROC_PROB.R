library(dplyr)
library(randomForest)
library(caret)

setwd("C:/Users/Administrator/Desktop/ML")
set.seed(100)
hr<-read.csv("HR Analytics.csv")
hr$Attrition<-as.factor(hr$Attrition)
hr_train<-hr[1:(0.7*nrow(hr)),]
hr_test<-hr[(0.7*nrow(hr)+1):nrow(hr),]

##### Random Forest

model_rf<-randomForest(Attrition~.,data=hr_train)
pred_probs<-predict(model_rf,hr_test,type='prob')
View(pred_probs)
##### 0.5
hr_test$pred_class<-ifelse(pred_probs[,2]>0.5,1,0)
hr_test$pred_class<-as.factor(hr_test$pred_class)
confusionMatrix(hr_test$pred_class,hr_test$Attrition,positive = '1')


#####0.4
hr_test$pred_class<-ifelse(pred_probs[,2]>0.4,1,0)
hr_test$pred_class<-as.factor(hr_test$pred_class)
confusionMatrix(hr_test$pred_class,hr_test$Attrition,positive = '1')


#####0.3
hr_test$pred_class<-ifelse(pred_probs[,2]>0.3,1,0)
hr_test$pred_class<-as.factor(hr_test$pred_class)
confusionMatrix(hr_test$pred_class,hr_test$Attrition,positive = '1')



#### ROC Curves
library(ROCR)
library(pROC)
x=roc(hr_test$Attrition,pred_probs[,2])
plot(x) 


x$thresholds
table(hr_test$Attrition)


####KNN
# Convert the Categorical Data into Numerical Data
# ** We should remove the Output Column when it is a Factor/Character

dummy_obj <- dummyVars(~.,data = hr %>% select(-Over18))
hr_knn <- data.frame(predict(dummy_obj,newdata=hr))

# Normalize or Scaling the Data.
library(BBmisc)
library(class)

hr_norm <- normalize(hr_knn,method = "range",range = c(0,1))
hr_train<-hr_norm[1:(0.7*nrow(hr)),]
hr_test<-hr_norm[(0.7*nrow(hr)+1):nrow(hr),]
hr_test$pred_probs_knn<-knn(hr_train ,
                    hr_test,
                    cl=as.factor(hr_train$Attritsion),
                    k=10,prob=TRUE)

hr_test$Attrition <- as.factor(hr_test$Attrition)
hr_train$Attrition <- as.factor(hr_train$Attrition)
hr_test$pred_probs_knn = as.factor(hr_test$pred_probs_knn)
confusionMatrix(hr_test$pred_probs_knn,hr_test$Attrition,positive = '1')


#####ROC
probs=data.frame(prob=attr(pred_probs_knn,'prob'),
                 class=pred_probs_knn)
probs[probs['class']==0,'prob'] =1 -
probs[probs['class']==0,'prob']

roc_knn=roc(as.factor(hr_test$Attrition),probs$prob)
{{plot(x)
  lines(roc_knn,col='red')
}}


##### Area Under Curve
auc(x)
auc(roc_knn)

pred_obj<- prediction(pred_probs[,2],as.factor(hr_test$Attrition))
cost.perf<-performance(pred_obj,'cost')
pred_obj@cutoffs[[1]][which.min(cost.perf@y.values[[1]])]
hr_test$new_class=as.factor(ifelse(pred_probs[,2]>0.43,1,0))
confusionMatrix(hr_test$new_class,as.factor(hr_test$Attrition),positive = '1')


####### Probabilities Calibration
####Mushroom dataset
mus<-read.csv("mushroom_full.csv")
mus_train<-mus[1:(0.7*nrow(mus)),]
mus_test<-mus[(0.7*nrow(mus)+1):nrow(mus),]
mus_rf<-randomForest(class~.,data=mus_train)
pred_probs<-predict(mus_rf,mus_test,type='prob')
pred_class<- as.factor(ifelse(pred_probs[,2]>0.5,'POISONOUS','EDIBLE'))
confusionMatrix(pred_class,mus_test$class,positive = 'POISONOUS')
histogram(pred_probs[pred_probs[,2]>0.5,2])


mus_test$prob_pois<-pred_probs[,2]
x_vals=c()
y_vals=c()
for(i in seq(0,1,0.05)){
  start_bin=i
  end_bin=i+0.05
  x_vals=c(x_vals,(start_bin+end_bin)/2)
  mus_subset<- mus_test %>% filter(prob_pois >start_bin &
                                     prob_pois<=end_bin)
  cur_y=nrow(mus_subset %>% filter(class=='POISONOUS'))/
    nrow(mus_subset)
  y_vals=c(y_vals,cur_y)
}
plot(x_vals,y_vals,type='l')
















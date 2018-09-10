setwd("E:/Machine Learning/Practice/Blood Donation")
bd_train = read.csv("train.csv")
bd_test = read.csv("test.csv")
bd_actual = read.csv("BloodDonationSubmissionFormat.csv")

bd_training = bd_train[sample(seq(1, nrow(bd_train)),(0.75*nrow(bd_train))),]

bd_testing = bd_train[sample(seq(1, nrow(bd_train)),(0.25*nrow(bd_train))),]


########### Logistic regression #############
model_logit = glm(Made.Donation.in.March.2007~., data = bd_training,
                  family = binomial(link = "logit"))

anova(model_logit, test = "Chisq")

pred_logit = predict(model_logit, bd_testing, type = "response")

pred_logit = ifelse(pred_logit > 0.5, 1, 0)
mean(pred_logit == bd_testing$Made.Donation.in.March.2007)

predict_logit = predict(model_logit, bd_test, type = "response")

df1 = data.frame(bd_test$X , Made.Donation.in.March.2007 = predict_logit)
colnames(df1) = c("","Made Donation in March 2007")
write.csv(df1, "C:/Users/Administrator/Desktop/Machine Learning/Practice/Blood Donation/Logistic1.csv", row.names = FALSE)


mod_logit = glm(Made.Donation.in.March.2007~Months.since.Last.Donation +
                  Number.of.Donations + Months.since.First.Donation,
                data = bd_training,
                family = binomial(link = "logit"))

predict_logit = predict(mod_logit, bd_testing, type = "response")

predict_logit = ifelse(predict_logit > 0.5, 1, 0)
mean(predict_logit == bd_testing$Made.Donation.in.March.2007)

predict_logit1 = predict(mod_logit, bd_test, type = "response")

df2 = data.frame(bd_test$X , Made.Donation.in.March.2007 = predict_logit1)
colnames(df2) = c("","Made Donation in March 2007")
write.csv(df2, "C:/Users/Administrator/Desktop/Machine Learning/Practice/Blood Donation/Logistic2.csv", row.names = FALSE)


###################################################################################

############## Decision Tree ##############
library(rpart)

model_dt_1 = rpart(Made.Donation.in.March.2007~., data = bd_training,
                 control = rpart.control(cp = 0), method = "class")
printcp(model_dt_1)

cp_value = model_dt_1$cptable[which.min(model_dt_1$cptable[,"xerror"]),"CP"]

model_dt = prune(model_dt_1, cp = cp_value)

pred_dt = predict(model_dt, bd_testing)

pred_dt = as.data.frame(pred_dt)

pred_dt = ifelse(pred_dt$`0`>pred_dt$`1`, 0, 1)
mean(pred_dt == bd_testing$Made.Donation.in.March.2007)

predict_dt = predict(model_dt, bd_test)
predict_dt = as.data.frame(predict_dt)
predict_dt = ifelse(predict_dt$`0`>predict_dt$`1`, predict_dt$`0`,predict_dt$`1`)

df = data.frame(bd_test$X , Made.Donation.in.March.2007 = predict_dt)
colnames(df) = c("","Made Donation in March 2007")
write.csv(df, "C:/Users/Administrator/Desktop/Machine Learning/Practice/Blood Donation/DecisionTree.csv", row.names = FALSE)

##################################################################################

############# Random Forest ##############

library(randomForest)

mtry1 = round(sqrt(length(colnames(bd_training))))

model_rf = randomForest(Made.Donation.in.March.2007~., data = bd_training,
                        mtry = mtry1,
                        ntree = 100)
pred_rf = predict(model_rf, bd_testing)

pred_rf = ifelse(pred_rf>0.5, 1, 0)

mean(pred_rf == bd_testing$Made.Donation.in.March.2007)

pred_rf1 = predict(model_rf, bd_test)

df = data.frame(bd_test$X , Made.Donation.in.March.2007 = pred_rf1)
colnames(df) = c("","Made Donation in March 2007")
write.csv(df, "C:/Users/Administrator/Desktop/Machine Learning/Practice/Blood Donation/randomForest.csv", row.names = FALSE)
##################################################################################

############# adabag ##############

library(adabag)
View(bd_training)
bd_training$Made.Donation.in.March.2007 <- as.factor(bd_training$Made.Donation.in.March.2007)

model_ada = boosting(Made.Donation.in.March.2007~. , data = bd_training)
pred_ada = predict(model_ada, bd_testing)

pred_ada$prob <- as.data.frame(pred_ada$prob)

pred_ada <- ifelse(pred_ada$prob$V1>pred_ada$prob$V2, pred_ada$prob$V1, pred_ada$prob$V2)
mean(pred_ada$class == bd_testing$Made.Donation.in.March.2007)

predict_ada <- predict(model_ada, bd_test)
predict_ada$prob <- as.data.frame(predict_ada$prob)
predict_ada = ifelse(predict_ada$prob$V1>predict_ada$prob$V2, predict_ada$prob$V1, predict_ada$prob$V2)

df = data.frame(bd_test$X , Made.Donation.in.March.2007 = predict_ada)
colnames(df) = c("","Made Donation in March 2007")
write.csv(df, "C:/Users/Administrator/Desktop/Machine Learning/Practice/Blood Donation/Adabag.csv", row.names = FALSE)

###############################################################################
library(glmnet)
library(dplyr)
model_ridge1 <- cv.glmnet(x = as.matrix(bd_training %>% select(-Made.Donation.in.March.2007)),
                          y = as.factor(bd_training$Made.Donation.in.March.2007),
                          type.measure = "mse", 
                          family = "binomial",
                          alpha = 0)
model_ridge1
new_dt <- model_ridge1$glmnet.fit
opt_lambda <- model_ridge1$lambda.min
pred_ridge1 <- predict(new_dt,s = opt_lambda, 
                                  as.matrix(bd_testing %>% 
                                              select(-Made.Donation.in.March.2007)),
                       type = "response")
pred_ridge1 <- ifelse(pred_ridge1>0.5,1,0)
plot(model_ridge1)
plot(model_ridge1$glmnet.fit)
mean(pred_ridge1==bd_testing$Made.Donation.in.March.2007)

model_ridge2 <- cv.glmnet(x = as.matrix(bd_training %>% select(-Made.Donation.in.March.2007)),
                          y = as.factor(bd_training$Made.Donation.in.March.2007),
                          type.measure = "mse",
                          family = "binomial",
                          alpha = 0,nfolds = 20)
model_ridge2_glm = model_ridge2$glmnet.fit
opt_lambda2 = model_ridge2$lambda.min

pred_ridge2 <- predict(model_ridge2_glm, s = opt_lambda2, 
                       as.matrix(bd_testing %>% select(-Made.Donation.in.March.2007)),
                       type = "class")
mean(pred_ridge2==bd_testing$Made.Donation.in.March.2007)

model_lasso <- cv.glmnet(x = as.matrix(bd_training %>%  select(-Made.Donation.in.March.2007)),
                         y = as.factor(bd_training$Made.Donation.in.March.2007),
                         type.measure = "mse",
                         family = "binomial",
                         alpha = 1)
model_lassso_glm <- model_lasso$glmnet.fit
opt_lambda_lasso <- model_lasso$lambda.min

pred_lasso1 <- predict(model_lassso_glm, s = opt_lambda_lasso,
                       as.matrix(bd_testing %>% dplyr::select(-Made.Donation.in.March.2007)),
                       type = "class")
mean(pred_lasso1==bd_testing$Made.Donation.in.March.2007)

model_lasso2 <- cv.glmnet(x = as.matrix(bd_training %>%  select(-Made.Donation.in.March.2007)),
                         y = as.factor(bd_training$Made.Donation.in.March.2007),
                         type.measure = "mse",
                         family = "binomial",
                         alpha = 1, nfolds = 20)
model_lasso2_glm <- model_lasso2$glmnet.fit
opt_lambda_lasso2 <- model_lasso2$lambda.min

pred_lasso2 <- predict(model_lasso2_glm, s = opt_lambda_lasso2,
                       as.matrix(bd_testing %>% select(-Made.Donation.in.March.2007)),
                       type = "class")
mean(pred_lasso2==bd_testing$Made.Donation.in.March.2007)

model_elastic_net <- cv.glmnet(x = as.matrix(bd_training %>% select(-Made.Donation.in.March.2007)),
                               y = as.factor(bd_training$Made.Donation.in.March.2007),
                               type.measure = "mse",
                               family = "binomial",
                               alpha = 0.3, nfolds = 30)

model_elastic_net_glm <- model_elastic_net$glmnet.fit
opt_lambda_elastic1 <- model_elastic_net$lambda.min
pred_elastic1<- predict(model_elastic_net_glm, s = opt_lambda_elastic1,
                        as.matrix(bd_testing %>% select(-Made.Donation.in.March.2007)),
                        type = "class")
mean(pred_elastic1==bd_testing$Made.Donation.in.March.2007)

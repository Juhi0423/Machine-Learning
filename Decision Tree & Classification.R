library(dplyr)
library(ggplot2)
library(plotly)

advert <- read.csv("E:/Machine Learning/CLASSES/Lecture 01 - Basic ML Algorithms - 20180424/R Code and Datasets/Advertising.csv",
                   stringsAsFactors = F,header = T)
advert_train <- advert[sample(seq(1:nrow(advert)),162),]
advert_test <- advert[sample(seq(1:nrow(advert)),38),]

model <- tree(sales ~ .,data = advert)
plot(model)
text(model)


# Implemeting it Manually =============== METHOD 1
unq.tv <- sort(unique(adv$TV))
unq.news <- sort(unique(adv$newspaper))
unq.rad <- sort(unique(adv$radio))
# ========= Tweaks  ( Finding Average of censecutive observation in a sample)
tv.cuts <- (unq.tv[1:length(unq.tv)-1] + unq.tv[2:length(unq.tv)])/2
rad.cuts <- (unq.rad[1:length(unq.rad)-1] + unq.rad[2:length(unq.rad)])/2
news.cuts <- (unq.news[1:length(unq.news)-1] + unq.news[2:length(unq.news)])/2
#===============
# TV
temp = advert
tv.cuts.mse <- c()
#i = tv.cuts[1]
for(i in tv.cuts)
{
  left.sub <- temp[temp$TV < i,]
  right.sub <- temp[temp$TV > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$TV < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  tv.cuts.mse <- c(tv.cuts.mse,MSE)
}

# Radio
rad.cuts.mse <- c()
for(i in rad.cuts)
{
  left.sub <- temp[temp$radio < i,]
  right.sub <- temp[temp$radio > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$radio < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  rad.cuts.mse <- c(rad.cuts.mse,MSE)
}

# News Paper
news.cuts.mse <- c()
for(i in news.cuts)
{
  left.sub <- temp[temp$newspaper < i,]
  right.sub <- temp[temp$newspaper > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$newspaper < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  news.cuts.mse <- c(news.cuts.mse,MSE)
}

cuts = c(tv.cuts,rad.cuts,news.cuts)
predictor = c(rep('TV',length(tv.cuts)),rep('NewsPaper',length(news.cuts)),rep('Radio',length(rad.cuts)))
MSE = c(tv.cuts.mse,rad.cuts.mse,news.cuts.mse)
result <- data.frame("Cuts" = cuts,"Predictor" = predictor,"MSE" = MSE)
result <- result[order(result$MSE),]
View(result)
result[1,]

#============= Next Level Tree Values 
# Left Sub Tree
# TV
temp = advert %>% filter(TV < 122.05)
tv.cuts.mse <- c()
i = tv.cuts[1]
for(i in tv.cuts)
{
  left.sub <- temp[temp$TV < i,]
  right.sub <- temp[temp$TV > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$TV < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  tv.cuts.mse <- c(tv.cuts.mse,MSE)
}

# Radio
rad.cuts.mse <- c()
for(i in rad.cuts)
{
  left.sub <- temp[temp$radio < i,]
  right.sub <- temp[temp$radio > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$radio < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  rad.cuts.mse <- c(rad.cuts.mse,MSE)
}

# News Paper
news.cuts.mse <- c()
for(i in news.cuts)
{
  left.sub <- temp[temp$newspaper < i,]
  right.sub <- temp[temp$newspaper > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$newspaper < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  news.cuts.mse <- c(news.cuts.mse,MSE)
}

cuts = c(tv.cuts,rad.cuts,news.cuts)
predictor = c(rep('TV',length(tv.cuts)),rep('NewsPaper',length(news.cuts)),rep('Radio',length(rad.cuts)))
MSE = c(tv.cuts.mse,rad.cuts.mse,news.cuts.mse)
result <- data.frame("Cuts" = cuts,"Predictor" = predictor,"MSE" = MSE)
result <- result[order(result$MSE),]
result[1,]

#=========Right Sub Tree
# TV
temp = advert %>% filter(TV > 122.05)
tv.cuts.mse <- c()
i = tv.cuts[1]
for(i in tv.cuts)
{
  left.sub <- temp[temp$TV < i,]
  right.sub <- temp[temp$TV > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$TV < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  tv.cuts.mse <- c(tv.cuts.mse,MSE)
}

# Radio
rad.cuts.mse <- c()
for(i in rad.cuts)
{
  left.sub <- temp[temp$radio < i,]
  right.sub <- temp[temp$radio > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$radio < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  rad.cuts.mse <- c(rad.cuts.mse,MSE)
}

# News Paper
news.cuts.mse <- c()
for(i in news.cuts)
{
  left.sub <- temp[temp$newspaper < i,]
  right.sub <- temp[temp$newspaper > i,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp$newspaper < i,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  news.cuts.mse <- c(news.cuts.mse,MSE)
}

cuts = c(tv.cuts,rad.cuts,news.cuts)
predictor = c(rep('TV',length(tv.cuts)),rep('NewsPaper',length(news.cuts)),rep('Radio',length(rad.cuts)))
MSE = c(tv.cuts.mse,rad.cuts.mse,news.cuts.mse)
result <- data.frame("Cuts" = cuts,"Predictor" = predictor,"MSE" = MSE)
result <- result[order(result$MSE),]
result[1,]


# Implementing it Manually ================= METHOD 2

# =================== Variance Deviation
# Formula for Variance Deviation : var(temp$sales) - [var(left.sub$sales) * (Nl/N)] - [var(right.sub$sales)]
# Nl -  nrow of left sample
# nr - nrow of right sample


cuts = c(tv.cuts,rad.cuts,news.cuts)
predictor = c(rep('TV',length(tv.cuts)),rep('radio',length(rad.cuts)),rep('newspaper',length(news.cuts)))
result <- data.frame("Cuts" = cuts,"Predictor" = predictor)
temp <- advert %>% filter(TV > 122.05 & radio < 26.85)
left.sub <- data.frame()
right.sub <- data.frame()
curr.cuts.mse <- c()
curr.cuts.vd <- c()
for(i in 1:nrow(result))
{
  cut = cuts[i]
  curr_col = predictor[i]
  left.sub <- temp[temp[,curr_col] < cut,]
  right.sub <- temp[temp[,curr_col] > cut,]
  left.val <- mean(left.sub$sales)
  right.val <- mean(right.sub$sales)
  temp$pred <- ifelse(temp[,curr_col] < cut,left.val,right.val)
  MSE <- sum((temp$sales - temp$pred)^2)/length(temp$pred)
  VD <- var(temp$sales) - (nrow(left.sub)/nrow(temp)* var(left.sub$sales)) - (nrow(right.sub)/nrow(temp) * var(right.sub$sales))
  curr.cuts.mse <- c(curr.cuts.mse,MSE)
  curr.cuts.vd <- c(curr.cuts.vd,VD)
}
result$MSE <- curr.cuts.mse
result$VD <- curr.cuts.vd

result %>% arrange(MSE) %>% head(1)
result %>% arrange(-VD) %>% head(1)

# ==================== Classification
library(tree)
ibm <- read.csv("E:/Machine Learning/CLASSES/Lecture 04 - Decision Tees & Classification - 20180507/HR Analytics.csv",
         header = T)

idm_train <- ibm[sample(seq(1,nrow(ibm)),0.7*nrow(ibm)),]
idm_test <- ibm[sample(seq(1,nrow(ibm)),0.3*nrow(ibm)),]


model <- tree(Attrition ~ .,data = idm_train)
plot(model)
text(model)

library(rattle)

model <- rpart::rpart(Attrition~OverTime + Gender,data = idm_train)
fancyRpartPlot(model = model)



















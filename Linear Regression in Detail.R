library(dplyr)
library(ggplot2)
library(plotly)

advert <- read.csv("E:/Machine Learning/CLASSES/Lecture 01 - Basic ML Algorithms - 20180424/R Code and Datasets/Advertising.csv",
                   stringsAsFactors = F,header = T)
advert_train <- advert[sample(seq(1:nrow(advert)),162),]
advert_test <- advert[sample(seq(1:nrow(advert)),38),]

# Fit a model
advert_model <- lm(sales ~ TV,data = advert_train)
advert_model

# Manually building a Linera Regression model
# Assuming random values for m and c
# Model 1
m = 0.01
c = 1
sales_pred <- m * advert_train$TV + c
error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
error

# Model 2
m = 0.02
c = 1
sales_pred <- m * advert_train$TV + c
error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
error

# Model 3
m = 0.03
c = 1
sales_pred <- m * advert_train$TV + c
error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
error

#Model 4
m = 0.04
c = 1
sales_pred <- m * advert_train$TV + c
error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
error

#Model 5
m = 0.05
c = 1
sales_pred <- m * advert_train$TV + c
error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
error

plot(advert_train$TV,advert_train$sales)
lines(advert_train$TV,sales_pred)

# Automating the process of building a Model
# Error curve for m value from 0 to +1
res <- c()
m.val <- seq(0,1,length.out = 100)
for(m in m.val)
{
  sales_pred <- m * advert_train$TV + c
  error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
  res <- c(res,error)
}
which(min(res)==res)
m.val[10]
# Building the Model by varying both m and c
m.val = seq(-1,1,length.out = 100)
c.val = seq(-10,10,length.out = 100)

m_rep = c()
c_rep = c()
res = c()
for(m in m.val)
{
  for(c in c.val)
  {
    sales_pred = m * advert_train$TV + c
    error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
    m_rep = c(m_rep,m)
    c_rep = c(c_rep,c)
    res <- c(res,error)
  }
}
model <- data.frame("m" = m_rep,"c" = c_rep,"error" = res)
View(model)
model[which(min(model$error) == model$error),]

#======================================================
# Builing model more efficiently
m.val = seq(-1,1,length.out = 100)
c.val = seq(-10,10,length.out = 100)
res <- c()
for(m in m.val){
  for(c in c.val)
  {
    sales_pred = m * advert_train$TV + c
    error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
    res <- c(res,error)
  }
}
ind <- which(res == min(res))
c.ind <- ind %% 100
m.ind <- ifelse(c.ind == 0,trunc(ind/100),trunc(ind/100)+1) 
m.val[m.ind]
c.val[c.ind]
#==========================================================
#==================plotly

library(plotly)
mspace = m.val
cspace = c.val
zspace = matrix(res,nrow = length(m.val),ncol = length(c.val))
plot_ly(x = mspace,y = cspace,z = zspace) %>% add_surface()

# Error Curve for m value from -1 to +1
res <- c()
par(mfrow = c(1,1))
for(m in seq(-1,1,length.out = 100) )
{
  sales_pred <- m * advert_train$TV + c
  plot(advert_train$TV,advert_train$sales)
  lines(advert_train$TV,sales_pred)
  error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
  res <- c(res,error)
}
plot(res)

#======================== Statistical Formaula Method ( Practise )

y <- advert$sales
x <- advert$TV
n <- length(x)
m <- (n * sum((x*y)) - (sum(x) * sum(y)))/((n * sum(x^2))-(sum(x))^2)
c <- mean(y) - (m*mean(x))
pred <- m*advert_test$TV + c
sum((advert_test$sales - pred)^2)/length(pred)


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
par(mfrow = c(1,1))
for(m in m.val)
{
  sales_pred <- m * advert_train$TV + c
  error <- sum((advert_train$sales - sales_pred)^2)/nrow(advert_train)
  res <- c(res,error)
}
min(res)

# Building the Model by varying both m and c
m.val = seq(0,10,length.out = 100)
c.val = seq(0,10,length.out = 100)

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

#==============================================================
#===== Gradient Descent
x = rnorm(100)  # Generating a normal distribution of 100 obs with mean 0 and sd 1
y = 0.05 * x # y = m * x
df_xy = data.frame("x" = x,"y" = y) 
plot(x,y) 
cor(x,y) # Correlation refers to the measure of relatedness between x and y
# Gradient Descent sum(yx-mx^2)
# Assume m = 0


m = 1000
alpha = 0.01
n_iter = 1000 # Let us assume the random number of iterations.
# More the number of iterations the better the accuracy of Model

#Creating the new required columns

res_err <- c()
for(i in 1:n_iter)
{
  err <- sum((y - (m*x))^2)/length(x)
  res_err <- c(res_err,err)
  df_xy$xy = df_xy$x * df_xy$y
  df_xy$mx2 = df_xy$x^2 * m
  df_xy$xy_mx2 = (df_xy$xy - df_xy$mx2)
  m_gradient = -2/length(x) * (sum(df_xy$xy_mx2))
  m = m - alpha*m_gradient
}
print(m)
print(res_err)
plot(res_err)


# Now changing the alpha values

m = 1000
alpha = 0.2
n_iter = 100 # Let us assume the random number of iterations.
# More the number of iterations the better the accuracy of Model

#Creating the new required columns

res_err <- c()
m_vals <- c()
for(i in 1:n_iter)
{
  m_vals <- c(m_vals,m)
  err <- sum((y - (m*x))^2)/length(x)
  res_err <- c(res_err,err)
  df_xy$xy = df_xy$x * df_xy$y
  df_xy$mx2 = df_xy$x^2 * m
  df_xy$xy_mx2 = (df_xy$xy - df_xy$mx2)
  m_gradient = -2/length(x) * (sum(df_xy$xy_mx2))
  m = m - alpha*m_gradient
}
print(m)
print(res_err)
plot(res_err)
plot(m_vals,res_err)
lines(m_vals,res_err)

# More the alpha minimum the number of steps required to reach the global minimum.


# ============ Varying both m and c now.
advert_train$TV <- scale(advert_train$TV)

advert_test <- advert[sample(seq(1:nrow(advert)),38),]
lm(sales~TV,advert_train)

# Using Gradient Descent on advert dataset
#advert_train <- advert[sample(seq(1:nrow(advert)),162),]
#advert_train$X <- NULL

m <- 10
n <- 1000
c <- 10
alpha = 0.2
res_err <- c()
m_vals <- c()
c_vals <- c()
advert_train$xy <- advert_train$TV*advert_train$sales
for(i in 1:n)
{
  err <- sum((advert_train$sales - (m*advert_train$TV+c))^2)/length(advert_train$TV)
  res_err <- c(res_err,err)
  advert_train$mx <- advert_train$TV*m
  advert_train$mx2 <- (advert_train$TV)^2 * m
  advert_train$cx <- advert_train$TV * c
  m_grad <- -2/length(advert_train$sales) * sum(advert_train$xy - advert_train$mx2 - advert_train$cx)
  c_grad <- -2/length(advert_train$sales) * sum(advert_train$sales - advert_train$mx - c)
  m_vals <- c(m_vals,m)
  c_vals <- c(c_vals,c)
  m <- m - (alpha * m_grad)
  c <- c - (alpha * c_grad)
  cat("m_grad =",m_grad," ","c_grad =",c_grad," ","m =", m," ","c =",c,"\n")
}
print(m)
print(c)
plot(res_err)

#================ rgl is used to plot 3D graphs in a seperate window.
#install.packages("rgl")
library(rgl)
open3d()
plot3d(x = m_rep,y = c_rep,z = res,col = rainbow(100))
plot3d(x = m_vals,y = c_vals,z = res_err,add =T) 


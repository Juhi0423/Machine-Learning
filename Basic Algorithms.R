# Loading and Installing the Packages
# ISLR contains the Datasets for Machine Learning
install.packages("ISLR")
library(ISLR)
library(data.table)
library(dplyr)

# Reading the data from Advertisement.csv
adv <- fread("E:/Machine Learning/CLASSES/Lecture 01 - Basic ML Algorithms - 20180424/R Code and Datasets/Advertising.csv")
numeric_summary(adv)
factor_summary(adv)


# Here inputs is amount spent on advertisements in TV,radio,newspapers
# Output is Sales.
# Sales - Dependant , TV,RADIO,NEWSPAPER - Independant Variable
# We need to build a model to predict todays sales based on the expenditure of advertisements
# I/P variables are also called as Features.
# The process of selecting the reqiured features are called as Feature Selection.
# In Feature Selection we select the some of input variables that mostly affects the output.

# ======
# Essential Format for analysis is "All Features Should be in from of columns, but not in the form of rows"
# There are 200 observations in our data, we need to split them into Training Sets and Test sets

# Identify the percentage of missing values
colSums(is.na(adv))

# Check for outliers
sapply(adv,function(x){length(boxplot.stats(x)$out)})

#===== Splitting the Data

# Method 1 : Using row numbers to split the data
adv_train <- adv[1:162,]
adv_test <- adv[163:200,]

# Method 2 : Using random numbers to pick the random data
# First generate the random numbers and use them as indexes for subsetting the Data
# Random sampling with Replacements
adv_train <- adv[sample(seq(1:nrow(adv)),162)]
adv_test <- adv[sample(seq(1:nrow(adv)),38)]

# Feature Selection
# Here using all the variables TV, NewsPaper, Radio
# Fit a model
# The below equation says that we are going to derive sales based on TV,radio,newspaper
adv_model <- lm(sales ~ TV + radio + newspaper, data = adv_train)

# The value adv model gives coe-fficients and intercepts
# sales = 2.8044555 + 0.0463 * TV + 0.1881 * radio - 0.0005743 * newspaper
# Predicting sales values for test data
# Donot provide the output column here, provide only the input columns

adv_test$sales_pred <- predict(adv_model,adv_test[,c("TV","radio","newspaper")])

# SSE - sum of squared errors
# Error -  Actual Value - Predicted Value.
# ** Error should not have any correlation between any input variables.
# Calculate the error row wise

adv_test$error <- adv_test$sales - adv_test$sales_pred
adv_test$sqr_error <- adv_test$error ^ 2

# SSE
sum(adv_test$sqr_error)

# The model with very low SSE Value is the most optimal model.
# So the models with very low SSE value will be best considered model

plot(adv_test$sales,type = "l")
lines(adv_test$sales_pred,col = "red")

# =================== CLASSIFICATION
library(data.table)
#### Error : Dont use fread here as it introduces NA's
bank <- read.csv("E:/Machine Learning/CLASSES/Lecture 01 - Basic ML Algorithms - 20180424/R Code and Datasets/bank/bank.csv",sep = ";")

View(bank)

# Data Reshaping in not reqiured as data is in the correct format
# Missing Values
colSums(is.na(bank))

# Outliers are present or not
sapply(bank,function(x){if(is.numeric(x))length(boxplot.stats(x)$out)})

# Splitting Data
bank_train <- bank[sample(seq(1,nrow(bank)),3616),]
bank_test <- bank[sample(seq(1,nrow(bank)),4521-3616),]

# Tree package contains the tree function that is used to create decision trees
install.packages("tree")
library(tree)
library(dplyr)
bank_model <- tree(y~.,data = bank_train)

# Predicting the Values with probabilities
probs <- as.data.frame(predict(bank_model,bank_test %>% select (-y)))
names(probs)

# Classifying the Predicted Values into Classes
probs$predict <- ifelse(probs$no > probs$yes,"no","yes")
sum(bank_test$y == probs$predict) / nrow(probs) * 100



# ============= Gini Impurity
library(tree)
ibm <- read.csv("E:/Machine Learning/CLASSES/Lecture 04 - Decision Tees & Classification - 20180507/HR Analytics.csv",
                header = T,stringsAsFactors = F)

ibm_train <- ibm[sample(seq(1,nrow(ibm)),0.7*nrow(ibm)),]
ibm_test <- ibm[sample(seq(1,nrow(ibm)),0.3*nrow(ibm)),]


model <- tree(Attrition ~ .,data = ibm_train)
plot(model)
text(model)

# ============ For Better Visualization of Model
library(rattle)

model <- rpart::rpart(Attrition~OverTime + Gender,data = idm_train)
fancyRpartPlot(model = model)

# ========== Gini Impurity for Attrition based on Overtime
left_overtime <- ibm_train %>% filter(OverTime == "Yes")
right_overtime <- ibm_train %>% filter(OverTime == "No")
Nl <- nrow(left_overtime)
Nr <- nrow(right_overtime)
N <- nrow(ibm_train)
table(left_overtime$Attrition)
GI.L <- 1 - (201/291)^2 - (90/291)^2
table(right_overtime$Attrition)
GI.R <- 1 - (657/738)^2 - (81/738)^2
GI <- (Nl/N)*GI.L + (Nr/N)*GI.R
GI

# ========== Gini Impurity for Attrition based on Gender
left_Gender <- ibm_train %>% filter(Gender == "Male")
right_Gender <- ibm_train %>% filter(Gender == "Female")
Nl <- nrow(left_Gender)
Nr <- nrow(right_Gender)
N <- nrow(ibm_train)
table(left_Gender$Attrition)
GI.L <- 1 - (508/608)^2 - (100/608)^2
table(right_Gender$Attrition)
GI.R <- 1 - (350/421)^2 - (71/421)^2
GI <- (Nl/N)*GI.L + (Nr/N)*GI.R

# ================ Gini Impurity for Attrition based on Whole Dataset
table(ibm_train$Attrition)
1 - (858/1029)^2 - (171/1029)^2


###============== Case 2, if the categorical variable has many classes
Marital.Stat <- unique(ibm_train$MaritalStatus)

for(stat in Marital.Stat)
{
  left_Stat <- ibm_train %>% filter(MaritalStatus == stat)
  right_Stat <- ibm_train %>% filter(MaritalStatus != stat)
  
  p0.l <- nrow(left_Stat %>% filter(Attrition == 0))/nrow(left_Stat)
  p1.l <- nrow(left_Stat %>% filter(Attrition == 1))/nrow(left_Stat)
  gi.l <- 1 - p0.l^2 - p1.l^2
  
  p0.r <- nrow(right_Stat %>% filter(Attrition == 0))/nrow(right_Stat)
  p1.r <- nrow(right_Stat %>% filter(Attrition == 1))/nrow(right_Stat)
  gi.r <- 1 - p0.r^2 - p1.r^2
  
  gi_stat = nrow(left_Stat)/nrow(ibm_train) * gi.l + nrow(right_Stat)/nrow(ibm_train) * gi.r  
  print("Left Node")
  print(stat)
  print("Right Nodes")
  print(Marital.Stat[!stat == Marital.Stat])
  print(gi_stat)
  print("----------------------")
}

### Categorical Variable with more than 2 Classes/ Levels
uniq.job <- unique(ibm_train$MonthlyIncome)
output <- data.frame(matrix(nrow = 0,ncol = 3),stringsAsFactors = F)
for( i in 1:trunc(length(uniq.job)/2))
{
  curr_comb <- combn(uniq.job,i,simplify = F)
  for(j in curr_comb)
  {
    left_nodes <- c()
    right_nodes <- c()
    left_nodes <- unlist(j) %>% as.character()
    right_nodes <- uniq.job[!uniq.job %in% left_nodes] %>% as.character()
    
    left_job <- ibm_train[ibm_train$MonthlyIncome %in% left_nodes,]
    right_job <- ibm_train[ibm_train$MonthlyIncome %in% right_nodes,]
    
    p0.l <- nrow(left_job %>% filter(Attrition == 0))/nrow(left_job)
    p1.l <- nrow(left_job %>% filter(Attrition == 1))/nrow(left_job)
    gi.l <- 1 - p0.l^2 - p1.l^2
    
    p0.r <- nrow(right_job %>% filter(Attrition == 0))/nrow(right_job)
    p1.r <- nrow(right_job %>% filter(Attrition == 1))/nrow(right_job)
    gi.r <- 1 - p0.r^2 - p1.r^2
    
    gi_stat = nrow(left_job)/nrow(ibm_train) * gi.l + nrow(right_job)/nrow(ibm_train) * gi.r  %>% as.numeric()
    left_nodes <- paste(left_nodes,collapse = ",") %>% as.character()
    right_nodes <- paste(right_nodes,collapse = ",") %>% as.character()
    
    res <- data.frame(left_nodes,right_nodes,gi_stat,stringsAsFactors = F)
    output <- rbind(output,res)
  }
}
colnames(output) <- c("Left Nodes","Right Nodes","Gini Impurity")
View(output)
output %>% arrange(`Gini Impurity`) %>% head(1)

#====================== Sir Code for same above problem

jobs_uniq = unique(ibm_train$MonthlyIncome)
comb_left = c()
comb_right  = c()
n = 2
comb_n = combn(jobs_uniq,n,simplify = FALSE)

for(i in seq(1,length(comb_n))){
  comb_left = comb_n[[i]]
  comb_right = jobs_uniq[jobs_uniq != comb_left]
  
  samples_left = ibm_train %>% filter(MonthlyIncome %in% comb_left)
  samples_right = ibm_train %>% filter(MonthlyIncome%in% comb_right )
  
  p0_left = nrow(samples_left %>% filter(Attrition == 0 ))/nrow(samples_left)
  p1_left = nrow(samples_left %>% filter(Attrition == 1 ))/nrow(samples_left)
  gi_left = 1 - p0_left^2 - p1_left^2
  
  p0_right = nrow(samples_right %>% filter(Attrition == 0 ))/nrow(samples_right)
  p1_right = nrow(samples_right %>% filter(Attrition == 1 ))/nrow(samples_right)
  gi_right = 1 - p0_right^2 - p1_right^2
  
  gi_status = nrow(samples_left)/nrow(ibm_train)*gi_left +
    nrow(samples_right)/nrow(ibm_train)*gi_right
  
  temp = jobs_uniq[jobs_uniq != status]
  
  print("left_node")
  print(status)
  print("right_node")
  print(temp)
  print(gi_status)
  print("----------------------")
}

#
jobs_uniq = unique(ibm_train$MonthlyIncome)
combinations_left = c()
combinations_right  = c()
gi_all = c()


comb_n = combn(jobs_uniq,n,simplify = FALSE)
for( n in c(1,2,3,4)){
  for(i in seq(1,length(comb_n))){
    comb_left = comb_n[[i]]
    comb_right = jobs_uniq[!jobs_uniq %in% comb_left]
    
    samples_left = ibm_train %>% filter(MonthlyIncome %in% comb_left)
    samples_right = ibm_train %>% filter(MonthlyIncome%in% comb_right )
    
    p0_left = nrow(samples_left %>% filter(Attrition == 0 ))/nrow(samples_left)
    p1_left = nrow(samples_left %>% filter(Attrition == 1 ))/nrow(samples_left)
    gi_left = 1 - p0_left^2 - p1_left^2
    
    p0_right = nrow(samples_right %>% filter(Attrition == 0 ))/nrow(samples_right)
    p1_right = nrow(samples_right %>% filter(Attrition == 1 ))/nrow(samples_right)
    gi_right = 1 - p0_right^2 - p1_right^2
    
    gi_status = nrow(samples_left)/nrow(ibm_train)*gi_left +
      nrow(samples_right)/nrow(ibm_train)*gi_right
    
    #temp = jobs_uniq[jobs_uniq != status]
    
    #print("left_node")
    #print(status)
    #print("right_node")
    #print(temp)
    #print(gi_status)
    #print("----------------------")
    
    combinations_left = c(combinations_left,paste0(comb_left,collapse = ','))
    combinations_right = c(combinations_right,paste0(comb_right,collapse = ','))
    gi_all = c(gi_all,gi_status)
    
  }
  
}

result = data.frame(left = combinations_left,right = combinations_right,gi = gi_all)
result %>% arrange(gi) %>% head(1)
View(result)



model = rpart::rpart(Attrition~MonthlyIncome, data = ibm_train)
fancyRpartPlot(model)

levels(ibm_train$MonthlyIncome)
{{plot(model)
  text(model)}}

#========================= Numerical Column

uniq.sal <- sort(unique(ibm_train$MonthlyIncome))
output <- data.frame(matrix(nrow = 0,ncol = 3),stringsAsFactors = F)
splits <- (uniq.sal[1:length(uniq.sal)-1] + uniq.sal[2:length(uniq.sal)])/2
colnames(output) <- c("split","Gini Impurity")

for(sp in splits)
{
 left_job <- ibm_train[ibm_train$MonthlyIncome < sp,]
 right_job <- ibm_train[ibm_train$MonthlyIncome > sp,]
 
 p0.l <- nrow(left_job %>% filter(Attrition == 0))/nrow(left_job)
 p1.l <- nrow(left_job %>% filter(Attrition == 1))/nrow(left_job)
 gi.l <- 1 - p0.l^2 - p1.l^2
 
 p0.r <- nrow(right_job %>% filter(Attrition == 0))/nrow(right_job)
 p1.r <- nrow(right_job %>% filter(Attrition == 1))/nrow(right_job)
 gi.r <- 1 - p0.r^2 - p1.r^2
 
 gi_stat = nrow(left_job)/nrow(ibm_train) * gi.l + nrow(right_job)/nrow(ibm_train) * gi.r  %>% as.numeric()
 res <- data.frame(sp,gi_stat,stringsAsFactors = F)
 output <- rbind(output,res)
}
View(output)
output %>% arrange(gi_stat) %>% head(1)

# Actual Model Value
model <- tree(Attrition ~ MonthlyIncome,ibm_train)
plot(model)
text(model)

model1 <- rpart(Attrition ~ MonthlyIncome,ibm_train)
fancyRpartPlot(model1)
#===================== Understanding the Plots
# Root Level 
table(ibm_train$Attrition)
prob.0 <- table(ibm_train$Attrition)[1]/nrow(ibm_train)
prob.1 <- table(ibm_train$Attrition)[2]/nrow(ibm_train) # The Value of root node

# Level 1 - Left
freq <- table(ibm_train %>% filter(OverTime == "No") %>% select(Attrition))
prob.0 <- freq[1]/sum(freq)
prob.1 <- freq[2]/sum(freq) # The value of left node

# Level 1 - Right
freq <- table(ibm_train %>% filter(OverTime == "Yes") %>% select(Attrition))
prob.0 <- freq[1]/sum(freq)
prob.1 <- freq[2]/sum(freq) # The value of Right node

# By Default R assumes that we are concerned with probability 1
# so the prob.values of 1 are displayed on the Node
mod1 <- rpart(Attrition ~ OverTime,data = ibm_train)
fancyRpartPlot(mod1)













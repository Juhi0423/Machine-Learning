# Kmeans also requires only numerical variables as input.
# If there are Ordinal Categorical Values Encode them as Numerical according to priority
# If they are Nominal,Convert them into seperate Columns.
odi <- fread("E:/Machine Learning/CLASSES/Lecture 09 - K-Means & Hierarchial Clustering/odi-batting.csv")
odi$century <- ifelse(odi$Runs>99,1,0)
odi$ducks <- ifelse(odi$Runs==0,1,0)
odi$above_150 <- ifelse(odi$Runs>149,1,0)
odi$fifty <- ifelse(odi$Runs>49&odi$Runs<100,1,0)
odi$missed_100 <- ifelse(odi$Runs>90&odi$Runs<100,1,0)

players_summary <- odi %>% group_by(Player) %>% summarise(
    matches = n(),
    totalruns=sum(Runs,na.rm = T),
    avg_runs=mean(Runs,na.rm = T),
    centuries=sum(century,na.rm = T),
    ducks=sum(ducks,na.rm = T),
    fifties=sum(fifty,na.rm = T),
    missed_cent=sum(missed_100,na.rm = T),
    above_150=sum(above_150,na.rm = T))

#View(players_summary)
# 2 Summary points
# 1. No.of Data point in each Cluster
# 2. Characteristics of each Cluster
top_players <- players_summary %>% arrange(-totalruns) %>% head(100)
data_kmean <- top_players %>% select(-Player)
data_norm <- normalize(data_kmean,range = c(0,1),method = "range")
model_kmean <- kmeans(data_norm,centers = 3)
top_players$cluster <- model_kmean$cluster
data_norm <- top_players
# Cluster Summary
barplot(table(top_players$cluster))

# Cluster Characteristics
model_kmean$centers

# Within Sum of Squares & Between SS
model_kmean$withinss
model_kmean$betweenss
model_kmean$tot.withinss

#============== How to find the Optimal K Value ?
# Find the total.within ss for each number of clusters.
# for 2 Clusters , 3 Clusters, 4 Clusters ....and N Clusters, find the tot.withinss
# The one with Lowest tot.wuthin SS will give the best best number of Clusters.
dim(data_norm)

View(as.matrix(dist(data_norm %>% select(-cluster))))
data_norm_2d = cmdscale(dist(data_norm %>% select(-cluster)))
data_norm_2d = as.data.frame(data_norm_2d)
data_norm_2d$cluster <- as.factor(data_norm$cluster)
plot(data_norm_2d)
ggplot(data_norm_2d,aes(x = V1,y=V2,color = cluster)) + geom_jitter(size = 3)

#============= HR Analytics Clustering
ibm
ibm_sub <- ibm %>% select(Age,MonthlyIncome)
ibm_sub_norm <- normalize(ibm_sub,method = "range",range = c(0,1))
model_ibm <- kmeans(ibm_sub_norm,centers = 3)
model_ibm$cluster <- as.factor(model_ibm$cluster)
ibm_sub$cluster <- model_ibm$cluster

data <- ibm_sub %>% group_by(cluster) %>% summarise("x" = mean(Age),"y" = mean(MonthlyIncome))

ggplot(ibm_sub,aes(x = Age,y = MonthlyIncome,color = cluster))+
  geom_point(size = 2) + geom_point(data = data,aes(x = x,y = y),size = 10)

#================= Hierarichal Cluster

hclust_model <- hclust(dist(ibm_sub %>% select(-cluster)))
plot(hclust_model)
ibm_sub$cluster <- cutree(hclust_model,k=4)

data_norm_2d = cmdscale(dist(data_norm %>% select(-cluster)))
data_norm_2d = as.data.frame(data_norm_2d)
data_norm_2d$cluster <- as.factor(data_norm$cluster)

ggplot(data_norm_2d,aes(x = V1,y = V2,col = cluster)) + geom_point()

#=============== Using Corplot
str(top_players)
cor_plot <- cor(t(top_players %>% head(10) %>% select(-cluster,-Player)))
corrplot(cor_plot,order = "hclust",addrect = 3)



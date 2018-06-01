library(arules)
library(arulesViz)
library(reshape2)
library(recommenderlab)
library(kableExtra)
library(dplyr)

####Apriori Algorithm
data('Groceries')
g<-Groceries
g
View(g)
class(g)

##it gives detail of first transaction
inspect(g[1])
##inspect(g[2]),inspect(g[3])

model= apriori(g,parameter = list(support=0.01,confidence=0.2))
inspect(model[1:50],by='lift')


baskets= list(c('a','b','c'),c('a','c','d'),c('e','a'),c('a','b'),c('d','a','b'))
baskets_trans=as(baskets,'transactions')
baskets_trans
summary(baskets_trans)
itemFrequencyPlot(baskets_trans)
itemFrequencyPlot(baskets_trans,topN=10)

model=apriori(baskets_trans,parameter = list(support=0.25,confidence=0))
inspect(sort(model,by='lift',decreasing = T))



model=apriori(g,parameter = list(support=0.01,confidence=0.2))
inspect(sort(model,decreasing = T,by='lift')[1:10])


##item based colabrative filtering
##user based colabrative filtering
##IBCF is better
##UBCF
##Movies dataset

setwd("C:/Users/Administrator/Desktop/ML")
movies<-read.csv("movies.csv")
View(movies)
ratings<-read.csv("ratings.csv")
kable(head(movies))
kable(head(ratings))
length(unique(ratings$userId))
rating_matrix<-dcast(ratings,userId~movieId,value.var = "rating")
rank_matrix<-as(as.matrix(rating_matrix[,-1]),"realRatingMatrix")
rank_matrix
##nn-number of neighbour
model=Recommender(rank_matrix,method='ubcf',param=list(method='Cosine',nn=30))
model
summary(model)

result=predict(model,rank_matrix[2,],n=10)
movies_rec<-as.numeric(as(result,'list')[[1]]) 
movies %>% filter(movieId %in% movies_rec) %>% select(title)

recommend_movies=function(model,userid){
  result=predict(model,rank_matrix[userid,],n=10)
  movies_rec=as.numeric(as(result,'list')[[1]])
  return_movies=movies%>% filter(movieId %in% movies_rec) %>% select(title)
  return(return_movies)
}
recommend_movies(model,54)



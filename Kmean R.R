mall_customers <- read.csv("F:/Mall_Customers.csv")
View(mall_customers)
attach(mall_customers)

X=mall_customers[4:5]
View(X)

##elbow plot

#set.seed(6)
#wcss=vector()
#for (i in 1:10) wcss[i]=sum(kmeans(X,i)$withinss)
#plot(1:10,wcss,type = 'b')

##clustering
set.seed(29)
kmeans=kmeans(X,5,iter.max = 300,nstart = 10)

library(cluster)

clusplot(X,
         kmeans$cluster,
         lines=0,
         shade=TRUE,
         color=TRUE,
         labels=2,
         plotchar=FALSE,
         span=TRUE,
         main = paste('Clusters of Clients'),
         xlab='Annual Income',
         ylab='Spending Score')

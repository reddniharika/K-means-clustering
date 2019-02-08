#Computing id:nb7ug
#Name: Niharika Reddy

#K-means clustering
library(ISLR)
library(tidyverse)

#Create function for randomly selecting k cluster centroids
centroids<-function(df,k){
  indices<-sample(1:nrow(df),k)
  centroids<-df[indices,]
  return(centroids)
}

#Creating function for Euclidean distance
EuclideanDistance=function(df,centroids){
  df=df[,-ncol(df)]
  euc = data.frame(matrix(0,nrow =nrow(df), ncol=nrow(centroids)))
  for (i in 1:nrow(centroids)){
    for(j in 1:nrow(df)){
      euc[j,i]<-sqrt(sum((df[j,]-centroids[i,])^2))
    }
  }
  return(euc)#Euclidean distance is an nxk matrix where k is the no. of clusters
}

#Creating function to reassign cluster labels based on distance from centroids
Reassign<-function(df,euc){
  df$labels<-as.matrix(apply(euc,1, which.min))# Finds cluster number for each row with the least Euclidean distance
  return(df)
}

#Creating the function for k-means clustering and printing sum of within cluster variation
cluster.kmeans<-function(df,k){
  old_df = data.frame('labels' = rep(0,nrow(df)))
  centroids=centroids(df,k)
  df$labels = rep(1,nrow(df))
  while(!all(old_df$labels==df$labels)){
    dist<-EuclideanDistance(df,centroids)
    new_df<-Reassign(df,dist)
    old_df=df
    df=new_df
    centroids=aggregate(.~labels,df,mean)
    centroids=centroids[,-1]
  }
  #Calculating the clustering optimality
  cluster_variation<-0
  for(i in unique(df$labels)){
    cluster_df = subset(df, labels==i)
    cluster_df = cluster_df[,-ncol(cluster_df)]
    cluster_variation<-cluster_variation + sum(dist(cluster_df)^2)/nrow(cluster_df)#Calculates the sum of within-cluster variation for all clusters
  }
  cat("Number of clusters:",k,"\n")
  print(df)
  cat("Sum of Within cluster variation for",k,"clusters is",cluster_variation,"\n","\n","\n")
}


#Implementing k-means clustering on iris dataset
iris<-iris[,1:4]
for(i in 1:10){
  cluster.kmeans(iris, i)
}


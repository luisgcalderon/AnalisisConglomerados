#Universidad Anahuac Mexico
#Proyecto de Investigacion: Analisis de Conglomerados
#Luis Gerardo Calderon Contreras

#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factoextra")
#install.packages("dendextend")


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

df <- USArrests
old<-faithful
df <- na.omit(df)
df <- scale(df)
head(df)

distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# K-means
# Inputs: DataSet, and K
k<-2
KSet <- kmeans(old, centers = k, nstart = 25)

for (i in 1:k)
x<-old[KSet$cluster==i,]
as.matrix(t(x))%*%as.matrix(x)/dim(x)[1]

k$centers
  

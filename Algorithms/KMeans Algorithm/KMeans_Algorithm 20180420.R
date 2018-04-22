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

KMeansStep<-function(data,k){
n<-dim(data)[1]
m<-dim(data)[2]
p<-numeric(k)
Mu<-array(data=0,dim = c(m,k))
Sig<-array(data = 0,dim = c(m^2,k))

KSet <- kmeans(data, centers = k, nstart = 25)
for (i in 1:k){
  p[i]<-sum(KSet$cluster==i)/n
  Mu[,i]<-KSet$centers[i,]
  x<-scale(data[KSet$cluster==i,],scale = F)
  Sig[,i]<-as.numeric(as.matrix(t(x))%*%as.matrix(x)/(n-1))
}

psi<-list(p=p,Mu=Mu,Sig=Sig)
return(psi)
}
x<-faithful  
k<-2

psi<-KMeansStep(data = x,k = k)
psi

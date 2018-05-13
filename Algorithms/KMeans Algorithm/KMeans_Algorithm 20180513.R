#Universidad Anahuac Mexico
#Proyecto de Investigacion: Analisis de Conglomerados
#Luis Gerardo Calderon Contreras

#install.packages("tidyverse")
#install.packages("cluster")
#install.packages("factoextra")
#install.packages("dendextend")


# library(tidyverse)  # data manipulation
# library(cluster)    # clustering algorithms
# library(factoextra) # clustering visualization
# library(dendextend) # for comparing two dendrograms


# K-means
# Inputs: DataSet, and K
# Outputs: Parameters of Model
KMeansStep<-function(data,k){
n<-dim(data)[1]
m<-dim(data)[2]
p<-numeric(k)
mu<-array(data=0,dim = c(m,k))
sig<-numeric(m)
Sig<-array(data = 0,dim = c(m^2,k))

KSet <- kmeans(data, centers = k, nstart = 1)
for (i in 1:k){
  p[i]<-1/k
  mu[,i]<-KSet$centers[i,]
  x<-scale(data[KSet$cluster==i,],scale = F)
  for (j in 1:m) {
    sig[j]<-as.numeric((as.matrix(t(x[,j]))%*%as.matrix(x[,j]))/(n-1))
  }
  Sig[,i]<-as.numeric(diag(x = sig,nrow = m,ncol = m))
}
psi<-list(p=p,mu=mu,Sig=Sig)
return(psi)
}


#Aplicacion de Funcion----
# x<-faithful
# k<-2
# psi.k2<-KMeansStep(data = x,k = k)


# df <- USArrests
# old<-faithful
# df <- na.omit(df)
# df <- scale(df)
# head(df)
#
# distance <- get_dist(df)
# fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))
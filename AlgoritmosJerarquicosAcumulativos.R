#### Proyecto de Investigacion: Analisis de Conglomerados
## Metodos de Analisis de Conglomerados
# Luis Gerardo Calderon Contreras
#20180314


#Descripcion: Script ilustrativo de Algoritmos Jerarquicos Acumulativos.
#Objetivos:
  # Aplicar el Analisis de Conglomerados en R
  # Interpretar el dendograma resultante del analisis
  # Clasificar Grupos a partir de distancias o medidas de similitud

#Paquetes Necesarios
install.packages(c("tidyverse","cluster","factoextra","dendextend"))
library(tidyverse)  # Manipulacion de datos.
install.packages("cluster")
library(cluster)    # Algoritmos de conglomerados.
library(factoextra) # Visualizar Conglomerados.
library(dendextend) # Comparar dendogramas.

X<-faithful
#Analisis Exploratorio

X<-na.omit(X)
X<-scale(X)

plot(X)
#Dissimilarity Matrix

d<-dist(X,method="manhattan") #Distancias disponibles euclidean, maximum, manhattan, canberra, binary, minkowski

#Ejecucion de Algoritmo Jerarquico
hc1<-hclust(d,method = "complete")
hc2<-hclust(d,method = "single")
hc3<-hclust(d,method = "median")
hc4<-hclust(d,method = "centroid")
hc5<-hclust(d,method = "ward.D")
#Graficacion de Dendegrama
plot(hc5, cex=0.6,hang=-1,main = "Ward's Method",labels = FALSE,xlab="OldFaithFull")


par(mfrow=c(2,2))
plot(hc1, cex=0.6,hang=-1,main = "Complete Linkage",labels = FALSE, xlab="OldFaithFull")
plot(hc2, cex=0.6,hang=-1,main = "Single Linkage",labels = FALSE,xlab="OldFaithFull")
plot(hc3, cex=0.6,hang=-1,main = "Average Linkage",labels = FALSE,xlab="OldFaithFull")
plot(hc4, cex=0.6,hang=-1,main = "Centroid",labels = FALSE,xlab="OldFaithFull")

par(mfrow=c(1,1))

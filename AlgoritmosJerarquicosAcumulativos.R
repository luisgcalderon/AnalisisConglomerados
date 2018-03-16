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
#Graficacion de Dendegrama
plot(hc1, cex=0.6,hang=-1,main = "Dendograma OldFaithFull",labels = FALSE)



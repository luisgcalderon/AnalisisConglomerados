#Universidad Anahuac Mexico
#Proyecto de Investigacion: Analisis de Conglomerados
#Jerarquico

x<-faithful
per<-0.20
w<-round(dim(x)[1]*per)

s<-x
for (i in 1:w) {
  z<-dim(s)[1]
  q<-runif(1,min = 1,max =z )
  s<-s[-q,]
}
s<-scale(s)
d<-dist(s,method="euclidean") #Distancias disponibles euclidean, maximum, manhattan, canberra, binary, minkowski
hc1<-hclust(d,method = "complete")
plot(hc1, cex=0.6,hang=-1,main = "Complete Linkage",labels = FALSE, xlab="Viejo Fiel")

hc2<-hclust(d,method = "single")
plot(hc2, cex=0.6,hang=-1,main = "Single Linkage",labels = FALSE,xlab="Viejo Fiel")

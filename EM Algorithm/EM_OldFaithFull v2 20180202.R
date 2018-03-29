### Universidad Anahuac Mexico Norte
## Application of EM Algorith In FaithFull Data
# Luis Gerardo Calderon Contreras

## Clear Environment ----
rm(list=ls())

## Installation of Packages----
install.packages("mixtools") #Mixtools package for drawing the elipse
install.packages("MASS") #MASS pachake for calculating bivariate normal probability
install.packages("ggplot2")

##Packages
library(mixtools)
library(MASS)
library(ggplot2)

##DataSet----
data(faithful)
str(faithful)
x<-faithful
ggplot(faithful,aes(waiting,eruptions))+
  geom_point()
View(faithful)

ggplot(faithful,aes(waiting,1))+
  geom_point()+
  stat_function(fun = dnorm,n = 101, args = list(mean=mean(x$eruptions),sd=sd(x$eruptions)))

# Data to Model ----
x<-faithful$waiting
# Parameters of the Mixture Model
p<- c(0.5,0.5) #Parametro de Proporcionalidad de las Distribuciones
g<-(length(p)) #Componentes del Modelo 
# Initial Parameters of the Normal Distribution
mu <- c(52,82); sig<- c(16,16) # Parametros Mu y Sigma respectivamente
psi<-data.frame(p,mu,sig)

#relativo, angulo, verosi
psi.rel<-EMAlgorithm1d(dato = x,g = g,psi = psi,metodo = "rel",difmin = 0.0001)
psi.ang<-EMAlgorithm1d(dato = x,g = g,psi = psi,metodo = "angulo",difmin = 0.0001)
psi.ver<-EMAlgorithm1d(dato = x,g = g,psi = psi,metodo = "verosi",difmin = 0.0001)

psi.rel
psi.ang
psi.ver
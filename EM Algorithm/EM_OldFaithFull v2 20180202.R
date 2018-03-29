### Universidad Anahuac Mexico Norte
## Application of EM Algorith In FaithFull Data
# Luis Gerardo Calderon Contreras

## Clear Environment ----
rm(list=ls())

## Installation of Packages----
install.packages("mixtools") #Mixtools package for drawing the elipse
install.packages("MASS") #MASS pachake for calculating bivariate normal probability
install.packages("ggplot2")

##Packages ---- 
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

ggplot(faithful,aes(waiting,0))+
  geom_point()+
  stat_function(fun = dnorm,n = 100, args = list(mean=mean(x$waiting),sd=sd(x$waiting)))

# Data to Model ----
x<-faithful["waiting"]
# Parameters of the Mixture Model
p<- c(0.5,0.5) #Parametro de Proporcionalidad de las Distribuciones
g<-(length(p)) #Componentes del Modelo 
# Initial Parameters of the Normal Distribution
mu <- c(52,82); sig<- c(16,16) # Parametros Mu y Sigma respectivamente
psi<-data.frame(p,mu,sig)

# Plot EM ----
rainbowcols <- rainbow(g)
d<-list()
for (i in 1:g) {
  d<-c(stat_function(fun = dnorm,n = 100, args = list(mean=psi[i,"mu"],
                                                      sd=sqrt(psi[i,"sig"])),
                     color=rainbowcols[i]),d)
}
plot1<-ggplot(x,aes(waiting,-0.005))+
  geom_point() + d + theme(
    panel.background = element_rect(fill = "white"))+ylim(-0.01,0.1) + ylab("")+ggtitle("Data Viejo Fiel")
plot(plot1)

# Application EM ----
psi.t<-EMSteps(x,g,psi,0)
psi.t1<-EMSteps(x,g,psi.t,1)
psi.t2<-EMSteps(x,g,psi.t1)
psi.t3<-EMSteps(x,g,psi.t2)
psi.t4<-EMSteps(x,g,psi.t3)
#relativo, angulo, verosi
psi.rel<-EMAlgorithm1d(dato = x,g = g,psi = psi,metodo = "relativo",difmin = 0.0001,t = 0)
psi.ang<-EMAlgorithm1d(dato = x,g = g,psi = psi,metodo = "angulo",difmin = 0.0001,t=0)
psi.ver<-EMAlgorithm1d(dato = x,g = g,psi = psi,metodo = "verosi",difmin = 0.00001,t=0)
psi.rel
psi.ang
psi.ver
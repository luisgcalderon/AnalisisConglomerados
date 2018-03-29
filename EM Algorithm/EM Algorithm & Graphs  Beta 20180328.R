### Universidad Anahuac Mexico Norte
## EM Algorithm 1-dimmension
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

# EM 1-D Algorithm ----

#Funcion Pasos EM
EMSteps<-function(x, g, psi){
  psi.t<-psi
  # M Step Estimate PI
  for (i in 1:(g-1)) {
    t<-numeric(length(x))
    #Compute Bayes Probability Formula
    t<-(psi[i,"p"]*dnorm(x,mean = psi[i,"mu"],
                         sd = sqrt(psi[i,"sig"])))
    for (j in 1:length(x)) {
      a<-numeric(1)
      for (w in ((1:g))) {
        a<-a+(psi[w,"p"]*dnorm(x[j],mean = psi[w,"mu"],
                               sd = sqrt(psi[w,"sig"])))
      }
      t[j]<-t[j]/a
    }
    #Estimate pi
    psi.t[i,"p"]=sum(t)/length(x)
    # estimate mu
    psi.t[i,"mu"]<-(t%*%x)/(sum(t))
    # estimate sigma
    psi.t[i,"sig"]<-(t%*%((x-psi.t[i,"mu"])^2))/(sum(t))
  }
  psi.t[g,"p"]=1-sum(psi.t[-g,"p"])
  # E Step Estimate mu & sigma
  i<-g
  t<-numeric(length(x))
  t<-(psi[i,"p"]*dnorm(x,mean = psi[i,"mu"],
                       sd = sqrt(psi[i,"sig"])))
  for (j in 1:length(x)) {
    a<-numeric(1)
    for (w in ((1:g))) {
      a<-a+(psi[w,"p"]*dnorm(x[j],mean = psi[w,"mu"],
                             sd = sqrt(psi[w,"sig"])))
    }
    t[j]<-t[j]/a
  }
  # estimate mu
  psi.t[i,"mu"]<-(t%*%x)/(sum(t))
  # estimate sigma
  psi.t[i,"sig"]<-(t%*%((x-psi.t[i,"mu"])^2))/(sum(t))
  
  #Model Plot--
  rainbowcols <- rainbow(g)
  d<-list()
  for (i in 1:g) {
    d<-c(stat_function(fun = dnorm,n = 100, args = list(mean=psi.t[i,"mu"],
                                                        sd=sqrt(psi.t[i,"sig"])),
                       color=rainbowcols[i]),d)
  }
  plot1<-ggplot(data.frame(i<-c(1:length(x)),x),aes(x,-0.005))+
          geom_point() + d + ylab("")+theme(
            panel.background = element_rect(fill = "white"))+ylim(-0.01,0.1)
  #End Plot--
  plot(plot1)
  return(psi.t)
}

# Función Calcular Densidad de Maxima Versomilitud
maxver<-function(x,g,psi){
  a<-data.frame(i=c(1:length(x)))
  for (i in 1:g) {
    a<-cbind(a,psi[i,"p"]*dnorm(x,mean = psi[i,"mu"],sd = sqrt(psi[i,"sig"])))
    colnames(a)[i+1]=as.character(i)
  }
  s<-prod(apply(a[,-1],MARGIN = 2 ,FUN = sum))
  return(s)
}


#Calcular angulo entre vectores
angle <- function(x,y){
  dot.prod <- x%*%y 
  norm.x <- norm(x,type="2")
  norm.y <- norm(y,type="2")
  theta <- acos(dot.prod / (norm.x * norm.y))
  as.numeric(theta)
}

# Funcion Convergencia-Iteracion 
# Pedimos datos, numero de componentes, parametros iniciales
#   y metodo de convergencia (relativo,angulo,maxvers),
#   diferencia minima de paro

EMAlgorithm1d<-function(dato,g,psi,metodo,difmin,t) {
  psi.t<-EMSteps(x = dato,g = g,psi = psi)
  a<-t+1
  if (metodo=="relativo") {
    if (sum(unlist(psi.t)-unlist(psi)>rep(difmin,g*3))>0) {
      psi<-psi.t
      EMAlgorithm1d(dato = dato,g = g,psi = psi,metodo = metodo, difmin = difmin,t=a)} else{
        print(a)
        return(psi.t)
      }
    } else if (metodo=="angulo"){
    w<-unlist(psi.t[-1])
    z<-unlist(psi[-1])
    if (abs(angle(w,z))>difmin){
      psi<-psi.t
      EMAlgorithm1d(dato = dato,g = g,psi = psi,metodo = metodo, difmin = difmin,t=a)
    }else{
      print(a)
      return(psi.t)
    }
  } else if (metodo=="verosi") {
    if (maxver(x = dato,g = g,psi = psi.t)-maxver(x = dato,g = g,psi = psi)>difmin){
      psi<-psi.t
      EMAlgorithm1d(dato = dato,g = g,psi = psi,metodo = metodo, difmin = difmin,t=a)
    }else{
      print(a)
      return(psi.t)
    }
  } else {print("No se definio correctamente el metodo")}
}







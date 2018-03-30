### Universidad Anahuac Mexico Norte
## EM Algorithm Multi-dimmension
# Luis Gerardo Calderon Contreras


## Clear Environment ----
rm(list=ls())

## Installation of Packages----
install.packages("mvtnorm") #mvtnorm package for calculating multivariate normal probability
install.packages("ggplot2")

##Packages ----
library(mvtnorm)
library(ggplot2)
v<-var(x)
class(v)
#Parametros ponderados, media, sigma, coeficiente de correlacion
x<-faithful
n<-dim(x)[2]
# Parameters of the Mixture Model
p<- c(0.5,0.5) #Parametro de Proporcionalidad de las Distribuciones
g<-(length(p)) #Componentes del Modelo 

# Initial Parameters of the Normal Distribution
# (x,y)
mu1 <- c(52,1.5); mu2<- c(82,3)
sig1<- c(16,1); sig2<-c(16,1.6) # Parametros Mu y Sigma respectivamente
rho1<-c(3);rho2<-c(4) # Coeficiente de correlación
Sig1<-matrix(c(sig1[1],rho1*sqrt(sig1[1]*sig1[2]),rho1*sqrt(sig1[1]*sig1[2]),sig1[2]),
             nrow = 2,byrow = T)
Sig2<-matrix(c(sig2[1],rho2*sqrt(sig2[1]*sig2[2]),rho2*sqrt(sig2[1]*sig2[2]),sig2[2]),
             nrow = 2,byrow = T)

psi<-data.frame(p,mu=data.frame(mu1,mu2),Sig=data.frame(Sig1,Sig2))
psi

# EM Multivariate Algorithm ----
#Funcion Pasos EM
EMSteps<-function(x, g, psi,m){
  psi.t<-psi
  # M Step Estimate PI
  for (i in 1:(g-1)) {
    t<-numeric(length(x[,1]))
    #Compute Bayes Probability Formula
    t<-(psi[i,"p"]*dnorm(x[,1],mean = psi[i,"mu"],
                         sd = sqrt(psi[i,"sig"])))
    for (j in 1:length(x[,1])) {
      a<-numeric(1)
      for (w in ((1:g))) {
        a<-a+(psi[w,"p"]*dnorm(x[j,1],mean = psi[w,"mu"],
                               sd = sqrt(psi[w,"sig"])))
      }
      t[j]<-t[j]/a
    }
    #Estimate pi
    psi.t[i,"p"]=sum(t)/length(x[,1])
    # estimate mu
    psi.t[i,"mu"]<-(t%*%x[,1])/(sum(t))
    # estimate sigma
    psi.t[i,"sig"]<-(t%*%((x[,1]-psi.t[i,"mu"])^2))/(sum(t))
  }
  psi.t[g,"p"]=1-sum(psi.t[-g,"p"])
  # E Step Estimate mu & sigma
  i<-g
  t<-numeric(length(x[,1]))
  t<-(psi[i,"p"]*dnorm(x[,1],mean = psi[i,"mu"],
                       sd = sqrt(psi[i,"sig"])))
  for (j in 1:length(x[,1])) {
    a<-numeric(1)
    for (w in ((1:g))) {
      a<-a+(psi[w,"p"]*dnorm(x[j,1],mean = psi[w,"mu"],
                             sd = sqrt(psi[w,"sig"])))
    }
    t[j]<-t[j]/a
  }
  # estimate mu
  psi.t[i,"mu"]<-(t%*%x[,1])/(sum(t))
  # estimate sigma
  psi.t[i,"sig"]<-(t%*%((x[,1]-psi.t[i,"mu"])^2))/(sum(t))
  
  #Model Plot--
  rainbowcols <- rainbow(g)
  d<-list()
  for (i in 1:g) {
    d<-c(stat_function(fun = dnorm,n = 100, args = list(mean=psi.t[i,"mu"],
                                                        sd=sqrt(psi.t[i,"sig"])),
                       color=rainbowcols[i]),d)
  }
  plot1<-ggplot(x,aes(waiting,-0.005))+
    geom_point() + d + ylab("")+theme(
      panel.background = element_rect(fill = "white"))+ylim(-0.01,0.1)+ggtitle(label = "Iteración",subtitle = m)
  #End Plot--
  plot(plot1)
  return(psi.t)
}

# Función Calcular Densidad de Maxima Versomilitud
maxver<-function(x,g,psi){
  a<-data.frame(i=c(1:length(x)))
  for (i in 1:g) {
    a<-cbind(a,psi[i,"p"]*dnorm(x[,1],mean = psi[i,"mu"],sd = sqrt(psi[i,"sig"])))
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

EMAlgorithm1d<-function(dato,g,psi,metodo,difmin,t=0) {
  a<-t+1
  psi.t<-EMSteps(x = dato,g = g,psi = psi,m = a)
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
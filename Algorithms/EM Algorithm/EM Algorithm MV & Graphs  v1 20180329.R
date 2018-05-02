### Universidad Anahuac Mexico Norte
## EM Algorithm Multi-dimmension
# Luis Gerardo Calderon Contreras


## Clear Environment ----
#rm(list=ls())

## Installation of Packages----
#install.packages("mvtnorm") #mvtnorm package for calculating multivariate normal probability
#install.packages("ggplot2")

##Packages ----
library(mvtnorm)
library(ggplot2)

# #Parametros ponderados, media, sigma, coeficiente de correlacion----
# x<-faithful
# # Parameters of the Mixture Model
# p<- c(0.5,0.5) #Parametro de Proporcionalidad de las Distribuciones
# g<-(length(p)) #Componentes del Modelo 
# 
# # Initial Parameters of the Normal Distribution
# # (x,y)
# mu1 <- c(1.5,52); mu2<- c(3,82)
# sig1<- c(1.2,184); sig2<-c(1.6,182) # Parametros Mu y Sigma respectivamente
# rho1<-c(0.5);rho2<-c(0.5) # Coeficiente de correlación
# Sig1<-c(sig1[1],rho1*sqrt(sig1[1]*sig1[2]),rho1*sqrt(sig1[1]*sig1[2]),sig1[2])
# Sig2<-c(sig2[1],rho2*sqrt(sig2[1]*sig2[2]),rho2*sqrt(sig2[1]*sig2[2]),sig2[2])
# 
# mu<-array(data=c(mu1,mu2),dim = c(2,2))
# Sig<-array(data = c(Sig1,Sig2),dim = c(4,2))
# 
# 
# psi<-list(p=p,mu=mu,Sig=Sig)

# EM Multivariate Algorithm ----

#Graficar

PlotEM_MV<-function(x,g,psi,q){
  #Graficar
  n<-dim(x)[1]
  m<-dim(x)[2]
  raincol<-rainbow(g)
  a.x<-data.frame(i=1:n)
  a.y<-data.frame(i=1:n)
  for (i in 1:g) {
    a<-rmvnorm(n,mean=psi$mu[,i],
               sigma=matrix(data = psi$Sig[,i],nrow = m))
    a.x[,i]<-a[,1]
    a.y[,i]<-a[,2]
  }
  
  g.p<-ggplot(x,aes(waiting,eruptions))+
    geom_point()+ggtitle("EM MV",subtitle = paste("Iteracion",q,sep=" "))+theme_bw()+
    stat_ellipse(aes(a.y[,1],a.x[,1]),colour=raincol[1])+
    stat_ellipse(aes(a.y[,2],a.x[,2]),colour=raincol[2])
  plot(g.p) 
  return(g.p)
}

# Funcion Pasos EM 
# Nota: Modificar Proceso de Graficacion

EMStepsMV<-function(x, g, psi, q,graf=0){
  m<-dim(x)[2] #dimension de x
  n<-dim(x)[1] #numero de observaciones
  psi.t<-psi
  for (i in 1:g) {
    t<-numeric(n)
    t<-psi$p[i]*dmvnorm(x = x,mean = psi$mu[,i],sigma = 
                          matrix(data = psi$Sig[,i],nrow=m))
    for (j in 1:n) {
      a<-numeric(1)
      for (w in 1:g) {
        a<-a+psi$p[w]*dmvnorm(x = x[j,],mean = psi$mu[,w],sigma = 
                                matrix(data = psi$Sig[,w],nrow = m))
        
      }
      t[j]<-t[j]/a
    }
    if (i<g){
      psi.t$p[i]<-sum(t)/n} else{
        psi.t$p[i]<-1-sum(psi.t$p[-i])
      }
    #estimacion de mu
    psi.t$mu[,i]<-as.numeric(t%*%as.matrix(x)/sum(t))
    #estimacion de sig
    txn<-x
    xn<-x
    for (j in 1:m) {
      xn[,j]<-(x[,j]-psi.t$mu[j,i])
      txn[,j]<-t*xn[,j]
    }
    psi.t$Sig[,i]<-as.numeric(
      (t(as.matrix(txn))%*%(as.matrix(xn)))/sum(t))
  }
  if (graf==1){
  g.p<-PlotEM_MV(x,g,psi.t,q)
  }
  return(psi.t)
}

# psi.t1<-EMStepsMV(x,2,psi,1)
# psi.t2<-EMStepsMV(x,2,psi.t1,2)
# psi.t3<-EMStepsMV(x,2,psi.t2,3)
# psi.t4<-EMStepsMV(x,2,psi.t3,4)
# psi.t5<-EMStepsMV(x,2,psi.t4,5)



# Función Calcular Densidad de Maxima Versomilitud
maxverMV<-function(x,g,psi){
  n<-dim(x)[1]
  m<-dim(x)[2]
  a<-data.frame(i=c(1:n))
  for (i in 1:g) {
    a<-cbind(a,psi$p[i]*dmvnorm(x = x,mean = psi$mu[,i],sigma = 
                                  matrix(data = psi$Sig[,i],nrow=m)))
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

EMAlgorithmMV<-function(dato,g,psi,metodo,difmin,t=0,graf=0) {
  a<-t+1
  m<-dim(x)[2]
  psi.t<-EMStepsMV(x = dato,g = g,psi = psi,q = a,graf = graf)
  if (metodo=="relativo") {
    if (sum(unlist(psi.t)-unlist(psi)>rep(difmin,length(unlist(psi))))>0) {
      psi<-psi.t
      EMAlgorithmMV(dato = dato,g = g,psi = psi,metodo = metodo, difmin = difmin,t=a,graf=graf)} else{
        return(psi.t)
      }
  } else if (metodo=="angulo"){
    w<-unlist(psi.t[-1])
    z<-unlist(psi[-1])
    if (abs(angle(w,z))>difmin){
      psi<-psi.t
      EMAlgorithmMV(dato = dato,g = g,psi = psi,metodo = metodo, difmin = difmin,t=a,graf=graf)
    }else{
      return(psi.t)
    }
  } else if (metodo=="verosi") {
    if (maxverMV(dato,g = g,psi = psi.t)-maxverMV(dato,g = g,psi = psi)>difmin){
      psi<-psi.t
      EMAlgorithmMV(dato = dato,g = g,psi = psi,metodo = metodo, difmin = difmin,t=a,graf=graf)
    }else{
      return(psi.t)
    }
  } else {print("No se definio correctamente el metodo")}
}

# PlotEM_MV(x,g,psi,0)
# psi.rel<-EMAlgorithmMV(x,2,psi,"relativo",0.0001,graf = 1)
# psi.angle<-EMAlgorithmMV(x,2,psi,"angulo",0.0001,graf=1)
# psi.vero<-EMAlgorithmMV(x,2,psi,"verosi",0.0001,graf=1)
# 
# #Calculo de Verosimilitud
# maxverMV(x,2,psi)
# maxverMV(x,2,psi.rel)
# maxverMV(x,2,psi.angle)
# maxverMV(x,2,psi.vero)




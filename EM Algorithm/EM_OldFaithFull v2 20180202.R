### Universidad Anahuac Mexico Norte
## EM Algorithm for Old Faithful Data Set
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
  }
  psi.t[g,"p"]=1-sum(psi.t[-g,"p"])
  # E Step Estimate mu & sigma
  for (i in 1:g) {
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
  }
  return(psi.t)
}


# Funcion Convergencia-Iteracion 
# Pedimos datos, numero de componentes, parametros iniciales
#   y metodo de convergencia (relativo,angulo,maxvers),
#   diferencia minima de paro

EMAlgorithm1d<-function(dato,g,psi,metodo,difmin) {
  psi.t<-EMSteps(x = dato,g = g,psi = psi)
  if (metodo=="relativo") {
    if (sum(unlist(psi.t)-unlist(psi)>rep(difmin,g*3))>0) {
      psi<-psi.t
      EMAlgorithm1d(dato = dato,g = g,psi = psi,metodo = metodo, difmin = difmin)
    }
  } else if (metodo=="angulo"){
    print(2)
  } else if (metodo=="difmin") {
    print(3)
  } else {print("No se definio correctamente el metodo")}
  return(psi.t)
}



# Data to Model ----
x<-faithful$waiting
# Parameters of the Mixture Model
p<- c(0.5,0.5) #Parametro de Proporcionalidad de las Distribuciones
g<-(length(p)) #Componentes del Modelo 
# Initial Parameters of the Normal Distribution
mu <- c(52,82); sig<- c(16,16) # Parametros Mu y Sigma respectivamente
psi<-data.frame(p,mu,sig)

res<-EMAlgorithm1d(dato = x,g = g,psi = psi,metodo = "relativo",difmin = 0.0001)
res


#Parametros de la Mixtura----

# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Vector Mean
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix

#Parametros para el modelo
psi<-list(p,mu,sigma)


# Function to draw ellipse for bivariate normal data----
bvn1 <- mvrnorm(N, mu = mu, Sigma = sigma ) # from MASS package
colnames(bvn1) <- c("bvn1_X1","bvn1_X2")

## Bivariate Normal Distribution
dbinormal<-function(x,mu,sigma,n){
  a<-0
  b<-0
  c<-0
  a<-1/(sqrt((2*pi)^n)*det(sigma))
  b<-(-1/2)*(((t(x-mu))%*%sigma)%*%(x-mu))
  c<-a*exp(b[1])
  return(c)
}
## EM Algorith 1 dimension
p.s<-c(0.8,0.2) #Ponderadores de cada distribucion
theta1<-c(mu1,s1) # Parametros de la primera distribucion
theta2<-c(mu2,s2) # Parametros de la segunda distribucion
parn<-list(p.s, theta1, theta2)
unlist(parn[3])

iteraciones<-function(x, parn, cparo=0.001){
  
  sc<-EMFaithfull1d(x = x,parn = parn)
  paro<-rep(cparo,(2*length(parn)-1))
  if (sum(sumpar(parn,sc)>paro)>0) {
    parn<-sc
    iteraciones(x,parn,0.05)
  } else{
   return(sc) 
  }
  
}

#Funcion para calcular la diferencia entre los parámetros
sumpar<-function(parn,sc){
  z<-numeric(2*length(parn)-1)
  z<-c((unlist(sc[1])[1]-unlist(parn[1])[1]),(unlist(sc[2])-unlist(parn[2])))
  return(z)
  
}



class(unlist(parn[1]))

dmvnorm(x,mu = mu,sigma = sigma)
?dmvnorm
plot(x=x$waiting,y =x$eruptions )
ellipse_bvn(bvn = x,alpha = .95)

ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = TRUE, draw = TRUE)

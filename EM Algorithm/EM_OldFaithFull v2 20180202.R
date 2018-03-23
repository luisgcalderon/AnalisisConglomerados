### Universidad Anahuac Mexico Norte
## EM Algorithm for Old Faithful Data Set
# Luis Gerardo Calderon Contreras

## Clear Environment
rm(list=ls())


##DataSet
x<-faithful$waiting
x<-c(3,7)

##Packages
install.packages("mixtools") #Mixtools package for drawing the elipse
library(mixtools)
install.packages("MASS") #MASS pachake for calculating bivariate normal probability
library(MASS)

## Initial Paramaters
# Target parameters for univariate normal distributions
p<-0.5 #Parametro de Proporcionalidad de Pimera Normal
rho <- -0.6  #Correlation Coefficient
mu1 <- 1; s1 <- 2 #First Parameters
mu2 <- 1; s2 <- 8 #Second Parameters

#Parametros de la Mixtura
p<- c(0.10,0.9)

# Parameters for bivariate normal distribution
mu <- c(mu1,mu2) # Vector Mean
sigma <- matrix(c(s1^2, s1*s2*rho, s1*s2*rho, s2^2),2) # Covariance matrix


# Function to draw ellipse for bivariate normal data
ellipse_bvn <- function(bvn, alpha){
  Xbar <- apply(x,2,mean)
  S <- cov(x)
  ellipse(Xbar, S, alpha = 0.05, col="red")
}

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

EMFaithfull1d<-function(x, parn){
  ep<-numeric(length(x))
  p.m<-unlist(parn[1])
  p<-numeric(length(p.s))
  
  ep<-((dnorm(x,mean = theta1[1],sd = sqrt(theta1[2]))*p.m[1])/((dnorm(x,mean = theta1[1],sd = sqrt(theta1[2]))*p.m[1])+(dnorm(x,mean = theta2[1],sd = sqrt(theta2[2]))*p.s[2])))
  p[1]<-mean(ep) #Aproximacion del Ponderador
  p[2]<-(1-p[1]) #Complemento a la otra distribución
  theta1[1]<- sum(ep*x)/p[1]
  theta1[2]<- sum(ep*(x-theta1[1])^2)/p[1]
  theta2[1]<- sum((1-ep)*x)/p[2]
  theta2[2]<- sum((1-ep)*(x-theta2[1])^2)/p[2]
  
  parn1<-list(p,theta1,theta2)
  return(parn1)
}


class(unlist(parn[1]))

dmvnorm(x,mu = mu,sigma = sigma)
?dmvnorm
plot(x=x$waiting,y =x$eruptions )
ellipse_bvn(bvn = x,alpha = .95)

ellipse(mu, sigma, alpha = .05, npoints = 250, newplot = TRUE, draw = TRUE)

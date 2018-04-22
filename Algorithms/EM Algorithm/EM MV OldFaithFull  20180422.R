#Universidad Anahuac Mexico
#Proyecto de Investigacion: Analisis de Conglomerados
#Aplicacion Viejo Fiel en Algoritmo EM Multivariado


# Definicion de funciones
source('~/Anahuac/AnalisisConglomerados/AnalisisConglomerados/Algorithms/EM Algorithm/EM Algorithm MV & Graphs  v1 20180329.R')


#Parametros ponderados, media, sigma, coeficiente de correlacion----
x<-faithful
# Parameters of the Mixture Model
p<- c(0.5,0.5) #Parametro de Proporcionalidad de las Distribuciones
g<-(length(p)) #Componentes del Modelo 

# Initial Parameters of the Normal Distribution
# (x,y)
mu1 <- c(1.5,52); mu2<- c(3,82)
sig1<- c(1.2,184); sig2<-c(1.6,182) # Parametros Mu y Sigma respectivamente
rho1<-c(0.5);rho2<-c(0.5) # Coeficiente de correlación
Sig1<-c(sig1[1],rho1*sqrt(sig1[1]*sig1[2]),rho1*sqrt(sig1[1]*sig1[2]),sig1[2])
Sig2<-c(sig2[1],rho2*sqrt(sig2[1]*sig2[2]),rho2*sqrt(sig2[1]*sig2[2]),sig2[2])

mu<-array(data=c(mu1,mu2),dim = c(2,2))
Sig<-array(data = c(Sig1,Sig2),dim = c(4,2))


# PlotEM_MV(x,g,psi,0)
# psi.rel<-EMAlgorithmMV(x,2,psi,"relativo",0.0001,graf = 1)
# psi.angle<-EMAlgorithmMV(x,2,psi,"angulo",0.0001,graf=1)
# psi.vero<-EMAlgorithmMV(x,2,psi,"verosi",0.0001,graf=1)

# #Calculo de Verosimilitud
# maxverMV(x,2,psi)
# maxverMV(x,2,psi.rel)
# maxverMV(x,2,psi.angle)
# maxverMV(x,2,psi.vero)


psi<-list(p=p,mu=mu,Sig=Sig)
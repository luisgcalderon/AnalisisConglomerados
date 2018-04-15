#Universidad Anahuac Mexico
#Proyecto de Inverstigacion:
#Analisis de Conglomerados
#Luis Gerardo Calderon Contreras



#Funciones Criterios de Informacion para evaluar Modelos Parametricos de Conglomerados

#Paquetes
library(mvtnorm)



# Akaike Information Criterion----
#Inputs:Muestra, Parametros del Modelo, y Método
#Output: Calificación Númerica de armonía del modelo con los datos

IC<-function(x,g,psi,metodo){
  #Calcular log-verosimilitud
  m<-dim(x)[2] #dimension de x
  n<-dim(x)[1] #numero de observaciones
  t<-numeric(n)
  for (i in 1:g) {
    t<-t+psi$p[i]*dmvnorm(x = x,mean = unlist(psi$mu[i]),sigma = 
                                    matrix(data = unlist(psi$Sig[i]),nrow = m))
  }
  if (metodo=="AIC"){
  FVer<-sum(log(t))
  FVer<-(-2)*FVer+2*(length(unlist(psi)))
  return(FVer)
  } else if (metodo=="AWE"){
    FVer<-sum(log(t))
    t<-matrix(0,nrow=n,ncol = g)
    for (i in 1:g) {
      t[,i]<-psi$p[i]*dmvnorm(x = x,mean = unlist(psi$mu[i]),sigma = 
                            matrix(data = unlist(psi$Sig[i]),nrow=m))
      for (j in 1:n) {
        a<-numeric(1)
        for (w in 1:g) {
          a<-a+psi$p[w]*dmvnorm(x = x[j,],mean = unlist(psi$mu[w]),sigma = 
                                  matrix(data = unlist(psi$Sig[w]),nrow = m))
          
        }
        t[j,i]<-t[j,i]/a
      }
    }
    ent<-numeric(1)
    for (w in 1:g){
      ent<-ent+t[,w]%*%log(t[,w])
    }
  FVer<-(-2)*(FVer-as.numeric(ent))+2*length(unlist(psi))*(3/2+log(n))
  return(FVer)
  } else if (metodo=="BIC"){
    FVer<-sum(log(t))
    FVer<-(-2)*FVer+(length(unlist(psi))*log(n))
    return(FVer)
  } else if (metodo=="CLC"){
    FVer<-sum(log(t))
    t<-matrix(0,nrow=n,ncol = g)
    for (i in 1:g) {
      t[,i]<-psi$p[i]*dmvnorm(x = x,mean = unlist(psi$mu[i]),sigma = 
                            matrix(data = unlist(psi$Sig[i]),nrow=m))
      for (j in 1:n) {
        a<-numeric(1)
        for (w in 1:g) {
          a<-a+psi$p[w]*dmvnorm(x = x[j,],mean = unlist(psi$mu[w]),sigma = 
                                  matrix(data = unlist(psi$Sig[w]),nrow = m))
        }
        t[j,i]<-t[j,i]/a
      }
    }
    ent<-numeric(1)
    for (w in 1:g){
      ent<-ent+t[,w]%*%log(t[,w])
    }
    FVer<-(-2)*FVer+2*as.numeric(ent)
    return(FVer)
  } else if (metodo=="KIC"){
    FVer<-sum(log(t))
    FVer<-(-2)*FVer+3*(length(unlist(psi))+1)
    return(FVer)
  } else {
    print("No se definio correctamente el metodo")
  }
}

#Ejemplo
IC(x,g,psi,metodo="AIC")
IC(x,g,psi,metodo="AWE")
IC(x,g,psi,metodo="BIC")
IC(x,g,psi,metodo="CLC")
IC(x,g,psi,metodo="KIC")

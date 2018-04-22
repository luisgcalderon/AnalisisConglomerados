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
    t<-t+psi$p[i]*dmvnorm(x = x,mean = psi$mu[,i],sigma = 
                                    matrix(data = psi$Sig[,i],nrow = m))
  }
  FVer<-sum(log(t))
  if (metodo=="AIC"|metodo=="ALL"){
  FVerAIC<-(-2)*FVer+2*(length(unlist(psi)))
    if (metodo=="AIC"){
      return(FVerAIC)
    }
  }
  if (metodo=="AWE"|metodo=="ALL"){
    mt<-matrix(0,nrow=n,ncol = g)
    for (i in 1:g) {
      mt[,i]<-psi$p[i]*dmvnorm(x = x,mean = psi$mu[,i],sigma = 
                            matrix(data = psi$Sig[,i],nrow=m))
      for (j in 1:n) {
        a<-numeric(1)
        for (w in 1:g) {
          a<-a+psi$p[w]*dmvnorm(x = x[j,],mean = psi$mu[,w],sigma = 
                                  matrix(data = psi$Sig[,w],nrow = m))
          
        }
        mt[j,i]<-mt[j,i]/a
      }
    }
    ent<-numeric(1)
    for (w in 1:g){
      lmt<-log(mt[,w])
      lmt[is.infinite(lmt)]<-0
      ent<-ent+mt[,w]%*%lmt
    }
  FVerAWE<-(-2)*(FVer-as.numeric(ent))+2*length(unlist(psi))*(3/2+log(n))
    if (metodo=="AWE"){
      return(FVerAWE)
    }
  }
  if (metodo=="BIC"|metodo=="ALL"){
    FVerBIC<-(-2)*FVer+(length(unlist(psi))*log(n))
    if (metodo=="BIC"){
      return(FVerBIC)
    }
  }
  if (metodo=="CLC"|metodo=="ALL"){
    if (metodo=="CLC"){
      mt<-matrix(0,nrow=n,ncol = g)
      for (i in 1:g) {
        mt[,i]<-psi$p[i]*dmvnorm(x = x,mean = psi$mu[,i],sigma = 
                                  matrix(data = psi$Sig[,i],nrow=m))
        for (j in 1:n) {
          a<-numeric(1)
          for (w in 1:g) {
            a<-a+psi$p[w]*dmvnorm(x = x[j,],mean = psi$mu[,w],sigma = 
                                    matrix(data = psi$Sig[,w],nrow = m))
          }
          mt[j,i]<-mt[j,i]/a
        }
      }
      ent<-numeric(1)
      for (w in 1:g){
        lmt<-log(mt[,w])
        lmt[is.infinite(lmt)]<-0
        ent<-ent+mt[,w]%*%lmt
      }
      FVerCLC<-(-2)*FVer+2*as.numeric(ent)
      return(FVerCLC)
    }
    FVerCLC<-(-2)*FVer+2*as.numeric(ent)
  }
  if (metodo=="KIC"|metodo=="ALL"){
    FVerKIC<-(-2)*FVer+3*(length(unlist(psi))+1)
    if (metodo=="KIC"){
      return(FVerKIC)  
    }
  } 
  if (metodo=="ALL"){
    return(c(AIC=FVerAIC,AWE=FVerAWE,BIC=FVerBIC,CLC=FVerCLC,KIC=FVerKIC))
  }else {
    print("No se definio correctamente el metodo")
  }
}

#Ejemplo
# IC(x,k,psi.k2,metodo="AIC")
# IC(x,g,psi,metodo="AWE")
# IC(x,g,psi,metodo="BIC")
# IC(x,g,psi,metodo="CLC")
# IC(x,g,psi,metodo="KIC")
# IC(x,k,psi.k2,metodo="ALL")

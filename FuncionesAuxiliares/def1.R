# Plot function ----
library(cowplot)
p1 <- ggplot(data = data.frame(x = c(-3, 3)), aes(x)) +
  stat_function(fun = dnorm, n = 101, args = list(mean = 0, sd = 1)) + ylab("") +
  scale_y_continuous(breaks = NULL)
p1

ggplot(data = data.frame(x = c(30, 100)), aes(x)) +
  stat_function(fun = dnorm,n = 101, args = list(mean=mean(x$waiting),sd=sd(x$waiting)),col="blue") +
  stat_function(fun = dnorm,n = 101, args = list(mean=mean(x$waiting)+1,sd=sd(x$waiting)),col="red") +
  q +
  ylab("")+
  scale_y_continuous(breaks = NULL)+
  geom_point(data = x,aes(waiting,0))

summary(x)
dnorm(96,mean=mean(x$waiting),sd=sd(x$waiting)) 

q<-function(c,me,des,colr){stat_function(fun = dnorm,n = 101, 
                                               args = list(mean=des,
                                                           sd=des,col=colr))
                                               }


?mvt
??mvt
#Idea particionar el dataframe como sumas de riemman
library(mvtnorm)
n<-2
dmvnorm(x = x,mean = psi[,],sigma = )
psi[,"mu.mu1"]
EMStepsMV<-function(x, g, psi, q){
  m<-dim(x)[2] #dimension de x
  n<-dim(x)[1] #numero de observaciones
  psi.t<-psi
  for (i in 1:(g-1)) {
    t<-numeric(n)
    t<-psi[i,"p"]*dmvnorm(x = x,mean = psi[1:m,1+i],sigma = 
                            matrix(data = c(psi[,g+2*i],psi[,g+2*i+1])))
    for (j in 1:n) {
      a<-numeric(n)
      for (w in 1:g) {
        a<-a+psi[w,1]*dmvnorm(x = x[j,],mean = psi[1:m,1+w],sigma = 
                                matrix(data = c(psi[,g+2*w],psi[,g+2*w+1]),nrow = 2))
        
      }
      t[j]<-t[j]/a
    }
    psi.t[i,"p"]<-sum(t)/n
    for (j in 1:m) {
      psi.t[j,1+i]<-sum(t%*%x[,j])/sum(t)
      
    }
    #estimacion de mu
  }
}

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
rainbowcols <- rainbow(6)



        

# EM 1-D Algorithm ----
library(mvtnorm)
# Target parameters for univariate normal distributions
p<- c(0.10,0.9) #Parametro de Proporcionalidad de Pimera Normal
mod.comp<-length(p)

mu <- c(1,1); sig<- c(2,4) #First Parameters
x<-faithful$waiting
l.psi<-data.frame(p,mu,sig)
EMFaithfull1d<-function(x, g, l.psi,mtd.e){
  l.psi.t<-l.psi
  
  # M Step
  for (i in 1:(g-1)) {
    t<-numeric(length(x))
    for (j in 1:length(x)) {
      t[j]<-(l.psi[i,"p"]*dnorm(x[j],mean = l.psi[i,"mu"],
                             sd = l.psi[i,"sig"]))
      a<-numeric(1)
      for (w in 1:(g)) {
        a<-a+(l.psi[w,"p"]*dnorm(x[j],mean = l.psi[w,"mu"],
                                 sd = l.psi[w,"sig"]))
      }
      t[j]<-t[j]/a
    }
    l.psi[i,"p"]=sum(t)/length(x)
  }
  l.psi[g,"p"]=1-sum(l.psi[-g,"p"])
  # E Step Estimate mu&sigma
  # estimate mu
  for (i in 1:g) {
    t<-numeric(length(x))
    t<-(l.psi[i,"p"]*dnorm(x,mean = l.psi[i,"mu"],
                           sd = l.psi[i,"sig"]))
    l.psi[i,"mu"]<-(t%*%x)/(sum(t))
    l.psi[i,"sig"]<-(t%*%((x-l.psi[i,"mu"])^2))/(sum(t))
  }
}


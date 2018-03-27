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
psi<-data.frame(p,mu,sig)
EMSteps<-function(x, g, psi){
  psi.t<-psi
  
  # M Step Estimate PI
  for (i in 1:(g-1)) {
    t<-numeric(length(x))
    #Compute Bayes Probability Formula
    for (j in 1:length(x)) {
      t[j]<-(psi.t[i,"p"]*dnorm(x[j],mean = psi.t[i,"mu"],
                             sd = psi.t[i,"sig"]))
      a<-t[j]
      for (w in ((1:g)[-i])) {
        a<-a+(psi.t[w,"p"]*dnorm(x[j],mean = psi.t[w,"mu"],
                                 sd = psi.t[w,"sig"]))
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
    t<-(psi.t[i,"p"]*dnorm(x,mean = psi.t[i,"mu"],
                           sd = psi.t[i,"sig"]))
    # estimate mu
    psi.t[i,"mu"]<-(t%*%x)/(sum(t))
    # estimate sigma
    psi.t[i,"sig"]<-(t%*%((x-psi.t[i,"mu"])^2))/(sum(t))
  }
  return(psi.t)
}


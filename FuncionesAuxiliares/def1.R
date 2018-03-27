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

# Funcion Convergencia-Iteracion ----


        

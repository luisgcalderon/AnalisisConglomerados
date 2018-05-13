#install.packages("factoextra")

library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

x<-faithful

xkm<-kmeans(x[,2],centers = 2)
xkm


rainbowcols <- rainbow(g)
x1<-cbind(x,cluster=xkm$cluster)

d<-list()
for (i in 1:g) {
  d<-c(stat_function(fun = dnorm,n = 100, args = list(mean=psi$mu[i],
                                                      sd=sqrt(psi$sig[i])),
                     color="black"),d)
}
plot1<-ggplot(x1,aes(waiting,-0.005))+
  geom_point() + d + theme(
    panel.background = element_rect(fill = "white"))+ylim(-0.01,0.1) + ylab("")+ggtitle("Viejo Fiel")
plot(plot1)


x<-cbind(waiting=faithful[,2],eruptions=faithful[,1])

k2<-kmeans(x,centers = 2)
k3<-kmeans(x,centers = 3)
fviz_cluster(xkm, data = x,lab)

p1 <- fviz_cluster(k2, geom = "point", data = x) + ggtitle("K-Medias",subtitle = "k=2") +theme_bw()
p2 <- fviz_cluster(k3, geom = "point",  data = x) + ggtitle("k = 3")
plot(p1)
plot(p2)

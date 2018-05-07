#Universidad Anahuac Mexico
#Proyecto de Investigacion
#Analisis de Conglomerados
#Luis Gerardo Calderon Contreras

#Clear Environment
rm(list = ls())

#Source

source('~/LuisCalderon/AnalisisConglomerados-RProject/AnalisisConglomerados/Algorithms/KMeans Algorithm/KMeans_Algorithm 20180420.R') #Algoritmos K-Means
source('~/Anahuac/AnalisisConglomerados/AnalisisConglomerados/Algorithms/EM Algorithm/EM Algorithm MV & Graphs  v1 20180329.R') #Algoritmo EM MV
source('~/LuisCalderon/AnalisisConglomerados-RProject/AnalisisConglomerados/Criterios de Informacion/Definicion de Funciones.R') #Criterios de Informacion
source('~/LuisCalderon/AnalisisConglomerados-RProject/AnalisisConglomerados/Modelo AHP/AHPClusterAnalysis beta 20180422.R') #Modelo AHP
#DataSet and Parameters ----

x<-faithful
g<-2 # cantidad de cluster Inicial
n<-4 # alternativas

#Step1 Hierarchy Structure----
#Graficar la estructura del Analisis

#Step2 DataSet Modelled with Algorithms----
##Step2.1 Modelled with K-Means
#Calcular los parametros iniciales para modelos de 2,3,4,5 componentes
#Correr K-MEANS
#Estimar Parametros Iniciales
psi.c2<-KMeansStep(x,2)
psi.c3<-KMeansStep(x,3)
psi.c4<-KMeansStep(x,4)
psi.c5<-KMeansStep(x,5)


##Step2.2 Modelled with EM
#Estimar los parametros finales

#Inputs datos,lista de psi, g, graf

psi.c2<-EMAlgorithmMV(dato = x,g = 2,psi = psi.c2,metodo = "verosi",difmin = 0.001)
psi.c3<-EMAlgorithmMV(dato = x,g = 3,psi = psi.c3,metodo = "verosi",difmin = 0.001)
psi.c4<-EMAlgorithmMV(dato = x,g = 4,psi = psi.c4,metodo = "verosi",difmin = 0.001)
psi.c5<-EMAlgorithmMV(dato = x,g = 5,psi = psi.c5,metodo = "verosi",difmin = 0.001)

#Step3 Criterion Information Validation----

MCriterion<-data.frame(matrix(data = c(IC(x,2,psi.c2,metodo="ALL"),IC(x,3,psi.c3,metodo="ALL"),IC(x,4,psi.c4,metodo="ALL"),IC(x,5,psi.c5,metodo="ALL"))
                            ,nrow = n,ncol = 5,byrow = T)); colnames(MCriterion)<-c("AIC","AWE","BIC","CLC","KIC");row.names(MCriterion)<-as.character(g:(g+n-1))
 MCriteriosEj<- read.csv("Modelo AHP/MatrizCriteriosEjemplo.csv")
 MCriteriosEj<-MCriteriosEj[,-1]
#View(MatrizCriteriosEjem)

MPropPairCom <- read.csv("Modelo AHP/ProposedPairwiseComparisonMatrix.csv")
#colnames(MPropPairCom)


AHPClusterAnalysis(MCriterion = MCriterion,MPropPairCom = MPropPairCom,n = n,g=g)
AHPClusterAnalysis(MCriterion = MCriteriosEj,MPropPairCom = MPropPairCom,n = n,g=g)

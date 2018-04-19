#Universidad Anahuac Mexico
#Proyecto de Investigacion
#Analisis de Conglomerados
#Luis Gerardo Calderon Contreras

#Proposed Pairwise Comparison Matrix

ProposedPairwiseComparisonMatrix <- read.csv("Modelo AHP/ProposedPairwiseComparisonMatrix.csv")
colnames(ProposedPairwiseComparisonMatrix)
CriterionMatrix<-as.matrix(ProposedPairwiseComparisonMatrix[,-1])


#Normalizar Matriz
sumc<-apply(CriterionMatrix,2,sum)
NMatrix<-CriterionMatrix
for (i in 1:(dim(CriterionMatrix)[1])) {
  NMatrix[i,]<-CriterionMatrix[i,]/sumc
}
apply(NMatrix,1,mean)

#AHP Model Analysis----

g<-2 # cantidad de cluster Inicial
n<-4 # alternativas

#Step1 Hierarchy Structure
#Graficar la estructura del Analisis

#Step2 DataSet Modelled with Algorithms
##Step2.1 Modelled with K-Means
#Calcular los parametros iniciales para modelos de 2,3,4,5 componentes
#Correr K-MEANS

#Estimar Parametros Iniciales

##Step2.2 Modelled with EM
#Estimar los parametros finales

#Inputs datos,psi, g, graf
psi.2<-EMAlgorithmMV(dato = x,g = g,psi = psi.2,metodo = "verosi",difmin = 0.001)
psi.3<-EMAlgorithmMV(dato = x,g = g+1,psi = psi.3,metodo = "verosi",difmin = 0.001)
psi.4<-EMAlgorithmMV(dato = x,g = g+2,psi = psi.4,metodo = "verosi",difmin = 0.001)
psi.5<-EMAlgorithmMV(dato = x,g = g+3,psi = psi.5,metodo = "verosi",difmin = 0.001)

#Step3 Criterion Information Validation

DeMatrix<-data.frame(matrix(data = c(IC(x,g,psi,metodo="ALL"),IC(x,g,psi,metodo="ALL"),IC(x,g,psi,metodo="ALL"),IC(x,g,psi,metodo="ALL"))
                            ,nrow = n,ncol = 5,byrow = T)); colnames(DeMatrix)<-c("AIC","AWE","BIC","CLC","KIC");row.names(DeMatrix)<-as.character(g:(g+n-1))



#Step4 Pairwise comparison Matrices for each Criterio
#AIC

#AWE
#BIC
#CLC
#KIC

#Step5 C-RIV 

#Step6 Highest C-RIV

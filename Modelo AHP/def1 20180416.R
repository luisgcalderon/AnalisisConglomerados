#Universidad Anahuac Mexico
#Proyecto de Investigacion
#Analisis de Conglomerados
#Luis Gerardo Calderon Contreras


#AHP Model Analysis----

g<-2 # cantidad de cluster Inicial
n<-4 # alternativas

#Step1 Hierarchy Structure----
#Graficar la estructura del Analisis

#Step2 DataSet Modelled with Algorithms----
##Step2.1 Modelled with K-Means
#Calcular los parametros iniciales para modelos de 2,3,4,5 componentes
#Correr K-MEANS

#Estimar Parametros Iniciales

##Step2.2 Modelled with EM
#Estimar los parametros finales

#Inputs datos,lista de psi, g, graf

psi.2<-EMAlgorithmMV(dato = x,g = 2,psi = psi.2,metodo = "verosi",difmin = 0.001)
psi.3<-EMAlgorithmMV(dato = x,g = 3,psi = psi.3,metodo = "verosi",difmin = 0.001)
psi.4<-EMAlgorithmMV(dato = x,g = 4,psi = psi.4,metodo = "verosi",difmin = 0.001)
psi.5<-EMAlgorithmMV(dato = x,g = 5,psi = psi.5,metodo = "verosi",difmin = 0.001)

#Step3 Criterion Information Validation----

MCriterion<-data.frame(matrix(data = c(IC(x,g,psi,metodo="ALL"),IC(x,g,psi,metodo="ALL"),IC(x,g,psi,metodo="ALL"),IC(x,g,psi,metodo="ALL"))
                            ,nrow = n,ncol = 5,byrow = T)); colnames(DeMatrix)<-c("AIC","AWE","BIC","CLC","KIC");row.names(DeMatrix)<-as.character(g:(g+n-1))


AHPClusterAnalysis<-function(MCriterion){

#Step4 Pairwise comparison Matrices for each Criterio----
MCriteriosEj <- read.csv("Modelo AHP/MatrizCriteriosEjemplo.csv")
MCriteriosEj[,-1]<-1/MCriteriosEj[,-1]*100
#View(MatrizCriteriosEjem)
MPairwiseComparison<-array(data=0,dim = c(n,n,5))
#ALL Pairwise Comparison Matrices
for (i in 1:5) {
  for (j in 1:4) {
    MPairwiseComparison[j,,i]<-MCriteriosEj[j,i+1]/MCriteriosEj[,i+1]
  }
}

# PairwiseComparisonMatri
#AIC PairwiseComparisonMatri[,,1]
#AWE PairwiseComparisonMatri[,,2]
#BIC PairwiseComparisonMatri[,,3]
#CLC PairwiseComparisonMatri[,,4]
#KIC PairwiseComparisonMatri[,,5]

#Step5 Consistency Test C-RIV ----

#Normalizar Matriz

for (i in 1:5) {
  sumc<-apply(MPairwiseComparison[,,i],2,sum)
  for (j in 1:(dim(MPairwiseComparison[,,i])[1])) {
    MPairwiseComparison[j,,i]<-MPairwiseComparison[j,,i]/sumc
  }
}

# Matriz de Composite Relative Importance Vector (C-RIV)

MCRIV<-data.frame(matrix(data=0,nrow = n,ncol = 5)); colnames(MCRIV)<-c("AIC","AWE","BIC","CLC","KIC");row.names(MCRIV)<-as.character(g:(g+n-1))
for(i in 1:5){
  MCRIV[,i]<-apply(MPairwiseComparison[,,i],1,mean)
}
#Step6 Highest C-RIV----

#Proposed Pairwise Comparison Matrix

MPropPairCom <- read.csv("Modelo AHP/ProposedPairwiseComparisonMatrix.csv")
#colnames(MPropPairCom)
MCriterion<-as.matrix(MPropPairCom[,-1])


#Normalizar Matriz
sumc<-apply(MCriterion,2,sum)

for (i in 1:(dim(MCriterion)[1])) {
  MCriterion[i,]<-MCriterion[i,]/sumc
}
CRIV_Criterion<-apply(MCriterion,1,mean)
CRIV<-as.matrix(MCRIV)%*%as.matrix(CRIV_Criterion)
return(CRIV)
}
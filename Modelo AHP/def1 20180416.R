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

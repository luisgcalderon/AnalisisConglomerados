AHPClusterAnalysis<-function(MCriterion,MPropPairCom,n,g){
  #Step4 Pairwise comparison Matrices for each Criterio----
  
  MCriterios<-1/MCriterion*100
  MPairwiseComparison<-array(data=0,dim = c(n,n,5))
  #ALL Pairwise Comparison Matrices
  for (i in 1:5) {
    for (j in 1:n) {
      MPairwiseComparison[j,,i]<-MCriterios[j,i]/MCriterios[,i]
    }
  }
  # View(MPairwiseComparison)
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

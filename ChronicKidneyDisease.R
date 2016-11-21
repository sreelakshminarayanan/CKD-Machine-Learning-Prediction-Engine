LoadLibraries<-function() {
  
  source("Preprocess.R")
  source("Outlier.R")
  source("RegressionAnalysis.R")
  source("LogisticRegression.R")
  source("DecisionTree.R")
  source("Clustering.R")
  source("PredictCKD.R")
  library('VIM')
}
main<-function() {
  
  #dev.off()
  #sink()
  CKD_Dataset<-readCKDData("Input","chronic_kidney_disease_formatted.csv",";")
  
  CKD_Dataset<-Compute_MissingValues(CKD_Dataset)
  
  CKD_Dataset<-HandleNoisy_Outlier_Data(CKD_Dataset)
  
  CKD_Dataset<-PerformRegressionAnalysis(CKD_Dataset)
  
  GenerateLogisticRegresion(CKD_Dataset,"LogisticRegressionOuput.txt")
  
  DecisionTree<-GenerateDecisionTree(CKD_Dataset)
  
  #  PredictCKD(DecisionTree)
  
  
}

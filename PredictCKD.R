library(tree)
Predict<-function(){
  source("Preprocess.R")
  DecisionTreepath=paste(c(getwd(),"/","Output","/","DecisionTree.txt"), collapse='')
  load(file=DecisionTreepath)
  CKD_Dataset_output<-PredictCKD(CKD_Tree)
  filename<-paste(c(getwd(),"/Output/","CKD_Output.csv"), collapse='')
  write.csv(CKD_Dataset_output,filename,row.names = FALSE)
 # dev.off()
#  print(CKD_Dataset_output)
}
PredictCKD<-function(DecisionTree) 
{
    Input_dataset<-readCKDData("Input","Predict.csv",",")
    head(Input_dataset)
    Input_dataset$Specific_Gravity<-as.factor(Input_dataset$Specific_Gravity)
    Input_dataset$Albumin<-as.factor(Input_dataset$Albumin)
    Input_dataset$Sugar<-as.factor(Input_dataset$Sugar)
    
    Input_dataset$CKD<-predict(DecisionTree,Input_dataset)
    Input_dataset$CKD_Output <- colnames(Input_dataset$CKD)[max.col(Input_dataset$CKD, ties.method = c("random"))] # predicted
    return(Input_dataset)

}
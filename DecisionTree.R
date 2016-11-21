library(tree)
library(rattle)
library(rpart)

#install.packages(rpart)
#library(rpart.plot)
#rattle()
#library(RColorBrewer)
GenerateDecisionTree<-function(CKD_Dataset)

{
          CKD_Dataset$TreeCKD<-NULL
          CKD_Dataset$pred.response<-NULL
          CKD_Dataset$Matches<-NULL
          CKD_Dataset$Result<-NULL
          sink()
          head(CKD_Dataset)
          CKD_Dataset<-CKD_Dataset[,c(-20)]
          trainingRowIndex <- sample(1:nrow(CKD_Dataset), 0.8*nrow(CKD_Dataset))  # row incices for training data
          #summary(CKD_Dataset)
          CKD_training_Dataset <- CKD_Dataset[trainingRowIndex, ]  # model training data
          #head(CKD_training_Dataset)
          CKD_test_Dataset  <- CKD_Dataset[-trainingRowIndex, ]   # test data
          
         # CKD_Tree <- tree(Kidney_Disease~.,data=CKD_training_Dataset)  # model the tree, including all the variables
          
          CKD_Tree <- tree(Kidney_Disease~Specific_Gravity+
                             Albumin+
                             Sugar+
                             
                             Pus_Cell+
                             Hemoglobin+
                             Serum_Creatinine+
                             Packed_Cell_Volume+
                             Sodium+Potassium,data=CKD_training_Dataset)  # model the tree, including all the variables
          plot(CKD_Tree)  
          text(CKD_Tree, pretty=0)  # Add text to the plot
          filename<-paste(c(getwd(),"/","Report","/","DecisionTree.txt"), collapse='')
          print(filename)
          sink(filename)
          CKD_test_Dataset$TreeCKD<-predict(CKD_Tree,CKD_test_Dataset)
          CKD_Dataset$TreeCKD<-predict(CKD_Tree,CKD_Dataset)
          
          pred.response <- colnames(CKD_test_Dataset$TreeCKD)[max.col(CKD_test_Dataset$TreeCKD, ties.method = c("random"))] # predicted
          CKD_Dataset$pred.response <- colnames(CKD_Dataset$TreeCKD)[max.col(CKD_Dataset$TreeCKD, ties.method = c("random"))] # predicted
          pred.response_all <- colnames(CKD_Dataset$TreeCKD)[max.col(CKD_Dataset$TreeCKD, ties.method = c("random"))] # predicted
          
          CKD_Dataset$Result <- colnames(CKD_Dataset$TreeCKD)[max.col(CKD_Dataset$TreeCKD, ties.method = c("random"))] # predicted
          
          CKD_Dataset$pred.response<-as.factor(CKD_Dataset$pred.response)
          summary(CKD_Dataset$pred.response)
          CKD_Dataset$Matches<-ifelse(CKD_Dataset$Kidney_Disease == CKD_Dataset$pred.response,"Matches","MisMatches")
          CKD_Dataset$Matches<-as.factor(CKD_Dataset$Matches)
          print("Match Report")
          
          print(summary(CKD_Dataset$Matches))
          print("")
          print("Match Report of test dataset")
          print(mean(CKD_test_Dataset$Kidney_Disease == pred.response)) # Calculate Mis-classification error.
          print("Match report of entire dataset")
          print(mean(CKD_Dataset$Kidney_Disease == pred.response_all)) # Calculate Mis-classification error.
          sink()
          treePrunedMod <- prune.misclass(CKD_Tree, best = 8) # set size corresponding to lowest value in prev plot.
       
          filename<-paste(c(getwd(),"/","Report","/","DecisionTree.jpeg"), collapse='')
          jpeg(filename,width = 900,height=900)
          
          plot(treePrunedMod)
          text(treePrunedMod, pretty=0)
          dev.off()
          DecisionTreepath=paste(c(getwd(),"/","Output","/","DecisionTree.txt"), collapse='')
          save(CKD_Tree,file=DecisionTreepath, ascii=TRUE)
          writeCKDData(CKD_Dataset,"Report","CKD_DecisionTree_MatchReport.csv")
          return(CKD_Tree)
  }
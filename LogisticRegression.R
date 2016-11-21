#rm(list=ls())
#install.packages("arulesViz")

library(ROCR)
library(arulesViz)

#CKD_Dataset <- read.csv("C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_FMT_BaseLine.csv",sep = ",", header=T) 
#CKD_Dataset$Albumin=as.factor(CKD_Dataset$Albumin)

#CKD_Dataset$Sugar=as.factor(CKD_Dataset$Sugar)
#CKD_Dataset$Specific_Gravity=as.factor(CKD_Dataset$Specific_Gravity)
#CKD_Dataset$Kidney_Disease=as.factor(CKD_Dataset$Kidney_Disease)
#summary(CKD_Dataset)
#iv.plot.summary(iv.mult(CKD_Dataset,"Kidney_Disease",vars=c("Specific_Gravity","Hemoglobin","Albumin","Sugar","Pus_Cell","Blood_Urea","Serum_Creatinine","Packed_Cell_Volume","Pedal_Edema"),TRUE))


#head(CKD_Dataset)
#set.seed(100)  # setting seed to reproduce results of random sampling

GenerateLogisticRegresion <- function (CKD_Dataset,fileidentifier) {

        CK_Dataset<-CKD_Dataset
        
        trainingRowIndex <- sample(1:nrow(CKD_Dataset), 0.8*nrow(CKD_Dataset))  # row incices for training data
        #summary(CKD_Dataset)
        CKD_training_Dataset <- CKD_Dataset[trainingRowIndex, ]  # model training data
        #head(CKD_training_Dataset)
        CKD_test_Dataset  <- CKD_Dataset[-trainingRowIndex, ]   # test data
        
        CKDModel<-glm(Kidney_Disease~Specific_Gravity+
                      Albumin+
                      Sugar+
                        
                      Pus_Cell+
                      Hemoglobin+
                      Serum_Creatinine+
                      Packed_Cell_Volume+
                      Sodium+Potassium,data=CKD_training_Dataset,family=binomial())
        
        filename<-paste(c(getwd(),"/","Report","/",fileidentifier), collapse='')
        
        sink(filename)
        print(summary(CKDModel))
        
        
        CKD_test_Dataset$prob<-predict(CKDModel,CKD_test_Dataset,type="response")
        CKD_training_Dataset$prob<-predict(CKDModel,CKD_training_Dataset,type="response")
        
        #summary(CKD_test_Dataset$prob)
        
        actuals_preds <- data.frame(cbind(actuals=CKD_test_Dataset$Kidney_Disease, predicteds=CKD_test_Dataset$prob))  # make actuals_predicteds dataframe.
        correlation_accuracy <- cor(actuals_preds)
        print("Correlation accurancy for test dataset")
        print(correlation_accuracy)
        
        actuals_tr_preds <- data.frame(cbind(actuals=CKD_training_Dataset$Kidney_Disease, predicteds=CKD_training_Dataset$prob))  # make actuals_predicteds dataframe.
        correlation_tr_accuracy <- cor(actuals_tr_preds)
        print("Corelation Accuracy for training dataset")
        print(correlation_tr_accuracy)
        print(CalculateConcordance(CKDModel))
        
       
        
        sink()
       
        
}


hosmerlem = function(y, yhat, g=10) { 
          cutyhat = cut(yhat, 
                        breaks = quantile(yhat, probs=seq(0, 
                                                          1, 1/g)), include.lowest=TRUE) 
          obs = xtabs(cbind(1 - y, y) ~ cutyhat) 
          expect = xtabs(cbind(1 - yhat, yhat) ~ cutyhat) 
          chisq = sum((obs - expect)^2/expect) 
          P = 1 - pchisq(chisq, g - 2) 
          return(list(chisq=chisq,p.value=P)) 
} 
        
       
        
    
        options(scipen = 999)
        
        
        
        CalculateConcordance <- function (myMod){
          
          fitted <- data.frame (cbind (myMod$y, myMod$fitted.values)) # actuals and fitted
          
          colnames(fitted) <- c('response','score') # rename columns
          
          ones <- fitted[fitted$response==1, ] # Subset ones
          
          zeros <- fitted[fitted$response==0, ] # Subsetzeros
          
          totalPairs <- nrow (ones) * nrow (zeros) # calculate total number of pairs to check
          
          conc <- sum (c (vapply (ones$score, function(x) {((x > zeros$score))}, FUN.VALUE=logical(nrow(zeros)))))
          
          disc <- totalPairs - conc
          
          # Calc concordance, discordance and ties
          
          concordance <- conc/totalPairs
          
          discordance <- disc/totalPairs
          
          tiesPercent <- (1-concordance-discordance)
          
          return(list("Concordance"=concordance, "Discordance"=discordance,
                      
                      "Tied"=tiesPercent, "Pairs"=totalPairs))
          
        }
        
       
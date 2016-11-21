
# Weightage of Evidence

library(woe)
PerformRegressionAnalysis<-function(CKD_Dataset) {
  
          iFMTBCKD<-CKD_Dataset
  
          filename=paste("Regression Summary",".txt")
  
          filename=paste(c(getwd(),"/Report/",filename), collapse='')
        
          sink(filename, type=c("output", "message"), append=TRUE, split=FALSE)
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Age"),FALSE))
         # Determined that Agehas least significance on CKD
      
          # Determined that blood pressure has least significance on CKD
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Blood_Pressure"),FALSE))
      
      
          iFMTBCKD$Specific_Gravity<-as.factor(iFMTBCKD$Specific_Gravity)
           print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Specific_Gravity"),FALSE))
      
          levels(iFMTBCKD$Specific_Gravity)[levels(iFMTBCKD$Specific_Gravity)=="1.005"] <- "1"
          levels(iFMTBCKD$Specific_Gravity)[levels(iFMTBCKD$Specific_Gravity)=="1.01"] <- "1"
          levels(iFMTBCKD$Specific_Gravity)[levels(iFMTBCKD$Specific_Gravity)=="1.015"] <- "1"
          levels(iFMTBCKD$Specific_Gravity)[levels(iFMTBCKD$Specific_Gravity)=="1.02"] <- "2"
          levels(iFMTBCKD$Specific_Gravity)[levels(iFMTBCKD$Specific_Gravity)=="1.025"] <- "2"

          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Albumin"),FALSE))
          iFMTBCKD$Albumin<-as.factor(iFMTBCKD$Albumin)
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Albumin"),FALSE))
          
          levels(iFMTBCKD$Albumin)[levels(iFMTBCKD$Albumin)=="2"] <- "1"
          levels(iFMTBCKD$Albumin)[levels(iFMTBCKD$Albumin)=="3"] <- "1"
          levels(iFMTBCKD$Albumin)[levels(iFMTBCKD$Albumin)=="4"] <- "1"
          levels(iFMTBCKD$Albumin)[levels(iFMTBCKD$Albumin)=="5"] <- "1"
          
          iFMTBCKD$Sugar<-as.factor(iFMTBCKD$Sugar)
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Sugar"),FALSE))
          
          levels(iFMTBCKD$Sugar)[levels(iFMTBCKD$Sugar)=="2"] <- "1"
          levels(iFMTBCKD$Sugar)[levels(iFMTBCKD$Sugar)=="3"] <- "1"
          levels(iFMTBCKD$Sugar)[levels(iFMTBCKD$Sugar)=="4"] <- "1"
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Pus_Cell"),FALSE))
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Pus_Cell_clumps"),FALSE))
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Bacteria"),FALSE))
          
          
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Blood_Glucose"),FALSE))
          
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Blood_Urea"),FALSE))
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Serum_Creatinine"),FALSE))
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Sodium"),FALSE))
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Potassium"),FALSE))
          
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Hemoglobin"),FALSE))
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Packed_Cell_Volume"),FALSE))
          
          
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("White_Blood_Cell_Count"),FALSE))
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Hypertension"),FALSE))
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Diabetes_Mellitus"),FALSE))
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Coronary_Artery_Disease"),FALSE))
          
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Appetite"),FALSE))
          
          
          print(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Pedal_Edema"),FALSE))
          
          sink();
          return(iFMTBCKD)

}
#iv.plot.summary(iv.mult(iFMTBCKD,"Kidney_Disease",vars=c("Specific_Gravity","Albumin","Sugar","Pus_Cell","Blood_Urea","Serum_Creatinine","Packed_Cell_Volume","Pedal_Edema"),TRUE))
#write.csv(iFMTBCKD,file="C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_FMT_BaseLine.csv",row.names=FALSE)

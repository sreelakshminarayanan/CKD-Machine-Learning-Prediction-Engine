
HandleNoisy_Outlier_Data<-function(CKD_Dataset) {
  
  CKD_Dataset<-CKD_Dataset
  CheckOutlier(CKD_Dataset,"CKD_Before_Outlier_Elimination.pdf")
  CKD_Dataset<-HandleOutliers(CKD_Dataset)
  CheckOutlier(CKD_Dataset,"CKD_After_Outlier_Elimination.pdf")
  return(CKD_Dataset)
  
}
CheckOutlier<-function(CKD_Dataset,filename) {
  
        iFMTCKD<-CKD_Dataset
        iFMTCKD$Albumin=as.factor(iFMTCKD$Albumin)
        iFMTCKD$Albumin=as.factor(iFMTCKD$Albumin)
        
        iFMTCKD$Sugar=as.factor(iFMTCKD$Sugar)
        iFMTCKD$Specific_Gravity=as.factor(iFMTCKD$Specific_Gravity)
        iFMTCKD$Kidney_Disease=as.factor(iFMTCKD$Kidney_Disease)
        
        
        filename=paste(c(getwd(),"/Report/",filename), collapse='')
        pdf(filename)
        boxplot(iFMTCKD$Blood_Pressure,horizontal = T,main="Box Plot of Blood Pressure")
        boxplot(iFMTCKD$Blood_Glucose,horizontal = T,main="Box Plot of Blood Glucose")
        boxplot(iFMTCKD$Blood_Urea,horizontal = T,main="Box Plot of Blood_Urea")
        boxplot(iFMTCKD$Serum_Creatinine,horizontal = T,main="Box Plot Serum_Creatinine ")
        boxplot(iFMTCKD$Sodium,horizontal = T,main="Box Plot Sodium ")
        boxplot(iFMTCKD$Potassium,horizontal = T,main="Box Plot Potassium ")
        boxplot(iFMTCKD$Packed_Cell_Volume,horizontal = T,main="Box Plot Packed_Cell_Volume ")
        boxplot(iFMTCKD$White_Blood_Cell_Count,horizontal = T,main="Box Plot White_Blood_Cell_Count ")
        dev.off()
   
}  
 
HandleOutliers<-function(CKD_Dataset) {
          iFMTCKD<-CKD_Dataset
          iFMTCKD$Age<-ifelse(iFMTCKD$Age<quantile(iFMTCKD$Age,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],quantile(iFMTCKD$Age,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],iFMTCKD$Age)
          iFMTCKD$Blood_Pressure<-ifelse(iFMTCKD$Blood_Pressure>quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],iFMTCKD$Blood_Pressure)
          iFMTCKD$Blood_Pressure<-ifelse(iFMTCKD$Blood_Pressure>quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],iFMTCKD$Blood_Pressure)
          iFMTCKD$Blood_Pressure<-ifelse(iFMTCKD$Blood_Pressure<quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],iFMTCKD$Blood_Pressure)
          iFMTCKD$Blood_Glucose<-ifelse(iFMTCKD$Blood_Glucose>quantile(iFMTCKD$Blood_Glucose,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],quantile(iFMTCKD$Blood_Glucose,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],iFMTCKD$Blood_Glucose)
          iFMTCKD$Blood_Urea<-ifelse(iFMTCKD$Blood_Urea>quantile(iFMTCKD$Blood_Urea,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],quantile(iFMTCKD$Blood_Urea,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],iFMTCKD$Blood_Urea)
          iFMTCKD$Serum_Creatinine<-ifelse(iFMTCKD$Serum_Creatinine>quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],iFMTCKD$Serum_Creatinine)
          iFMTCKD$Serum_Creatinine<-ifelse(iFMTCKD$Serum_Creatinine<quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],iFMTCKD$Serum_Creatinine)
          iFMTCKD$Sodium<-ifelse(iFMTCKD$Sodium<quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],iFMTCKD$Sodium)
          iFMTCKD$Sodium<-ifelse(iFMTCKD$Sodium>quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[9],quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[9],iFMTCKD$Sodium)
          iFMTCKD$Potassium<-ifelse(iFMTCKD$Potassium<quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[3],quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[3],iFMTCKD$Potassium)
          iFMTCKD$Potassium<-ifelse(iFMTCKD$Potassium>quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],iFMTCKD$Potassium)
          iFMTCKD$White_Blood_Cell_Count<-ifelse(iFMTCKD$White_Blood_Cell_Count<quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1],quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1],iFMTCKD$White_Blood_Cell_Count)
          iFMTCKD[which(iFMTCKD$White_Blood_Cell_Count>quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8]),]
          iFMTCKD$White_Blood_Cell_Count<-ifelse(iFMTCKD$White_Blood_Cell_Count>quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],iFMTCKD$White_Blood_Cell_Count)
          return(iFMTCKD)

}
#boxplot(iFMTCKD[,c(-3,-4,-5,-6,-7,-8,-9,-19,-20,-21,-22,-23,-24,-25)],horizontal=T)
#str(iFMTCKD)
#barplot(iFMTCKD$Specific_Gravity)
#boxplot(iFMTCKD$Age,horizontal = T)
#quantile(iFMTCKD$Age,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))
#iFMTCKD[which(iFMTCKD$Age<quantile(iFMTCKD$Age,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2]),]

#summary(iFMTCKD$Hemoglobin)

#boxplot(iFMTCKD$Blood_Pressure,horizontal = T)
#quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))
#iFMTCKD[which(iFMTCKD$Blood_Pressure>quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[6]),]
#iFMTCKD$Blood_Pressure<-ifelse(iFMTCKD$Blood_Pressure>quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],iFMTCKD$Blood_Pressure)
#iFMTCKD$Blood_Pressure<-ifelse(iFMTCKD$Blood_Pressure>quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],iFMTCKD$Blood_Pressure)
#iFMTCKD[which(iFMTCKD$Blood_Pressure<quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2]),]
#iFMTCKD$Blood_Pressure<-ifelse(iFMTCKD$Blood_Pressure<quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],quantile(iFMTCKD$Blood_Pressure,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],iFMTCKD$Blood_Pressure)
#iFMTCKD$Packed_Cell_Volume<-ifelse(iFMTCKD$Packed_Cell_Volume<quantile(iFMTCKD$Packed_Cell_Volume,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1],quantile(iFMTCKD$Packed_Cell_Volume,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1],iFMTCKD$Packed_Cell_Volume)
#iFMTCKD$White_Blood_Cell_Count<-ifelse(iFMTCKD$White_Blood_Cell_Count>quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],iFMTCKD$White_Blood_Cell_Count)
#iFMTCKD$White_Blood_Cell_Count<-ifelse(iFMTCKD$White_Blood_Cell_Count>quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],iFMTCKD$White_Blood_Cell_Count)


#boxplot(iFMTCKD$Blood_Glucose,horizontal = T)
#quantile(iFMTCKD$Blood_Glucose,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))
#iFMTCKD$Blood_Glucose<-ifelse(iFMTCKD$Blood_Glucose>quantile(iFMTCKD$Blood_Glucose,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],quantile(iFMTCKD$Blood_Glucose,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],iFMTCKD$Blood_Glucose)


#boxplot(iFMTCKD$Blood_Urea,horizontal = T)
#quantile(iFMTCKD$Blood_Urea,c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))
#iFMTCKD[which(iFMTCKD$Blood_Urea>quantile(iFMTCKD$Blood_Urea,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7]),]
#iFMTCKD$Blood_Urea<-ifelse(iFMTCKD$Blood_Urea>quantile(iFMTCKD$Blood_Urea,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],quantile(iFMTCKD$Blood_Urea,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],iFMTCKD$Blood_Urea)

#boxplot(iFMTCKD$Serum_Creatinine,horizontal = T)

#quantile(iFMTCKD$Serum_Creatinine,c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.87,0.90,0.95,0.99,1))
#iFMTCKD[which(iFMTCKD$Serum_Creatinine>quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[6]),]
#iFMTCKD$Serum_Creatinine<-ifelse(iFMTCKD$Serum_Creatinine>quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[7],iFMTCKD$Serum_Creatinine)

#iFMTCKD[which(iFMTCKD$Serum_Creatinine<quantile(iFMTCKD$Serum_Creatinine,c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2]),]
#iFMTCKD$Serum_Creatinine<-ifelse(iFMTCKD$Serum_Creatinine<quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],quantile(iFMTCKD$Serum_Creatinine,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],iFMTCKD$Serum_Creatinine)

#boxplot(iFMTCKD$Serum_Creatinine,horizontal = T)

  

#boxplot(iFMTCKD$Sodium,horizontal = T)
#quantile(iFMTCKD$Sodium,c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))
#iFMTCKD[which(iFMTCKD$Sodium<quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2]),]
#iFMTCKD$Sodium<-ifelse(iFMTCKD$Sodium<quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[2],iFMTCKD$Sodium)
#iFMTCKD[which(iFMTCKD$Sodium>quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[9]),]
#iFMTCKD$Sodium<-ifelse(iFMTCKD$Sodium>quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[9],quantile(iFMTCKD$Sodium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[9],iFMTCKD$Sodium)

#boxplot(iFMTCKD$Potassium,horizontal = T)
#quantile(iFMTCKD$Potassium,c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))
#iFMTCKD[which(iFMTCKD$Potassium<quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[3]),]
#iFMTCKD$Potassium<-ifelse(iFMTCKD$Potassium<quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[3],quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[3],iFMTCKD$Potassium)
#iFMTCKD[which(iFMTCKD$Potassium>quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8]),]
#iFMTCKD$Potassium<-ifelse(iFMTCKD$Potassium>quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],iFMTCKD$Potassium)
#iFMTCKD[which(iFMTCKD$Potassium>quantile(iFMTCKD$Potassium,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8]),]

#str(iFMTCKD)

#boxplot(iFMTCKD$Packed_Cell_Volume,horizontal = T)
#quantile(iFMTCKD$Packed_Cell_Volume,c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))

#iFMTCKD[which(iFMTCKD$Packed_Cell_Volume<quantile(iFMTCKD$Packed_Cell_Volume,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1]),]
#iFMTCKD$Packed_Cell_Volume<-ifelse(iFMTCKD$Packed_Cell_Volume<quantile(iFMTCKD$Packed_Cell_Volume,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1],quantile(iFMTCKD$Packed_Cell_Volume,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1],iFMTCKD$Packed_Cell_Volume)

#boxplot(iFMTCKD$White_Blood_Cell_Count,horizontal = T)

#quantile(iFMTCKD$White_Blood_Cell_Count,c(0,0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))

#iFMTCKD[which(iFMTCKD$White_Blood_Cell_Count>quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8]),]

#iFMTCKD$White_Blood_Cell_Count<-ifelse(iFMTCKD$White_Blood_Cell_Count>quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[8],iFMTCKD$White_Blood_Cell_Count)

#iFMTCKD[which(iFMTCKD$White_Blood_Cell_Count<quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1]),]

#iFMTCKD$White_Blood_Cell_Count<-ifelse(iFMTCKD$White_Blood_Cell_Count<quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1],quantile(iFMTCKD$White_Blood_Cell_Count,c(0.01,0.05,0.1,0.25,0.50,0.75,0.90,0.95,0.99,1))[1],iFMTCKD$White_Blood_Cell_Count)

#write.csv(iFMTCKD,file="C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_BaseLine.csv",row.names=FALSE)
#head(iFMTCKD)

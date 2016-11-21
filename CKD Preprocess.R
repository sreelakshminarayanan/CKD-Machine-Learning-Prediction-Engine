install.packages("rrcovNA")

CKD <- read.csv("C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/chronic_kidney_disease_formatted.csv",sep = ";", header=T) 
# COlumns Red Blood Cells and Red Blood Cell count were removed there were  30% missing values
rm(list=ls())
str(CKD)

summary(CKD)
head(CKD)
boxplot(CKD$Blood_Pressure)
summary(CKD$Age)
summary(CKD$Blood_Pressure)
summary(CKD$Pedal_Edema)
summary(CKD$Hypertension)

summary(CKD$Specific_Gravity)
summary(CKD$Pus_Cell_clumps)
summary(CKD$Bacteria)
NormalizeLevels<-function (CKD_dataset) {
        
  
        CKD$Albumin=as.factor(CKD$Albumin)
        CKD$Sugar=as.factor(CKD$Sugar)
        CKD$Specific_Gravity=as.factor(CKD$Specific_Gravity)

        # Replace 1..005 as Factor of 1
        # Replace 1..01 as Factor of 2
        # Replace 1..015 as Factor of 3
        # Replace 1..02 as Factor of 4
        # Replace 1..025 as Factor of 5
        levels(CKD$Specific_Gravity)[levels(CKD$Specific_Gravity)=="1.005"] <- "1"
        levels(CKD$Specific_Gravity)[levels(CKD$Specific_Gravity)=="1.01"] <- "2"
        levels(CKD$Specific_Gravity)[levels(CKD$Specific_Gravity)=="1.015"] <- "3"
        levels(CKD$Specific_Gravity)[levels(CKD$Specific_Gravity)=="1.02"] <- "4"
        levels(CKD$Specific_Gravity)[levels(CKD$Specific_Gravity)=="1.025"] <- "5"
        # Convert Alumbin as Factor variable



        # Mark Pus cells Normal as 1
        # Mark Pus Cells abnormal as 0
        levels(CKD$Pus_Cell)[levels(CKD$Pus_Cell)=="normal"] <- "1"
        levels(CKD$Pus_Cell)[levels(CKD$Pus_Cell)=="abnormal"] <- "0"
        
        
        levels(CKD$Pus_Cell_clumps)[levels(CKD$Pus_Cell_clumps)=="present"] <- "1"
        levels(CKD$Pus_Cell_clumps)[levels(CKD$Pus_Cell_clumps)=="notpresent"] <- "0"

        levels(CKD$Bacteria)[levels(CKD$Bacteria)=="present"] <- "1"
        levels(CKD$Bacteria)[levels(CKD$Bacteria)=="notpresent"] <- "0"
        
        
        levels(CKD$Hypertension)[levels(CKD$Hypertension)=="yes"] <- "1"
        levels(CKD$Hypertension)[levels(CKD$Hypertension)=="no"] <- "0"
        
        
        
        levels(CKD$Diabetes_Mellitus)[levels(CKD$Diabetes_Mellitus)=="yes"] <- "1"
        levels(CKD$Diabetes_Mellitus)[levels(CKD$Diabetes_Mellitus)=="no"] <- "0"
        
        
        levels(CKD$Coronary_Artery_Disease)[levels(CKD$Coronary_Artery_Disease)=="yes"] <- "1"
        levels(CKD$Coronary_Artery_Disease)[levels(CKD$Coronary_Artery_Disease)=="no"] <- "0"
        
        levels(CKD$Appetite)[levels(CKD$Appetite)=="good"] <- "1"
        levels(CKD$Appetite)[levels(CKD$Appetite)=="no"] <- "2"
        
        levels(CKD$Appetite)[levels(CKD$Appetite)=="poor"] <- "3"
        
        levels(CKD$Pedal_Edema)[levels(CKD$Pedal_Edema)=="yes"] <- "1"
        levels(CKD$Pedal_Edema)[levels(CKD$Pedal_Edema)=="no"] <- "2"
        
        levels(CKD$Pedal_Edema)[levels(CKD$Pedal_Edema)=="good"] <- NA
        
        
        
        levels(CKD$Anemia)[levels(CKD$Anemia)=="yes"] <- "1"
        levels(CKD$Anemia)[levels(CKD$Anemia)=="no"] <- "0"
        

}

write.csv(CKD,file="C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_Proceessed.csv",row.names=FALSE)
library("rrcovNA")
imputed <- impSeq(CKD) 

write.csv(imputed,file="C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_Imputed.csv",row.names=FALSE)

rm(list=ls())
head(imputed)
summary(iCKD$Bacteria)
str(iCKD)
summary(iCKD)
iCKD <- read.csv("C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_Imputed.csv",sep = ",", header=T) 
iCKD$Age<-round(iCKD$Age)
iCKD$Blood_Pressure<-round(iCKD$Blood_Pressure)
iCKD$Albumin<-round(iCKD$Albumin)
iCKD$Specific_Gravity<-round(iCKD$Specific_Gravity)
iCKD$Sugar<-round(iCKD$Sugar)
iCKD$Pus_Cell<-round(iCKD$Pus_Cell)
iCKD$Pus_Cell_clumps<-round(iCKD$Pus_Cell_clumps)
iCKD$Bacteria<-round(iCKD$Bacteria)

iCKD$Blood_Glucose<-round(iCKD$Blood_Glucose)
iCKD$Blood_Urea<-round(iCKD$Blood_Urea)
iCKD$Serum_Creatinine<-round(iCKD$Serum_Creatinine)
iCKD$Sodium<-round(iCKD$Sodium)
iCKD$Potassium<-round(iCKD$Potassium)
iCKD$Hemoglobin<-round(iCKD$Hemoglobin)
iCKD$Packed_Cell_Volume<-round(iCKD$Packed_Cell_Volume)
iCKD$White_Blood_Cell_Count<-round(iCKD$White_Blood_Cell_Count)
iCKD$Hypertension<-round(iCKD$Hypertension)
iCKD$Diabetes_Mellitus<-round(iCKD$Diabetes_Mellitus)
iCKD$Coronary_Artery_Disease<-round(iCKD$Coronary_Artery_Disease)
iCKD$Appetite<-round(iCKD$Appetite)
iCKD$Pedal_Edema<-round(iCKD$Pedal_Edema)
iCKD$Anemia<-round(iCKD$Anemia)
iCKD$Kidney_Disease<-round(iCKD$Kidney_Disease)
summary(iCKD)

write.csv(iCKD,file="C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_ImputedTransformed.csv",row.names=FALSE)
iTCKD <- read.csv("C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_ImputedTransformed.csv",sep = ",", header=T) 
summary(iTCKD$Kidney_Disease)
iTCKD$Kidney_Disease=as.factor(iTCKD$Kidney_Disease)
iTCKD$Kidney_Disease<-ifelse(iTCKD$Kidney_Disease==1,0,1)
iv.plot.summary(iv.mult(iTCKD,"Kidney_Disease",TRUE))
summary(iTCKD$Specific_Gravity)
iTCKD$Specific_Gravity=as.factor(iTCKD$Specific_Gravity)
levels(iTCKD$Specific_Gravity)[levels(iTCKD$Specific_Gravity)=="1"] <- "1.005"
levels(iTCKD$Specific_Gravity)[levels(iTCKD$Specific_Gravity)=="2"] <- "1.01"
levels(iTCKD$Specific_Gravity)[levels(iTCKD$Specific_Gravity)=="3"] <- "1.015"
levels(iTCKD$Specific_Gravity)[levels(iTCKD$Specific_Gravity)=="4"] <- "1.02"
levels(iTCKD$Specific_Gravity)[levels(iTCKD$Specific_Gravity)=="5"] <- "1.025"
levels(iTCKD$Specific_Gravity)[levels(iTCKD$Specific_Gravity)=="-1"] <- "1.005"

iTCKD$Albumin=as.factor(iTCKD$Albumin)
summary(iTCKD$Albumin)
levels(iTCKD$Albumin)[levels(iTCKD$Albumin)=="1"] <- "0"
levels(iTCKD$Albumin)[levels(iTCKD$Albumin)=="2"] <- "1"
levels(iTCKD$Albumin)[levels(iTCKD$Albumin)=="3"] <- "2"
levels(iTCKD$Albumin)[levels(iTCKD$Albumin)=="4"] <- "3"
levels(iTCKD$Albumin)[levels(iTCKD$Albumin)=="5"] <- "4"
levels(iTCKD$Albumin)[levels(iTCKD$Albumin)=="6"] <- "5"



--
  summary(iTCKD$Sugar)
iTCKD$Sugar<-as.factor(iTCKD$Sugar)

levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="7"] <- "6"
levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="0"] <- "1"

levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="1"] <- "0"
levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="2"] <- "1"
levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="3"] <- "2"
levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="4"] <- "3"
levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="5"] <- "4"
levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="6"] <- "5"

iTCKD$Pus_Cell<-as.factor(iTCKD$Pus_Cell)

summary(iTCKD$Pus_Cell)
levels(iTCKD$Pus_Cell)[levels(iTCKD$Pus_Cell)=="2"] <- "normal"


levels(iTCKD$Pus_Cell)[levels(iTCKD$Pus_Cell)=="1"] <- "abnormal"

boxplot(iTCKD$Serum_Creatinine)
hist(iTCKD$Serum_Creatinine)
# Mark Pus cells Normal as 1
# Mark Pus Cells abnormal as 0
str(iTCKD)
 iTCKD$Sugar<-as.factor(iTCKD$Sugar)
 summary(iTCKD$Sugar)
 summary(CKD$Sugar)
 
 levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="1"] <- "0"
 levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="2"] <- "1"
 levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="3"] <- "2"
 levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="4"] <- "3"
 levels(iTCKD$Sugar)[levels(iTCKD$Sugar)=="5"] <- "4"
 iTCKD$Pus_Cell_clumps =as.factor(iTCKD$Pus_Cell_clumps)
summary(iTCKD$Pus_Cell_clumps)

levels(iTCKD$Pus_Cell_clumps)[levels(iTCKD$Pus_Cell_clumps)=="2"] <- "present"
levels(iTCKD$Pus_Cell_clumps)[levels(iTCKD$Pus_Cell_clumps)=="1"] <- "notpresent"

summary(iTCKD$Bacteria)
iTCKD$Bacteria<-as.factor(iTCKD$Bacteria)
levels(iTCKD$Bacteria)[levels(iTCKD$Bacteria)=="2"] <- "present"
levels(iTCKD$Bacteria)[levels(iTCKD$Bacteria)=="1"] <- "notpresent"

#levels(iTCKD$Bacteria)[levels(iTCKD$Bacteria)=="present"] <- "0"
#levels(iTCKD$Bacteria)[levels(iTCKD$Bacteria)=="notpresent"] <- "1"

#levels(iTCKD$Bacteria)[levels(iTCKD$Bacteria)=="0"] <- "notpresent"
#levels(iTCKD$Bacteria)[levels(iTCKD$Bacteria)=="1"] <- "present"

summary(iTCKD$Hypertension)
iTCKD$Hypertension<-as.factor(iTCKD$Hypertension)
levels(iTCKD$Hypertension)[levels(iTCKD$Hypertension)=="2"] <- "yes"
levels(iTCKD$Hypertension)[levels(iTCKD$Hypertension)=="1"] <- "no"

summary(CKD$Diabetes_Mellitus)

summary(iTCKD$Diabetes_Mellitus)
iTCKD$Diabetes_Mellitus<-as.factor(iTCKD$Diabetes_Mellitus)
levels(iTCKD$Diabetes_Mellitus)[levels(iTCKD$Diabetes_Mellitus)=="2"] <- "yes"
levels(iTCKD$Diabetes_Mellitus)[levels(iTCKD$Diabetes_Mellitus)=="1"] <- "no"

summary(iTCKD$Coronary_Artery_Disease)

iTCKD$Coronary_Artery_Disease<-as.factor(iTCKD$Coronary_Artery_Disease)
levels(iTCKD$Coronary_Artery_Disease)[levels(iTCKD$Coronary_Artery_Disease)=="2"] <- "yes"
levels(iTCKD$Coronary_Artery_Disease)[levels(iTCKD$Coronary_Artery_Disease)=="1"] <- "no"

summary(iTCKD$Appetite)
iTCKD$Appetite=as.factor(iTCKD$Appetite)
levels(iTCKD$Appetite)[levels(iTCKD$Appetite)=="1"] <- "good"
levels(iTCKD$Appetite)[levels(iTCKD$Appetite)=="2"] <- "no"

levels(iTCKD$Appetite)[levels(iTCKD$Appetite)=="3"] <- "poor"

summary(iTCKD$Pedal_Edema)
iTCKD$Pedal_Edema<-as.factor(iTCKD$Pedal_Edema)
levels(iTCKD$Pedal_Edema)[levels(iTCKD$Pedal_Edema)=="1"] <- "yes"
levels(iTCKD$Pedal_Edema)[levels(iTCKD$Pedal_Edema)=="2"] <- "no"

levels(CKD$Pedal_Edema)[levels(CKD$Pedal_Edema)=="good"] <- NA
boxplot(iTCKD$Specific_Gravity)
summary(iTCKD$Anemia)
iTCKD$Anemia=as.factor(iTCKD$Anemia)
levels(iTCKD$Anemia)[levels(iTCKD$Anemia)=="1"] <- "no"
levels(iTCKD$Anemia)[levels(iTCKD$Anemia)=="2"] <- "yes"
iTCKD$Kidney_Disease<-as.factor(iTCKD$Kidney_Disease)
str(iTCKD$Kidney_Disease)

summary(iTCKD)

iv.plot.summary(iv.mult(iTCKD,"Kidney_Disease",TRUE))

write.csv(iTCKD,file="C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_ImputedTransformedFmt.csv",row.names=FALSE)
iFMTCKD <- read.csv("C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_ImputedTransformedFmt.csv",sep = ",", header=T) 
str(iFMTCKD)
iFMTCKD$Albumin=as.factor(iFMTCKD$Albumin)
iFMTCKD$Sugar=as.factor(iFMTCKD$Sugar)
iFMTCKD$Sugar=as.factor(iFMTCKD$Sugar)
iFMTCKD$Kidney_Disease=as.factor(iFMTCKD$Kidney_Disease)
iv.plot.summary(iv.mult(iTCKD,"Kidney_Disease",TRUE))

head(iFMTCKD)

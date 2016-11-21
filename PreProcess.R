#install.packages("rrcovNA")
library("rrcovNA")
library('VIM')

Compute_MissingValues<-function (CKD_Dataset) {
  #     1) Generate Missing Historgram for Chronic Kidney dataset 
  #     2) Compute missing values in the dataset 
  #     3) Before compute missing values, normalize the ordinal values as integer values.
  #     4) After compute missing values, round off the values to nearest integer and 
  #     5) Generate Missing Histogram for chronic Kidney dataset after imputation.
  CKD_Dataset<-CKD_Dataset
  CheckMissingValues(CKD_Dataset,"Before Imputation1.pdf")
  CKD_Dataset<-NormalizeLevels(CKD_Dataset)
  CKD_Dataset<-ImputeCKD(CKD_Dataset)
  writeCKDData(CKD_Dataset,"Output","CKD_Imputed.csv")
  CKD_Dataset<-readCKDData("Output","CKD_Imputed.csv",",")
  CKD_Dataset<-RoundOffImputedValues(CKD_Dataset)
  CKD_Dataset<-DeNormalizeLevels(CKD_Dataset)
  CheckMissingValues(CKD_Dataset,"After Imputation.pdf")
  return(CKD_Dataset)
}

readCKDData <- function(filepath,file_identfier,delimiter) {
  
  print("Reading Chronic Kidney Disease Dataset")
  print(filepath)
  print(file_identfier)
 # file_identfier<-"chronic_kidney_disease_formatted.csv"
  filename<-paste(c(getwd(),"/",filepath,"/",file_identfier), collapse='')
  print(filename)
  CKD <- read.csv(filename,sep = delimiter, header=T) 
# COlumns Red Blood Cells and Red Blood Cell count were removed there were  30% missing values
  return( CKD)
}

CheckMissingValues<- function(CKD_Dataset,filename) {
   # filename<-"Before Imputation.pdf"
    filename=paste(c(getwd(),"/Report/",filename), collapse='')
    pdf(filename)
    histMiss(CKD_Dataset$Age,main="Histogram of Age")
    histMiss(CKD_Dataset$Blood_Pressure,main="Histogram of Blood_Pressure")
    histMiss(CKD_Dataset$Specific_Gravity,main="Histogram of Specific_Gravity")
    histMiss(CKD_Dataset$Albumin,main="Histogram of Albumin")
    histMiss(CKD_Dataset$Sugar,main="Histogram of Sugar")
    histMiss(CKD_Dataset$Pus_Cell,main="Histogram of Pus_Cell")
    histMiss(CKD_Dataset$Pus_Cell_clumps,main="Histogram of Pus_Cell_clumps")
    histMiss(CKD_Dataset$Bacteria,main="Histogram of Bacteria")
    histMiss(CKD_Dataset$Blood_Urea,main="Histogram of Blood_Urea")
    histMiss(CKD_Dataset$Serum_Creatinine,main="Histogram of Serum_Creatinine")
    histMiss(CKD_Dataset$Sodium,main="Histogram of Sodium")
    histMiss(CKD_Dataset$Potassium,main="Histogram of Potassium")
    histMiss(CKD_Dataset$Hemoglobin,main="Histogram of Hemoglobin")
    histMiss(CKD_Dataset$Packed_Cell_Volume,main="Histogram of Packed_Cell_Volume")
    histMiss(CKD_Dataset$White_Blood_Cell_Count,main="Histogram of White_Blood_Cell_Count")
    histMiss(CKD_Dataset$Hypertension,main="Histogram of Hypertension")
    dev.off()
}
writeCKDData <- function(CKD_dataset,filepath,file_identfier) {
  
  print("Writing Chronic Kidney Disease Dataset")
  #file_identfier<-"chronic_kidney_disease_formatted.csv"
  filename<-paste(c(getwd(),"/",filepath,"/",file_identfier), collapse='')
  # COlumns Red Blood Cells and Red Blood Cell count were removed there were  30% missing values
  write.csv(CKD_dataset,filename,row.names=FALSE)
}

NormalizeLevels<-function (CKD_dataset) {
        
        CKD<-CKD_dataset
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
        
        return(CKD)
}

ImputeCKD <- function(CKD_Dataset) {

  imputed <- impSeq(CKD_Dataset)  
} 
 
RoundOffImputedValues<-function(CKD_Dataset)
{
      iCKD<-CKD_Dataset
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
      return(iCKD)
     # writeCKDData(iCKD,"Output","CKD_ImputedTransformed.csv")
}
DeNormalizeLevels<- function(CKD_Dataset){
  
      iTCKD<-CKD_Dataset
      iTCKD$Kidney_Disease=as.factor(iTCKD$Kidney_Disease)
      iTCKD$Kidney_Disease<-ifelse(iTCKD$Kidney_Disease==1,0,1)
      #summary(iTCKD$Specific_Gravity)
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
     # str(iTCKD)
       iTCKD$Sugar<-as.factor(iTCKD$Sugar)
       summary(iTCKD$Sugar)
       
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
      
#      boxplot(iTCKD$Specific_Gravity)
      summary(iTCKD$Anemia)
      iTCKD$Anemia=as.factor(iTCKD$Anemia)
      levels(iTCKD$Anemia)[levels(iTCKD$Anemia)=="1"] <- "no"
      levels(iTCKD$Anemia)[levels(iTCKD$Anemia)=="2"] <- "yes"
      iTCKD$Kidney_Disease<-as.factor(iTCKD$Kidney_Disease)
      str(iTCKD$Kidney_Disease)
      return(iTCKD)
}

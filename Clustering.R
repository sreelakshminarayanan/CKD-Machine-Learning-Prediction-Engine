

GenerateCluster<-function(CKD_Dataset) {

      CKD_Dataset_Cluster<-CKD_Dataset[,c("Kidney_Disease","Hemoglobin"
              ,"Serum_Creatinine","Packed_Cell_Volume","Potassium","Sodium")]

      CKD_Dataset_Cluster_k<-CKD_Dataset_Cluster[,c(-1)]
      medians = apply(CKD_Dataset_Cluster_k,2,median)
      mads = apply(CKD_Dataset_Cluster_k,2,mad)
      CKD_Dataset_Cluster_k.dist=dist(CKD_Dataset_Cluster_k)
      CKD_Dataset_Cluster.hclust = hclust(CKD_Dataset_Cluster_k.dist)
      #plot(CKD_Dataset_Cluster.hclust,labels=CKD_Dataset_Cluster$Kidney_Disease,main='Default from hclust')
      #text(CKD_Dataset_Cluster.hclust)
      groups.3 = cutree(CKD_Dataset_Cluster.hclust,3)
      #groups.5 = cutree(CKD_Dataset_Cluster.hclust,5)
      #groups.6 = cutree(CKD_Dataset_Cluster.hclust,6)
      #groups.2 = cutree(CKD_Dataset_Cluster.hclust,2)
      filename<-paste(c(getwd(),"/","Report","/","Clustering.txt"), collapse='')
      
      sink(filename)
      # draw dendogram with red borders around the 5 clusters 
      CKD_Dataset$groups<-groups.3
      #head(CKD_Dataset)
      table(groups.3)
      counts = sapply(2:6,function(ncl)table(cutree(CKD_Dataset_Cluster.hclust,ncl)))
      names(counts) = 2:6
      print(counts)
      CKD_Dataset$Kidney_Disease[groups.3 == 1]
      print("Cluster Groups of Kidney Disease")
      print(table(groups.3,CKD._Dataset$Kidney_Disease))
      #table(groups.2,CKD_Dataset$Kidney_Disease)
      #head(groups.3)
      #table(groups.5,CKD_Dataset$Kidney_Disease)
      #table(groups.6,CKD_Dataset$Kidney_Disease)
      print("Aggregate Cluster of Chronic Kidney disease")
      print(aggregate(CKD_Dataset_Cluster_k,list(groups.3),median))

      sink()
      writeCKDData(CKD_Dataset_Cluster_k,"Report","CKD_Cluster.csv")
      return(CKD_Dataset_Cluster.hclust)
}


     # write.csv(CKD_Dataset,file="C:/Users/sreelakshminarayanan/Desktop/Project/ChronicKidneyDisease/CKD_Cluster.csv",row.names=FALSE)


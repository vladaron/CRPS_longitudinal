
library(data.table)
library(xlsx)

rm(list=ls())

inpdata <- fread("C:/Users/shenrard/OneDrive - UCL/Fichiers partagés/MHLouis/data new patients test.csv")
script_path <- "C:/Users/shenrard/OneDrive - UCL/Fichiers partagés/MHLouis/ScoringSyntax.R"


source(script_path)

outdata<-inpdata
for(i in 1:nrow(outdata)){
  scoring<-lg_scoring(outdata[i,])
  outdata[i,"Cluster_modal"]<-scoring$Cluster_modal
  outdata[i,"Cluster_1"]<-scoring$Cluster_1
  outdata[i,"Cluster_2"]<-scoring$Cluster_2
  outdata[i,"Cluster_3"]<-scoring$Cluster_3
  outdata[i,"Cluster_4"]<-scoring$Cluster_4
}

write.xlsx(outdata,"C:/Users/shenrard/OneDrive - UCL/Fichiers partagés/MHLouis/Predictions_new_patients.xlsx")





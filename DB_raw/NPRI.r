getwd()

NPRI=read.csv("NPRI_Locations.csv",sep=",")
colnames(NPRI)[colnames(NPRI)=="Latitude...Latitude"]="lat"
colnames(NPRI)[colnames(NPRI)=="Longitude...Longitude"]="long"
write.csv(NPRI,"NPRI_Locations.csv")
NPRI2=read.csv("DB_NPRI_qc.csv",sep=",")
write.csv(NPRI2,"DB_NPRI_qc.csv")

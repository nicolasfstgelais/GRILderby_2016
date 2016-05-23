rm(list=ls(all=TRUE))

setwd("./DB_rawData/fish")
files=list.files(pattern = "CSV")
spList=NA
i=1

for(i in 1:length(files)){
pa_temp=read.csv(files[i],sep=";")
sp=as.character(unique(pa_temp$Espèce))
if(sp%in%spList)print(paste(files[i],"is a duplicate"))
if(length(sp)==1)spList[i]=as.character(unique(pa_temp$Espèce))
if(length(sp)!=1)print(paste("Error with",files[i]))
if(i==1)pa_fish=pa_temp
if(i!=1)pa_fish=rbind(pa_fish,pa_temp)
}

spList=sort(spList)
spList

colnames(pa_fish)[colnames(pa_fish)=="Année.Mois"]="date"
colnames(pa_fish)[colnames(pa_fish)=="Latitude"]="lat"
colnames(pa_fish)[colnames(pa_fish)=="Longitude"]="long"

setwd("..")
setwd("..")
write.csv(pa_fish,"DB_fish.csv")

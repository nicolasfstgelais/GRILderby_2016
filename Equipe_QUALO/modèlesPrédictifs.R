rm(list=ls(all=TRUE))
setwd("./Equipe_QUALO")
#upload files
qualo=read.table("../DB_QUALO_mtl.csv",header=T,sep=",", stringsAsFactors = F)
meteo=read.table("../DB_weather_yul.csv",header=T,sep=",", stringsAsFactors = F)
rain=read.table("../DB_precip_yul.csv",header=T,sep=",", stringsAsFactors = F)
flow=read.table("../DB_flow_LaSalle.csv",header=T,sep=",", stringsAsFactors = F)
wl=read.table("../DB_waterLevel_PointeClaire.csv",header=T,sep=",", stringsAsFactors = F)
stations=read.csv("../stationQUALO.csv",header=T,sep=",", stringsAsFactors = F,row.names=1)
coords=stations[,c("Lat","Long")]

#sélectionner les stations désirées
selected=c("BLAP-01","BLAP-2.5","BLAP-7","FSL-170","FSL-400","IBIZ-11.5","LSL-17","RDP-60", "RDP-250", "RDP-360")
qualo.sub=qualo[qualo$Point.d.echantillonnage%in%selected,]
qualo.sub=qualo

#retirer les NA de Ecoli
qualo.sub=qualo.sub[!is.na(qualo.sub$Coliformes..colonies.100ml.),]
qualo.sub=qualo.sub[!is.na(qualo.sub$date),]

meteo.ag=aggregate(meteo,by=list(meteo$date),FUN=mean,na.rm=T)

#mettres les données en numérique
#meteo$temp=as.numeric(as.character(meteo$temp))
#meteo.ag$wd=as.numeric(as.character(meteo$wd))
#meteo.ag$ws=as.numeric(as.character(meteo$ws))
#qualo.sub$date=as.character(qualo.sub$date)
#qualo.sub$Coliformes..colonies.100ml.=as.numeric(as.character(qualo.sub$Coliformes..colonies.100ml.))
#rain$date=as.character(rain$date)


#log
qualo.sub$logEcoli=log10(qualo.sub$Coliformes..colonies.100ml.)

#ici Group.1 est la date en numérique
meteo.ag$Group.1=as.character(meteo.ag$Group.1)

#Créer des nouvelles colonnes pour les variables à ajouter
qualo.sub$airT=NA
qualo.sub$ws=NA
qualo.sub$wd=NA
qualo.sub$precip=NA
qualo.sub$maxtemp=NA
qualo.sub$mintemp=NA
qualo.sub$meantemp=NA
qualo.sub$precip.j1=NA
qualo.sub$precip.j12=NA
qualo.sub$precip.j15=NA
qualo.sub$precip.j2=NA
qualo.sub$precip.j3=NA
qualo.sub$precip.j4=NA
qualo.sub$precip.j5=NA
qualo.sub$precip.j6=NA
qualo.sub$flow=NA
qualo.sub$ws=NA
qualo.sub$wl=NA
qualo.sub$rh=NA
qualo.sub$ceil=NA
qualo.sub$atmos=NA
qualo.sub$wd=NA
qualo.sub$year=NA
qualo.sub$month=NA
qualo.sub$rainPA=F

#faire le for loop
for(i in 1:nrow(qualo.sub)){ 
  date.temporaire=qualo.sub[i,"date"]
   qualo.sub[i,"ws"]=meteo.ag[meteo.ag$Group.1==date.temporaire,"ws"]
   qualo.sub[i,"wd"]=meteo.ag[meteo.ag$Group.1==date.temporaire,"wd"]
    qualo.sub[i,"atmos"]=meteo.ag[meteo.ag$Group.1==date.temporaire,"atmos_pres"]
    qualo.sub[i,"flow"]=flow[flow$date==date.temporaire&flow$PARAM==1,"Valeur"]
        qualo.sub[i,"wl"]=wl[wl$date==date.temporaire&wl$PARAM==2,"Valeur"]
    qualo.sub[i,"wl"]=wl[wl$date==date.temporaire,"Valeur"]
    qualo.sub[i,"rh"]=meteo.ag[meteo.ag$Group.1==date.temporaire,"rh"]
        qualo.sub[i,"ceil"]=meteo.ag[meteo.ag$Group.1==date.temporaire,"ceil_hgt"]
    qualo.sub[i,"meantemp"]=meteo.ag[meteo.ag$Group.1==date.temporaire,"temp"]
  qualo.sub[i,"precip"]=rain[rain$date==date.temporaire,"precip.mm"]
    qualo.sub[i,"maxtemp"]=rain[rain$date==date.temporaire,"tempMax"]
        qualo.sub[i,"mintemp"]=rain[rain$date==date.temporaire,"tempMin"]
 #extraire les précipitations des jours précédents
  qualo.sub[i,"precip.j1"]=rain[which(rain$date==date.temporaire)-1,"precip.mm"]
  qualo.sub[i,"precip.j2"]=rain[which(rain$date==date.temporaire)-2,"precip.mm"]
  qualo.sub[i,"precip.j3"]=rain[which(rain$date==date.temporaire)-3,"precip.mm"]
  qualo.sub[i,"precip.j4"]=rain[which(rain$date==date.temporaire)-4,"precip.mm"]
    qualo.sub[i,"precip.j5"]=rain[which(rain$date==date.temporaire)-5,"precip.mm"]
      qualo.sub[i,"precip.j6"]=rain[which(rain$date==date.temporaire)-6,"precip.mm"]
  qualo.sub[i,"precip.j12"]=rain[rain$date==date.temporaire,"precip.mm"]+rain[which(rain$date==date.temporaire)-1,"precip.mm"]
}

qualo.sub$precip.mj12=apply(cbind(qualo.sub$precip.j1,qualo.sub$precip.j2),1,max,na.rm=T)
qualo.sub$precip.mj123=apply(cbind(qualo.sub$precip.j1,qualo.sub$precip.j2,qualo.sub$precip.j3),1,max,na.rm=T)
qualo.sub$precip.j15=apply(cbind(qualo.sub$precip.j1,qualo.sub$precip.j2,qualo.sub$precip.j3,qualo.sub$precip.j4,qualo.sub$precip.j5),1,sum,na.rm=T)
qualo.sub$precip.mj15=apply(cbind(qualo.sub$precip.j1,qualo.sub$precip.j2,qualo.sub$precip.j3,qualo.sub$precip.j4,qualo.sub$precip.j5),1,mean,na.rm=T)


summary(qualo.sub)

#aller chercher le mois et l'année
library(lubridate)
qualo.sub$month=month(as.POSIXlt(qualo.sub$date, format="%Y-%m-%d"))
qualo.sub$year=year(as.POSIXlt(qualo.sub$date, format="%Y-%m-%d"))

#présence/absence de pluie
for(i in 1:nrow(qualo.sub)){
  if(qualo.sub[i,"precip"]>0|qualo.sub[i,"precip.j1"]>0)qualo.sub[i,"rainPA"]=T
}

#créer les catégoriques de Ecoli
qualo.sub$Ecolicat1=NA
qualo.sub$Ecolicat2=NA
qualo.sub[qualo.sub$Coliformes..colonies.100ml.<200,"Ecolicat1"]="A"
qualo.sub[qualo.sub$Coliformes..colonies.100ml.>1000,"Ecolicat1"]="C"
qualo.sub[qualo.sub$Coliformes..colonies.100ml.>200&qualo.sub$Coliformes..colonies.100ml.<1000,"Ecolicat1"]="B"

#cat 2
qualo.sub[qualo.sub$Coliformes..colonies.100ml.<=200,"Ecolicat2"]="A"
qualo.sub[qualo.sub$Coliformes..colonies.100ml.>200,"Ecolicat2"]="B"


#random forest
# Set a random seed
library(randomForest)
#set.seed(123)
#BLAP-01","BLAP-2.5","BLAP-7","FSL-170","FSL-400","IBIZ-11.5","LSL-17","RDP-60", "RDP-250", "RDP-360"


sel_st=unique(qualo.sub$Point.d.echantillonnage)[unique(qualo.sub$Point.d.echantillonnage)%in%rownames(stations)[(rowSums(stations[,6:22])>5)]]

RF=matrix(0,length(sel_st),4,dimnames=list(sel_st,c("B_error","A_error","var","B")))
for (i in sel_st){
  
#i="BLAP-0.5"
sel=intersect(grep(i, qualo.sub$Point.d.echantillonnage),which(qualo.sub$year>1990))
QUALOsel=qualo.sub[sel,]
plot(qualo.sub[sel,]$Coliformes..colonies.100ml.,log="y")
abline(h=200,col="red",lwd=2)

# Build the model (note: not all possible variables are used)
for(j in 1:ncol(QUALOsel)){
  QUALOsel[is.na(QUALOsel[,j]), j] <- mean(QUALOsel[,j], na.rm = TRUE)
}

if(length(unique(qualo.sub[sel,]$Ecolicat2))==1){
  print(paste("onlyA for",i))
  next
}
prev=as.factor(c("A",QUALOsel$Ecolicat2[1:(nrow(QUALOsel)-1)]))

RF[i,"B"]=length(which(QUALOsel$Ecolicat2%in%"B"))/nrow(QUALOsel)

as.factor(QUALOsel$Ecolicat2)
summary(QUALOsel)
rf_model <- randomForest(as.factor(Ecolicat2) ~precip+precip.j1+precip.j2+maxtemp+month+precip.j3+precip.j4
                                              +precip.j12+ws+wd+mintemp+meantemp+rh+atmos+ceil+flow+wl+prev
                                              +precip.mj123+precip.mj15+precip.mj12+precip.j15
                                              ,data = QUALOsel)

#rf_model2 <- randomForest(as.factor(Ecolicat) ~precip+precip.j1+precip.j2+maxtemp+month
                                              #+precip.j12+ws+wd+mintemp+meantemp+rh+atmos+ceil+flow+wl
                                              #,data = QUALOsel)
#print(rf_model)
RF[i,"B_error"]=rf_model$confusion["B","class.error"]
RF[i,"A_error"]=rf_model$confusion["A","class.error"]
print(i)

# Get importance
importance    <- importance(rf_model)
varImportance <- data.frame(Variables = row.names(importance), 
                            Importance = round(importance[ ,'MeanDecreaseGini'],2))
RF[i,"var"]=as.character(varImportance[which.max(varImportance$Importance),"Variables"])
}

RF=as.data.frame(RF)
RF$B_error=as.numeric(as.character(RF$B_error))
RF$A_error=as.numeric(as.character(RF$A_error))
RF$B=as.numeric(as.character(RF$B))
RF$B_error[is.nan(RF$B_error)]=0
plot(RF[,2]~RF[,4])


print(rf_model)
plot(RF)

# Show model error
plot(rf_model, ylim=c(0,1),main=sel_st)
legend('topright', colnames(rf_model$err.rate), col=1:4, fill=1:4)

# Create a rank variable based on importance
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#',dense_rank(desc(Importance))))
rankImportance
which.max(rankImportance$Importance)

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
    y = Importance, fill = Importance)) +
  geom_bar(stat='identity') + 
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
    hjust=0, vjust=0.55, size = 4, colour = 'red') +
  labs(x = 'Variables') +
  coord_flip() + 
  theme_few()

# Predict using the test set
prediction <- predict(rf_model, test)

# Save the solution to a dataframe with two columns: PassengerId and Survived (prediction)
solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)

RF=cbind(RF,coords[rownames(RF),])

#plot the  hybrid Google Maps basemap
library(ggmap)
map <- qmap('Montreal',maptype ="satellite")
RF$col=NA
RF$B_error=as.numeric(as.character(RF$B_error))
RF$B_error[is.nan(RF$B_error)]=0
RF[RF$B_error>0.7,"col"]="red"
RF[RF$B_error<=0.5&RF$B_error>0.01,"col"]="green"
RF[RF$B_error<0.7&RF$B_error>0.5,"col"]="orange"
RF[RF$B_error<=0.01,"col"]="black"

#plot the  hybrid Google Maps basemap
RF$col=NA
RF$A_error[is.nan(RF$A_error)]=0
RF[RF$A_error>0.5,"col"]="red"
RF[RF$A_error<=0.1,"col"]="green"
RF[RF$A_error<0.5&RF$A_error>0.1,"col"]="orange"
RF[RF$A_error==0,"col"]="red"

RF$col2=NA
RF[RF$B>0.5,"col2"]="red"
RF[RF$B<=0.1,"col2"]="green"
RF[RF$B<=0.5&RF$B>0.1,"col2"]="orange"
RF[RF$B_error<=0.01,"col"]="black"

#plot the crime points on top
ppi=300
png("map_pred.png",width=7*ppi, height=5*ppi,bg="transparent",res=ppi)
map + geom_point(data = RF, aes(x = Long, y = Lat), color=RF$col, size=3, alpha=0.8)
dev.off()

map + geom_point(data = RF, aes(x = Long, y = Lat), color=RF$col2, size=3, alpha=0.5)

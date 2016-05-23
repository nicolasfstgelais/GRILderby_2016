rm(list=ls(all=TRUE))

setwd("./DB_rawData/crawfish")
files=list.files(pattern = "CSV")
crawfish=read.csv(files[1],sep=",")

dates=as.data.frame(do.call(rbind,strsplit(as.character(crawfish$Année),"-")))
dates[,2]=as.numeric(paste("20",dates[,2],sep=""))
dates[,1]=as.character(dates[,1])
dates[dates[,1]%in%"Apr",1]=04
dates[dates[,1]%in%"Aug",1]=08
dates[dates[,1]%in%"Jul",1]=07
dates[dates[,1]%in%"Mar",1]=03
dates[dates[,1]%in%"May",1]=05
dates[dates[,1]%in%"Nov",1]=11
dates[dates[,1]%in%"Oct",1]=10
dates[dates[,1]%in%"Sep",1]=09
dates[dates[,1]%in%"Jun",1]=06
dates=dates[,c(2,1)]
colnames(dates)=c("year","month")
crawfish=data.frame(dates,crawfish)
date=paste(crawfish$year,formatC(as.numeric(crawfish$month), width=2, flag="0"),sep="-")
crawfish=data.frame(date,crawfish)

setwd("..")
setwd("..")
getwd()
write.csv(crawfish,"DB_crawfish.csv")
crawfish$date=as.character(crawfish$date)

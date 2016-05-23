rm(list=ls(all=TRUE))
library(date)
getwd()
setwd("./DB_rawData")
flow_LA=read.csv("DB_flow_LaSalle.csv")
wl_LP=read.csv("DB_waterLevel_LaPrairie.csv")
wl_PC=read.csv("DB_waterLevel_PointeClaire.csv")


date=as.Date(flow_LA$JJ-1, origin=as.Date(paste(flow_LA$ANNÃ.E,"01-01",sep="-")))
flow_LA=data.frame(date,flow_LA)

date=as.Date(wl_LP$JJ-1, origin=as.Date(paste(wl_LP$ANNÃ.E,"01-01",sep="-")))
wl_LP=data.frame(date,wl_LP)

date=as.Date(wl_PC$JJ-1, origin=as.Date(paste(wl_PC$ANNÃ.E,"01-01",sep="-")))
wl_PC=data.frame(date,wl_PC)

setwd("..")
getwd()
write.csv(flow_LA,"DB_flow_LaSalle.csv")
write.csv(flow_LA,"DB_waterLevel_LaPrairie.csv")
write.csv(flow_LA,"DB_waterLevel_PointeClaire.csv")

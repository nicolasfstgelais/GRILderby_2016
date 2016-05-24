rm(list=ls(all=TRUE))
library(stationaRy)
library(rnoaa)
library('plyr')
library('rnoaa')

#won't work in Rstudio

#You'll need an API key to use this package (essentially a password). 
#Go to the NCDC website (http://www.ncdc.noaa.gov/cdo-web/token) to get
#one. You can't use this package without an API key.

#insert your api key 
options(noaakey = "QWlWJuQsHjCHRPUfYuOybgyeADcltdVJ")


#this part won't work in Rstudio
# see wheather station in a given geographical area
station=get_isd_stations(lower_lat = 45.000,upper_lat = 46.000,lower_lon = -74.000,upper_lon = -73.000)

isd_stations_search(lat = 45, lon = -74, radius = 100)

# select "716270-99999" for Trudeau station
# select "713710-99999" for St-Hubert station
data=get_isd_station_data("716270-99999", 2000,2015, local_tz = TRUE)
?get_isd_station_data

date=paste(data$year,data$month,data$day,sep="-")
data=data.frame(date,data)

setwd("C://Users//Nicolas//Documents//GitHub//DataDerby2016")
write.csv(data,"DB_weather_yul.csv")


## lat, long, radius
isd_stations_search(lat = 38.4, lon = -123, radius = 250)
ncdc(datasetid='GHCND', stationid='716270-99999', startdate = '2013-10-01',
enddate = '2013-12-01')



# extract precipitations for yul NOAA
# to find a station http://www.ncdc.noaa.gov/cdo-web/datatools/findstation

yrs=(2000:2015)
datatypeid="PRCP"
stationid = "GHCND:CA007025250" # trudeau station
datasetid = "GHCND"

extractNOAA<- function(datatypeid="PRCP",yrs,datasetid = "GHCND",stationid = "GHCND:CA007025250"){
  for(i in yrs){
out <- ncdc(datasetid = datasetid, stationid = stationid, datatypeid = datatypeid,
    startdate = paste(i,"01-01",sep="-"), enddate = paste(i,"12-31",sep="-"),limit=1000)
outT=out$data
date=as.data.frame(do.call(rbind,strsplit(as.character(outT$date),"T")))[1]
outT=data.frame(date,outT$value)
if(i==yrs[1]){sum=outT}else{
  sum=rbind(sum,outT)}
}
  return(sum)}

precip=extractNOAA("PRCP",yrs=2000:2015);precip$outT.value=precip$outT.value/10
tempMAX=extractNOAA("TMAX",yrs=2000:2015);tempMAX$outT.value=tempMAX$outT.value/10
tempMIN=extractNOAA("TMIN",yrs=2000:2015);tempMIN$outT.value=tempMIN$outT.value/10

setwd("C://Users//Nicolas//Documents//GitHub//DataDerby2016")
out=merge(precip,tempMAX,by = "V1")
out=merge(out,tempMIN,by = "V1")
colnames(out)=c("date","precip.mm","tempMax","tempMin")
write.csv(out,"DB_precip_yul.csv")

#idenitfy datasets for a station
var=ncdc_datacats(stationid= "GHCND:CA007025250", limit =50)
c=var$data

res <- ncdc_datasets()
res$data$id

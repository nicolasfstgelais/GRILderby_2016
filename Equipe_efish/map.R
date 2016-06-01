# Map with ggplot 

library(maps)
library(data.table)
library(PBSmapping)
library(ggplot2)

#create rect for each sector
DB_fish <- read.csv("C:/Users/louis/Desktop/GRILderby_2016-master/DB_fish.csv")

library(plyr)
data_cord <- DB_fish[,c("Secteur","lat","long")]

range <- ddply(data_cord, "Secteur", summarize, 
      min_lat = min(lat, na.rm=T),
      max_lat = max(lat, na.rm=T),
      min_long = min(long, na.rm=T),
      max_long = max(long, na.rm=T))

#Create matrix with Station Coord 

data_cord2 <- DB_fish[,c("Station","lat","long")]

dupl <- duplicated(data_cord2$Station)
single <- which(dupl == "FALSE")
Station_coord <-data_cord2[single,]
Station_coord <-Station_coord[order(Station_coord$Station),]


#map Canada

worldmap = map_data("world","Canada") 
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))

q = ggplot() +
  #   coord_map(xlim=xlim,ylim=ylim) +
  geom_polygon(data=worldmap,aes(x=X,y=Y,group=PID),
               fill = "#B7DBDB",color="#B7DBDB") +
  labs(y="",x="") +
  geom_point(data=range,aes(x=min_long[10],y=max_lat[10])
             ,fill="#EEC900",size=3, colour="#EEC900")+
  theme(panel.background = element_rect(fill = "white")
        ,axis.line=element_blank()
        ,axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none")+
  coord_fixed(xlim = c(-135, -50),  ylim = c(40, 85), ratio = 1.5)

q

#Map Montreal
#by secteur
worldmap = map_data("world")
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))

p = ggplot() +
    #coord_map(xlim=xlim,ylim=ylim) +
  geom_rect(data=range,aes(xmin=min_long[1],xmax=max_long[1],
                           ymin=min_lat[1],ymax=max_lat[1]+0.5),fill="#f46d43")+
  geom_rect(data=range,aes(xmin=min_long[2],xmax=max_long[2],
                           ymin=min_lat[2],ymax=max_lat[2]),fill="#3288bd")+
  geom_rect(data=range,aes(xmin=min_long[3],xmax=max_long[3],
                           ymin=min_lat[3],ymax=max_lat[3]),fill="#d53e4f")+
  geom_rect(data=range,aes(xmin=min_long[4],xmax=max_long[4],
                           ymin=min_lat[4],ymax=max_lat[4]),fill="#5e4fa2")+
  geom_rect(data=range,aes(xmin=min_long[5],xmax=max_long[5],
                           ymin=min_lat[5],ymax=max_lat[5]),fill="#66c2a5")+
  geom_rect(data=range,aes(xmin=min_long[6],xmax=max_long[6],
                           ymin=min_lat[6],ymax=max_lat[6]),fill="#ffffbf")+
  geom_rect(data=range,aes(xmin=min_long[7],xmax=max_long[7],
                           ymin=min_lat[7],ymax=max_lat[7]+0.05),fill="#e6f598")+
  geom_rect(data=range,aes(xmin=min_long[8],xmax=max_long[8],
                           ymin=min_lat[8]-0.1,ymax=max_lat[8]+0.022),fill="#fee08b")+
  geom_rect(data=range,aes(xmin=min_long[9],xmax=max_long[9],
                           ymin=min_lat[9]-0.5,ymax=max_lat[9]),fill="#9e0142")+
  geom_rect(data=range,aes(xmin=min_long[10],xmax=max_long[10],
                           ymin=min_lat[10],ymax=max_lat[10]),fill="#abdda4")+
  geom_polygon(data=worldmap,aes(x=X,y=Y,group=PID),
               fill = "#B7DBDB",color="#B7DBDB") +
  labs(y="",x="") +
  theme(panel.background = element_rect(fill = NA))+
  coord_fixed(xlim = c(-75, -70),  ylim = c(45, 47), ratio = 1.5)

p

#by station
worldmap = map_data("world")
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))

p = ggplot() +
  #coord_map(xlim=xlim,ylim=ylim) +
  geom_polygon(data=worldmap,aes(x=X,y=Y,group=PID),fill = "#B7DBDB",color="#B7DBDB") +
  labs(y="",x="") +
  geom_point(data = Station_coord,aes(x=long, y=lat, color = as.numeric(rich2[,2])), size = 1)+
  scale_colour_gradient2(limits=c(0, 30),low = "#5e4fa2", mid = "#ffffbf",high = "#9e0142"
                        , midpoint = 0, space = "Lab",na.value = "grey50"
                        , guide = "colourbar",name = "Richesse spécifique")+
  coord_fixed(xlim = c(-75, -70),  ylim = c(45, 47), ratio = 1.5)+
  theme(panel.background = element_rect(fill = NA))
  

p

# Combined map
library(grid)
vp1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
print(p,vp=vp1)
vp2 <- viewport(width = 0.4, height = 0.4, x = 0.8, y = 0.4)
print(q,vp=vp2)

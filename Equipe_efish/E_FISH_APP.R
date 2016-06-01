# In this script: Beta-diversity focused analyses and beta-diversity partitioning 
# Beta-diversity focused analyses 

# Load libraries ####
library(vegan) #Ordination 
library(sp) #For meauring distances between coordinates
library(MASS) #Chi square 
library(permute) #For rarefied richness
library(boot) #For rarefied richness
library(rich) #For rarefied richness
library(Iso) #For rarefied richness
library(corrplot) #Correlation matrices 
library(reshape2) #Casting and melting
library(ggplot2) #Plotting 
library(maptools) #Maps
library(maps) #Maps 
library(rgeos) #Maps 
library(RColorBrewer) #Colour-schemes 
library(gridExtra) #Arranging plots 
library(plyr)

# Source functions ####
# Beta.div for computing beta diversity (LCBD and SCBD) 
source("beta.div.R")

# Beta.div.comp for partitioning beta diversity into components 
source("beta.div.comp.R")

# LCBD.comp for partition LCBD into components
source("LCBD.comp.R")

# TBI
source("TBI.R")

# Load and transform data ####
setwd("~/Desktop/DataDerby2016-master")
data <- read.csv("DB_fish.csv")

# Transfrom pa to binary
data$pa <- data$Presence_Absence
data$pa <- as.factor(data$pa)
data$pa <- gsub("Absence", "0", data$pa)
data$pa <- gsub("Pr\x8esence", "1", data$pa)
data$pa <-  as.numeric(data$pa)

# Cast data to summarize pa across all years
data_cast_sp <- dcast(data, Secteur ~ Espece, value.var="pa", fun=sum,fill=0)
data_cast_yr <- dcast(data, year ~ Espece, value.var="pa", fun=sum,fill=0)

data_cast <- dcast(data, Secteur ~ year + Station, value.var="pa", fun=sum,fill=0)
data_cast_pa <- decostand(data_cast[,2:1557],method="pa")

new_data <- cbind(data_cast_sp,rowSums(data_cast_pa))
new_data <- new_data[,2:59]/new_data[,60]
new_data <- cbind(data_cast[,1],new_data)
new_data[1:5]
colnames(new_data)[1] <- "year"
sort(new_data[9,])

# Range of lat and long for each Secteur
data_cord <- data[,c("Secteur","lat","long")]
ddply(data_cord, "Secteur", summarize, 
      min_lat = min(lat, na.rm=T),
      max_lat = max(lat, na.rm=T),
      min_long = min(long, na.rm=T),
      max_long = max(long, na.rm=T))

 
# Trend of dominant species of over time for one site (example LSP)
unique(data$Secteur)
LSP <- data[data$Secteur == "Lac Saint-Pierre",]

LSP_cast_sp <- dcast(LSP, year ~ Espece, value.var="pa", fun=sum,fill=0)
head(LSP_cast_sp)[1:5]

LSP_cast <- dcast(LSP, year ~ Station, value.var="pa", fun=sum,fill=0)
LSP_cast_pa <- decostand(LSP_cast[,2:215],method="pa")

new_LSP <- cbind(LSP_cast_sp,rowSums(LSP_cast_pa))
new_LSP <- new_LSP[,2:59]/new_LSP[,60]
new_LSP <- cbind(LSP_cast[,1],new_LSP)
colnames(new_LSP)[1] <- "year"

melt_LSP <- melt(new_LSP,id.vars=c("year"))
head(melt_LSP)

ggplot(melt_LSP,aes(x=year,y=value)) + geom_path(aes(col=variable))


melt_LSP_sub <- melt_LSP[melt_LSP$variable == "Dor<8e> jaune" | melt_LSP$variable == "Meunier noir" |
                melt_LSP$variable == "Perchaude" | melt_LSP$variable == "Dor<8e> noir" |
                melt_LSP$variable == "Chevalier rouge",]

r <- ggplot(melt_LSP_sub,aes(x=year,y=value)) + geom_path(aes(col=variable),size=1.75) + 
  scale_colour_manual(values = c("#b4b867","#ffeb59", "#b7dbdb", "#345b8e", "#6ca18f")) + 
 theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank(),
       panel.background=element_blank(), legend.position="none") + ylab("Frequency across stations") + xlab("Year")

r <- r + geom_point(aes(x=year,y=value),size=4)
r <- r + geom_point(aes(x=year,y=value,col=variable),size=2) + scale_colour_manual(values = c("#b4b867","#ffeb59", "#b7dbdb", "#345b8e", "#6ca18f"))
r

data2 <- read.csv("data2.csv") # données de Charles
data2 <- as.data.frame(data2)

data2 <- data2[data2$species == "achiganpet" | data2$species == "carpe" |
                 data2$species == "craproch" |data2$species == "perchaude" |
                 data2$species == "cherouge", ]

quartz()
ggplot(data2, aes(y=value, x= species)) + geom_bar(stat = "identity",aes(fill=species))  + coord_flip() +
  scale_fill_manual(values = c("#b4b867","#ffeb59", "#b7dbdb", "#345b8e", "#6ca18f")) + 
  theme(panel.background=element_blank(),legend.position="none",
        axis.title.y=element_blank(), axis.title.x=element_blank()) + 
  labs(x="Frequence d'occurence") 

 
# Map of sites ####
# Map of Montreal
my_theme <- theme(axis.line=element_blank(),axis.text.x=element_blank(),
                  axis.text.y=element_blank(),axis.ticks=element_blank(),
                  axis.title.x=element_blank(),
                  axis.title.y=element_blank(),legend.position="none",
                  panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
                  panel.grid.minor=element_blank(),plot.background=element_blank())

xlim = c(-75,-70)
ylim = c(45,47)
worldmap = map_data("world") 
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
worldmap = clipPolys(worldmap, xlim=xlim,ylim=ylim, keepExtra=TRUE)

q = ggplot() +
  #   coord_map(xlim=xlim,ylim=ylim) +
  geom_polygon(data=worldmap,aes(x=X,y=Y,group=PID),
               fill = "#B7DBDB",color="white") 
  labs(y="",x="") 
q <- q + my_theme
q

# Map of Canada
xlim = c(-160,-50)
ylim = c(40,90)
worldmap = map_data("world","Canada") 
setnames(worldmap, c("X","Y","PID","POS","region","subregion"))
worldmap = clipPolys(worldmap, xlim=xlim,ylim=ylim, keepExtra=TRUE)

p = ggplot() +
  #   coord_map(xlim=xlim,ylim=ylim) +
  geom_polygon(data=worldmap,aes(x=X,y=Y,group=PID),
               fill = "white",color="white") 
labs(y="",x="") 
p <- p + my_theme

# Combined map
library(grid)
vp1 <- viewport(width = 1, height = 1, x = 0.5, y = 0.5)
print(q,vp=vp1)
vp2 <- viewport(width = 0.5, height = 0.5, x = 0.3, y = 0.8)
print(p,vp=vp2)

# Spatial beta-diversity analysis #### 

# Use beta.div to calculate total spatial BD on appropriately transformed data  
# NB: beta.div uses non-transformed quantitative data 

BD <- beta.div(data, sqrt.D=FALSE, samp=TRUE, nperm=999, save.D=FALSE, clock=FALSE)
summary(BD)
BD$SStotal_BDtotal 
BD$SCBD
BD$LCBD
BD$p.LCBD

# Look at SCBD specifically
SCBD.summary<- as.data.frame(BD$SCBD)

# Use percentage difference 
BD_per <- beta.div(data, method="percentagedifference", sqrt.D=FALSE, samp=TRUE, nperm=999, save.D=FALSE, clock=FALSE)
summary(BD_per)
BD_per$SStotal_BDtotal 
BD_per$SCBD  
BD_per$LCBD
BD_per$p.LCBD

# Make a dataframe with LCBD information using percentage difference
LCBD.values<- as.data.frame(BD_per$LCBD)
LCBD.p<- as.data.frame(BD_per$p.LCBD)
LCBD.summary<- as.data.frame(cbind(data, LCBD.values, LCBD.p))

# Look at sites with significant (p<0.05)
LCBD.summary.sig <- as.data.frame(subset(LCBD.summary, LCBD.p<0.05))


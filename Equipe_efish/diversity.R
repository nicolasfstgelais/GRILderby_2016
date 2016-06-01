library(reshape2)
DB_fish <- read.csv("C:/Users/louis/Desktop/GRILderby_2016-master/DB_fish.csv")

#changer pres/abs par 0/1
DB_fish$Présence...Absence <- gsub("Absence", "0", DB_fish$Présence...Absence)
DB_fish$Présence...Absence <- gsub("Présence", "1", DB_fish$Présence...Absence)
DB_fish$Présence...Absence <-  as.numeric(DB_fish$Présence...Absence)

com <- dcast(DB_fish, Secteur ~ Espèce, value.var="Présence...Absence", fun=sum,fill=0)
com2 <- dcast(DB_fish, Station ~ Espèce, value.var="Présence...Absence", fun=sum,fill=0)

#standardiser par le nombre d'observation par Secteur 

data_cast <- dcast(DB_fish, Secteur ~ year + Station, value.var="Présence...Absence",
                   fun=sum,fill=0)
data_cast_pa <- decostand(data_cast[,2:1557],method="pa")
obs <- cbind(as.character(data_cast[,1]),rowSums(data_cast_pa))

new <- com[,2:59]/as.numeric(obs[,2])

#standardiser par le nombre d'observation par sites

data_cast2 <- dcast(DB_fish, Station ~ year , value.var="Présence...Absence",
                   fun=sum,fill=0)
data_cast_pa2 <- decostand(data_cast2[,2:18],method="pa")
obs2 <- cbind(as.character(data_cast2[,1]),rowSums(data_cast_pa2))

new2 <- com2[,2:59]/as.numeric(obs2[,2])

#Richness

library(vegan)

#Per secteur

pacom <- decostand(com[2:59],method="pa")
rich <- cbind(as.character(com[,1]),rowSums(pacom))

#Per station

pacom2 <- decostand(com2[2:59],method="pa")
rich2 <- cbind(as.character(com2[,1]),rowSums(pacom2))

#LCBD

# In this script: Beta-diversity focused analyses and beta-diversity partitioning 
# Beta-diversity focused analyses 

# Load data
data <- read.csv("DataDerby2016-master/DB_fish.csv")

# Load libraries ####
library(vegan) #Ordination 
library(sp) #For meauring distances between coordinates
library(MASS) #Chi square 
library(permute) #For rarefied richness
library(boot) #For rarefied richness
library(rich) #For rarefied richness
library(Iso) #For rarefied richness
library(corrplot) #Correlation matrices 

# Visuals 
library(ggplot2) #Plotting 
library(maptools) #Maps
library(maps) #Maps 
library(rgeos) #Maps 
library(RColorBrewer) #Colour-schemes 
library(gridExtra) #Arranging plots 

# Source functions ####
# Beta.div for computing beta diversity (LCBD and SCBD) 
source ("beta-div.R")

# Beta.div.comp for partitioning beta diversity into components 
source("beta-div-comp.R")

# LCBD.comp for partition LCBD into components
source("LCBD.R")

# TBI
source("TBI.R")

# Spatial beta-diversity analysis #### 

# Use beta.div to calculate total spatial BD on appropriately transformed data  
# NB: beta.div uses non-transformed quantitative data 

#per secteur

BD <- beta.div(new, sqrt.D=FALSE, samp=TRUE, nperm=999, save.D=FALSE, clock=FALSE)
summary(BD)
BD$SStotal_BDtotal 
BD$SCBD
BD$LCBD
BD$p.LCBD

# Look at SCBD specifically
SCBD.summary<- as.data.frame(BD$SCBD)

# Use percentage difference 
BD_per <- beta.div(new, method="percentagedifference", sqrt.D=FALSE, samp=TRUE, nperm=999, save.D=FALSE, clock=FALSE)
summary(BD_per)
BD_per$SStotal_BDtotal 
BD_per$SCBD  
BD_per$LCBD
BD_per$p.LCBD

# Make a dataframe with LCBD information using percentage difference
LCBD.values<- as.data.frame(BD_per$LCBD)
LCBD.p<- as.data.frame(BD_per$p.LCBD)
LCBD.summary<- as.data.frame(cbind(com[,1],new, LCBD.values, LCBD.p))

# Look at sites with significant (p<0.05)
LCBD.summary.sig <- as.data.frame(subset(LCBD.summary, LCBD.p<0.05))

#per station 

BD2 <- beta.div(new2, sqrt.D=FALSE, samp=TRUE, nperm=999, save.D=FALSE, clock=FALSE)
summary(BD2)
BD2$SStotal_BDtotal 
BD2$SCBD
BD2$LCBD
BD2$p.LCBD

# Look at SCBD specifically
SCBD.summary2<- as.data.frame(BD2$SCBD)

# Use percentage difference 
BD_per2 <- beta.div(new2, method="percentagedifference", sqrt.D=FALSE, samp=TRUE, nperm=999, save.D=FALSE, clock=FALSE)
summary(BD_per2)
BD_per2$SStotal_BDtotal 
BD_per2$SCBD  
BD_per2$LCBD
BD_per2$p.LCBD

# Make a dataframe with LCBD information using percentage difference
LCBD.values2<- as.data.frame(BD_per2$LCBD)
LCBD.p2<- as.data.frame(BD_per2$p.LCBD)
LCBD.summary2<- as.data.frame(cbind(com2[,1],new2, LCBD.values2, LCBD.p2))

# Look at sites with significant (p<0.05)
LCBD.summary.sig2 <- as.data.frame(subset(LCBD.summary2, LCBD.p2<0.05))




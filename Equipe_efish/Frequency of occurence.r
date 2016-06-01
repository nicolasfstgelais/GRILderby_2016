data <- read.csv("FISH.txt", header = TRUE)    #"FISH.txt" is the original data set given by the GRIL Ressearch Derby
serialno <- data[,1]    #define variables in dataset
year <- data[,2]
month <- data[,3]
yearmonth <- data[,4]
station <- data[,5]
engin <- data[,6]
latitude <- data[,7]
longitude <- data[,8]
secteur <- data[,9]
species <- data[,10]
comments <- data[,11]
presence <- data[,12]

newone <- table(year, secteur, species, presence)     #create subset of important variables from the original data table
clean <- tapply(presence,list(secteur,species), sum)  #calculate apearance frequencies of the species by sector

archipel <- clean[1,]     #define variables in subset "clean"
becancour <- clean[2,]
missisquoi <- clean[3,]
grondines <- clean[4,]
richelieu <- clean[5,]
deux <- clean[6,]
francois <- clean[7,]
louis <- clean[8,]
pierre <- clean[9,]
msorel <- clean[10,]

n1 = 157      #number of observations per sectors
n2 = 230
n3 = 14
n4 = 103
n5 = 18
n6 = 46
n7 = 229
n8 = 275 
n9 = 488
n10 = 127

archipel2 <- (archipel/n1)*100     #standardize the appearance frequencies
becancour2 <- (becancour/n2)*100
missisquoi2 <- (missisquoi/n3)*100
grondines2 <- (grondines/n4)*100
richelieu2 <- (richelieu/n5)*100
deux2 <- (deux/n6)*100
francois2 <- (francois/n7)*100
louis2 <- (louis/n8)*100
pierre2 <- (pierre/n9)*100
msorel2 <- (msorel/n10)*100

df <- data.frame(archipel2,becancour2,missisquoi2)     # create an example data frame with standardized frequencies from three differents sites
library(reshape2)
meltdata <- melt(df, id.vars=c("species"))     #extract species frequencies from the "df" data frame by sectors
d <- meltdata[meltdata$variable == "archipel",]    #extract species frequencies from the "df" data frame by sectors
species <- d[,1]
sector <- d[,2]
value <- d[,3]
library(ggplot2)
ggplot(d, aes(x=species, y=value, fill = species)) + geom_bar(stat ="identity") + coord_flip() + ggtitle("Fréquences standardisées des présences des différentes espèces ichtyologiques du Québec dans le secteur de l'Archipel du Lac Saint-Pierre") + labs(x="Espèces",y="Fréquences standardisées des présences") + theme(legend.position="none")  #create bar graph of the standardized frequencies for the "archipel" sector

e <- meltdata[meltdata$variable == "missisquoi",]
species <- d[,1]
sector <- d[,2]
value <- d[,3]
library(ggplot2)
ggplot(e, aes(x=species, y=value, fill = species)) + geom_bar(stat ="identity") + coord_flip() + ggtitle("Fréquences standardisées des présences des différentes espèces ichtyologiques du Québec dans le secteur de la Baie-Missisquoi") + labs(x="Espèces",y="Fréquences standardisées des présences") + theme(legend.position="none")  #create bar graph of the standardized frequencies for the "missisquoi" sector

f <- meltdata[meltdata$variable == "becancour",]
species <- d[,1]
sector <- d[,2]
value <- d[,3]
library(ggplot2)
ggplot(f, aes(x=species, y=value, fill = species)) + geom_bar(stat ="identity") + coord_flip() + ggtitle("Fréquences standardisées des présences des différentes espèces ichtyologiques du Québec dans le secteur de Bécancour-Batiscan") + labs(x="Espèces",y="Fréquences standardisées des présences") + theme(legend.position="none")  #create bar graph of the standardized frequencies for the "becancour" sector












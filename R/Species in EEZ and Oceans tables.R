# Project: GPSR shark MPA Project
# Objective: Count the number of species layers falling within a polygon region
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer
# Date: 02/03/18

library(sp)
library(rgeos)
#library(sf)
library(raster)
library(plotly)
library(leaflet)
library(rgdal)

# 0. subset the species rasters according to threat status

## Load the Species data 
Speciesdata <- read.csv("Data/datatable containing species names and IUCN categories.csv")

## Which row numbers are threatened species??
iCRENVU <- Speciesdata %>%
  which(code=="CR" | code=="EN" | code=="VU")

iCREN <- Speciesdata %>%
  which(code=="CR" | code=="EN")

iCR <- Speciesdata %>%
  which(code=="CR")

## Load the raster 
specrast <- brick("GIS/multilayerspecrast.tif")

## Generate the subsetted list objects
specrast_CRENVU <- specrast_CRENVU[iCRENVU]
specrast_CREN <- specrast_CRENVU[iCREN]
specrast_CR <- specrast_CRENVU[iCR]


##################
#1. run it on the EEZs

## Load the EEZs
EEZ <- readOGR("GIS/World_EEZ_v9_20161021","eez")
EEZ@data

## plot the data to make sure it loaded ok
plot(EEZ[1,])
plot(specrast[[8]],add=TRUE)

plot(EEZ[1,])
plot(specrast_CRENVU[1],add=TRUE)

## 1.1 - run it for all sharks and rays
Nospecies <- rep(0,nrow(EEZ)) 
for(i in 1:nrow(EEZ)){
  print(paste0(i," out of ",nrow(EEZ)))
  mr <- crop(specrast, EEZ[i,])
  mr <- mask(mr, EEZ[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
EEZ_spec <- data.frame(EEZ@data,Nospecies)
write.csv(EEZ_spec,"Data/Sharks and rays in EEZs.csv",row.names=FALSE)

## 1.2 - run it for all threatened sharks and rays (CR, EN, VU)
Nospecies <- rep(0,nrow(EEZ)) 
for(i in 1:nrow(EEZ)){
  print(paste0(i," out of ",nrow(EEZ)))
  mr <- crop(specrast_CRENVU, EEZ[i,])
  mr <- mask(mr, EEZ[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
EEZ_spec_CRENVU <- data.frame(EEZ@data,Nospecies)
write.csv(EEZ_spec_CRENVU,"Data/Sharks and rays in EEZs_CRENVU.csv",row.names=FALSE)

## 1.3 - run it for all CR and EN sharks and rays
Nospecies <- rep(0,nrow(EEZ)) 
for(i in 1:nrow(EEZ)){
  print(paste0(i," out of ",nrow(EEZ)))
  mr <- crop(specrast_CREN, EEZ[i,])
  mr <- mask(mr, EEZ[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
EEZ_spec_CREN <- data.frame(EEZ@data,Nospecies)
write.csv(EEZ_spec_CREN,"Data/Sharks and rays in EEZs_CREN.csv",row.names=FALSE)

## 1.4 - run it for all CR and EN sharks and rays
Nospecies <- rep(0,nrow(EEZ)) 
for(i in 1:nrow(EEZ)){
  print(paste0(i," out of ",nrow(EEZ)))
  mr <- crop(specrast_CREN, EEZ[i,])
  mr <- mask(mr, EEZ[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
EEZ_spec_CREN <- data.frame(EEZ@data,Nospecies)
write.csv(EEZ_spec_CREN,"Data/Sharks and rays in EEZs_CREN.csv",row.names=FALSE)


##################

# 2. run it on FAO Major Fishing areas
FAO <- readOGR("GIS/FAO_AREAS","FAO_AREAS")
FAO@data

## Merge file with the english and french names found in 
## http://www.fao.org/fishery/area/search/en
F_AREA <- read.csv('Data/Capture_2017.1.1/CL_FI_AREA_GROUPS.csv')  
names(F_AREA)[1] <- "F_AREA"
FAO2 <- merge(FAO,F_AREA)
FAO2 <- FAO2[order(FAO2$FID),] # reorder object according to FID following merge 

plot(FAO2[2,])
plot(specrast[[8]],add=TRUE)

## 2.1 - run it for all sharks and rays
Nospecies <- rep(0,nrow(FAO2)) 
for(i in 1:nrow(FAO2)){
  print(paste0(i," out of ",nrow(FAO2)))
  mr <- crop(specrast, FAO2[i,])
  mr <- mask(mr, FAO2[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
FAO_spec <- data.frame(FAO2@data,Nospecies)
write.csv(FAO_spec,"Data/Sharks and rays in FAO regions.csv",row.names=FALSE)

## 2.1 - run it for all threatened sharks and rays (CR, EN, VU)
Nospecies <- rep(0,nrow(FAO2)) 
for(i in 1:nrow(FAO2)){
  print(paste0(i," out of ",nrow(FAO2)))
  mr <- crop(specrast_CRENVU, FAO2[i,])
  mr <- mask(mr, FAO2[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
FAO_spec_CRENVU <- data.frame(FAO2@data,Nospecies)
write.csv(FAO_spec_CRENVU,"Data/Sharks and rays in FAO regions_CRENVU.csv",row.names=FALSE)

## 2.2 - run it for all threatened sharks and rays (CR, EN)
Nospecies <- rep(0,nrow(FAO2)) 
for(i in 1:nrow(FAO2)){
  print(paste0(i," out of ",nrow(FAO2)))
  mr <- crop(specrast_CREN, FAO2[i,])
  mr <- mask(mr, FAO2[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
FAO_spec_CREN <- data.frame(FAO2@data,Nospecies)
write.csv(FAO_spec_CREN,"Data/Sharks and rays in FAO regions_CREN.csv",row.names=FALSE)

## 2.3 - run it for all critically endangered sharks and rays (CR)
Nospecies <- rep(0,nrow(FAO2)) 
for(i in 1:nrow(FAO2)){
  print(paste0(i," out of ",nrow(FAO2)))
  mr <- crop(specrast_CR, FAO2[i,])
  mr <- mask(mr, FAO2[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
FAO_spec_CR <- data.frame(FAO2@data,Nospecies)
write.csv(FAO_spec_CR,"Data/Sharks and rays in FAO regions_CR.csv",row.names=FALSE)

##################

# 3. run it on Large Marine Ecosystems
LMEcos <- readOGR("GIS/LME66","LME66")

## 3.1 - run it for all sharks and rays
Nospecies <- rep(0,nrow(LMEcos)) 
for(i in 1:nrow(LMEcos)){
  print(paste0(i," out of ",nrow(LMEcos)))
  mr <- crop(specrast, LMEcos[i,])
  mr <- mask(mr, LMEcos)
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
LME_spec <- data.frame(LMEcos@data,Nospecies)
write.csv(LME_spec,"Data/Sharks and rays in Large Marine Ecosystems.csv",row.names=FALSE)

## 3.2 - run it for all CR EN and VU sharks and rays
Nospecies <- rep(0,nrow(LMEcos)) 
for(i in 1:nrow(LMEcos)){
  print(paste0(i," out of ",nrow(LMEcos)))
  mr <- crop(specrast_CRENVU, LMEcos[i,])
  mr <- mask(mr, LMEcos)
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
LME_spec_CRENVU <- data.frame(LMEcos@data,Nospecies)
write.csv(LME_spec_CRENVU,"Data/Sharks and rays in Large Marine Ecosystems_CRENVU.csv",row.names=FALSE)

## 3.3 - run it for all CR and EN sharks and rays
Nospecies <- rep(0,nrow(LMEcos)) 
for(i in 1:nrow(LMEcos)){
  print(paste0(i," out of ",nrow(LMEcos)))
  mr <- crop(specrast_CREN, LMEcos[i,])
  mr <- mask(mr, LMEcos)
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
LME_spec_CREN <- data.frame(LMEcos@data,Nospecies)
write.csv(LME_spec_CREN,"Data/Sharks and rays in Large Marine Ecosystems_CREN.csv",row.names=FALSE)

## 3.4 - run it for all CR and EN sharks and rays
Nospecies <- rep(0,nrow(LMEcos)) 
for(i in 1:nrow(LMEcos)){
  print(paste0(i," out of ",nrow(LMEcos)))
  mr <- crop(specrast_CR, LMEcos[i,])
  mr <- mask(mr, LMEcos)
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}
LME_spec_CR <- data.frame(LMEcos@data,Nospecies)
write.csv(LME_spec_CR,"Data/Sharks and rays in Large Marine Ecosystems_CR.csv",row.names=FALSE)

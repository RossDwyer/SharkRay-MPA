# Project: GPSR shark MPA Project
# Objective: Count the number of species layers falling within a polygon region
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer
# Date: 22/01/18

library(sp)
library(rgeos)
#library(sf)
library(raster)
library(plotly)
library(leaflet)
library(rgdal)

# First run it on the EEZs
EEZ <- readOGR("GIS/World_EEZ_v9_20161021","eez") ## Need to update to...
#EEZ <- readOGR("GIS/World_EEZ_v10_20180221","eez")  ## this
EEZ_Simp <- readOGR("GIS/eez_v10_mapshaper","eez_v10")

EEZ_names_n <- c("GeoName")
EEZ_Simp1 <- EEZ_Simp[,EEZ_names_n]

specrast <- brick("GIS/multilayerspecrast.tif")

plot(specrast[[8]])
plot(EEZ[1,],add=TRUE)

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

# This crashes my mac as the files are too big
# try functions memory.limit() and memory.size() on windows to use this function
# library(rmapshaper)
# EEZ2 <- ms_simplify(EEZ,keep = 0.5)
# EEZ_spec3 <- ms_simplify(EEZ2,keep = 0.5)
# EEZ_spec3 <- ms_simplify(EEZ, keep = 0.001)
# library("rgeos")
# EEZ_gSimplify <- gSimplify(EEZ, tol = 0.5, topologyPreserve = TRUE)

EEZ_CRENVU_df <- read.csv("Data/Sharks and rays in EEZs_CRENVU.csv")
EEZ_Simp2 <- left_join(EEZ_Simp1@data,EEZ_CRENVU_df,by="GeoName") # we lost 5 rows of data from the CRENVU dataset
#EZ_names_n1 <- c(,"MRGID.x","GeoName","MRGID_Ter1.x","Territory1.x","Sovereign1.x","ISO_Ter1.x","Area_km2.x")
EEZ_Simp1@data <- EEZ_Simp2

writeOGR(EEZ_Simp1,"GIS","simplifiedEEZ_counts1", driver="ESRI Shapefile")
##################

# Second run it on the Ocean basins
Oceans <- readOGR("GIS/ne_10m_geography_marine_polys","ne_10m_geography_marine_polys")
#specrast <- brick("GIS/multilayerspecrast.tif")

Oceans@data

plot(Oceans[2,])
plot(specrast[[8]],add=TRUE)

Nospecies <- rep(0,nrow(Oceans)) 

for(i in 1:nrow(Oceans)){
  print(paste0(i," out of ",nrow(Oceans)))
  mr <- crop(specrast, Oceans[i,])
  mr <- mask(mr, Oceans[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}

Oceans_spec <- data.frame(Oceans@data,Nospecies)
write.csv(Oceans_spec,"Data/Sharks and rays in Oceans.csv",row.names=FALSE)

##################

# Third run it on FAO Major Fishing areas
FAO <- readOGR("GIS/FAO_AREAS","FAO_AREAS")
specrast <- brick("GIS/multilayerspecrast.tif")
FAO@data

## Merge file with the enlish and french names found in 
# http://www.fao.org/fishery/area/search/en
F_AREA <- read.csv('Data/Capture_2017.1.1/CL_FI_AREA_GROUPS.csv')  
names(F_AREA)[1] <- "F_AREA"

FAO2 <- merge(FAO,F_AREA)
FAO2 <- FAO2[order(FAO2$FID),] # reorder object according to FID following merge 

names(F_AREA)

plot(FAO2[2,])
plot(specrast[[8]],add=TRUE)

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

##merge the MAJOR spatial.polydf with the species counts df
FAO_CRENVU_df <- read.csv("Data/Sharks and rays in MAJOR FAOs_CRENVU.csv")
FAO_CRENVU_df <- FAO_CRENVU_df[order(FAO_CRENVU_df[,"FID_1"]),] # reorder so in the correct format for merging
row.names(FAO_CRENVU_df) <- FAO_CRENVU_df$FID_1
#remove duplicate columns
FAO_CRENVU_df <- FAO_CRENVU_df %>%
  select("F_CODE","Name_en","Nospecies","CR","EN","VU","rest")

FAO_Major <- subset(FAO,F_LEVEL=="MAJOR")
FAO_Major <- FAO_Major[order(FAO_Major@data[,"FID"]),]
row.names(FAO_Major@data) <- NULL

FAO_CRENVU_spdf <- sp::merge(x= FAO_Major, y=FAO_CRENVU_df)
FAO_CRENVU_spdf@data <- FAO_CRENVU_spdf@data %>%
  select("FID","F_CODE","Name_en","OCEAN","SUBOCEAN","F_AREA","SURFACE","Nospecies","CR","EN","VU","rest")
row.names(FAO_CRENVU_spdf@data) <- NULL

writeOGR(FAO_CRENVU_spdf,"GIS","simplifiedFAO_counts", driver="ESRI Shapefile")


##merge the SUBAREAS spatial.polydf with the species counts df
FAO_Sub_CRENVU_df <- read.csv("Data/Sharks and rays in SUBAREA FAOs_CRENVU.csv")
FAO_Sub_CRENVU_df <- FAO_Sub_CRENVU_df[order(FAO_Sub_CRENVU_df[,"FID_1"]),] # reorder so in the correct format for merging
row.names(FAO_Sub_CRENVU_df) <- FAO_Sub_CRENVU_df$FID_1
#remove duplicate columns
FAO_Sub_CRENVU_df <- FAO_Sub_CRENVU_df %>%
  select("F_CODE","Name_en","Nospecies","CR","EN","VU","rest")

FAO_Subarea <- subset(FAO,F_LEVEL=="SUBAREA")
FAO_Subarea <- FAO_Subarea[order(FAO_Subarea@data[,"FID"]),]
row.names(FAO_Subarea@data) <- NULL

FAO_Sub_CRENVU_spdf <- sp::merge(x= FAO_Subarea, y=FAO_Sub_CRENVU_df)
FAO_Sub_CRENVU_spdf@data <- FAO_Sub_CRENVU_spdf@data %>%
  select("FID","F_CODE","Name_en","OCEAN","SUBOCEAN","F_AREA","SURFACE","Nospecies","CR","EN","VU","rest")
row.names(FAO_Sub_CRENVU_spdf@data) <- NULL

writeOGR(FAO_Sub_CRENVU_spdf,"GIS","simplifiedFAO_subarea_counts", driver="ESRI Shapefile")

##################

# Fourth run it on Large Marine Ecosystems
LMEcos <- readOGR("GIS/LME66","LME66")
specrast <- brick("GIS/multilayerspecrast.tif")
LMEcos@data

names(LMEcos)

plot(specrast[[8]])
plot(LMEcos[2,],add=TRUE)

Nospecies <- rep(0,nrow(LMEcos)) 

for(i in 1:nrow(LMEcos)){
  print(paste0(i," out of ",nrow(LMEcos)))
  mr <- crop(specrast, LMEcos[i,])
  mr <- mask(mr, LMEcos[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}

LME_spec <- data.frame(LMEcos@data,Nospecies)
write.csv(LME_spec,"Data/Sharks and rays in Large Marine Ecosystems.csv",row.names=FALSE)

library("rgeos")
LME_gSimplify <- gSimplify(LMEcos, tol = 0.05, topologyPreserve = TRUE)

##gSimplify doesn't preserve the @data frame, though, so we should re-create it:
LME_CRENVU_df <- read.csv("Data/Sharks and rays in LMEs_CRENVU.csv")
LME_CRENVU_gSimplify <- sp::SpatialPolygonsDataFrame(LME_gSimplify, LME_CRENVU_df,match.ID = F) 

writeOGR(LME_CRENVU_gSimplify,"GIS","simplifiedLME66", driver="ESRI Shapefile")
##################

# Fifth run it on MPAs (added 15 Aug 2019)
MPAcos <- readOGR("GIS","MPA_Atlas_20190619")
specrast <- brick("GIS/multilayerspecrast.tif")
MPAcos@data

names(MPAcos)

plot(specrast[[8]])
plot(MPAcos[3,],add=TRUE)

Nospecies <- rep(0,nrow(MPAcos)) 

for(i in 1:nrow(MPAcos)){
  print(paste0(i," out of ",nrow(MPAcos)))
  mr <- crop(specrast, MPAcos[i,])
  mr <- mask(mr, MPAcos[i,])
  mr[is.na(mr)] <- 0
  Nospecies[i] <- sum(cellStats(mr,max),na.rm=TRUE)
}

MPA_spec <- data.frame(MPAcos@data,Nospecies)
write.csv(MPA_spec,"Data/Sharks and rays in MPAs.csv",row.names=FALSE)




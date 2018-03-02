# Project: GPSR shark MPA Project
# Objective: Convert species distribution polygons (large file size) to raster brick object (small file size) with a specified resolution
#          : Sum layers based on IUCN category or Order
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer
# Date: 02/03/18

library(sp)
library(rgeos)
library(raster)
library(plotly)
library(leaflet)
library(rgdal)

#world <- readOGR(dsn="GIS",layer="ne_110m_admin_0_countries")

IUCNStat <- read.csv("Data/Chondrichthyan_IUCNstatus.csv")
IUCNStatShort <- IUCNStat[c("genus_name","species_na","order_name","family_nam","code")]
IUCNStatShort$binomial <- paste(IUCNStat$genus_name,IUCNStat$species_na,by=" ")
IUCNStatShort2 <- IUCNStat[c("binomial","order_name","family_nam","code")]

## using rgdal package upload the distribution data polygons (takes some time due to the lage file size) 
sharkdist <- readOGR("GIS","Chondrichthyan_simplified")

# Merges the species polygons with the IUCN categories
sharkdistIUCN <- merge(sharkdist, IUCNStatShort2, by="binomial")
sharkdat <- sharkdistIUCN@data # Extract the dataframe and save to disk
write.csv(sharkdat,"Data/datatable containing species names and IUCN categories.csv", row.names=FALSE)

## Now convert all 1083 species distributions to a raster stack object
r <- raster(ncol=740, nrow=360) # Change this value to alter resolution of species distribution rasters
species1 <- 1
species2 <- 1083 # all of them

specrast <- stack()
for(species in species1:species2){
  sp11 <- sharkdist[species,] # extracts one species
  r.polys1 <- rasterize(sp11, r)
  r.polys2 <- rasterize(as(sp11, 'SpatialLines'), r, field=1:3) # Apply rasterize so that it includes ALL cells touching a polygon
  r3 <- cover(r.polys1, r.polys2) # combine these rasters
  specrast <- stack(specrast ,r.polys, quick=TRUE)
  print(species)
}
names(specrast) <- sharkdat$binomial[species1:species2]

# saves the multi band raster object
writeRaster(specrast, filename="GIS/multilayerspecrast.tif", options="INTERLEAVE=BAND", overwrite=TRUE)

################

### Create Raster brick object of species number according to IUCN status

specrast <- brick("GIS/multilayerspecrast.tif")
sharkdat <- read.csv("Data/datatable containing species names and IUCN categories.csv")[1:dim(specrast)[3],] # limited by the number of sharks loaded into the raster 

iall <- subset(specrast, 1:nrow(sharkdat))
iCR <- subset(specrast, which(sharkdat$code=="CR"))
iEN <- subset(specrast, which(sharkdat$code=="EN"))
iNT <- subset(specrast, which(sharkdat$code=="NT"))
iVU <- subset(specrast, which(sharkdat$code=="VU"))
iLC <- subset(specrast, which(sharkdat$code=="LC"))
iDD <- subset(specrast, which(sharkdat$code=="DD"))

specrast_all_sum <- sum(specrast,na.rm=T)    
specrast_CR_sum <- calc(subset(specrast, which(sharkdat$code=="CR")),sum,na.rm=T)    
specrast_EN_sum <- calc(subset(specrast, which(sharkdat$code=="EN")),sum,na.rm=T)    
specrast_NT_sum <- calc(subset(specrast, which(sharkdat$code=="NT")),sum,na.rm=T)    
specrast_VU_sum <- calc(subset(specrast, which(sharkdat$code=="VU")),sum,na.rm=T)    
specrast_LC_sum <- calc(subset(specrast, which(sharkdat$code=="LC")),sum,na.rm=T)    
specrast_DD_sum <- calc(subset(specrast, which(sharkdat$code=="DD")),sum,na.rm=T)    
specrast_CREN_sum <- specrast_CR_sum+specrast_EN_sum
specrast_CRENVU_sum <- specrast_CR_sum+specrast_EN_sum + specrast_VU_sum

specrast_IUCN_brick_sum <- brick(specrast_all_sum,
                                 specrast_CR_sum,
                                 specrast_EN_sum,
                                 specrast_NT_sum,
                                 specrast_VU_sum,
                                 specrast_LC_sum,
                                 specrast_DD_sum,
                                 specrast_CREN_sum,
                                 specrast_CRENVU_sum)

writeRaster(specrast_IUCN_brick_sum, filename="GIS/IUCNsum_specrast.tif", options="INTERLEAVE=BAND", overwrite=TRUE)

#####

### Create Raster brick object of species number according to classification order

specrast_CARCHARHINIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="CARCHARHINIFORMES")),sum,na.rm=T)    
specrast_CHIMAERIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="CHIMAERIFORMES")),sum,na.rm=T)    
specrast_HETERODONTIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="HETERODONTIFORMES")),sum,na.rm=T)    
specrast_HEXANCHIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="HEXANCHIFORMES")),sum,na.rm=T)    
specrast_LAMNIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="LAMNIFORMES")),sum,na.rm=T)    
specrast_ORECTOLOBIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="ORECTOLOBIFORMES")),sum,na.rm=T) 
specrast_PRISTIOPHORIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="PRISTIOPHORIFORMES")),sum,na.rm=T) 
specrast_RAJIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="RAJIFORMES")),sum,na.rm=T) 
specrast_SQUALIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="SQUALIFORMES")),sum,na.rm=T) 
specrast_SQUATINIFORMES_sum <- calc(subset(specrast, which(sharkdat$order_name=="SQUATINIFORMES")),sum,na.rm=T) 

specrast_order_brick_sum <- brick(specrast_all_sum,
                                  specrast_CARCHARHINIFORMES_sum,
                                  specrast_CHIMAERIFORMES_sum,
                                  specrast_HETERODONTIFORMES_sum,
                                  specrast_HEXANCHIFORMES_sum,
                                  specrast_LAMNIFORMES_sum,
                                  specrast_ORECTOLOBIFORMES_sum,
                                  specrast_PRISTIOPHORIFORMES_sum,
                                  specrast_RAJIFORMES_sum,
                                  specrast_SQUALIFORMES_sum,
                                  specrast_SQUATINIFORMES_sum)

writeRaster(specrast_order_brick_sum, filename="GIS/ordersum_specrast.tif", options="INTERLEAVE=BAND", overwrite=TRUE)


################

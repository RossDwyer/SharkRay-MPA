# Project: GPSR shark MPA Project
# Objective: Add the common names, IUCN web addresses and fishbase link to the table
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer
# Date: 24/08/19

library(taxize)
library(worms)
library(stringi)

IUCNStat <- read.csv("Data/Chondrichthyan_IUCNstatus.csv") ## Note: This looks just like the data exported from WoRMS
IUCNStatShort <- IUCNStat[c("binomial","order_name","family_nam","code","id_no","genus_name","species_na")]
names(IUCNStatShort)[1] <- "ScientificName"

## Generate a vector of web addresses to the IUCN red list files 
#web_redlist <- paste0("http://www.iucnredlist.org/details/classify/",IUCNStatShort$id_no,"/0") # Older IUCN link (expired mid 2019)
web_redlist <- paste0("https://www.iucnredlist.org/species/",IUCNStatShort$id_no,"/","10221801") # Updated but final number incorrect
assessment_redlist <- paste0("http://www.iucnredlist.org/pdflink.",IUCNStatShort$id_no)
web_fishbase <- paste0("http://www.fishbase.org/summary/",IUCNStatShort$genus_name,"-",IUCNStatShort$species_na,".html")

species <- as.character(IUCNStatShort$ScientificName)

#uids <- get_uid(IUCNStatShort2[,1])
# keep only uids which you have in the database
#uids.found <- as.uid(uids[!is.na(uids)])
#uids.found <- as.uid(uids)
# keep only species names  corresponding to your ids
#species.found <- species[!is.na(uids)]

## Searches the Encycoledia of Life http://eol.org/ for common names
common.names <- sci2comm(species, simplify = TRUE) # Warning - Takes some time to run!

# Creates empty dataframe where to write species information
dfCommon <- data.frame(ScientificName= IUCNStatShort$ScientificName, 
                       CommonName="")
dfCommon$CommonName <- as.character(dfCommon$CommonName)

# For each species line in the data frame add in the common name info into a single cell seperated by commas
for(i in 1:nrow(dfCommon)){
  dat2 <- stri_list2matrix(common.names[i], byrow=TRUE) # Convert the list to a set of vectors
  dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))   # find indices of words with non-ASCII characters
  dat4 <- dat2[-dat3]  # subset original vector of words to exclude words with non-ASCII char
  dat5 <- grep('&', dat4, value=FALSE)   # find indices of words with '&' character
  if(length(dat5)>0){ # only do this if there are '&' characters
    dat6 <- dat4[-dat5]  # subset original vector of words to exclude words with '&' char
  }else{
    dat6 <- dat4 # do nothing
  }
  dat7 <- paste(dat6, collapse = ", ")   # convert vector back to a string
  dfCommon$CommonName[i] <- dat7
}

#add the other dataframe columns
IUCNStatWeb <- cbind(dfCommon,
                     IUCNStatShort[2:5],
                     web_redlist,
                     assessment_redlist,
                     web_fishbase)

## Need to do this step to make sure file alligns with the species distribution rasters
IUCNStatWeb <- IUCNStatWeb[order(IUCNStatWeb$ScientificName),]
row.names(IUCNStatWeb) <- NULL
write.csv(IUCNStatWeb,"Data/IUCNStatWeb.csv",row.names=FALSE)

############ This earloier section crashes for me

## Start from here
## Check out Fishbase to attach ecological data

library(rfishbase)

IUCNStatWeb <- read.csv("Data/IUCNStatWeb.csv")
IUCNStatWeb$ScientificName <- as.character(IUCNStatWeb$ScientificName)
IUCNStatWeb$CommonName <- as.character(IUCNStatWeb$CommonName)

## First step to get the rfishbase package working
fish <- validate_names(as.character(IUCNStatWeb[,1]))

## Need to find common names that were not in the repo and add them into our table
#sci_to_common(fish[which(IUCNStatWeb$CommonName=="")])
#IUCNStatWeb$CommonName[which(IUCNStatWeb$CommonName=="")] <- as.character(sci_to_common(fish[which(IUCNStatWeb$CommonName=="")]))

specinfo <- species(fish, fields=c("Species","SpecCode", "Vulnerability", "DemersPelag","FBname","Length","LongevityWild"))
#remove duplicates
specinfo <- specinfo %>% distinct() #%>%
  #group_by(Species)%>% 
  #filter(n()>1)

# These should be the same - looks like we've lost a few on specinfo
dim(IUCNStatWeb)
dim(specinfo)

library(dplyr)
specinfo2 <- right_join(specinfo,IUCNStatWeb,by=c("Species"="ScientificName"))

which(duplicated(specinfo2[,1])==TRUE) # At least there's no duplicates!

#Extract stock info and merge with other dataframe
resil <- stocks(fish, fields=c("Species","Resilience"))
resil2 <- resil%>% na.omit() %>% distinct()
specresil <- left_join(specinfo2, resil2)

# THese should be the same!
dim(IUCNStatWeb)
dim(specinfo)
dim(specresil)

library(stringr)
IUCNFishbaseWeb <- specresil %>%
  mutate(order_name=str_to_sentence(order_name, locale = "en"),
         family_nam=str_to_sentence(family_nam, locale = "en")) %>%
  select(Species,FBname,
         order_name,family_nam,
         DemersPelag,
         code, 
         Vulnerability,Resilience,
         Length, LongevityWild,
         web_redlist,
         web_fishbase,
         CommonName)
names(IUCNFishbaseWeb)[1] <- "ScientificName"

#' ##                colnames=c("Species name", 
#' "Common names",
#' 'Order name', 'Family name',
#' 'Habitat','Disperal data',
#' 'Vulnerability index', 'Resilience',
#' "Length"             "LongevityWild" 
#' 'IUCN threat category', 
#' 'IUCN web',
#' #'Download IUCN assessment',
#' 'Fishbase web'),

#Add the multiple common names in various languages
# IUCNFishbaseWeb$CommonName2 <- IUCNFishbaseWeb$CommonName
# for(i in 1:nrow(IUCNFishbaseWeb)){
#   if(!is.na(IUCNFishbaseWeb$FBname[i])){
#     IUCNFishbaseWeb$CommonName2[i] <- paste(IUCNFishbaseWeb$FBname[i],IUCNFishbaseWeb$CommonName[i],sep=", ")
#   }else{
#     IUCNFishbaseWeb$CommonName2[i] <- IUCNFishbaseWeb$CommonName[i]
#   }
# }


#Add the Dispersal information
Df <- read.csv("Data/DispersalKernel_Properties.csv")
Df_MarkRecapt <- subset(Df,tag_type=="mark_recap")
Df_passive <- subset(Df,tag_type=="passive")
Df_satellite <- subset(Df,tag_type=="satellite")

Df_MarkRecapt <- Df_MarkRecapt %>% select(ScientificName,Num_tags,mean,sd)
names(Df_MarkRecapt) <- c("ScientificName","MarkRecapt_tags","MarkRecapt_mean","MarkRecapt_sd")
Df_passive <- Df_passive %>% select(ScientificName,Num_tags,mean,sd)
names(Df_passive) <- c("ScientificName","passive_tags","passive_mean","passive_sd")
Df_satellite <- Df_satellite %>% select(ScientificName,Num_tags,mean,sd)
names(Df_satellite) <- c("ScientificName","satellite_tags","satellite_mean","satellite_sd")

IUCNFishbaseWeb2 <- left_join(IUCNFishbaseWeb,Df_MarkRecapt)
IUCNFishbaseWeb3 <- left_join(IUCNFishbaseWeb2,Df_passive)
IUCNFishbaseWeb4 <- left_join(IUCNFishbaseWeb3,Df_satellite)


# Save the file for loading into shiny
write.csv(IUCNFishbaseWeb4,"Data/IUCNFishbaseWebDispersal.csv",row.names=FALSE)

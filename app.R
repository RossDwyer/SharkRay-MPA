# Project: GPSR shark MPA Project
# Objective: Generate a GUI for users to investigate conservation priorities for sharks and rays
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer

DateUpdated <-  "10-July-2019" ## Date last updated

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


##############################################################################
# Libraries
##############################################################################

library(shiny)
library(ggplot2)
library(gtable)
library(grid)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(DT)
library(tibble)
library(shinycssloaders)
library(highcharter)
library(fmsb)

##############################################################################
# Data
##############################################################################

sharkdat <- read.csv("Data/IUCNFishbaseWeb.csv")
names(sharkdat)[1] <- 'binomial' # Ensures continuity between pages

EEZ_spec <- read.csv("Data/Sharks and rays in EEZs_CRENVU.csv")
FAO_spec <- read.csv("Data/Sharks and rays in MAJOR FAOs_CRENVU.csv")
FAOsub_spec <- read.csv("Data/Sharks and rays in SUBAREA FAOs_CRENVU.csv")
LME_spec <- read.csv("Data/Sharks and rays in LMEs_CRENVU.csv")

##Loads GIS files: reduced.MPAs,allspecrast,worldmap,orderrast,iucnrast ----
orderrast <- brick("GIS/ordersum_specrast.tif")
iucnrast <- brick("GIS/IUCNsum_specrast.tif")
allspecrast <- brick("GIS/multilayerspecrast.tif")
#reduced.MPAs <- readOGR(dsn="GIS",layer="simplifiedMPA")
# Works only on Windows!
##load('myMPA.RData') ##Loads GIS files: reduced.MPAs,allspecrast,worldmap,orderrast,iucnrast
#Works on Mac though had to sort errors on the loaded rasters
#library(repmis)
#source_data("https://github.com/RossDwyer/SharkRay-MPA/blob/master/myMPA.RData?raw=true")

# Load Country shapefile
reduced.countries <- readOGR(dsn="GIS/TM_WORLD_BORDERS_SIMPL-0.3","TM_WORLD_BORDERS_SIMPL-0.3")

# Load FAO areas shapefile and simplify topology for quick loading
# FAOs<- readOGR(dsn="GIS/FAO_AREAS","FAO_AREAS")
# library(rgeos)
# ## Extract Major FAOs
# FAOs_major.df = aggregate(FAOs, by = "F_AREA")
# FAOs_major.simple <- gSimplify(FAOs_major.df, tol = 1,topologyPreserve=TRUE)
# FAOs_major.simpledf <- SpatialPolygonsDataFrame(FAOs_major.simple, FAOs_major.df@data)
# writeOGR(FAOs_major.simpledf,dsn="GIS/FAO_AREAS",layer="FAO_AREAS_major_simple", driver="ESRI Shapefile",overwrite_layer = TRUE)
# ## Extract Minor FAOs
# FAOs.simple <- gSimplify(FAOs[,7], tol = 1,topologyPreserve=TRUE)
# ## Coerce to SpatialPolygonsDataFrame 
# FAOs.simpledf <- SpatialPolygonsDataFrame(FAOs.simple, FAOs@data)
# writeOGR(FAOs.simpledf[1:8],dsn="GIS/FAO_AREAS",layer="FAO_AREAS_simple", driver="ESRI Shapefile",overwrite_layer = TRUE)

FAOsimple<- readOGR(dsn="GIS/FAO_AREAS","FAO_AREAS_simple")
FAO_major_simple<- readOGR(dsn="GIS/FAO_AREAS","FAO_AREAS_major_simple")

## tab 1 species lookup table
order.name <- c("CARCHARHINIFORMES",
                "CHIMAERIFORMES",
                "HETERODONTIFORMES",
                "HEXANCHIFORMES",
                "LAMNIFORMES",
                "ORECTOLOBIFORMES",
                "PRISTIOPHORIFORMES",
                "RAJIFORMES",
                "SQUALIFORMES",
                "SQUATINIFORMES")

# Edit dataframe to include web links - note: target="_blank" ensures links opened in a new tab
sharkdat <- sharkdat %>% 
  mutate(
    web_redlist = sprintf('<a href="%s" target="_blank" class="btn btn-link">iucnredlist.org</a>',web_redlist),
    assessment_redlist = sprintf('<a href="%s" target="_blank" class="btn btn-primary">PDF</a>',assessment_redlist),
    web_fishbase = sprintf('<a href="%s" target="_blank" class="btn btn-link">fishbase.org</a>',web_fishbase),
    pointborder = "1"
  ) %>% 
  select(binomial,CommonName,order_name,family_nam,
         DemersPelag,Vulnerability,Resilience,
         code,web_redlist,web_fishbase,pointborder)
levels(sharkdat$Resilience) <- c("Very low", "Low", "Medium", "High")

#Read in Dispersal distance data
Df <- read.csv("Data/DispersalKernel_Properties.csv")
# Add a column detailing if we have Dispersal distance data for a species
sharkdat$DispersalKernel <- "No"
for (i in 1:nrow(Df)){
  inum <- which(as.character(sharkdat$binomial)==as.character(Df$ScientificName[i]))
  sharkdat$DispersalKernel[inum] <- "Yes"
}

# Function to make the empty dispersal plot
makeDispersalplot <- function(xmax){
  xx <- seq(0, log(xmax), length=1000)
  yy <- rep(0,length(xx))
  
  #par(oma = c(0.1,1,2,1.5))
  par(mar = c(4, 4, 2, .5), font.axis = 2,font.lab = 2) # stops the arial font issue
  plot(x=xx,y=yy, xaxt="n", las=1, 
       xlab="Maximum dispersal distance (km)", 
       ylab="Probability of dispersal", type="n",ylim=c(0,1),
       bty="l")
  axis(1, at= log(c(0.01, seq(0.1,1,l=10), 
                    seq(1,10,l=10),seq(10,100,l=10), 
                    seq(100,1000,l=10), 
                    seq(1000,10000,l=10))+1), labels=F, tcl=-0.3)
  axis(1, at= log(c(0.1,1,10,100,1000,10000)+1), 
       labels=c(0.1,1,10,100,1000,10000))
  
  legend("top",bty="n",
         inset = c(0,-0.13),
         xpd = TRUE, horiz = TRUE,
         lty=1,lwd=2,
         col=c(1:3),
         legend = c("Mark-recapture",
                    "Passive acoustic",
                    "Satellite"))
}

# Function to make the empty landings plot
makeLandingsplot <- function(xmax){
  xd <- seq(1950, 2010, by=1)
  yd <- rep(0,0,length(xd))
  
  #par(oma = c(0.1,1,2,1.5))
  par(mar = c(4, 4, 2, .5), font.axis = 2,font.lab = 2) # stops the arial font issue
  plot(x=xd,y=yd, las=1, 
       xlab="Year", 
       ylab="Reported catch landings ('000 t)", type="n",
       ylim=c(0,600),
       bty="l")
  
  # legend("top",bty="n",
  #        inset = c(0,-0.13),
  #        xpd = TRUE, horiz = TRUE,
  #        lty=1,lwd=2,
  #        col=c(1:3),
  #        legend = c("Mark-recapture",
  #                   "Passive acoustic",
  #                   "Satellite"))
}

iucnimg <- paste0('img/DD.png')
iucnlink <- paste0("https://github.com/RossDwyer/SharkRay-MPA/blob/master/img/DD.png")
sharkdat$flag <- ifelse(sharkdat$code=='CR','<img src=img/CR.png> </img>',
                        ifelse(sharkdat$code=='EN','<img src=img/EN.png> </img>',
                               ifelse(sharkdat$code=='VU','<img src=img/VU.png> </img>',
                                      ifelse(sharkdat$code=='NT','<img src=img/NT.png> </img>',
                                             ifelse(sharkdat$code=='LC','<img src=img/LC.png> </img>',
                                                    '<img src=img/DD.png> </img>')))))

cleantable <- sharkdat %>%
  select(binomial, 
         CommonName,
         order_name, family_nam,
         DemersPelag,
         DispersalKernel,
         Vulnerability,Resilience,
         flag,
         web_redlist,
         #assessment_redlist,
         web_fishbase)

# tab 1a - species distribution maps
species.name <- sharkdat$binomial # Names of species for the species range maps  
#pal <- c("#253494","#f93") # HEX code for the colour of the raster [1] and the MPAs [2]

pal <- c("#de2d26","#f93") 
###
# tab 2 - Country EEZ datasets 
reduced.countries <- readOGR(dsn="GIS/TM_WORLD_BORDERS_SIMPL-0.3","TM_WORLD_BORDERS_SIMPL-0.3")
CI_FinalPCA_spec <- read.csv("Data/CI_Database_FinalPCA_with_species.csv")

# merge function with the duplicateGeoms argument set to TRUE 
# if TRUE geometries in x are duplicated if there are multiple matches between records in x and y
CL2sp <- sp::merge(reduced.countries, CI_FinalPCA_spec, by.x="ISO3",by.y= "ISO_Ter1",
                   all=F,duplicateGeoms = TRUE)

# Choose the columns and the orders. Note order needs to match renaming in DT
CL2sp@data <- CL2sp@data[,c("GeoName","Territory1","ISO3","Sovereign1","Area_km2",
                            "EconomicVulnerability","DependMarineResource","Education",
                            "Tourism","Corruption","ChallengeIndex",
                            "OpportunityIndex","CLI",
                            "Nospecies","Threatened",
                            "CR","EN","VU","rest")]


## Round SocioEco values to 2 Dec places
CL2sp@data$EconomicVulnerability <- round(CL2sp@data$EconomicVulnerability, digits=2)
CL2sp@data$DependMarineResource <- round(CL2sp@data$DependMarineResource, digits=2)
CL2sp@data$Education <- round(CL2sp@data$Education, digits=2)
CL2sp@data$Tourism <- round(CL2sp@data$Tourism, digits=2)
CL2sp@data$Corruption <- round(CL2sp@data$Corruption, digits=2)
CL2sp@data$ChallengeIndex <- round(CL2sp@data$ChallengeIndex, digits=2)
CL2sp@data$OpportunityIndex <- round(CL2sp@data$OpportunityIndex, digits=2)
CL2sp@data$CLI <- round(CL2sp@data$CLI, digits=2)

## Spider / radar plot
socioeconomic_devel <- CL2sp@data %>%
  filter(is.na(ChallengeIndex)==FALSE)#,

iMax <- socioeconomic_devel%>%
  summarize(Corruption=max(Corruption),
            EconomicVulnerability=max(EconomicVulnerability),
            Tourism=max(Tourism),
            DependMarineResource=max(DependMarineResource),
            Education=max(Education))

iMin <- socioeconomic_devel%>%
  summarize(Corruption=min(Corruption),
            EconomicVulnerability=min(EconomicVulnerability),
            Tourism=min(Tourism),
            DependMarineResource=min(DependMarineResource),
            Education=min(Education))

###
# tab 3 (Shark MPA page)
SharkMPAs_coords <- read.csv("Data/Table 1 Shark MPA draft with coords.csv")

# legend html generator:
markerLegendHTML <- function(IconSet) {
  # container div:
  legendHtml <- "<div style='padding: 10px; padding-bottom: 10px;'><h4 style='padding-top:0; padding-bottom:10px; margin: 0;'> Shark MPA </h4>"
  
  n <- 1
  # add each icon for font-awesome icons icons:
  for (Icon in IconSet) {
    if (Icon[["library"]] == "fa") {
      legendHtml<- paste0(legendHtml, "<div style='width: auto; height: 45px'>",
                          "<div style='position: relative; display: inline-block; width: 36px; height: 45px' class='awesome-marker-icon-",Icon[["markerColor"]]," awesome-marker'>",
                          "<i style='margin-left: 8px; margin-top: 11px; 'class= 'fa fa-",Icon[["icon"]]," fa-inverse'></i>",
                          "</div>",
                          "<p style='position: relative; top: -20px; display: inline-block; ' >", names(IconSet)[n] ,"</p>",
                          "</div>")    
    }
    n<- n + 1
  }
  paste0(legendHtml, "</div>")
}

createLink <- function(val) {
  sprintf('<a href="https://www.google.com/#q=%s" target="_blank" class="btn btn-primary">Info</a>',val)
}

IconSet <- awesomeIconList(
  "Entire EEZ"   = makeAwesomeIcon(icon= 'star', markerColor = 'green', library = "fa"),
  "Part EEZ" = makeAwesomeIcon(icon= 'star', markerColor = 'blue', library = "fa")
)

###
#  tab 3A - For the order/IUCN hotspot maps
iorder <- orderrast[[1]]
iCARCHARHINIFORMES <- orderrast[[2]]
iCHIMAERIFORMES <- orderrast[[3]]
iHETERODONTIFORMES <- orderrast[[4]]
iHEXANCHIFORMES <- orderrast[[5]]
iLAMNIFORMES <- orderrast[[6]]
iORECTOLOBIFORMES <- orderrast[[7]]
iPRISTIOPHORIFORMES <- orderrast[[8]]
iRAJIFORMES <- orderrast[[9]]
iSQUALIFORMES <- orderrast[[10]]
iSQUATINIFORMES <- orderrast[[11]]

istatus <- iucnrast[[1]]
iCR <- iucnrast[[2]]
iEN <- iucnrast[[3]]
iNT <- iucnrast[[4]]
iVU <- iucnrast[[5]]
iLC <- iucnrast[[6]]
iDD <- iucnrast[[7]]
iCREN <- iucnrast[[8]]
iCRENVU <- iucnrast[[9]]

# Colour scale for the category maps
scolours.ord <- c("#e5f5e0", "#a1d99b", "#31a354")
scolours.iucn <- c("#fee0d2", "#fc9272", "#de2d26")

###
# tab 3B - visualise no species in EEZ/FAOs/LMEs
ivis <- 50 # No species in EEZ/FAOs/LMEs to visualise in the table
sbarchart_colours <- rev(colorRampPalette(brewer.pal(9,"Blues")[-1])(ivis))
EEZ_spec1 <- data.frame(EEZ_spec[order(EEZ_spec$Nospecies,decreasing=TRUE),][1:ivis,],row.names=NULL)
FAO_spec1 <- data.frame(FAO_spec[order(FAO_spec$Nospecies,decreasing=TRUE),][1:ivis,],row.names=NULL)
FAOsub_spec1 <- data.frame(FAOsub_spec[order(FAOsub_spec$Nospecies,decreasing=TRUE),][1:ivis,],row.names=NULL)
LME_spec1 <- data.frame(LME_spec[order(LME_spec$Nospecies,decreasing=TRUE),][1:ivis,],row.names=NULL)

#sbarchart_colours <- rev(colorRampPalette(brewer.pal(9,"Blues")[-1]))
#EEZ_spec1 <- data.frame(EEZ_spec[order(EEZ_spec$Nospecies,decreasing=TRUE),],row.names=NULL)
#FAO_spec1 <- data.frame(FAO_spec[order(FAO_spec$Nospecies,decreasing=TRUE),],row.names=NULL)
#LME_spec1 <- data.frame(LME_spec[order(LME_spec$Nospecies,decreasing=TRUE),],row.names=NULL)

sEEZ_count <- data.frame(x=EEZ_spec1$GeoName, y=EEZ_spec1$Nospecies, y1=EEZ_spec1$CR, y2=EEZ_spec1$EN, y3=EEZ_spec1$VU, y4=EEZ_spec1$rest, x1=EEZ_spec1$Territory1, Area=EEZ_spec1$Area_km2)
sFAO_count <- data.frame(x=FAO_spec1$Name_en, y=FAO_spec1$Nospecies, y1=FAO_spec1$CR, y2=FAO_spec1$EN, y3=FAO_spec1$VU, y4=FAO_spec1$rest, x1=FAO_spec1$F_CODE, Area=FAO_spec1$SURFACE)
sFAOsub_count <- data.frame(x=FAOsub_spec1$Name_en, y=FAOsub_spec1$Nospecies, y1=FAOsub_spec1$CR, y2=FAOsub_spec1$EN, y3=FAOsub_spec1$VU, y4=FAOsub_spec1$rest, x1=FAOsub_spec1$F_CODE, Area=FAOsub_spec1$SURFACE)
sLME_count <- data.frame(x=LME_spec1$LME_NAME, y=LME_spec1$Nospecies, y1=LME_spec1$CR, y2=LME_spec1$EN, y3=LME_spec1$VU, y4=LME_spec1$rest, x1=LME_spec1$LME_NAME, Area=LME_spec1$Shape_Area)
# Add 'threatened' category
sEEZ_count <- sEEZ_count %>% mutate(y5 = y1 + y2 + y3) 
sFAO_count <- sFAO_count %>% mutate(y5 = y1 + y2 + y3) 
sFAOsub_count <- sFAOsub_count %>% mutate(y5 = y1 + y2 + y3) 
sLME_count <- sLME_count %>% mutate(y5 = y1 + y2 + y3) 

###

# tab 4 (About page)
noSpecies <- length(species.name) # number of species considered


# User interface ----
PageTitle <- "App for Conservation status of Sharks and Rays"


##############################################################################
# UI Side
##############################################################################

ui <- navbarPage(
  ## Add the Shark Conservation Fund logo
  #  titlePanel(#windowTitle = PageTitle, # Failed attempt to 
  #title =
  # div(
  img( 
    src = "img/shark-conservation-fund-lock-up-blk-RGB.jpg",
    height = 50,
    width = 150,
    style = "margin:-15px 0px; padding-top:-10px",
    alt="Shark Conservation Fund"
  ),
  
  
  ## TAB 1
  tabPanel(title="Species explorer",
           
           fluidPage(
             # Top left panel             
             column(4, plotOutput('x2', height = 350),
                    column(12,radioButtons(inputId = "sMakePlots", 
                                           label = "Select which plot to visualise:",
                                           choices = c("Vulnerability"= "sVulnPlot",
                                                       "Dispersal distances" = "sDistPlot",
                                                       "Landings per year" = "sSpLandYr"),
                                           selected = "sVulnPlot",
                                           inline = TRUE))#,
                    #conditionalPanel(
                    #  condition = "input.sMakePlots == 'sDistPlot'",
                    #  selectInput("sMakeDistPlot", 
                    #              label = "Choose the data to display",
                    #              choices = list("mark_recap",
                    #                             "passive",
                    #                             "satellite"),
                    #              selected = "mark_recap"))
             ),
             # Top right panel
             column(8,
                    leafletOutput("mapSpecies", width = '100%',height=350) %>% 
                      withSpinner(color="#3182bd")),
             
             
             # Bottom panel
             DT::dataTableOutput("speciestable", width = '100%', height = 200)
           )
  ),  
  
  ## TAB 2
  tabPanel(title="Country explorer",
           fluidPage(
             # Top  panel  
             fluidRow(
               column(4,
                      selectInput("tab.x3z",
                                  label = "Select Z axis",
                                  choices = list("Area_km2",
                                                 "DependMarineResource","Education",
                                                 "Tourism","Corruption",
                                                 "EconomicVulnerability",
                                                 "ChallengeIndex","OpportunityIndex","CLI",
                                                 "Nospecies","Threatened",
                                                 "CR","EN","VU","rest"),
                                  selected = "Threatened")#,
                      #actionButton("runif", "Go!")
               ),
               
               column(8,
                      leafletOutput("mapCounty", width = '100%', height = 350) %>%
                        withSpinner(color="#3182bd")
               )),
             
             #Middle Panel
             fluidRow(
               column(2,
                      selectInput("tab.x3x",
                                  label = "Select X axis",
                                  choices = list("Area_km2",
                                                 "DependMarineResource","Education",
                                                 "Tourism","Corruption",
                                                 "EconomicVulnerability",
                                                 "ChallengeIndex","OpportunityIndex","CLI",
                                                 "Nospecies","Threatened",
                                                 "CR","EN","VU","rest"),
                                  selected = "DependMarineResource"),
                      selectInput("tab.x3y",
                                  label = "Select Y axis",
                                  choices = list("Area_km2",
                                                 "DependMarineResource","Education",
                                                 "Tourism","Corruption",
                                                 "EconomicVulnerability",
                                                 "ChallengeIndex","OpportunityIndex","CLI",
                                                 "Nospecies","Threatened",
                                                 "CR","EN","VU","rest"),
                                  selected = "Threatened")
               ),
               
               column(4,
                      plotlyOutput('x3', height = 300)),
               column(3, 
                      plotOutput('xspider', height = 300)),
               column(3,
                      plotlyOutput('xthreatbar', height = 300))
             ),
             
             
             # fluidRow(
             #   column(6,
             #          sliderInput(
             #            inputId = "sld21_ChallengeIndex",
             #            label="Challenge index:", 
             #            min=min(CL2sp@data$ChallengeIndex,na.rm=T), max=max(CL2sp@data$ChallengeIndex,na.rm=T),
             #            value=c(min(CL2sp@data$ChallengeIndex,na.rm=T),max(CL2sp@data$ChallengeIndex,na.rm=T)), 
             #            step=0.1,round=1),
             #          sliderInput(
             #            inputId = "sld22_OpportunityIndex",
             #            label="Opportunity index:", 
             #            min=min(CL2sp@data$OpportunityIndex,na.rm=T), max=max(CL2sp@data$OpportunityIndex,na.rm=T),
             #            value=c(min(CL2sp@data$OpportunityIndex,na.rm=T),max(CL2sp@data$OpportunityIndex,na.rm=T)),
             #            step=0.1,round=1)
             #   ),
             #   column(6,
             #          sliderInput(
             #            inputId = "sld23_CLI",
             #            label="Conservation likelihood index:",
             #            min=min(CL2sp@data$CLI,na.rm=T), max=max(CL2sp@data$CLI,na.rm=T),
             #            value=c(min(CL2sp@data$CLI,na.rm=T),max(CL2sp@data$CLI,na.rm=T)),
             #            step=0.1,round=1),
             #          sliderInput(
             #            inputId = "sld24_Threatened",
             #            label="No. threatened species:",
             #            min=min(CL2sp@data$Threatened,na.rm=T), max=max(CL2sp@data$Threatened,na.rm=T),
             #            value=c(min(CL2sp@data$Threatened,na.rm=T),max(CL2sp@data$Threatened,na.rm=T)),
             #            step=1,round=1)
             #   )
             # )
             # ),
             
             
             # Bottom panel
             DT::dataTableOutput("countryTable", width = '100%', height = 200)
           )
  ),
  
  
  ## TAB 3
  tabPanel(title="Region explorer",
           fluidPage(
             # Top  panel
             fluidRow(
               column(4,
                      #absolutePanel(top = 0, left = 0,
                      
                      radioButtons(inputId= "layerOverlap",
                                   label = "Sort by:",
                                   choices = c("IUCN listing" = "iucn", "Taxonomic order"= "order"),
                                   selected = "iucn",
                                   inline = FALSE),
                      
                      conditionalPanel(
                        condition = "input.layerOverlap == 'iucn'",
                        selectInput("var.iucn",
                                    label = "IUCN codes",
                                    choices = list("all",
                                                   "CR+EN+VU",
                                                   "CR+EN",
                                                   "CR",
                                                   "EN",
                                                   "VU",
                                                   "NT",
                                                   "LC",
                                                   "DD"),
                                    selected = "all"),
                        sliderInput("range2",
                                    "Upper % species displayed :",
                                    min = 0, max = 100, step = 10,  value = 90)
                      ),
                      
                      conditionalPanel(
                        condition = "input.layerOverlap == 'order'",
                        selectInput("var.order",
                                    label = "Taxonomic orders",
                                    choices = list("all",
                                                   "CARCHARHINIFORMES",
                                                   "CHIMAERIFORMES",
                                                   "HETERODONTIFORMES",
                                                   "HEXANCHIFORMES",
                                                   "LAMNIFORMES",
                                                   "ORECTOLOBIFORMES",
                                                   "PRISTIOPHORIFORMES",
                                                   "RAJIFORMES",
                                                   "SQUALIFORMES",
                                                   "SQUATINIFORMES")),#,selected = "all"),
                        sliderInput("range1",
                                    "Upper % species displayed :",
                                    min = 0, max = 100, step = 10,  value = 90)
                        #                                    )
                      )
                      
                      # # Select whether or not to visualise MPA markers
                      # checkboxInput("showMPAs", "Show MPAs?", FALSE),
                      # checkboxInput("showFAOs", "Show FAO regions?", FALSE)
               ),
               
               # Plot the map
               column(8,
                      leafletOutput("mapRegion", width = "100%", height = 350) %>%
                        withSpinner(color="#3182bd"))
             ),
             
             # Select which dataset to visualise
             radioButtons(inputId = "sSelectRegionDisplay",
                          label = "Select data to visualise:",
                          choices = c("MPA table"= "SRANKSDT",
                                      "Species in region plot" = "sRegionPlot"),
                          selected = "SRANKSDT",
                          inline = TRUE),
             
             # If selected draw the MPA data table
             conditionalPanel(
               condition = "input.sSelectRegionDisplay == 'SRANKSDT'",
               DT::dataTableOutput('ranksDT')
             ),
             
             # If selected draw christmas tree figures
             conditionalPanel(
               condition = "input.sSelectRegionDisplay == 'sRegionPlot'",
               
               tags$div(class="header", checked=NA,
                        tags$strong("This plot displays the number of shark and ray species present in marine and coastal regions")),
               
               radioButtons("sAreaPolygons", "Select which areas to visualise:",
                            c("FAO Regions" = "sFAO_count",
                              "FAO Subareas" = "sFAOsub_count",
                              "Large Marine Ecosystems" = "sLME_count",
                              "Exclusive Economic Zones"= "sEEZ_count"),
                            inline = TRUE),
               
               selectInput("tab.order",
                           label = "Sort the table by the following feature",
                           choices = list("No Species","Threatened Species","Area"),
                           selected = "No Species"),
               #hr(),
               plotlyOutput("plot", width = "100%", height = "100%") %>%
                 withSpinner(color="#3182bd"),
               
               verbatimTextOutput("event"),
               
               tags$div(class="header", checked=NA,
                        tags$p("For more information about the regions, click the links below..."),
                        tags$a(href="https://www.arcgis.com/home/item.html?id=5433d0112fc8448e96f61594c90011c6", "Exclusive Economic Zones | "),
                        tags$a(href="http://www.fao.org/fishery/area/search/en", "FAO Regions | "),
                        tags$a(href="http://www.lme.noaa.gov/index.php?option=com_content&view=article&id=1&Itemid=112", "Large Marine Ecosystems")
               )
             )
           )
  ),
  
  
  ## TAB 4
  tabPanel(title="About",
           tags$body(
             h4('This Shiny App was built to help visualise shark and ray distribution information across the globe'),
             p('The purpose of the tool is to help identify priority areas on a global scale where spatial protection would provide the greatest benefit to shark and ray conservation efforts.'),
             br(),
             p('The app was funded by <a href="www.sharksandrays.org">The Shark Conservation Trust</a> (formally the Global Partnership for Sharks & Rays), a sponsored project of Rockefeller Philanthropy Advisors.'),  
             p('It contains range distribution information of 1083 shark and ray species downloaded from <a href="%22http://www.iucnredlist.org%22">The IUCN Red List of Threatened Species</a>.'),
             br(),
             a(href = "For more information, visit www.sharksandrays.org", "For more information, visit www.sharksandrays.org"),
             br(),
             br(),
             br(),
             p("The application was build and maintained by Dr Ross Dwyer and is powered by...")),
           #tags$img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gradient.png", width = "180px", height = "60px"),
           #tags$img(src = "http://www.mcclellandlegge.com/img/shiny-logo.png", width = "100px", height = "100px"),
           #tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/13/Leaflet_logo.svg/2000px-Leaflet_logo.svg.png", width = "180px", height = "60px"),
           tags$img(src = "img/RStudio-Logo-Blue-Gradient.png", width = "180px", height = "60px"),
           tags$img(src = "img/shiny-logo.png", width = "100px", height = "100px"),
           tags$img(src = "img/Leaflet_logo.svg.png", width = "180px", height = "60px"),
           br(),
           a(href = "https://github.com/RossDwyer","Our project is on GitHub"),
           br(),
           br(),
           p(paste0("Date last updated: ",DateUpdated))
  )
  
)


##############################################################################
# Server Side
##############################################################################

# server logic required to draw the map ----

server <- function(input, output, session) {
  
  #### TAB 1: Species Explorer map and table #### 
  
  # Top left panel - Plot the map with no species selected
  output$mapSpecies <- renderLeaflet({
    leaflet() %>% 
      setView(lng = 0, lat = 0,  zoom = 1) %>% 
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      addLegend(colors = pal[1], 
                labels = c("Select a species")) 
  })
  
  # Top right panel - Choose which plot to visualise
  observeEvent(input$sMakePlots, { 
    
    output$x2 <- renderPlot({
      par(mar = c(4, 4, 1, .1), font.axis = 2,font.lab = 2) # stops the arial font issue   
      if (input$sMakePlots == 'sVulnPlot'){   
        ggplot(data = sharkdat,
               aes(x = code, y = Vulnerability, 
                   fill = pointborder,colour = pointborder
               )) + 
          geom_dotplot(dotsize = 0.4,binwidth = 2, 
                       binaxis = "y", stackdir = "center", binpositions="all") + 
          scale_fill_manual(values=c("#d3d3d3"))+
          scale_color_manual(values=c("#d3d3d3"))+
          scale_x_discrete(limits=c("CR", "EN", "VU", "NT", "LC", "DD"))+
          labs(title="",x="IUCN code", y = "Vulnerability Index")+
          theme_minimal()+
          theme(legend.position="none") # Remove legend
      }
      if (input$sMakePlots == 'sDistPlot')
        makeDispersalplot(1500) 
      if (input$sMakePlots == 'sSpLandYr') 
        makeLandingsplot() 
    })
  })
  
  # Choose which species to visualise on the map and in the plots using the datatable   
  observeEvent(input$speciestable_rows_selected, {  
    
    x <- input$speciestable_rows_selected  # Assign the row number of the species to display
    specName <- sharkdat$binomial[x]  # Assign the species name
    newdata <- allspecrast[[x]]       # Assign the species raster
    newdata[newdata <= 0] <- NA 
    
    proxy <- leafletProxy("mapSpecies")
    proxy %>% 
      clearImages() %>% # removes earlier rasters
      clearControls() %>% # # removes earlier legends
      addLegend(colors = pal[1], # Adds new legend with species name (binomial)
                position = "topright",
                labels = specName) %>%
      addRasterImage(layerId ="layer2",
                     newdata,
                     colors=pal[1], 
                     opacity = 0.5,
                     group = "Species") %>%
      mapOptions(zoomToLimits = "first")
    
    observeEvent(input$sMakePlots, { 
      # select what plot to visualise
      if (input$sMakePlots == 'sVulnPlot'){     
        sharkdat2 <- sharkdat # So that our dataset isn't overwritten
        output$x2 <- renderPlot({
          draw_vul <- function(x1){
            # If a row has been selected - make border a different colour
            if (length(x1)) sharkdat2$pointborder[x1] <- "2"
            ggplot(data = sharkdat2,
                   aes(x = code, y = Vulnerability, 
                       fill = pointborder,colour = pointborder)) + 
              geom_dotplot(dotsize = 0.4,binwidth = 2, 
                           binaxis = "y", stackdir = "center", binpositions="all") +
              ##geom_hline(col = "#b63737")# + # Add marker lines here for Y axis??
              ##geom_vline(col = "#b63737")## Add marker lines here for X axis??
              scale_fill_manual(values=c("#d3d3d3", "#b63737"))+
              scale_color_manual(values=c("#d3d3d3", "#b63737"))+
              scale_x_discrete(limits=c("CR", "EN", "VU", "NT", "LC", "DD"))+
              labs(title = "",x = "IUCN code", y = "Vulnerability Index")+
              theme_minimal()+
              theme(legend.position="none") # Remove legend
          }
          draw_vul(x)
        })
      }
      
      if (input$sMakePlots == 'sDistPlot'){
        output$x2 <- renderPlot(
          {
            displot2 <- function(data, species,
                                 xmax=1500,
                                 xlab="Maximum dispersal distance (km)", 
                                 ylab="Probability of dispersal", ...){
              
              makeDispersalplot(xmax) 
              
              dat <- data[data$ScientificName %in% species,]
              xx <- seq(0, log(xmax), length=1000)
              
              if(nrow(dat)==0){
                graphics::text(5,0.5, "No dispersal data available for this species...")
                graphics::text(5,0.4, "Please select another row")       
              }else{
                
                dat_mark_recap <- dat[dat$tag_type %in% "mark_recap",]
                dat_passive <- dat[dat$tag_type %in% "passive",]
                dat_satellite <- dat[dat$tag_type %in% "satellite",]
                
                if(nrow(dat_mark_recap)>0){ 
                  yfit <- dgamma(xx, shape=dat_mark_recap$shape, scale=dat_mark_recap$scale)
                  yy <- yfit/max(yfit)
                  lines(xx, yy, col=1, lwd=2)
                }
                if(nrow(dat_passive)>0){    
                  yfit <- dgamma(xx, shape=dat_passive$shape, scale=dat_passive$scale)
                  yy <- yfit/max(yfit)
                  lines(xx, yy, col=2, lwd=2)
                }
                if(nrow(dat_satellite)>0){    
                  yfit <- dgamma(xx, shape=dat_satellite$shape, scale=dat_satellite$scale)
                  yy <- yfit/max(yfit)
                  lines(xx, yy, col=3, lwd=2)
                }
              }
            }
            
            ###
            
            displot2(data = Df,                                                            
                     species = as.character(specName))
          })
      }
      
      if (input$sMakePlots == 'sSpLandYr'){
        output$x2 <- renderPlot(
          {
            
            makeLandingsplot() 
            graphics::text(1990,300, "No landings data available for this species...")
            graphics::text(1990,250, "Please select another row")       
            # }
            
            # landplot1 <- function(data, species,
            #                      xmax=1500,
            #                      xlab="Maximum dispersal distance (km)", 
            #                      ylab="Probability of dispersal", ...){
            #   
            #   #makeLandingsplot() 
            #   
            #   dat <- data[data$ScientificName %in% species,]
            #   xx <- seq(0, log(xmax), length=1000)
            #   
            #   if(nrow(dat)==0){
            #     graphics::text(5,0.5, "No dispersal data available for this species...")
            #     graphics::text(5,0.4, "Please select another row")       
            #   }else{
            #     
            #     dat_mark_recap <- dat[dat$tag_type %in% "mark_recap",]
            #     dat_passive <- dat[dat$tag_type %in% "passive",]
            #     dat_satellite <- dat[dat$tag_type %in% "satellite",]
            #     
            #   }
            # }
            
            # displot2(data = Df,                                                            
            #          species = as.character(specName))
          })
      }
      
    })
    
  })
  
  ## Generate Species data explorer table
  output$speciestable <- DT::renderDataTable(
    {
      generateNewDT <- function(x){ 
        
        #Change the header rows of the shiny datatable (note. only changes the display of the columns, not the underlying names)
        output_dt <- DT::datatable(x, 
                                   options=list(
                                     pageLength = 5, # number of rows per page
                                     scrollX = TRUE,
                                     autoWidth = TRUE,
                                     searchHighlight = TRUE, #Highlight searchesd text with yellow
                                     columnDefs = list(list(#width = '50px', 
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 30 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                         "}")
                                     ))), 
                                   caption = 'Search species information table', # <a href="#" onclick="alert('This script allows you to write help text for an item');">help me</a> #
                                   filter = 'top', 
                                   selection = 'single', # selects only one row at a time
                                   rownames = FALSE,  # no row names
                                   colnames=c("Species name", 
                                              "Common names",
                                              'Order name', 'Family name',
                                              'Habitat','Disperal data',
                                              'Vulnerability index', 'Resilience',
                                              'IUCN threat category', 
                                              'IUCN web',
                                              #'Download IUCN assessment',
                                              'Fishbase web'),
                                   callback = JS('table.page(3).draw(false);'),
                                   
                                   #initComplete = JS(
                                   #  "function(settings, json) {",
                                   #  "$(this.api().table().header()).css({'font-size': '90%'});",
                                   #  "}"),
                                   #class = 'white-space: nowrap', # stops wrapping of rows
                                   escape = FALSE  # This bit is to stop the links from rendering literally (i.e. text only)
        )
        #formatStyle(columns = c(1:10), fontSize = '80%')
        
        return(output_dt)
      }
      generateNewDT(cleantable)
    }
  )
  
  #### TAB 2: Country explorer map and table #### 
  
  # Slider tool to subset data (Defunct)
  # qSub2 <-  reactive({
  #   subset <- subset(CL2sp, 
  #                    CL2sp@data$ChallengeIndex>=input$sld21_ChallengeIndex[1] &
  #                      CL2sp@data$ChallengeIndex<=input$sld21_ChallengeIndex[2] &
  #                      CL2sp@data$OpportunityIndex>=input$sld22_OpportunityIndex[1] &
  #                      CL2sp@data$OpportunityIndex<=input$sld22_OpportunityIndex[2] &
  #                      CL2sp@data$CLI>=input$sld23_CLI[1] &
  #                      CL2sp@data$CLI<=input$sld23_CLI[2] &
  #                      CL2sp@data$Threatened>=input$sld24_Threatened[1] &
  #                      CL2sp@data$Threatened<=input$sld24_Threatened[2]
  #   )
  # })
  
  # On the selection of a z axis visiualise the map and the scatterplot
  observeEvent(input$tab.x3z,{
    
    ## 1. Plot the map
    output$mapCounty <- renderLeaflet({
      
      zaxis1 <- input$tab.x3z # Select which column to visualise on the map
      zcols1 <- colorNumeric(palette="viridis", 
                             domain=CL2sp@data[zaxis1])
      
      leaflet(data = CL2sp) %>%     
        setView(lng = 0, lat = 0,  zoom = 1) %>% 
        addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
        addPolygons(smoothFactor = 0.2,
                    #fill=TRUE,weight = 0, 
                    stroke = TRUE,weight=1,
                    opacity = 1.0, fillOpacity = 0.6,
                    color = ~zcols1(CL2sp[[zaxis1]]),
                    popup = ~GeoName, 
                    highlightOptions = highlightOptions(color = "white",
                                                        weight = 2,bringToFront = TRUE)
        )
    })
    
    ## 2. Generate the scatterplot
    plot.df <- data.frame(CL2sp@data[,1],  ## Create a convenience data.frame which can be used for charting
                          CL2sp@data[input$tab.x3x],
                          CL2sp@data[input$tab.x3y],
                          CL2sp@data[input$tab.x3z])
    plot.df <- na.omit(plot.df) # Remove NAs for plotting
    colnames(plot.df) <- c('Name','x','y','z')     # Add column names
    
    ### Add legend title
    legendtitle <- list(yref='paper',
                        xref='paper',
                        y=1,x=1,
                        text=input$tab.x3z,
                        showarrow=F)
    
    l <- list(
      font = list(
        family = "sans-serif",
        size = 12,
        color = "#000"),
      bgcolor = "#E2E2E2",
      bordercolor = "#FFFFFF",
      borderwidth = 2)
    
    ### Do a plotly scatter plot to visualize the two features
    ### Note the use of 'source' argument
    output$x3 <- renderPlotly({
      plotly_scat <- plot_ly(data = plot.df, 
                             x = ~x, y = ~y, 
                             text = ~Name,
                             color = ~z, size = ~z, 
                             #alpha=0.4,
                             hoverinfo = "text",
                             mode = "markers", 
                             type = "scatter", 
                             source = "subset") 
      plotly_scat %>%
        layout(xaxis = list(title = input$tab.x3x),
               yaxis = list(title = input$tab.x3y),
               annotations = legendtitle)
      #legend = list(orientation = "h"))#,     
      #title = paste(input$input$tab.x3x, "vs ", input$tab.x3y),
      #dragmode =  "select"
    })
  })
  
  ## 3. Country Explorer table
  output$countryTable <- DT::renderDataTable(
    {
      generateNewDT <- function(x){ 
        
        #Change the header rows of the shiny datatable (note. only changes the display of the columns, not the underlying names)
        output_dt <- DT::datatable(x, 
                                   options=list(
                                     pageLength = 5, # number of rows per page
                                     scrollX = TRUE,
                                     autoWidth = TRUE,
                                     searchHighlight = TRUE, #Highlight searchesd text with yellow
                                     columnDefs = list(list(#width = '50px', 
                                       targets = 1,
                                       render = JS(
                                         "function(data, type, row, meta) {",
                                         "return type === 'display' && data.length > 30 ?",
                                         "'<span title=\"' + data + '\">' + data.substr(0, 30) + '...</span>' : data;",
                                         "}")
                                     ))), 
                                   caption = 'Search country information table', # <a href="#" onclick="alert('This script allows you to write help text for an item');">help me</a> #
                                   #filter = 'top', 
                                   selection = 'single', # selects only one row at a time
                                   rownames = FALSE,  # no row names
                                   colnames=c("EEZ", 
                                              "Territory",
                                              "ISO",
                                              "Sovereign",
                                              "Area (km2)",
                                              'Economic vulnerability', 
                                              'Dependence on marine resources', 
                                              'Education',
                                              'Tourism',
                                              'Corruption',
                                              'Challenge index',
                                              'Opportunity index',
                                              'Conservation likelihood index',
                                              'No. shark and ray species',
                                              'No. theatened species',
                                              'Critically endangered species',
                                              'Endangered species',
                                              'Vulnerable species',
                                              'Non-threatened species'),
                                   callback = JS('table.page(3).draw(false);'),
                                   
                                   #initComplete = JS(
                                   #  "function(settings, json) {",
                                   #  "$(this.api().table().header()).css({'font-size': '90%'});",
                                   #  "}"),
                                   #class = 'white-space: nowrap', # stops wrapping of rows
                                   escape = FALSE  # This bit is to stop the links from rendering literally (i.e. text only)
        )
        #formatStyle(columns = c(1:10), fontSize = '80%')
        
        return(output_dt)
      }
      generateNewDT(CL2sp@data) # Generate the DT based on the SpatialPolygonsDF
      #generateNewDT(qSub2()@data) # Generate the DT based on the SpatialPolygonsDF
    })
  
  ## Based on the Rows selected from the Country Explorer table, modify existing plots
  observeEvent(input$countryTable_rows_selected, {  # Test: Choose which species to visualise on the map using the datatable   
    x <- input$countryTable_rows_selected           # Test: Assign the row number of the species to display
    newdata <- CL2sp[x,] # Select DT row from the SpatialPolygonsDF 
    sNAME <- newdata@data[,1] # Select Country name
    
    ## 1. highlight country on map
    proxy4 <- leafletProxy("mapCounty")
    proxy4 %>% 
      addPolygons(layerId ="layer1",
                  data=newdata,
                  fill = FALSE, stroke = TRUE, weight=3,
                  color = 'red',
                  group = "MPAs") %>% 
      mapOptions(zoomToLimits = "first")
    
    ## 2. RE-Generate the scatterplot
    
    #Where to plot X and Ys for scatter plot
    xaxiscol <- as.integer(newdata@data[input$tab.x3x])
    yaxiscol <- as.integer(newdata@data[input$tab.x3y])
    
    if(!is.na(xaxiscol) & !is.na(yaxiscol)){
      # Top right panel - 
      plot.df <- data.frame(CL2sp@data[,1],  ## Create a convenience data.frame which can be used for charting
                            CL2sp@data[input$tab.x3x],
                            CL2sp@data[input$tab.x3y],
                            CL2sp@data[input$tab.x3z])
      plot.df <- na.omit(plot.df) # Remove NAs for plotting
      colnames(plot.df) <- c('Name','x','y','z')     # Add column names
      
      output$x3 <- renderPlotly({
        plotly_scat2 <- plot_ly(data = plot.df, 
                                x = ~x, y = ~y, 
                                text = ~Name,
                                color = ~z, size = ~z, 
                                hoverinfo = "text",
                                mode = "markers", 
                                type = "scatter", 
                                source = "subset") %>%
          layout(xaxis = list(title = input$tab.x3x),
                 yaxis = list(title = input$tab.x3y))
        plotly_scat2 %>%
          #layout(annotations = a)
          add_annotations(
            x = as.numeric(newdata@data[input$tab.x3x]),
            y = as.numeric(newdata@data[input$tab.x3y]),
            text = sNAME,
            xref = "x",
            yref = "y",
            ax = 20,
            ay = -20,
            xanchor = 'left',
            showarrow = T,
            arrowcolor = 'rgba(222,45,38,1)',
            # Styling annotations' text:
            font = list(color = 'rgba(222,45,38,1)',
                        size = 12)
          )
        #title = paste(input$input$tab.x3x, "vs ", input$tab.x3y),
        #dragmode =  "select",
        #plot_bgcolor = "6A446F")})
      })
      
    }
    
    ## 4. Generate the radar plot
    ### Create socionomic df
    socioEEZ <- CL2sp@data %>%
      filter(GeoName==sNAME)%>%
      select(Corruption,
             EconomicVulnerability,
             Tourism,
             DependMarineResource,
             Education)
    socioEEZ_df <- rbind(iMax,iMin,socioEEZ) # add the max and min of each topic to show on the plot
    
    output$xspider <- renderPlot({
      par(mar = c(0, 0, 0, 0))
      radarchart(socioEEZ_df, axistype=1, # fmsb package
                 pcol=rgb(0.2,0.5,0.5,0.9), pfcol=rgb(0.2,0.5,0.5,0.5), plwd=4, #custom polygon
                 cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,#custom the grid
                 vlcex=1.1,
                 na.itp=F
      ) #custom labels 
    })
    
    ## 5. Create species threat status barplot
    specEEZ <- CL2sp@data %>%
      filter(GeoName==sNAME)%>%
      select(Nospecies,
             Threatened,
             CR,
             EN,
             VU) 
    specEEZ_df <- data.frame(x = c("All species","Threatened","CR","EN","VU"), 
                             y=as.integer(specEEZ))
    
    specEEZ_df$x <- factor(specEEZ_df$x, levels = c("All species","Threatened","CR","EN","VU"))
    
    
    ## Threat status of Species barplot
    output$xthreatbar <- renderPlotly({
      pspec <- plot_ly(specEEZ_df, 
                       x = ~x, y = ~y, type = 'bar',
                       marker = list(color = c('rgba(204,204,204,1)', 'rgba(254,232,200,0.8)',
                                               'rgba(222,45,38,1)', 'rgba(252,146,114,1)', 
                                               'rgba(254,224,210,1)'))
      ) 
      pspec %>%
        layout(title = list(text= sNAME,
                            font=list(size=12)),
               xaxis = list(title = ""),
               yaxis = list(title = "")
        )
    })
    
  })
  
  
  #### TAB 3: Hotspot map and shark MPA details ####
  
  # Arrange data in format for plots and mapping
  lats <- SharkMPAs_coords[,"Lat"]
  longs <- SharkMPAs_coords[,"Long"]
  popups <- SharkMPAs_coords[,"Shark.Marine.Protected.Areas"]
  layerids <- SharkMPAs_coords[,"Shark.Marine.Protected.Areas"]
  iconNames <- ifelse(SharkMPAs_coords[,"Entire.EEZ"] == "Y", "star", "star")
  iconColors <- ifelse(SharkMPAs_coords[,"Entire.EEZ"] == "Y", "green", "blue")
  locationRanks <- data_frame(Name = popups,
                              Date=SharkMPAs_coords[,"Date"],
                              Area.km2=SharkMPAs_coords[,"Area..km2."],
                              Territory.name=SharkMPAs_coords[,"Territory.name"],
                              Sovereign=SharkMPAs_coords[,"Sovereign"],
                              Entire.EEZ=SharkMPAs_coords[,"Entire.EEZ"],
                              Source=createLink(SharkMPAs_coords[,"Source"]),
                              lats,longs, popups,layerids,iconNames,iconColors)
  
  # Convert the dataframe to an interactive DataTable
  d1 <- datatable(locationRanks[,c("Name", "Date",
                                   "Area.km2", "Territory.name",
                                   "Sovereign","Entire.EEZ",
                                   "Source")],
                  selection = 'single',
                  rownames=FALSE,
                  colnames=c("Name", "Date installed",
                             'Area (km2)', 'Territory',
                             'Sovereign', 'Entire EEZ?',
                             'Source'),
                  escape = FALSE,
                  
                  options = list(dom = 'tpi',
                                 pageLength =5,
                                 paging=FALSE,
                                 searching=FALSE,
                                 stateSave = TRUE,
                                 columnDefs = list(list(className = 'dt-left',
                                                        targets = 0:4)))) %>%
    formatCurrency(3, '',digits = 0) # adds the comma seperators for km2
  
  # Render our Shark MPA df as an interactive DataTable in the shiny app
  output$ranksDT <- DT::renderDataTable(d1)
  
  # create a reactive value that will store the position on a map click
  mapClick <- reactiveValues(clickedMarker=NULL)
  mapClick <- reactiveValues(clickedGroup=NULL)
  
  # create a reactive for the DT table
  locationClick <- reactiveValues(clickedRow = NULL)
  
  # observe map click events
  observe({
    mapClick$clickedMarker <- paste(input$SharkMPAMap_marker_click$id)
    mapClick$clickedGroup <- paste(input$SharkMPAMap_marker_click$group)
    #locationClick$clickedRow <- input$ranksDT_rows_selected
  })
  
  # define a proxy variable for the data table
  proxy1 <- dataTableProxy('ranksDT')
  
  # to keep track of previously selected row
  prev_row <- reactiveVal()
  
  # if map is clicked, make the same table row selection
  observeEvent(input$SharkMPAMap_marker_click$id, {
    a <- which(locationRanks[1] == input$SharkMPAMap_marker_click$id)
    proxy1 %>% selectRows(a)
  })
  
  # The icon style change on a map or table click
  my_icon <- makeAwesomeIcon(icon = 'flag', 
                             markerColor = 'red', 
                             iconColor = 'white') # new icon style
  
  # if table is clicked, highlight the same marker from the map
  observeEvent(input$ranksDT_rows_selected,{
    row_selected <- locationRanks[input$ranksDT_rows_selected,]
    # define a proxy that lets us customize and control our SharkMPAMap that has already been rendered.
    proxy2 <- leafletProxy('SharkMPAMap', session = shiny::getDefaultReactiveDomain())
    print(row_selected)
    proxy2 %>%
      addAwesomeMarkers(
        popup=as.character(row_selected$popups),
        layerId = as.character(row_selected$layerids),
        lng = row_selected$longs,
        lat = row_selected$lats,
        icon = my_icon)
    
    # Reset previously selected marker
    if(!is.null(prev_row()))
    {
      proxy2 %>%
        addAwesomeMarkers(popup=as.character(prev_row()$popups),
                          layerId = as.character(prev_row()$layerids),
                          lng = prev_row()$longs,
                          lat = prev_row()$lats,
                          icon = makeAwesomeIcon(icon = "star",
                                                 markerColor = prev_row()$iconColors)
        )
    }
    
    #iconNames <- ifelse(SharkMPAs_coords[,"Entire.EEZ"] == "Y", "star", "star")
    #iconColors <- ifelse(SharkMPAs_coords[,"Entire.EEZ"] == "Y", "green", "blue")
    
    # set new value to reactiveVal
    prev_row(row_selected)
  })
  
  
  # If a row is selected in the data table, highlight the icon on the map
  # observeEvent(input$ranksDT_rows_selected, {
  #   row_selected = locationRanks[input$ranksDT_rows_selected,]
  #   proxy2 %>%
  #     addAwesomeMarkers(
  #       popup=as.character(row_selected$popups),
  #       layerId = as.character(row_selected$layerids),
  #       lng = row_selected$longs,
  #       lat = row_selected$lats,
  #       icon = my_icon)
  # })
  
  
  # Our Leaflet hotspot map containing order AND iucn category ####
  output$mapRegion <- renderLeaflet({
 
    leaflet() %>%
      setView(lng = 0, lat = 0,  zoom = 1) %>%
      addProviderTiles(providers$OpenStreetMap.BlackAndWhite) %>%
      addAwesomeMarkers(lat = locationRanks$lats,    # Add MPAs
                        lng = locationRanks$longs,
                        popup = locationRanks$popups,
                        layerId = locationRanks$layerids,
                        group = "MPAs",
                        icon = makeAwesomeIcon(icon = locationRanks$iconNames,
                                               markerColor = locationRanks$iconColors)) %>%
      addPolygons(data=FAOsimple,       # Add FAO layers
                  smoothFactor = 0.2,
                  stroke = TRUE,weight=1,
                  opacity = 1.0, fillOpacity = 0.1,
                  color = "white",
                  popup = ~F_CODE,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,bringToFront = TRUE),
                  dashArray = "3",
                  group = "FAO_subareas") %>%
      
      addPolygons(data=FAO_major_simple,       # Add FAO layers
                  smoothFactor = 0.2,
                  stroke = TRUE,weight=1,
                  opacity = 1.0, fillOpacity = 0.1,
                  color = "white",
                  popup = ~F_AREA,
                  highlightOptions = highlightOptions(color = "white",
                                                      weight = 2,bringToFront = TRUE),
                  dashArray = "3",
                  group = "FAO_regions") %>%
      
            addLayersControl(baseGroups = c("OSM (default)"),       # Layers control
                       overlayGroups = c("MPAs","FAO_regions","FAO_subareas"),
                       options = layersControlOptions(collapsed = TRUE)) %>%
      # addControl(html = markerLegendHTML(IconSet = IconSet),
      #            position = "bottomright") %>%
      hideGroup(group=c("MPAs","FAO_regions","FAO_subareas"))
  })
  
  proxy3 <- leafletProxy("mapRegion")
  
  observe({
    # select what order to visualise
    if (input$layerOverlap == 'order'){
      data1 <- switch(input$var.order,
                      "all"= iorder,
                      "CARCHARHINIFORMES" = iCARCHARHINIFORMES,
                      "CHIMAERIFORMES" = iCHIMAERIFORMES,
                      "HETERODONTIFORMES" = iHETERODONTIFORMES,
                      "HEXANCHIFORMES" = iHEXANCHIFORMES,
                      "LAMNIFORMES" = iLAMNIFORMES,
                      "ORECTOLOBIFORMES" = iORECTOLOBIFORMES,
                      "PRISTIOPHORIFORMES"= iPRISTIOPHORIFORMES,
                      "RAJIFORMES"= iRAJIFORMES,
                      "SQUALIFORMES" = iSQUALIFORMES,
                      "SQUATINIFORMES" = iSQUATINIFORMES)
      
      # Standardise the display slider
      maxVal <- maxValue(data1)
      minDisplay <- ((100-input$range1)/100) * maxVal
      
      rast_sel_sum <- data1
      rast_sel_sum[rast_sel_sum < minDisplay] <- NA
      
      pal1 <- colorNumeric(scolours.ord,
                           values(rast_sel_sum),
                           na.color = "transparent")
      
      proxy3 %>%
        clearImages() %>% # removes earlier rasters
        clearControls() %>% # # removes earlier legends
        #clearShapes() %>%
        addRasterImage(layerId = "layer3",
                       rast_sel_sum,
                       colors = pal1,
                       opacity = 0.7,
                       group="order") %>%
        addLegend(pal = pal1,
                  values = values(data1),
                  position = "bottomleft",
                  title = "No. species") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)"),
          overlayGroups = c("MPAs","FAO_regions","FAO_subareas","order"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
      #labels = c("Species distribution","MPAs")) %>%
      #addPolygons(#layerId ="layer1",
      #  data=reduced.MPAs,
      #  fill = TRUE, stroke = TRUE, weight=3,
      #  color = pal[2],
      #  group = "MPAs") #%>%
      # Layers control
      #addLayersControl(
      # overlayGroups = c("MPAs"),
      #options = layersControlOptions(collapsed = FALSE))
      
    }else
    {
      # alternatively select what iucn category to visualise
      if (input$layerOverlap == 'iucn'){
        data1 <- switch(input$var.iucn,
                        "all"= istatus,
                        "CR" = iCR,
                        "EN" = iEN,
                        "VU" = iVU,
                        "NT" = iNT,
                        "LC" = iLC,
                        "DD" = iDD,
                        "CR+EN" = iCREN,
                        "CR+EN+VU" = iCRENVU)
        
        # Standardise the display slider
        maxVal <- maxValue(data1)
        minDisplay <- ((100-input$range2)/100) * maxVal
        
        rast_sel_sum <- data1
        rast_sel_sum[rast_sel_sum < minDisplay] <- NA
        
        # Colour of Species distributions and 
        pal1 <- colorNumeric(palette = scolours.iucn,
                             domain = values(rast_sel_sum),
                             na.color = "transparent")
        
        proxy3 %>%
          clearImages() %>% # removes earlier rasters
          clearControls() %>% # # removes earlier legends
          addRasterImage(layerId = "layer4",
                         rast_sel_sum,
                         colors = pal1,
                         opacity = 0.7,
                         group="IUCN") %>%
          addLegend(pal = pal1,
                    values = values(data1),
                    position = "bottomleft",
                    title = "No. species")  %>%
          # Layers control
          addLayersControl(
            baseGroups = c("OSM (default)"),
            overlayGroups = c("MPAs","FAO_regions","FAO_subareas","IUCN"),
            options = layersControlOptions(collapsed = TRUE)
          )
        #addPolygons(#layerId ="layer1",
        # data=reduced.MPAs,
        # fill = TRUE, stroke = TRUE, weight=3,
        # color = pal[2],
        # group = "MPAs")
      }
    }
  })
  
  
  #### TAB 3B:  Interactive Plot containing species counts in zones ####
  output$plot <- renderPlotly({
    
    # select what data to visualise
    if (input$sAreaPolygons == 'sEEZ_count'){
      data1 <- sEEZ_count
      iheight = 1400
    }
    if (input$sAreaPolygons == 'sFAO_count'){
      data1 <- sFAO_count
      iheight = 750
    }
    if (input$sAreaPolygons == 'sFAOsub_count'){
      data1 <- sFAOsub_count
      iheight = 1400
    }
    if (input$sAreaPolygons == 'sLME_count'){
      data1 <- sLME_count
      iheight = 1400
    }
    
    #Set factor names in decreasing order so y axis always plotted in this order
    
    if(input$tab.order=="No Species")
      data1$x <- factor(data1$x, levels = data1$x[order(data1$y,decreasing =FALSE)])
    
    if(input$tab.order=="Threatened Species")
      data1$x <- factor(data1$x, levels = data1$x[order(data1$y5,decreasing =FALSE)])
    
    if(input$tab.order=="Area")
      data1$x <- factor(data1$x, levels = data1$x[order(data1$Area,decreasing =FALSE)])
    
    row.names(data1) <- NULL
    
    setmar1 <- list(l = 400, r = 0, b = 50, t = 0, pad = 1) # The Number of species plot
    setmar2 <- list(l = 0, r = 50, b = 50, t = 0, pad = 1) # The Areas plot
    
    # Generate first plot of species number per country
    p1 <- plot_ly(data1, 
                  x = ~y1, y = ~x, 
                  type = 'bar', orientation = 'h',
                  name = 'Critically Endangered',
                  marker = list(color = 'rgb(202,0,32)',
                                line = list(color = 'rgb(202,0,32)',
                                            width = 1)),
                  height = iheight, # Use an ifelse statement to set this depending on the area entered
                  hoverinfo = 'text',
                  text = ~paste('<b>',x,'</b>',
                                '<br>',y1,' out of ', y,' species listed as Critically Endangered')) %>%
      add_trace(x = ~ y2, 
                name = 'Endangered',
                marker = list(color = 'rgb(244,165,130)',
                              line = list(color = 'rgb(244,165,130)',
                                          width = 1)),
                hoverinfo = 'text',
                text = ~paste('<b>',x,'</b>',
                              '<br>',y2,' out of ', y,' species listed as Endangered')) %>%
      add_trace(x = ~ y3, 
                name = 'Vulnerable',
                marker = list(color = 'rgb(146,197,222)',
                              line = list(color = 'rgb(146,197,222)',
                                          width = 1)),
                hoverinfo = 'text',
                text = ~paste('<b>',x,'</b>',
                              '<br>',y3,' out of ', y,' species listed as Vulnerable')) %>%
      add_trace(x = ~ y4, 
                name = 'Not threatened | Data deficient',
                marker = list(color = 'rgb(5,113,176)',
                              line = list(color = 'rgb(5,113,176)',
                                          width = 1)),
                hoverinfo = 'text',
                text = ~paste('<b>',x,'</b>',
                              '<br>',y4,' out of ', y,' species not threatened or listed as Data Deficient')) %>%
      layout(barmode="stack",
             legend = list(x = 0, y = 0.91, orientation = 'h'),
             title = "",
             yaxis = list(title = "",
                          showgrid = FALSE, showline = FALSE, 
                          showticklabels = FALSE, domain= c(0, 0.85)),
             xaxis = list(title = "Number of Species Present",
                          zeroline = FALSE, showline = FALSE, 
                          showticklabels = TRUE, showgrid = TRUE),
             margin = setmar1) #%>%
    
    # Generate Second plot of Polygon area
    # plot as a scatter plot
    p2 <- plot_ly(data1,
                  type = 'scatter', 
                  x = ~-Area, y = ~x,
                  size = ~Area, 
                  name = 'Area dot',
                  height = iheight,
                  marker = list(color = ~Area,
                                colorscale='Inferno'),
                  hoverinfo = 'text',
                  text = ~paste('<b>',x,'</b>',
                                '<br> Area: ',Area)) %>%
      # plot as a bar plot
      add_trace(x = ~ -Area, 
                name = 'Area bar',
                type = 'bar', 
                size = ~Area,
                marker = list(color = 'rgb(44,162,95)',
                              line = list(color = 'rgb(44,162,95)',
                                          width = 1)),
                hoverinfo = 'text',
                text = ~paste('<b>',x,'</b>',
                              '<br> Area: ',Area),
                visible = "legendonly") %>%
      # Sort the plot layout
      layout(title = "",
             xaxis = list(title = "Area",
                          zeroline = FALSE, showline = FALSE, 
                          showticklabels = TRUE, showgrid = TRUE),
             yaxis = list(title = "",
                          showgrid = FALSE, showline = FALSE, 
                          showticklabels = TRUE, domain= c(0, 0.85)),
             margin = setmar2) 
    
    # Arrange the plots in a suplot with a grouped legend
    subplot(p2, p1, widths = c(0.3,0.7), shareY = TRUE, titleX = TRUE)
    
  })
  
  
}

##############################################################
# Run the application 
shinyApp(ui = ui, server = server)
##############################################################
# Project: GPSR shark MPA Project
# Objective: Generate a GUI for users to investigate conservation priorities for sharks and rays
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer

DateUpdated <-  "29-May-2018" ## Date last updated

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.

# Find out more about building applications with Shiny here: http://shiny.rstudio.com/

# Load required packages
library(shiny)
library(leaflet)
library(sp)
library(rgdal)
library(raster)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(tibble)
library(shinycssloaders)
library(highcharter)

# Load data tables ----
#sharkdat <- read.csv("Data/IUCNStatWeb.csv")
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
reduced.MPAs <- readOGR(dsn="GIS",layer="simplifiedMPA")
# Works only on Windows!
##load('myMPA.RData') ##Loads GIS files: reduced.MPAs,allspecrast,worldmap,orderrast,iucnrast
#Works on Mac though had to sort errors on the loaded rasters
#library(repmis)
#source_data("https://github.com/RossDwyer/SharkRay-MPA/blob/master/myMPA.RData?raw=true")

# tab 1 species lookup table
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
    web_fishbase = sprintf('<a href="%s" target="_blank" class="btn btn-link">fishbase.org</a>',web_fishbase)
  ) %>% 
  select(binomial,CommonName,order_name,family_nam,
         DemersPelag,Vulnerability,Resilience,
         code,web_redlist,web_fishbase)

iucnimg <- paste0('img/DD.png')
iucnlink <- paste0("https://github.com/RossDwyer/SharkRay-MPA/blob/master/img/DD.png")
sharkdat$flag <- ifelse(sharkdat$code=='CR','<img src=img/CR.png> </img>',
                        ifelse(sharkdat$code=='EN','<img src=img/EN.png> </img>',
                               ifelse(sharkdat$code=='VU','<img src=img/VU.png> </img>',
                                      ifelse(sharkdat$code=='NT','<img src=img/NT.png> </img>',
                                             ifelse(sharkdat$code=='LC','<img src=img/LC.png> </img>',
                                                    '<img src=img/DD.png> </img>')))))

cleantable <- sharkdat

# tab 2 - species distribution maps
species.name <- sharkdat$binomial # Names of species for the species range maps  
pal <- c("#253494","#f93") # HEX code for the colour of the raster [1] and the MPAs [2]

#  tab 3 - For the order/IUCN hotspot maps
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

# tab 4
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


# tab 5 (Shark MPA page)
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

# tab 6 (About page)
noSpecies <- length(species.name) # number of species considered


# User interface ----
PageTitle <- "App for Conservation status of Sharks and Rays"

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
                   #style="float:right; padding-right:25px"
               #  ))

               #)
                 
   #              )            
  ),
  
  
  ## TAB 1
  tabPanel(title="Species explorer", 
           fluidRow(
             column(3,
                    selectInput("order_name", "Order name", c("Select order"="", structure(order.name, names=order.name)), multiple=TRUE)
             ),
             column(3,
                    conditionalPanel("input.order_name",
                                     selectInput("family_nam", "Family name", c("All families"=""), multiple=TRUE)
                    )
             ),
             column(3,
                    conditionalPanel("input.order_name",
                                     selectInput("binomial", "Species name", c("Select species"=""), multiple=TRUE)
                    )
             )
           ),
           fluidRow(
             column(6,
                    checkboxGroupInput("code", "Show IUCN listing",
                                       choices = c(
                                         CR = "CR",
                                         EN = "EN",
                                         VU = "VU",
                                         NT = "NT",
                                         LC = "LC",
                                         DD = "DD"),
                                       selected = c("CR","EN","VU","NT","LC","DD")
                    )
             )
           ),
           hr(),
           DT::dataTableOutput("mytable")  %>% 
             withSpinner(color="#3182bd"),
           tags$div(class="header", checked=NA,
                    tags$p("Species common names sourced from..."),
                    tags$a("the Encyclopedia of Life", href="http://eol.org/", target="_blank"))
  ),
  
  
  ## TAB 2
  tabPanel(title="Species map",
           
           div(class="outer",
               
               # taken from https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               leafletOutput("map2", width = "100%", height = 700) %>% 
                 withSpinner(color="#3182bd"),
               
               absolutePanel(top = 10, left = 50,
                             selectInput(inputId ="var2", 
                                         label ="Choose a species to map its distribution", 
                                         choices = species.name)
               ),
               absolutePanel(bottom = 20, left = 10,
                             a(href = "http://www.iucnredlist.org","Species distribution layers sourced from The IUCN Red List of Threatened Species"))
           )
  ),
  
  
  ## TAB 3
  tabPanel(title="Shark MPA explorer",
           fluidPage(
             leafletOutput("SharkMPAMap"),
             DT::dataTableOutput('ranksDT')
           )
  ),
  
  ## TAB 4
  tabPanel(title="Hotspot map",
           div(class="outer",
               
               # taken from https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
               tags$head(
                 # Include our custom CSS
                 includeCSS("styles.css"),
                 includeScript("gomap.js")
               ),
               
               #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
               leafletOutput("map", width = "100%", height = 700) %>% 
                 withSpinner(color="#3182bd"),
               
               absolutePanel(top = 10, left = 50,
                             
                             radioButtons("layerOverlap", "Choose which layers to overlap:",
                                          c("Taxonomic order"= "order",
                                            "IUCN listing" = "iucn"),
                                          inline = TRUE),
                             
                             conditionalPanel(
                               condition = "input.layerOverlap == 'order'",
                               selectInput("var.order", 
                                           label = "Choose the Taxonomic order to display",
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
                                                          "SQUATINIFORMES"),
                                           selected = "all"),
                               sliderInput("range1",
                                           "Upper % species displayed :",
                                           min = 0, max = 100, step = 10,  value = 90)
                             ),
                             
                             conditionalPanel(
                               condition = "input.layerOverlap == 'iucn'",
                               selectInput("var.iucn", 
                                           label = "Choose the IUCN code to display",
                                           choices = list("all",
                                                          "CR",
                                                          "EN",
                                                          "VU",
                                                          "NT",
                                                          "LC",
                                                          "DD",
                                                          "CR+EN",
                                                          "CR+EN+VU"),
                                           selected = "all"),
                               sliderInput("range2",
                                           "Upper % species displayed :",
                                           min = 0, max = 100, step = 10,  value = 90)
                               #checkboxInput("legend", "Show legend", TRUE)
                             )
               )
           )
  ),
  
  
  ## TAB 5
  tabPanel(title="Interactive chart",
           fluidPage(
             tags$div(class="header", checked=NA,
                      tags$strong("This plot displays the number of shark and ray species present in marine and coastal regions")
             ),
             radioButtons("sAreaPolygons", "Select which areas to visualise:",
                          c("Exclusive Economic Zones"= "sEEZ_count",
                            "FAO Regions" = "sFAO_count",
                            "FAO Subareas" = "sFAOsub_count",
                            "Large Marine Ecosystems" = "sLME_count"),
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
             ))),
  
  
  ## TAB 6
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


# server logic required to draw the map ----

server <- function(input, output, session) {
  
  #### TAB 1: Species Explorer #### 
  
  observe({
    family_nam <- if (is.null(input$order_name)) character(0) else {
      filter(cleantable, order_name %in% input$order_name) %>%
        `$`('family_nam') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$family_nam[input$family_nam %in% family_nam])
    updateSelectInput(session, "family_nam", choices = family_nam,
                      selected = stillSelected)
  })
  
  observe({
    binomial <- if (is.null(input$order_name)) character(0) else {
      cleantable %>%
        filter(order_name %in% input$order_name,
               is.null(input$family_nam) | family_nam %in% input$family_nam) %>%
        `$`('binomial') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$binomial[input$binomial %in% binomial])
    updateSelectInput(session, "binomial", choices = binomial,
                      selected = stillSelected)
  })
  
  ## Generated data explorer table
  output$mytable <- DT::renderDataTable(
    {
      df <- cleantable %>%
        filter(
          is.null(input$order_name) | order_name %in% input$order_name,
          is.null(input$family_nam) | family_nam %in% input$family_nam,
          is.null(input$binomial) | binomial %in% input$binomial,
          is.null(input$code)  | code %in% input$code)  %>%
        select(binomial, CommonName,
               order_name, family_nam,
               DemersPelag,Vulnerability,Resilience,
               flag,
               web_redlist,
               #assessment_redlist,
               web_fishbase)
      
      #Change the header rows of the shiny datatable (note. only changes the display of the columns, not the underlying names)
      df <- datatable(df, 
                      colnames=c("Species name", "Common names",
                                 'Order name', 'Family name',
                                 'Habitat', 'Vulnerability index', 'Resilience',
                                 'IUCN threat category', 
                                 'IUCN Red List',
                                 #'Download IUCN assessment',
                                 'Fishbase'),
                      escape = FALSE) # This bit is to stop the links from rendering literally (i.e. text only)
    }
  )
  
  #mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', 
  #                      Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  #action <- DT::dataTableAjax(session, df)
  
  
  #DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  
  
  
  #### TAB 2: Interactive map containing species distributions ####
  
  output$map2 <- renderLeaflet({
    
    leaflet() %>% 
      setView(lng = 0, lat = 0,  zoom = 2) %>% 
      addTiles(group = "OSM (default)") %>%
      addPolygons(#layerId ="layer1",
        data=reduced.MPAs,
        fill = TRUE, stroke = TRUE, weight=3,
        color = pal[2],
        group = "MPAs") %>% 
      addLegend(colors = pal, 
                labels = c("Species distribution","MPAs")) %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("MPAs"),
        options = layersControlOptions(collapsed = FALSE))
    
  })
  
  #eventReactive(input$var2, { 
  observeEvent(input$var2, { 
    
    x <- which(sharkdat$binomial == input$var2)     # Set which species to display
    newdata <- allspecrast[[x]]
    newdata[newdata <= 0] <- NA 
    proxy <- leafletProxy("map2")
    
    #clearShapes() %>%
    #addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>% #http://leaflet-extras.github.io/leaflet-providers/preview/
    #addProviderTiles("Esri.WorldGrayCanvas",options = providerTileOptions(minZoom=10, maxZoom=18))%>%
    
    proxy %>% 
      addRasterImage(layerId ="layer2",
                     newdata,
                     colors=pal[1], opacity = 0.5,
                     group = "Species") %>%
      mapOptions(zoomToLimits = "first")
    
  })
  
  ####TAB 3:  Interactive Chart and map containing shark MPA details ####
  lats <- SharkMPAs_coords[,"Lat"]
  longs <- SharkMPAs_coords[,"Long"]
  popups <- SharkMPAs_coords[,"Shark.Marine.Protected.Areas"]
  layerids <- SharkMPAs_coords[,"Shark.Marine.Protected.Areas"]
  iconNames <- ifelse(SharkMPAs_coords[,"Entire.EEZ"] == "Y", "star", "star")
  iconColors <- ifelse(SharkMPAs_coords[,"Entire.EEZ"] == "Y", "green", "blue")
  
  # Reactive expression for the data subsetted to what the user selected
  #sampleData <- reactive({
  #  sampleData <- data_frame(lats,longs, popups,layerids,iconNames,iconColors)
  #  iconColors[a] <- 'red'
  #})
  
  #type <- factor(
  #  ifelse(SharkMPAs_coords[,"Entire.EEZ"] == "Y", "Entire EEZ", "Part EEZ"),
  #  c("Regular Ship", "Pirate Ship"))
  
  # old code not reactive
  sampleData <- data_frame(lats,longs, popups,layerids,iconNames,iconColors)
  
  Area.km2 <- SharkMPAs_coords[,"Area..km2."]
  
  locationRanks <- data_frame(Name = popups,
                              Date=SharkMPAs_coords[,"Date"],
                              Area.km2,
                              Territory.name=SharkMPAs_coords[,"Territory.name"],
                              Sovereign=SharkMPAs_coords[,"Sovereign"],
                              Entire.EEZ=SharkMPAs_coords[,"Entire.EEZ"],
                              #Comments = SharkMPAs_coords[,"Comment..map.source.when.not.entire.EEZ"],
                              Source=createLink(SharkMPAs_coords[,"Source"]))
  
  # The name of our Map
  output$SharkMPAMap <- renderLeaflet({
    
    leaflet() %>%
      addTiles() %>% 
      addAwesomeMarkers(lat = sampleData$lats, 
                        lng = sampleData$longs, 
                        popup = sampleData$popups, 
                        layerId = sampleData$layerids,
                        icon = makeAwesomeIcon(icon=sampleData$iconNames, 
                                               markerColor=sampleData$iconColors)) %>% 
      addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")
  })
  
  output$ranksDT <- DT::renderDataTable({
    d1 <- datatable(locationRanks,
                    selection = 'single',
                    rownames=FALSE,
                    options = list(dom = 'tpi',
                                   pageLength =5,
                                   paging=FALSE,
                                   searching=FALSE,
                                   columnDefs = list(list(className = 'dt-left', 
                                                          targets = 0:4))))
    d1 %>% 
      formatCurrency(3, '',digits = 0) # adds the comma seperators for km2
  })
  
  # create a reactive value that will store the click position
  mapClick <- reactiveValues(clickedMarker=NULL)
  mapClick <- reactiveValues(clickedGroup=NULL)
  
  # create a reactive for the DT  table
  locationClick <- reactiveValues(clickedRow = NULL)
  
  # observe click events
  observe({
    mapClick$clickedMarker <- paste(input$SharkMPAMap_marker_click$id)
    mapClick$clickedGroup <- paste(input$SharkMPAMap_marker_click$group)
    locationClick$clickedRow <- input$ranksDT_rows_selected
  })
  
  # define a proxy variable for the table
  proxy1 = dataTableProxy('ranksDT')
  
  # when map is clicked, make the same table row selection - need row number
  observeEvent(input$SharkMPAMap_marker_click$id, {
    a <- which(locationRanks[1] == input$SharkMPAMap_marker_click$id)
    proxy1 %>% selectRows(a)
  })
  
  # Creates a map-like object that can be used to customize and control our SharkMPAMap that has already been rendered.  
  proxy2 = leafletProxy('SharkMPAMap', session = shiny::getDefaultReactiveDomain())
  # if table is clicked, select the same marker from the map
  observeEvent(locationClick$clickedRow, {
    a <- locationClick$clickedRow
  })
  
  
  ####TAB 4: Interactive hotspot map containing order AND iucn category #### 
  
  output$map <- renderLeaflet({
    
    leaflet() %>% 
      setView(lng = 0, lat = 0,  zoom = 2) %>% 
      addTiles(group = "OSM (default)")
  })
  
  proxy <- leafletProxy("map") 
  
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
      
      proxy %>% 
        clearImages() %>% # removes earlier rasters
        clearControls() %>% # # removes earlier legends
        addRasterImage(layerId = "layer3",
                       rast_sel_sum, 
                       colors = pal1, 
                       opacity = 0.7,
                       group="order") %>%
        addLegend(pal = pal1, 
                  values = values(data1),
                  position = "topright",
                  title = "No. species") 
      
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
        
        pal1 <- colorNumeric(palette = scolours.iucn, 
                             domain = values(rast_sel_sum),
                             na.color = "transparent")
        
        proxy %>% 
          clearImages() %>% # removes earlier rasters
          clearControls() %>% # # removes earlier legends
          addRasterImage(layerId = "layer4",
                         rast_sel_sum, 
                         colors = pal1, 
                         opacity = 0.7,
                         group="IUCN") %>%
          addLegend(pal = pal1, 
                    values = values(data1),
                    position = "topright",
                    title = "No. species") 
      }
    }
  })
  
  
  ####TAB 5:  Interactive Plot containing species counts in zones ####
  
  #v <- reactiveValues(data = NULL)
  
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

# Run the application 
shinyApp(ui = ui, server = server)

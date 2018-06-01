# Project: GPSR shark MPA Project
# Objective: Generate a GUI for users to investigate conservation priorities for sharks and rays
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer

DateUpdated <-  "31-May-2018" ## Date last updated

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.


##############################################################################
# Libraries
##############################################################################

library(shiny)
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
library(ggplot2)

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
reduced.MPAs <- readOGR(dsn="GIS",layer="simplifiedMPA")
# Works only on Windows!
##load('myMPA.RData') ##Loads GIS files: reduced.MPAs,allspecrast,worldmap,orderrast,iucnrast
#Works on Mac though had to sort errors on the loaded rasters
#library(repmis)
#source_data("https://github.com/RossDwyer/SharkRay-MPA/blob/master/myMPA.RData?raw=true")

# tab 1c species lookup table
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

cleantable <- sharkdat %>%
  select(binomial, 
         CommonName,
         order_name, family_nam,
         DemersPelag,Vulnerability,Resilience,
         flag,
         web_redlist,
         #assessment_redlist,
         web_fishbase)

# tab 1a - species distribution maps
species.name <- sharkdat$binomial # Names of species for the species range maps  
pal <- c("#253494","#f93") # HEX code for the colour of the raster [1] and the MPAs [2]

###

# tab 2 (Shark MPA page)
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

###

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

###

# tab 6 (About page)
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
             column(6,
                    leafletOutput("map2", width = '100%',height=300) %>% 
                      withSpinner(color="#3182bd")),
             
             # Top right panel             
             column(6, plotOutput('x2', height = 300)),
             
             # Panel to select species from dropdown
             #absolutePanel(top = 70, left = 70,
             #               selectInput(inputId ="var2", 
             #                          label ="Select species to map", 
             #                         choices = species.name)
             #),
             
             # Text to say where mapping data came from
             #absolutePanel(top = 50, left = 10,
             #              a(href = "http://www.iucnredlist.org",
             #                "Species distribution layers sourced from The IUCN Red List of Threatened Species")),
             
             # Bottom panel
             DT::dataTableOutput("mytable", width = '100%', height = 200)
           )
  ),  
  
  ## TAB 2
  tabPanel(title="Shark MPA explorer",
           
           fluidPage(
             #titlePanel("Visualization of Fiji Earthquake"),
             
             # side panel
             sidebarPanel(
               sliderInput(
                 inputId = "sld01_Mag",
                 label="Show Shark MPAs of size:", 
                 min=min(SharkMPAs_coords$Area..km2.), max=max(SharkMPAs_coords$Area..km2.),
                 value=c(min(SharkMPAs_coords$Area..km2.),max(SharkMPAs_coords$Area..km2.), step=1000)
               ),
               
               plotlyOutput('hist01')
             ),
             
             # main panel
             mainPanel(
               leafletOutput("SharkMPAMap"),
               DT::dataTableOutput('ranksDT')
             )
           )
  ),
  
  ## TAB 3
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
  
  
  ## TAB 4
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
  
  
  ## TAB 5
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
  
  # Top panel - PLot the map
  output$map2 <- renderLeaflet({
    
    leaflet() %>% 
      setView(lng = 0, lat = 0,  zoom = 1) %>% 
      addTiles(group = "OSM (default)") %>%
      #addPolygons(#layerId ="layer1",
      #  data=reduced.MPAs,
      #  fill = TRUE, stroke = TRUE, weight=3,
      #  color = pal[2],
      #  group = "MPAs") %>% 
      addLegend(colors = pal[1], 
                labels = c("Species distribution")) #%>%
    #labels = c("Species distribution","MPAs")) %>%
    #addPolygons(#layerId ="layer1",
    #  data=reduced.MPAs,
    #  fill = TRUE, stroke = TRUE, weight=3,
    #  color = pal[2],
    #  group = "MPAs") %>% 
    # Layers control
    #addLayersControl(
    #  overlayGroups = c("MPAs"),
    #  options = layersControlOptions(collapsed = FALSE))
    
  })
  
  #observeEvent(input$var2, {                       # Choose which species to visualise on the map using the dropdown
  #x <- which(sharkdat$binomial == input$var2)      # Assign the row number of the species to display
  
  observeEvent(input$mytable_rows_selected, {  # Test: Choose which species to visualise on the map using the datatable   
    x <- input$mytable_rows_selected           # Test: Assign the row number of the species to display
    
    newdata <- allspecrast[[x]]
    newdata[newdata <= 0] <- NA 
    proxy <- leafletProxy("map2")
    
    proxy %>% 
      addRasterImage(layerId ="layer2",
                     newdata,
                     colors=pal[1], opacity = 0.5,
                     group = "Species") %>%
      mapOptions(zoomToLimits = "first")
  })
  
  #bottom tab: Species Explorer #### 
  ## Generate data explorer table
  output$mytable <- DT::renderDataTable(
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
                                              'Habitat', 'Vulnerability index', 'Resilience',
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
  
  # highlight selected rows in the scatterplot
  output$x2 <- renderPlot(
    {
      s <- input$mytable_rows_selected
      par(mar = c(4, 4, 1, .1))
      par(font.axis = 2,font.lab = 2)  
      plot(sharkdat$Vulnerability,sharkdat$Vulnerability,
           cex=1,
           xlab="Vulnerability index",ylab="Vulnerability index")
      #ggplot(mapping = aes(x = code, y = Vulnerability)) + 
      #xlab("IUCN code") + ylab("Vulnerability index") +
      if (length(s)) 
        points(sharkdat$Vulnerability[s],sharkdat$Vulnerability[s], pch = 19, cex = 2,col='red')
    }
  )
  
  ######
  
  ####TAB 2:  Interactive Chart and map containing shark MPA details ####
  
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
  
  ## Slider tool to subset data
  qSub <-  reactive({
    subset <- subset(locationRanks, locationRanks$Area.km2>=input$sld01_Mag[1] &
                       locationRanks$Area.km2<=input$sld01_Mag[2]) %>% head(25)
  })
  
  
  ## Render our frequency plot
  output$hist01 <- renderPlotly({
    par(font.axis = 2,font.lab = 2)
    ggplot(data=qSub(), aes(x=Entire.EEZ)) + 
      geom_bar(stat = "count",
               show.legend=FALSE,
               aes(fill = Entire.EEZ))+
      xlab('') +
      ylab('Count') +
      ylim(0, 20) +
      scale_fill_manual(values=c("#41ab5d", "#2b8cbe"))+
      ggtitle('Is MPA entire EEZ?')+
      theme(legend.position="none")
  })
  
  # Render our Shark MPA table
  output$ranksDT <- DT::renderDataTable({
    d1 <- datatable(qSub()[,c("Name", "Date",
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
                                                          targets = 0:4))))
    d1 %>% 
      formatCurrency(3, '',digits = 0) # adds the comma seperators for km2
  })
  
  # Render our Shark MPA map
  output$SharkMPAMap <- renderLeaflet({
    leaflet(data = qSub()) %>%
      addTiles() %>% 
      addAwesomeMarkers(lat = qSub()$lats,#sampleData$lats, 
                        lng = qSub()$longs, 
                        popup = qSub()$popups, 
                        layerId = qSub()$layerids,
                        icon = makeAwesomeIcon(icon = qSub()$iconNames, 
                                               markerColor = qSub()$iconColors)) %>% 
      addControl(html = markerLegendHTML(IconSet = IconSet), position = "bottomleft")
  })
  
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
    a <- which(qSub()[1] == input$SharkMPAMap_marker_click$id)
    proxy1 %>% selectRows(a)
  })
  
  # The icon style change on a map or table click
  my_icon <- makeAwesomeIcon(icon = 'flag', markerColor = 'red', iconColor = 'white') # new icon style
  
  # if table is clicked, highlight the same marker from the map   
  observeEvent(input$ranksDT_rows_selected,{ 
    row_selected <- qSub()[input$ranksDT_rows_selected,]
    # define a proxy that lets us customize and control our SharkMPAMap that has already been rendered.
    proxy2 <- leafletProxy('SharkMPAMap', session = shiny::getDefaultReactiveDomain())
    print(row_selected)
    proxy2 %>%
      addAwesomeMarkers(
        popup=as.character(row_selected$popups),
        layerId = as.character(row_selected$layerids),
        lng = row_selected$longs, 
        lat = row_selected$lats, 
        icon = my_icon) # %>%
      #mapOptions(zoomToLimits = "first")    
  
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
  observeEvent(input$ranksDT_rows_selected, {
    row_selected = qSub()[input$ranksDT_rows_selected,]
    proxy %>%
      addAwesomeMarkers(
        popup=as.character(row_selected$popups),
        layerId = as.character(row_selected$layerids),
        lng = row_selected$longs, 
        lat = row_selected$lats,
        icon = my_icon)
  })
  
  
  ####TAB 3: Interactive hotspot map containing order AND iucn category #### 
  
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
                  title = "No. species") #%>%
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
                    title = "No. species")  #%>%
        #addPolygons(#layerId ="layer1",
        # data=reduced.MPAs,
        # fill = TRUE, stroke = TRUE, weight=3,
        # color = pal[2],
        # group = "MPAs")
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

##############################################################
# Run the application 
shinyApp(ui = ui, server = server)
##############################################################
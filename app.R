# Project: GPSR shark MPA Project
# Objective: Generate a GUI for users to investigate conservation priorities for sharks and rays
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer

DateUpdated <-  "15-Jan-2018" ## Date last updated

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
library(DT)

# Load data tables ----
sharkdat <- read.csv("Data/datatable containing species names and IUCN categories.csv")#[1:dim(specrast)[3],] # limited by the number of sharks loaded into the raster 
EEZ_spec <- read.csv("Data/Sharks and rays in EEZs.csv")
Oceans_spec <- read.csv("Data/Sharks and rays in Oceans.csv")

#library(RCurl)
#sharkdat <- read.csv(text = getURL("https://raw.githubusercontent.com/RossDwyer/SharkRay-MPA/master/Data/datatable%20containing%20species%20names%20and%20IUCN%20categories.csv"),header=T)
#EEZ_spec <- read.csv(text = getURL("https://raw.githubusercontent.com/RossDwyer/SharkRay-MPA/master/Data/Sharks%20and%20rays%20in%20EEZs.csv"),header=T)
#Oceans_spec <- read.csv(text = getURL("https://raw.githubusercontent.com/RossDwyer/SharkRay-MPA/master/Data/Sharks%20and%20rays%20in%20Oceans.csv"),header=T)

##Loads GIS files: reduced.MPAs,allspecrast,worldmap,orderrast,iucnrast ----

# Works only on Windows!
##load('myMPA.RData') ##Loads GIS files: reduced.MPAs,allspecrast,worldmap,orderrast,iucnrast

#Works on Mac though had to sort errors on the loaded rasters
#library(repmis)
#source_data("https://github.com/RossDwyer/SharkRay-MPA/blob/master/myMPA.RData?raw=true")
orderrast <- brick("GIS/ordersum_specrast.tif")
iucnrast <- brick("GIS/IUCNsum_specrast.tif")
allspecrast <- brick("GIS/multilayerspecrast.tif")
reduced.MPAs <- readOGR(dsn="GIS",layer="simplifiedMPA")

# tab 1 lookup table
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
cleantable <- sharkdat

# tab 2
species.name <- sharkdat$binomial # Names of Species for the species range maps  
pal <- c("#253494","#f93") # HEX code for the colour of the raster and the MPAs

#  tab 3 - For the order/IUCN category maps
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

# Colour scale for the category maps
scolours.ord <- c("#e5f5e0", "#a1d99b", "#31a354")
scolours.iucn <- c("#fee0d2", "#fc9272", "#de2d26")

# tab 4
ivis <- 10 # No EEZ/Oceans to visualise in the table
sbarchart_colours <- rev(colorRampPalette(brewer.pal(9,"Blues")[-1])(ivis))
EEZ_spec1 <- data.frame(EEZ_spec[order(EEZ_spec$Nospecies,decreasing=TRUE),][1:ivis,],row.names=NULL)
Oceans_spec1 <- data.frame(Oceans_spec[order(Oceans_spec$Nospecies,decreasing=TRUE),][1:ivis,],row.names=NULL)

sEEZ_count <- data.frame(x=EEZ_spec1$Territory1,y=EEZ_spec1$Nospecies)
sOceans_count <- data.frame(x=Oceans_spec1$name,y=Oceans_spec1$Nospecies)

# tab 5 (About page)
noSpecies <- length(species.name) # number of species considered


# User interface ----

ui <- navbarPage("GPSR MPA project", 
                 id="nav",
                 
                 
                 ## TAB 1
                 tabPanel("Data explorer", 
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
                                                        NT = "NT",
                                                        VU = "VU",
                                                        LC = "LC",
                                                        DD = "DD"),
                                                      selected = c("CR","EN","NT","VU","LC","DD")
                                   )
                            )
                          ),
                          hr(),
                          DT::dataTableOutput("mytable")
                 ),
                 
                 
                 ## TAB 2
                 tabPanel("Species map",
                          fluidRow(
                            column(3,
                                   selectInput(inputId ="var2", 
                                               label ="Species name", 
                                               choices = species.name))
                            
                          ),
                          
                          div(class="outer",
                              
                              # taken from https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              
                              
                              #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                              leafletOutput("map2", width = "100%", height = 700)
                              
                              #DT::dataTableOutput("mytable")
                          )
                 ),
                 
                 
                 ## TAB 3
                 tabPanel("Interactive map",
                          div(class="outer",
                              
                              # taken from https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
                              tags$head(
                                # Include our custom CSS
                                includeCSS("styles.css"),
                                includeScript("gomap.js")
                              ),
                              
                              #tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                              leafletOutput("map", width = "100%", height = 700),
                              
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
                                                          "% species displayed :",
                                                          min = 0, max = 100, step = 10,  value = 90)
                                            ),
                                            
                                            conditionalPanel(
                                              condition = "input.layerOverlap == 'iucn'",
                                              selectInput("var.iucn", 
                                                          label = "Choose the IUCN code to display",
                                                          choices = list("all",
                                                                         "CR",
                                                                         "EN",
                                                                         "NT",
                                                                         "VU",
                                                                         "LC",
                                                                         "DD"),
                                                          selected = "all"),
                                              sliderInput("range2",
                                                          "% species displayed :",
                                                          min = 0, max = 100, step = 10,  value = 90)
                                              #checkboxInput("legend", "Show legend", TRUE)
                                            )
                              )
                          )
                 ),
                 
                 
                 ## TAB 4
                 tabPanel(title="Interactive chart",
                          fluidPage(
                            radioButtons("sAreaPolygons", "Choose which areas to visualise:",
                                         c("Exclusive Economic Zones"= "sEEZ_count",
                                           "Oceans" = "sOceans_count"),
                                         inline = TRUE),
                            hr(),
                            plotlyOutput("plot", width = "100%", height = "100%"),
                            verbatimTextOutput("event"))),
                 

                 ## TAB 5
                 tabPanel(title="About",
                          tags$body(
                            h4('This Shiny App was built to help visualise shark and ray distribution information across the globe'),
                            p(paste0("Our application contains range distribution information of ",noSpecies," shark and ray species downloaded from...")),
                            a(href = "http://www.iucnredlist.org","The IUCN Red List of Threatened Species"),
                            br(),
                            br(),
                            p("Our application is powered by...")),
                          tags$img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gradient.png", width = "180px", height = "60px"),
                          tags$img(src = "http://www.mcclellandlegge.com/img/shiny-logo.png", width = "100px", height = "100px"),
                          tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/13/Leaflet_logo.svg/2000px-Leaflet_logo.svg.png", width = "180px", height = "60px"),
                          br(),
                          br(),
                          br(),
                          p("This application was built by Dr Ross Dwyer as part of the Global Partnership for Sharks and Rays project"),
                          a(href = "https://github.com/RossDwyer","Our project is on GitHub"),
                          br(),
                          br(),
                          p(paste0("Date last updated: ",DateUpdated))
                          )
          
)


# server logic required to draw the map ----

server <- function(input, output, session) {
  
  #### TAB 1: Data Explorer #### 
  
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
  output$mytable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        is.null(input$order_name) | order_name %in% input$order_name,
        is.null(input$family_nam) | family_nam %in% input$family_nam,
        is.null(input$binomial) | binomial %in% input$binomial,
        is.null(input$code)  | code %in% input$code

      ) #%>%
      #mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', 
      #                      Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    #action <- DT::dataTableAjax(session, df)
  })
  
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
  
  
    ####TAB 3: Interactive map containing order AND iucn category #### 
    
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
                         opacity = 0.5,
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
                          "NT" = iNT,
                          "VU" = iVU,
                          "LC" = iLC,
                          "DD" = iDD)
          
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
                           opacity = 0.5,
                           group="IUCN") %>%
            addLegend(pal = pal1, 
                      values = values(data1),
                      position = "topright",
                      title = "No. species") 
        }
      }
    })
    
    
  ####TAB 4:  Interactive Plot containing species counts ####
    
  #v <- reactiveValues(data = NULL)

  output$plot <- renderPlotly({
    
    # select what data to visualise
    if (input$sAreaPolygons == 'sEEZ_count')
      data1 <- sEEZ_count
    
    if (input$sAreaPolygons == 'sOceans_count')
      data1 <- sOceans_count
    
    #data1 <- v$data 
    data1$x <- factor(data1$x, levels = data1$x[order(data1$y,decreasing =TRUE)])
    row.names(data1) <- NULL
    
    plot_ly(data1, 
            x = ~x, y = ~y, 
            type = 'bar',
            marker = list(color = sbarchart_colours),
            height = 600, width = 900,
            hoverinfo = 'text',
            text = ~paste('<b>',x,'</b>',
                          '<br> Species number: ', y)) %>%
      layout(title = "Number of Shark and Ray Species Present",
             xaxis = list(title = "",
                          tickangle = 45,
                          zeroline = FALSE,
                          showline = FALSE,
                          #showticklabels = FALSE,
                          showgrid = FALSE),
             yaxis = list(title = "Number of Species"),
             margin = list(l = 50, r = 50, b = 180, t = 50, pad = 4))
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)


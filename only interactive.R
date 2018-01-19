# Project: GPSR shark MPA Project
# Objective: Generate a GUI for users to investigate conservation priorities for sharks and rays
# Species: All >1000 sharks and Rays
# Developer: Ross Dwyer

DateUpdated <-  "11-Jan-2018" ## Date last updated

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

orderrast <- brick("GIS/ordersum_specrast.tif")
iucnrast <- brick("GIS/IUCNsum_specrast.tif")
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

scolours.ord <- c("#e5f5e0", "#a1d99b", "#31a354")
scolours.iucn <- c("#fee0d2", "#fc9272", "#de2d26")

# User interface ----

ui <- navbarPage("GPSR MPA project", 
                 id="nav",
                 
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
                 )
)


# server logic required to draw the map ----

server <- function(input, output, session) {
  
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
  
}

# Run the application 
shinyApp(ui = ui, server = server)


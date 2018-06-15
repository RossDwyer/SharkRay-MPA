library(readxl)
library(dplyr)
library(plotly)

CL <- read.csv("Data/data.Fig4_all.csv")
reduced.countries <- readOGR(dsn="GIS/TM_WORLD_BORDERS_SIMPL-0.3","TM_WORLD_BORDERS_SIMPL-0.3")

##Load Amy's dataset
CI_FinalPCA <- read_excel("Data/CI_Database_FinalPCA.xlsx")
CI_FinalPCA <- as.data.frame(CI_FinalPCA)

CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Venezuela (Bolivarian Republic of)")] <- "Venezuela"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Timor Leste")] <- "East Timor"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Taiwan Province of China")] <- "Taiwan"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Syrian arab republic")] <- "Syria"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Russian Federation")] <- "Russia"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Palestine, (west bank & gaza)")] <- "Palestine"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Nauru * add more data")] <- "Nauru"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Peoples Republic of Korea (north)")] <- "North Korea"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Republic of Korea (south)")] <- "South Korea"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Iran (Islamic Republic of)")] <- "Iran"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Cote d'Ivoire")] <- "Ivory Coast"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Democratic Republic of Congo")] <- "Democratic Republic of the Congo"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Cabo Verde")] <- "Cape Verde"
CI_FinalPCA$CountryName[which(CI_FinalPCA$CountryName=="Brunei Darussalam")] <- "Brunei" 

names(CI_FinalPCA)[1] <- "Territory1" # Rename for merging with the EEZ species table

# Load species dataset
EEZ_spec <- read.csv("Data/Sharks and rays in EEZs_CRENVU.csv")

EEZ_spec_CI <- left_join(EEZ_spec,CI_FinalPCA,by="Territory1")
EEZ_spec_CI <- EEZ_spec_CI[order(EEZ_spec_CI$Territory1),]
EEZ_specCI_clean <- na.omit(EEZ_spec_CI) # Clean out any NAs
row.names(EEZ_specCI_clean) <- NULL # Remove the old row numbers



EEZ_specCI_clean <- EEZ_specCI_clean %>%
  rename(ChallengeIndex = "Challenge Index",
         OpportunityIndex = "Opportunity Index",
         DependMarineResource = "Dependance on Marine Resource") %>%
  mutate(Threatened = CR+EN+VU) %>%
  select(GeoName,
         Sovereign1, 
         Territory1,
         Area_km2,
         EconomicVulnerability,
         DependMarineResource,
         Education,
         Tourism,
         Corruption,
         ChallengeIndex,
         OpportunityIndex,
         CLI,
         Nospecies,
         CR,
         EN,
         VU,
         Threatened,
         rest)

write.csv(EEZ_specCI_clean,"Data/Sharks and rays in EEZs_CI_CRENVU.csv",row.names=FALSE)
#####################################################

myPalette <- colorRampPalette(rev(brewer.pal(11, "Spectral")))
sc <- scale_colour_gradientn(colours = myPalette(100), limits=c(1, 8))



par(mar = c(4, 4, 1, .1))
#par(font.axis = 2,font.lab = 2)  
#dimensions 5.X8
ct <- EEZ_specCI_clean %>%
  ggplot(aes(x=ChallengeIndex, y = OpportunityIndex,colour = Threatened)) + 
  #scale_y_continuous(breaks = c(0,8.5), 
  #                   limits = c(0,9),
  #                   labels = c("None", "High"), 
  #                   name = "Presence of management") +
  #annotate("rect", xmin = -Inf, xmax = mean(CL$nclsum), ymin = -Inf, ymax = mean(CL$mmtsum), fill= "grey80")  + 
  #annotate("rect", xmin = mean(CL$nclsum), xmax = Inf, ymin = -Inf, ymax = mean(CL$mmtsum) , fill= "grey40") + 
  #annotate("rect", xmin = -Inf, xmax = mean(CL$nclsum), ymin = mean(CL$mmtsum), ymax = Inf, fill= "grey60") + 
  #annotate("rect", xmin = mean(CL$nclsum), xmax = Inf, ymin = mean(CL$mmtsum), ymax = Inf, fill= "grey20") + 
  geom_point(shape=21,aes(size=Nospecies,
                          colour=Threatened,
                          fill=Threatened)) + sc
  scale_fill_gradient2()
  scale_size(range = c(3, 20)) +
  scale_x_continuous(name = "Conservation likelihood", labels = c("Low", "High"), 
                     breaks = c(-7,3), limits = c(-7,3.2)) + 
  #scale_fill_manual(values = c("#78B7C5","#3B9AB2", "#E1AF00","#F21A00", "#EBCC2A"))+
  #scale_colour_manual(values = c("black","white"))+

  scale_alpha_manual(values = c(0.95, 0.95, 0.95, 0.95, 0.95))+
  theme (plot.background = element_rect(fill = "NA", colour = "NA"), 
         axis.line = element_blank(),
         panel.grid.major = element_blank(), 
         panel.grid.minor = element_blank(), 
         axis.ticks.length = unit(0,"lines"),
         panel.background = element_rect(fill = "white", colour = "white"), 
         axis.text.x= element_text(size = 20, vjust=1,colour = "grey20"),
         axis.text.y= element_text(size = 20, colour= c("grey20")),
         axis.title.x= element_text(size = 20, colour = "grey20"),
         axis.title.y= element_text(size = 20, colour = "grey20"),
         legend.key = element_rect(fill = "grey60", colour = "grey60"))

ct + theme(legend.position="none", 
           panel.border = element_rect(fill="NA",colour = "NA", size=1, linetype="solid"),
           legend.text = element_blank())

#####################################################
sEEZ_count <- data.frame(x=EEZ_spec1$GeoName, 
                         y=EEZ_spec1$Nospecies, 
                         y1=EEZ_spec1$CR, 
                         y2=EEZ_spec1$EN, 
                         y3=EEZ_spec1$VU, 
                         y4=EEZ_spec1$rest, 
                         x1=EEZ_spec1$Territory1, 
                         Area=EEZ_spec1$Area_km2)


sEEZ_count <- EEZ_specCI_clean

#####################################################

input <- data.frame(sAreaPolygons = 'sEEZ_count',
                    tab.order=="No Species")

# select what data to visualise
if (input$sAreaPolygons == 'sEEZ_count'){
  data1 <- sEEZ_count
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




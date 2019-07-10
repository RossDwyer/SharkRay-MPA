fsocialplot <- function(fdata,fx,fy,fz,fcol){
  
  # fdata = CL2sp@data
  # fx = "Threatened"
  # fy = "OpportunityIndex"
  # fz = "ChallengeIndex"
  # fcol= c("white","red")

  # Create a convenience data.frame which can be used for charting
  plot.df <- data.frame(CL2sp@data[,'Territory'],
                        CL2sp@data[,fx],
                        CL2sp@data[,fy],
                        Class = CL2sp@data[,fz])
  
  
  # Add column names
  colnames(plot.df) <- c('Name','x','y','Class')
  
  # Do a plotly contour plot to visualize the two featres with
  # the number of malignant cases as size
  # Note the use of 'source' argument
    plot_ly(plot.df, x = ~x, y = ~y, text = ~Name,
            mode = "markers", 
            type = "scatter", color = ~Class, source = "subset",
            marker = list(size = 30)) %>%
      layout(title = paste(fx, " vs ", fy),
             xaxis = list(title = fx),
             yaxis = list(title = fy),
             dragmode =  "select",
             plot_bgcolor = "6A446F")
  })
  
  
  newdata <- fdata[1,] 
  xaxiscol <- as.integer(newdata[fx])
  yaxiscol <- as.integer(newdata[fy])
  
  
  par(mar = c(4, 4, 1, .1))  #par(font.axis = 2,font.lab = 2)  
  #dimensions 5.X8  
  
  sc <- scale_colour_gradientn(colours=fcol, limits=c(min(fdata[,fz]), max(fdata[,fz])))
  sf <- scale_fill_gradientn(colours=fcol, limits=c(min(fdata[,fz]), max(fdata[,fz])))
  
  #theme(plot.background = element_rect(fill = "NA", colour = "NA"), 
  #      axis.line = element_blank(),
  #      panel.grid.major = element_blank(), 
  #      panel.grid.minor = element_blank(), 
  #      axis.ticks.length = unit(0,"lines"),
  #      panel.background = element_rect(fill = "white", colour = "white"), 
  #      axis.text.x= element_text(size = 15, vjust=1,colour = "grey20"),
  #      axis.text.y= element_text(size = 15, colour= c("grey20")),
  #      axis.title.x= element_text(size = 15, colour = "grey20"),
  #      axis.title.y= element_text(size = 15, colour = "grey20"),
  #      legend.key = element_rect(fill = "grey60", colour = "grey60"))
  if(fx == "ChallengeIndex" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      #geom_point(data=fdata[pointborder == 1], aes(x=ChallengeIndex, y=ChallengeIndex), 
      #           colour="blue", size=5)+
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "ChallengeIndex" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "ChallengeIndex" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "ChallengeIndex" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "ChallengeIndex" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Opportunity index") 
  } 
  if(fx == "ChallengeIndex" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Opportunity index") 
  } 
  if(fx == "ChallengeIndex" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "ChallengeIndex" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "ChallengeIndex" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = CLI, 
                           colour = ChallengeIndex, fill = Threatened,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "ChallengeIndex" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "ChallengeIndex" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "ChallengeIndex" & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "ChallengeIndex" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "ChallengeIndex" & fy == "Threatened" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "ChallengeIndex" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "ChallengeIndex" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = ChallengeIndex, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Challenge index") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "OpportunityIndex" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "OpportunityIndex" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "OpportunityIndex" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "OpportunityIndex" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "OpportunityIndex" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "OpportunityIndex" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "OpportunityIndex" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "OpportunityIndex" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "OpportunityIndex" & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "OpportunityIndex" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "OpportunityIndex" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "OpportunityIndex" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = CLI, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "OpportunityIndex" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "No. threatened species") 
  }  
  if(fx == "OpportunityIndex" & fy == "Threatened" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "No. threatened species") 
  }  
  if(fx == "OpportunityIndex" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "No. threatened species") 
  }  
  if(fx == "OpportunityIndex" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = OpportunityIndex, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Opportunity index") +
      scale_y_continuous(name = "No. threatened species") 
  }  
  if(fx == "CLI" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = CLI, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "CLI" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = CLI, 
                           colour = ChallengeIndex, fill = ChallengeIndexv,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "CLI" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = CLI, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "CLI" & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }
  if(fx == "CLI" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = CLI, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "CLI" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndexv,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Challenge index") 
  }  
  if(fx == "CLI" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = CLI, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "CLI" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "CLI" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = CLI, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "CLI" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Opportunity index") 
  }    
  if(fx == "CLI" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = CLI, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "CLI" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "CLI" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = CLI, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "No. threatened species") 
  }  
  if(fx == "CLI" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "No. threatened species") 
  }  
  if(fx == "CLI" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "No. threatened species") 
  }  
  if(fx == "CLI" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = CLI, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Conservation likelihood index") +
      scale_y_continuous(name = "No. threatened species") 
  }  
  if(fx == "Threatened" & fy == "Threatened" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Threatened, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Threatened" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Threatened, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "Threatened" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Threatened, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Threatened" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Threatened, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Threatened" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Threatened, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Threatened" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Threatened, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Challenge index") 
  } 
  if(fx == "Threatened" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Threatened, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Threatened" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Threatened, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Threatened" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Threatened, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "Threatened" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Threatened, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "Threatened" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Threatened, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "Threatened" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Threatened, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "Threatened" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Threatened, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "Threatened" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Threatened, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  if(fx == "Threatened" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Threatened, y = CLI, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "Threatened" & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Threatened, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "No. threatened species") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  if(fx == "EconomicVulnerability" & fy == "Threatened" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "EconomicVulnerability" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "EconomicVulnerability" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "EconomicVulnerability" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "EconomicVulnerability" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "EconomicVulnerability" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Challenge index") 
  } 
  if(fx == "EconomicVulnerability" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "EconomicVulnerability" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Challenge index") 
  }
  
  if(fx == "EconomicVulnerability" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "EconomicVulnerability" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "EconomicVulnerability" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "EconomicVulnerability" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "EconomicVulnerability" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "EconomicVulnerability" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  if(fx == "EconomicVulnerability" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = CLI, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "EconomicVulnerability" & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  
  ##
  
  if(fx == "EconomicVulnerability" & fy == "EconomicVulnerability" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "EconomicVulnerability" & fy == "EconomicVulnerability" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  } 
  if(fx == "EconomicVulnerability" & fy == "EconomicVulnerability" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "EconomicVulnerability" & fy == "EconomicVulnerability" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  
  ###
  
  if(fx == "EconomicVulnerability" & fy == "Threatened" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Threatened, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "EconomicVulnerability" & fy == "ChallengeIndex" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = ChallengeIndex, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Challenge index") 
  }
  
  if(fx == "EconomicVulnerability" & fy == "OpportunityIndex" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = OpportunityIndex, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  
  if(fx == "EconomicVulnerability" & fy == "CLI" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = CLI, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  
  #### end of old ones
  
  if(fx == "EconomicVulnerability"  & fy == "EconomicVulnerability" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "EconomicVulnerability"  & fy == "DependMarineResource" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = DependMarineResource, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Education" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Education, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Tourism" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Tourism, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerabilityq, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "EconomicVulnerability"  & fy == "Corruption" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Corruption, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  
  if(fx == "EconomicVulnerability"  & fy == "EconomicVulnerability" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  } 
  
  if(fx == "EconomicVulnerability"  & fy == "DependMarineResource" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = DependMarineResource, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Education" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Education, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Tourism" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Tourism, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "EconomicVulnerability"  & fy == "Corruption" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Corruption, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "EconomicVulnerability" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "EconomicVulnerability"  & fy == "DependMarineResource" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = DependMarineResource, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Education" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Education, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Tourism" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Tourism, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "EconomicVulnerability"  & fy == "Corruption" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Corruption, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "EconomicVulnerability"  & fy == "EconomicVulnerability" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "EconomicVulnerability"  & fy == "DependMarineResource" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = DependMarineResource, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Dependence on  Marine Resources") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Education" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Education, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Tourism" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Tourism, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "EconomicVulnerability"  & fy == "Corruption" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Corruption, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "EconomicVulnerability"  & fy == "EconomicVulnerability" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = EconomicVulnerability, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "EconomicVulnerability"  & fy == "DependMarineResource" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = DependMarineResource, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Education" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Education, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "EconomicVulnerability"  & fy == "Tourism" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Tourism, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "EconomicVulnerability"  & fy == "Corruption" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = EconomicVulnerability, y = Corruption, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Economic vulnerability index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  ####################################
  
  if(fx == "DependMarineResource" & fy == "Threatened" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "DependMarineResource" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "DependMarineResource" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "DependMarineResource" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "DependMarineResource" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "DependMarineResource" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Challenge index") 
  } 
  if(fx == "DependMarineResource" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "DependMarineResource" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "DependMarineResource" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "DependMarineResource" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "DependMarineResource" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "DependMarineResource" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "DependMarineResource" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "DependMarineResource" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  if(fx == "DependMarineResource" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = CLI, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "DependMarineResource " & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Conservation likelihood index") }
  
  ##
  
  if(fx == "DependMarineResource" & fy == "DependMarineResource" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  if(fx == "DependMarineResource" & fy == "DependMarineResource" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  } 
  if(fx == "DependMarineResource" & fy == "DependMarineResource" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  if(fx == "DependMarineResource" & fy == "DependMarineResource" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  
  ###
  
  if(fx == "DependMarineResource" & fy == "Threatened" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Threatened, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "DependMarineResource" & fy == "ChallengeIndex" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = ChallengeIndex, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Challenge index") 
  }
  
  if(fx == "DependMarineResource" & fy == "OpportunityIndex" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = OpportunityIndex, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Opportunity index") 
  }
  
  if(fx == "DependMarineResource" & fy == "CLI" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = CLI, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  
  #### end of old ones
  
  if(fx == "DependMarineResource"  & fy == "EconomicVulnerability" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = EconomicVulnerability, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "DependMarineResource"  & fy == "DependMarineResource" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Education" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Education, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Tourism" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Tourism, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerabilityq, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "DependMarineResource"  & fy == "Corruption" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Corruption, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  
  if(fx == "DependMarineResource"  & fy == "EconomicVulnerability" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = EconomicVulnerability, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Economic vulnerability index") 
  } 
  
  if(fx == "DependMarineResource"  & fy == "DependMarineResource" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Education" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Education, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Tourism" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Tourism, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "DependMarineResource"  & fy == "Corruption" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Corruption, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  if(fx == "DependMarineResource"  & fy == "EconomicVulnerability" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = EconomicVulnerability, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "DependMarineResource"  & fy == "DependMarineResource" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Education" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Education, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Tourism" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Tourism, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "DependMarineResource"  & fy == "Corruption" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Corruption, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "DependMarineResource"  & fy == "EconomicVulnerability" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = EconomicVulnerability, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "DependMarineResource"  & fy == "DependMarineResource" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on  Marine Resources") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Education" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Education, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Tourism" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Tourism, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "DependMarineResource"  & fy == "Corruption" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Corruption, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "DependMarineResource"  & fy == "EconomicVulnerability" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = EconomicVulnerability, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "DependMarineResource"  & fy == "DependMarineResource" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = DependMarineResource, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Education" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Education, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "DependMarineResource"  & fy == "Tourism" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Tourism, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "DependMarineResource"  & fy == "Corruption" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = DependMarineResource, y = Corruption, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Dependence on Marine Resources") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  ######################################################
  
  if(fx == "Education" & fy == "Threatened" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Education, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Education" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "Education" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Education, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Education" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Education" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Education, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Education" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Challenge index") 
  } 
  if(fx == "Education" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Education, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Education" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Education" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Education, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "Education" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "Education" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Education, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "Education" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "Education" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Education, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "Education" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Education, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  if(fx == "Education" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = CLI, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "Education " & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  
  ##
  
  if(fx == "Education" & fy == "Education" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  }
  if(fx == "Education" & fy == "Education" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  } 
  if(fx == "Education" & fy == "Education" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  }
  if(fx == "Education" & fy == "Education" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  }
  
  ###
  
  if(fx == "Education" & fy == "Threatened" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = Threatened, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Education" & fy == "ChallengeIndex" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = ChallengeIndex, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Challenge index") 
  }
  
  if(fx == "Education" & fy == "OpportunityIndex" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = OpportunityIndex, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  
  if(fx == "Education" & fy == "CLI" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = CLI, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  
  #### end of old ones
  
  if(fx == "Education" & fy == "EconomicVulnerability" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Education, y = EconomicVulnerability, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Education" & fy == "DependMarineResource" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Education, y = DependMarineResource, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Education" & fy == "Education" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Education" & fy == "Tourism" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Education, y = Tourism, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerabilityq, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Education" & fy == "Corruption" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Education, y = Corruption, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  
  if(fx == "Education" & fy == "EconomicVulnerability" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Education, y = EconomicVulnerability, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  } 
  
  if(fx == "Education" & fy == "DependMarineResource" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Education, y = DependMarineResource, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Education" & fy == "Education" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Education" & fy == "Tourism" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Education, y = Tourism, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Education" & fy == "Corruption" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Education, y = Corruption, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  if(fx == "Education" & fy == "EconomicVulnerability" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = EconomicVulnerability, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Education" & fy == "DependMarineResource" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = DependMarineResource, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Education" & fy == "Education" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Education" & fy == "Tourism" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = Tourism, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Education" & fy == "Corruption" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Education, y = Corruption, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "Education" & fy == "EconomicVulnerability" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Education, y = EconomicVulnerability, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Education" & fy == "DependMarineResource" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Education, y = DependMarineResource, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Dependence on  Marine Resources") 
  }
  
  if(fx == "Education" & fy == "Education" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Education" & fy == "Tourism" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Education, y = Tourism, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Education" & fy == "Corruption" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Education, y = Corruption, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "Education" & fy == "EconomicVulnerability" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Education, y = EconomicVulnerability, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Education" & fy == "DependMarineResource" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Education, y = DependMarineResource, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Education" & fy == "Education" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Education, y = Education, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Education" & fy == "Tourism" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Education, y = Tourism, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Education" & fy == "Corruption" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Education, y = Corruption, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Education index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  ##################################################
  
  if(fx == "Tourism" & fy == "Threatened" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Tourism" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "Tourism" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Tourism" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Tourism" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Tourism, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Tourism" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Challenge index") 
  } 
  if(fx == "Tourism" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Tourism, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Tourism" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Tourism" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Tourism, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "Tourism" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "Tourism" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Tourism, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "Tourism" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "Tourism" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Tourism, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "Tourism" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Tourism, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  if(fx == "Tourism" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = CLI, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "Tourism " & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  
  ##
  
  if(fx == "Tourism" & fy == "Tourism" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  }
  if(fx == "Tourism" & fy == "Tourism" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  } 
  if(fx == "Tourism" & fy == "Tourism" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  }
  if(fx == "Tourism" & fy == "Tourism" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  }
  
  ###
  
  if(fx == "Tourism" & fy == "Threatened" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Threatened, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Tourism" & fy == "ChallengeIndex" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = ChallengeIndex, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Challenge index") 
  }
  
  if(fx == "Tourism" & fy == "OpportunityIndex" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = OpportunityIndex, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  
  if(fx == "Tourism" & fy == "CLI" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = CLI, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  
  ####
  
  #### end of old ones
  
  if(fx == "Tourism" & fy == "EconomicVulnerability" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Tourism, y = EconomicVulnerability, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Tourism" & fy == "DependMarineResource" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Tourism, y = DependMarineResource, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Tourism" & fy == "Education" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Education, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Tourism" & fy == "Tourism" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerabilityq, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Tourism" & fy == "Corruption" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Corruption, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  
  if(fx == "Tourism" & fy == "EconomicVulnerability" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Tourism, y = EconomicVulnerability, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  } 
  
  if(fx == "Tourism" & fy == "DependMarineResource" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Tourism, y = DependMarineResource, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Tourism" & fy == "Education" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Education, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Tourism" & fy == "Tourism" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Tourism" & fy == "Corruption" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Corruption, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  if(fx == "Tourism" & fy == "EconomicVulnerability" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Tourism, y = EconomicVulnerability, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Tourism" & fy == "DependMarineResource" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Tourism, y = DependMarineResource, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Tourism" & fy == "Education" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Education, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Tourism" & fy == "Tourism" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Tourism" & fy == "Corruption" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Corruption, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "Tourism" & fy == "EconomicVulnerability" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = EconomicVulnerability, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Tourism" & fy == "DependMarineResource" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = DependMarineResource, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Dependence on  Marine Resources") 
  }
  
  if(fx == "Tourism" & fy == "Education" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Education, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Tourism" & fy == "Tourism" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Tourism" & fy == "Corruption" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Corruption, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "Tourism" & fy == "EconomicVulnerability" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Tourism, y = EconomicVulnerability, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Tourism" & fy == "DependMarineResource" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Tourism, y = DependMarineResource, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Tourism" & fy == "Education" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Education, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Tourism" & fy == "Tourism" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Tourism, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Tourism" & fy == "Corruption" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Tourism, y = Corruption, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Tourism index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  ###############################################################
  
  if(fx == "Corruption" & fy == "Threatened" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Threatened, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Corruption" & fy == "Threatened" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Threatened, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "No. threatened species") 
  } 
  if(fx == "Corruption" & fy == "Threatened" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Threatened, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Corruption" & fy == "Threatened" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Threatened, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Corruption" & fy == "ChallengeIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Corruption, y = ChallengeIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Corruption" & fy == "ChallengeIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = ChallengeIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Challenge index") 
  } 
  if(fx == "Corruption" & fy == "ChallengeIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Corruption, y = ChallengeIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Corruption" & fy == "ChallengeIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = ChallengeIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Challenge index") 
  }
  if(fx == "Corruption" & fy == "OpportunityIndex" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Corruption, y = OpportunityIndex, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "Corruption" & fy == "OpportunityIndex" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = OpportunityIndex, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "Corruption" & fy == "OpportunityIndex" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Corruption, y = OpportunityIndex, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  if(fx == "Corruption" & fy == "OpportunityIndex" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = OpportunityIndex, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Opportunity index") 
  }  
  if(fx == "Corruption" & fy == "CLI" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Corruption, y = CLI, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "Corruption" & fy == "CLI" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Corruption, y = CLI, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  if(fx == "Corruption" & fy == "CLI" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = CLI, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  if(fx == "Corruption " & fy == "CLI" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = CLI, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  } 
  
  ##
  
  if(fx == "Corruption" & fy == "Corruption" & fz == "Threatened"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = Threatened, fill = Threatened,
                           size = Threatened, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  }
  if(fx == "Corruption" & fy == "Corruption" & fz == "ChallengeIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = ChallengeIndex, fill = ChallengeIndex,
                           size = ChallengeIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  } 
  if(fx == "Corruption" & fy == "Corruption" & fz == "CLI"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = CLI, fill = CLI,
                           size = CLI, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  }
  if(fx == "Corruption" & fy == "Corruption" & fz == "OpportunityIndex"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = OpportunityIndex, fill = OpportunityIndex,
                           size = OpportunityIndex, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  ###
  
  if(fx == "Corruption" & fy == "Threatened" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Threatened, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "No. threatened species") 
  }
  if(fx == "Corruption" & fy == "ChallengeIndex" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = ChallengeIndex, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Challenge index") 
  }
  
  if(fx == "Corruption" & fy == "OpportunityIndex" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = OpportunityIndex, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Opportunity index") 
  }
  
  if(fx == "Corruption" & fy == "CLI" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = CLI, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Conservation likelihood index") 
  }  
  
  #### end of old ones
  
  if(fx == "Corruption" & fy == "EconomicVulnerability" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Corruption, y = EconomicVulnerability, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Corruption" & fy == "DependMarineResource" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Corruption, y = DependMarineResource, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Corruption" & fy == "Education" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Education, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Corruption" & fy == "Tourism" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Tourism, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerabilityq, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Corruption" & fy == "Corruption" & fz == "EconomicVulnerability"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = EconomicVulnerability, fill = EconomicVulnerability,
                           size = EconomicVulnerability, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  
  if(fx == "Corruption" & fy == "EconomicVulnerability" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Corruption, y = EconomicVulnerability, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  } 
  
  if(fx == "Corruption" & fy == "DependMarineResource" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Corruption, y = DependMarineResource, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Corruption" & fy == "Education" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Education, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Corruption" & fy == "Tourism" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Tourism, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Corruption" & fy == "Corruption" & fz == "DependMarineResource"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = DependMarineResource, fill = DependMarineResource,
                           size = DependMarineResource, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  if(fx == "Corruption" & fy == "EconomicVulnerability" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Corruption, y = EconomicVulnerability, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Corruption" & fy == "DependMarineResource" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Corruption, y = DependMarineResource, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Corruption" & fy == "Education" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Education, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Corruption" & fy == "Tourism" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Tourism, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Corruption" & fy == "Corruption" & fz == "Education"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = Education, fill = Education,
                           size = Education, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "Corruption" & fy == "EconomicVulnerability" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Corruption, y = EconomicVulnerability, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Corruption" & fy == "DependMarineResource" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Corruption, y = DependMarineResource, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Dependence on  Marine Resources") 
  }
  
  if(fx == "Corruption" & fy == "Education" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Education, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Corruption" & fy == "Tourism" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Tourism, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Corruption" & fy == "Corruption" & fz == "Tourism"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = Tourism, fill = Tourism,
                           size = Tourism, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  }
  
  #####
  if(fx == "Corruption" & fy == "EconomicVulnerability" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = EconomicVulnerability, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Economic vulnerability index") 
  }
  if(fx == "Corruption" & fy == "DependMarineResource" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = DependMarineResource, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Dependence on Marine Resources") 
  }
  
  if(fx == "Corruption" & fy == "Education" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Education, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Education index") 
  }
  
  if(fx == "Corruption" & fy == "Tourism" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Tourism, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) +
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Tourism index") 
  }  
  
  if(fx == "Corruption" & fy == "Corruption" & fz == "Corruption"){
    ct <- ggplot(fdata,aes(x = Corruption, y = Corruption, 
                           colour = Corruption, fill = Corruption,
                           size = Corruption, text = Territory1)) + 
      geom_point(shape=21) + sc + sf +
      scale_x_continuous(name = "Corruption index") +
      scale_y_continuous(name = "Corruption index") 
  }
  ggplotly(ct,
           tooltip = c("size","text")) %>%
    layout(plot_bgcolor='white')#,

}

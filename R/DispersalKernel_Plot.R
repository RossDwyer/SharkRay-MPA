### Create dispersal kernel plots for SharkRayMPA project

displot<-function(data, species, tag = "passive",
                  xmax = 1500, lcol=2, ggplot=FALSE,
                  xlab="Maximum dispersal distance (km)", ylab="Probability of dispersal", ...)
{
  dat <- data[data$ScientificName %in% species & data$tag_type %in% tag,]
  if(nrow(dat) == 0){stop("No data for this species/tag type\nTry another tag type (`passive`, `satellite` or `mark_recap`)")}
  xx <- seq(0, log(xmax), length=1000)
  yfit <- dgamma(xx, shape=dat$shape, scale=dat$scale)
  yy <- yfit/max(yfit)
  if(ggplot){
    require(ggplot2)
    ggplot() +
      geom_line(aes(x= exp(xx), y= yy), col=lcol, ...) +
      scale_x_log10() + xlab(xlab) + ylab(ylab) +
      annotation_logticks(sides = "b", scaled= T) 
  }else{
    plot(xx, yy, col=lcol, lwd=2, xaxt="n", las=1, xlab=xlab, ylab=ylab, type="l", ...)
    axis(1, at= log(c(0.01, seq(0.1,1,l=10), seq(1,10,l=10),seq(10,100,l=10), seq(100,1000,l=10), seq(1000,10000,l=10))+1), labels=F, tcl=-0.3)
    axis(1, at= log(c(0.1,1,10,100,1000,10000)+1), labels=c(0.1,1,10,100,1000,10000))
  }
}


displot2 <- function(data, species,
                     xmax = 1500, 
                     xlab="Maximum dispersal distance (km)", 
                     ylab="Probability of dispersal", ...){
  
  dat <- data[data$ScientificName %in% species,]
  
  xx <- seq(0, log(xmax), length=1000)
  
  plot(x=xx,y=rep(0,1000), xaxt="n", las=1, 
       xlab=xlab, ylab=ylab, type="n",ylim=c(0,1),
       bty="l")
  axis(1, at= log(c(0.01, seq(0.1,1,l=10), 
                    seq(1,10,l=10),seq(10,100,l=10), 
                    seq(100,1000,l=10), 
                    seq(1000,10000,l=10))+1), labels=F, tcl=-0.3)
  axis(1, at= log(c(0.1,1,10,100,1000,10000)+1), 
       labels=c(0.1,1,10,100,1000,10000))
  
  legend("topright",bty="n",
         lty=1,lwd=2,
         col=c(1:3),
         legend = c("Mark-recapture",
                    "Passive acoustic",
                    "Satellite"))

  if(nrow(dat)==0){
    text(5,0.5, "No dispersal data available for this species...")
    text(5,0.4, "Please select another row")       
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

Df <- read.csv("Data/DispersalKernel_Properties.csv")

head(Df)
displot2(data = Df,                                                            
         species = "Callorhinchus mili", 
         xmax = 1500)


## maximum x axis variables to calculate the dispersal curve for (in km)   
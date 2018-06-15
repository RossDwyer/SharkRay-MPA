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


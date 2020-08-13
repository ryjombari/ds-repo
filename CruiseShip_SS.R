# CruiseShip_SS.R
#
# John Ryan, MBARI, July 2020
#
# load data for focal cruise ship analyses


lodit <- FALSE
if(lodit) {
  library(tidyverse)
  library(pracma)
  library(lubridate)
  
  # Load land mask
  load("CAcoast") 
  
  # Load 2019 AIS data file
  load("./AISdata_Screen1/AISdata_201901_201907_Screen1.RData"); c1 <- C
  load("./AISdata_Screen1/AISdata_201908_201912_Screen1.RData"); c2 <- C
  C <- rbind(c1,c2); rm(c1,c2)
  A <- C %>% group_by(PERIOD) %>% distinct_at(vars(MMSI), .keep_all = TRUE) %>% ungroup()
  rm(C)
  
  A$time <- as.POSIXct(A$PERIOD, tz = "GMT")
  A$mo = month(A$time)
  A$Speed <- as.numeric(A$SPEED_KNOTS)
  
  # Jack identified priority transits to examine
  CS <- read.csv("./MB02_CruiseShipTimesForAIS.csv", stringsAsFactors = FALSE)
  CS$stime <- as.POSIXct(CS$StartUTC, tz = "GMT"); 
  CS$etime <- as.POSIXct(CS$EndUTC, tz = "GMT")
  
  MARS <- data.frame(lat = 36.7128, lon = -122.1860)
  MB01 <- data.frame(lat = 36.798, lon = -121.976)
  MB02 <- data.frame(lat = 36.6484, lon = -121.9075)
  HARP <- data.frame(lat = 36.37021, lon = -122.314903)
}


SUMRIZE <- TRUE
if(SUMRIZE) {
  
  # File Load loop (final request is a duplicate due to no data in mini windows)
  for (i in 1:length(CS$MMSI)) {
    
    # Subset by MMSI and specified time
    As <- A[which(A$MMSI == CS$MMSI[i] & A$time >= CS$stime[i] & A$time <= CS$etime[i]),]
    nrec <- length(As$MMSI); vnam <- unique(As$NAME)
    message(paste(vnam,"(MMSI",CS$MMSI[i],") ",CS$StartUTC[i],"to",CS$EndUTC[i],":",nrec,"records"))
    
    # Expand to all data for that month
    Asm <- A[which(A$MMSI == CS$MMSI[i] & A$mo == month(CS$stime[i])),]
    nrec <- length(Asm$MMSI); vnam <- unique(As$NAME)
    message(paste("  ",vnam,"(MMSI",CS$MMSI[i],") Month ",month(CS$stime[i]),":",nrec,"records"))
  }
}


# Extract records for focal vessels and times
XTR <- FALSE
if(XTR) {
  
  # File Load loop (final request is a duplicate due to no data in mini windows)
  for (i in 1:length(CS$MMSI)) {
    
    # Subset by MMSI and time (specified or entire month)
    # As <- A[which(A$MMSI == CS$MMSI[i] & A$time >= CS$stime[i] & A$time <= CS$etime[i]),]
    As <- A[which(A$MMSI == CS$MMSI[i] & A$mo == month(CS$stime[i])),]
    nrec <- length(As$MMSI)
    message(paste(CS$Cruise.Ship[i],"(MMSI",CS$MMSI[i],") ",CS$StartUTC[i],"to",CS$EndUTC[i],":",nrec,"records"))
    
    if(i == 1) {
      Xdata <- As
    } else {
      Xdata <- bind_rows(Xdata,As)
    }
  }
  # write.csv(Xdata,file="CruiseShipData.csv")
}




# Plot maps
PLT <- FALSE
if(PLT) {
  
  library(grid)
  library(gridExtra)
  
  for (i in 1:length(CS$MMSI)) {
    
    # Subset by MMSI and specified time
    As <- A[which(A$MMSI == CS$MMSI[i] & A$time >= CS$stime[i] & A$time <= CS$etime[i]),]
    nrec <- length(As$MMSI); vnam <- unique(As$NAME)
    message(paste(vnam,"(MMSI",CS$MMSI[i],") ",CS$StartUTC[i],"to",CS$EndUTC[i],":",nrec,"records"))
    
    # Expand to all data for that month
    Asm <- A[which(A$MMSI == CS$MMSI[i] & A$mo == month(CS$stime[i])),]
    nrec <- length(Asm$MMSI); vnam <- unique(As$NAME)
    message(paste("  ",vnam,"(MMSI",CS$MMSI[i],") Month ",month(CS$stime[i]),":",nrec,"records"))
    
    XL <- c(-123.25,-120.5); YL <- c(35,38)
    
    ftime <- min(As$time); ltime <- max(As$time)
    cstime <- min(As$time); cetime <- max(As$time)
    tstr1 <- paste(CS$Cruise.Ship[i],"MMSI",CS$MMSI[i],"\n",cstime,"to",cetime)
    cstime <- min(Asm$time); cetime <- max(Asm$time)
    tstr2 <- paste(CS$Cruise.Ship[i],"MMSI",CS$MMSI[i],"\n",cstime,"to",cetime)

    # Overview map
    # Map vessels and their speeds
    p1 <- ggplot(As, aes(LON_AVG,LAT_AVG)) + 
      geom_point(aes(colour = Speed), alpha=0.8, size=.6) +
      scale_colour_gradientn(colours = hcl.colors(64)) +  #, limits = c(5,16)) +
      geom_polygon(data=LP,aes(x=lon,y=lat)) + 
      coord_map(projection="mercator", xlim=XL+c(+.15,-.15), ylim = YL+c(.15,-.15)) +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
      theme(plot.title = element_text(size=11)) +
      geom_point(data=MARS, aes(x=lon,y=lat), size = 2, shape = 1) +
      geom_point(data=HARP, aes(x=lon,y=lat), size = 2, shape = 1) +
      geom_point(data=MB01, aes(x=lon,y=lat), size = 2, shape = 1) +
      geom_point(data=MB02, aes(x=lon,y=lat), size = 2, shape = 1) +
      scale_x_continuous(breaks = waiver(), n.breaks = 3) +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle(tstr1)
    
    p2 <- ggplot(Asm, aes(LON_AVG,LAT_AVG)) + 
      geom_point(aes(colour = Speed), alpha=0.8, size=.6) +
      scale_colour_gradientn(colours = hcl.colors(64)) +  #, limits = c(5,16)) +
      geom_polygon(data=LP,aes(x=lon,y=lat)) + 
      coord_map(projection="mercator", xlim=XL+c(+.15,-.15), ylim = YL+c(.15,-.15)) +
      theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
      theme(plot.title = element_text(size=11)) +
      geom_point(data=MARS, aes(x=lon,y=lat), size = 2, shape = 1) +
      geom_point(data=HARP, aes(x=lon,y=lat), size = 2, shape = 1) +
      geom_point(data=MB01, aes(x=lon,y=lat), size = 2, shape = 1) +
      geom_point(data=MB02, aes(x=lon,y=lat), size = 2, shape = 1) +
      scale_x_continuous(breaks = waiver(), n.breaks = 3) +
      theme(plot.title = element_text(hjust = 0.5)) +
      ggtitle(tstr2)
    
    ofn <- paste0(vnam,"_2019_Month",month(CS$stime[i]),".pdf")
    ofn <- str_replace_all(ofn, fixed(" "), "")
    pdf(ofn)
    grid.arrange(p1,p2,nrow=2)
    dev.off()
  }
}



plotem <- FALSE
if(plotem) {
  # Plot
  load("AISdata_RoyalPrincess_Oct2019.RData");
  A$Speed <- as.numeric(A$SPEED_KNOTS)
  A$time <- as.POSIXct(A$PERIOD, tz = "GMT")
  load("CAcoast")
  
  MARS <- data.frame(lat = 36.7128, lon = -122.1860)
  MB01 <- data.frame(lat = 36.798, lon = -121.976)
  MB02 <- data.frame(lat = 36.6484, lon = -121.9075)
  HARP <- data.frame(lat = 36.37021, lon = -122.314903)
  
  tstr <- paste(unique(A$NAME),"\n",min(A$time),"to",max(A$time))
  
  XL <- c(-123.25,-120.5); YL <- c(35,38)
  
  # Overview map
  # Map vessels and their speeds
  p1 <- ggplot(A, aes(LON_AVG,LAT_AVG)) + 
    geom_point(aes(colour = Speed), alpha=0.8, size=.6) +
    scale_colour_gradientn(colours = hcl.colors(64), limits = c(5,16)) +
    geom_polygon(data=LP,aes(x=lon,y=lat)) + 
    coord_map(projection="mercator", xlim=XL+c(+.15,-.15), ylim = YL+c(.15,-.15)) +
    theme(axis.title.x=element_blank(),axis.title.y=element_blank()) +
    geom_point(data=MARS, aes(x=lon,y=lat), size = 2, shape = 1) +
    geom_point(data=HARP, aes(x=lon,y=lat), size = 2, shape = 1) +
    geom_point(data=MB01, aes(x=lon,y=lat), size = 2, shape = 1) +
    geom_point(data=MB02, aes(x=lon,y=lat), size = 2, shape = 1) +
    scale_x_continuous(breaks = waiver(), n.breaks = 3) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ggtitle(tstr)
}

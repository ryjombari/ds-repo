# AIS_Primary_Load.R
#
# John Ryan, MBARI, July 2020
#
# primary load of AIS data files, subsetting by:
# * variables
#   - Many are not useful for analysis.
# * location 
#   - points over land
#   - optionally, points within SF Bay (choose mask polygon file)
# * non-redundancy:
#   - Each 5-minute bin should list a vessel only once.
#
# There are (at least) 2 types of duplicate records in the csv files: 
# (1) exact duplicates from the start of one month that appear at 
#     the end of the csv from the previous month
# (2) nearly exact duplicates that appear in sequence, with just one
#     field having a slight difference
# Use of distinct(A) removes only exact duplicates.  The method used here
# removes both types.
#
# There are additional needs for screening data, but they are better 
# handled after reducing the data with this initial subsetting.

library(tidyverse)
library(pracma)

# define variables to carry forward
keeps <- c("MMSI","LAT_AVG","LON_AVG","PERIOD","SHIP_AND_CARGO_TYPE","NAME",
           "SPEED_KNOTS","DRAUGHT","DIM_BOW","DIM_STERN")

# Load land mask
load("CAcoast_sansSFB")  # This one excludes SF Bay (can't hear!).
                         # Use "CAcoast" to keep data from SF Bay.

# csv file list
flist <- list.files(path = "/Volumes/DataWell/AIS/csv/", full.names = TRUE)

# Subset file list for this example.  You can concatenate all months into a
# single output data file, or save each as a monthly file, whatever works.
# This example will concatenate two months just to demonstrate that option.
flist <- flist[8:12]; 
# Specify output file name for this period of data
ofile <- "/Users/ryjo/ds-repo/AISdata_201908_201912.RData"

# File Load loop
for (i in 1:length(flist)) {
  
  # Load a monthly csv file.
  message(c("Loading ",flist[i]))
  A <- read.csv(flist[i], stringsAsFactors = FALSE)
  nrec <- length(A$MMSI)
  
  # Subset variables.
  A <- A[keeps]; 
  
  # Remove data as specified by previously loaded coastal polygon.
  Q <- inpolygon(A$LON_AVG,A$LAT_AVG,LPs$lon,LPs$lat)
  A <- A[!Q,]; nrec2 <- length(A$MMSI)
  pr <- round(10000*(nrec-nrec2)/nrec)/100
  message(paste("  Removed",pr,"% of records using land+ polygon mask."))
  
  # Remove records that are redundant within 5-minute blocks, based on MMSI.
  A <- A %>% group_by(PERIOD) %>% distinct_at(vars(MMSI), .keep_all = TRUE) %>% ungroup()
  nrec3 <- length(A$MMSI)
  pr <- round(10000*(nrec2-nrec3)/nrec2)/100
  message(paste("  Removed additional",pr,"% of records that were redundant."))
  
  # You can save the individual months as R data files, or concatenate.
  # If individual files, construct output file name, perhaps from input file 
  # name, then follow the format below for saving.
  #
  # This example concatenates, then saves after the monthly loop completes.
  if(i == 1) {C <- A} else {C <- rbind(C,A)}
}

save(C,file=ofile)
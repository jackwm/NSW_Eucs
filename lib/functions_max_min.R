# These scripts are related to the finding of the maximum and minimum value for each day.

# Calculates the max and min of a tree in a given time frame, we use it in max min per day
# Could use this to find the max and min between 2 hours, 2 months, 2 days, 40 seconds etc...
require('lubridate')

MaxMinTreeDaily <- function(dataframe,tree){
  # Find first instance of midnight
  start <- ceiling_date(dataframe$TIMESTAMP[1],'day')
  # Find last instance of midnight
  end <- floor_date(dataframe$TIMESTAMP[nrow(dataframe)],'day')
  # Calculate the difference in days
  days <- as.numeric(difftime(end,start,units="days"))
  day <- 24*3600 - 10; end <- start + day
  part_day <- 24*3600; # This represents the hours from one minima, to then find the next maxima.
  int.max <- GetTimeIntTree(dataframe,tree,start,end)
  int.min <- GetTimeIntTree(dataframe,tree,start,end)
  max <- max(int.max[,tree]); tmax <- int.max[which.max(int.max[,tree]),"TIMESTAMP"]
  min <- min(int.min[,tree]); tmin <- int.min[which.min(int.min[,tree]),"TIMESTAMP"]
  # Here I set the column values once to stop time values coercion into numerics
  max.col <- c(max); tmax.col <- c(tmax); min.col <- c(min); tmin.col <- c(tmin)
  start <- end; end <- start + day
  int.max <- GetTimeIntTree(dataframe,tree,tmin,tmin+part_day)
  int.min <- GetTimeIntTree(dataframe,tree,start,end)
  for(n in 2:days){
    min <- min(int.min[,tree]);
    tmin <- int.min[which.min(int.min[,tree]),"TIMESTAMP"]
    max <- max(int.max[,tree]); tmax <- int.max[which.max(int.max[,tree]),"TIMESTAMP"]
    max.col <- c(max.col,max); tmax.col <- c(tmax.col,tmax)
    min.col <- c(min.col,min); tmin.col <- c(tmin.col,tmin)
    start <- end; end <- start + day
    int.max <- GetTimeIntTree(dataframe,tree,tmin,tmin+part_day)
    int.min <- GetTimeIntTree(dataframe,tree,start,end)
  }
  result <- data.frame(max.col,tmax.col,
                       min.col,tmin.col,
                       stringsAsFactors=FALSE)
  names(result) <- c(tree.names[tree,"max"],tree.names[tree,"tmax"],
                     tree.names[tree,"min"],tree.names[tree,"tmin"])
  print(paste(tree,": Max and min info found for: ",days," days"))
  return(result)
}
#a <- MaxMinTreeDaily(temp.osc,tree,start,days)
# 
# test <- MaxMinTreeDaily(Sap.All,trees[[1]],"2012-04-20",25)
# test

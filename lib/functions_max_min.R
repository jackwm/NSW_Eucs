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
  part_day <- 24*3600 - 10; # This represents the hours from one minima, to then find the next maxima.
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

MaxMin.SMA <- function(DF,tree,...){
  args <- list(...)
  if(!is.null(args$trace)){print(match.call()[[1]])}
  # Working around non-unique data
  DF <- unique(DF)
  #Finding first day and how many full days there are
  DF <- data.frame(DF[,"TIMESTAMP"],DF[,tree],stringsAsFactors=FALSE)
  DF <- na.trim(DF); names(DF)<-c("TIMESTAMP",tree)
  #Creating a vector containing the oscillations
  rename <- c("TIMESTAMP",tree)
  osc <- Detrend(DF,tree,6*24,...) #Here we assume we want to average over 6*24 10 min measurments (A day)
  tree.times <- DF$TIMESTAMP; tree.vals <- DF[,tree]; temp.osc <- data.frame(tree.times,osc)
  names(temp.osc) <- rename
  temp.orig <- data.frame("TIMESTAMP"=tree.times,tree=tree.vals)
  names(temp.orig) <- rename
  mm.osc <- MaxMinTreeDaily(temp.osc,tree)
  # Merging:
  x <- mm.osc[ ,2]
  x <- data.frame(x)
  colnames(x) <- 'TIMESTAMP'
  mm.osc.max <- merge(x, temp.orig,
                      by = "TIMESTAMP",
                      all.x = TRUE, all.y = FALSE)
  
  y <- mm.osc[ ,4]
  y <- data.frame(y)
  colnames(y) <- 'TIMESTAMP'
  mm.osc.min <- merge(y, temp.orig,
                      by = "TIMESTAMP",
                      all = FALSE)
  
  # If you want Plotting
  if(!is.null(args$with.plot) && args$with.plot=='SMA'){
    plot(temp.orig,type='l',
         xlab="Date",ylab="Measurment (mm)",
         main=tree)
    points(mm.osc.max[,1],mm.osc.max[,2],col='red',pch=2)
    points(mm.osc.min[,1],mm.osc.min[,2],col='red',pch=1)
  }
  
  min.row <- min(nrow(mm.osc.max),nrow(mm.osc.min))
  result <- data.frame(mm.osc.max[1:min.row,],mm.osc.min[1:min.row,])
  names(result) <- c(tree.names[tree,"tmax"],tree.names[tree,"max"],
                     tree.names[tree,"tmin"],tree.names[tree,"min"])
  return(result)
}
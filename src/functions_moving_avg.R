# LOADED - Moving average functions

# This script provides a function to analyise the moving average of the
# data and return it as a dataframe of timestamp and moving average

# loading data
# source.with.encoding('~/Dropbox/phd/r_scripts/loading00.R', encoding='UTF-8') #

require('TTR')
require('lubridate')
# You must give that function a data frame and tree (as a string) with no NA's within it.
# plot should be given as either TRUE or FALSE and plots the moving average with the original values
# n is the interval you are to average over, a reasonable number is 6*24 which is a day
ZLSMA <- function(DF,tree,n,with.plot,...){
  args <- list(...)
  if(!is.null(args$trace)){print(match.call()[[1]])}
  if(n%%2==0) n<-n+1
  y <- DF[,tree]; lag <- (n-1)/2; a <- SMA(y,n)
  a <- na.trim(a); steps <- c(1:lag)
  # This 'steps' is a dummy vector to be added onto the...
  # ...start and end of the SMA vector, as it loses some entries
  
  # Fitting a linear model to the start of the moving average
  start.fit <- lm(a[1:lag]~steps); start.int <- start.fit$coefficients[[1]]
  start.slope <- start.fit$coefficients[[2]]; start.vec <- start.int + start.slope*steps - (lag-1)*start.slope
  #fitting a linear model to the end of the moving average
  end.fit <- lm(a[(length(a)-lag+1):length(a)] ~ steps); end.int <- end.fit$coefficients[[1]]
  end.slope <- end.fit$coefficients[[2]]; end.vec <- end.int + end.slope*steps + (lag-1)*end.slope
  
  # This model adds a linea fit to the start and end of the moving average vector to replace the NA's
  # NAs introduced at the start and end by SMA, only half a day (or n/2)
  # adding the new linear fits to the original Simple moving average vector
  a <- c(start.vec,a,end.vec)

  if(!is.null(args$with.plot) && args$with.plot=='ZLSMA'){
    plot(DF$TIMESTAMP,y,type='l',col='red',
         xlab="TIMESTAMP",
         ylab=c(tree,"measurment (mm)"),
         main=c(tree," values compared with moving average"))
    lines(DF$TIMESTAMP,a,type='l')
  }
  return(a)
}

# test <- Sap.All[6000:7000,]
# b <- ZLSMA(test,trees[[1]],6*24,TRUE)

ZLSMA.approx.na <- function(DF,tree,n,gap,...){
  args <- list(...)
  if(!is.null(args$trace)){print(match.call()[[1]])}
  DF <- data.frame(DF$TIMESTAMP,DF[,tree])
  names(DF) <- c("TIMESTAMP",tree)
  DF <- na.trim(DF)
  y <- na.approx(DF[,tree],maxgap=gap) # This is linear interpolation of NA's
  new.df <- data.frame(DF$TIMESTAMP,y,stringsAsFactors=FALSE)
  names(new.df) <- c("TIMESTAMP",tree)
  result <- ZLSMA(new.df,tree,n,...)
  return(result)
}

# Sap.All[6785,trees[[1]]]<-(Sap.All[6784,trees[[1]]]+Sap.All[6786,trees[[1]]])/2
#b <- ZLSMA.approx.na(DF=(Sap.All),tree=trees[[2]],n=6*24,gap=Inf,with.plot=TRUE)

# Basically all that function does is minus the moving avg from the measured value
#DF <- Sap.All; tree <- trees[[5]]
Detrend <- function(DF,tree,n,...){
  args <- list(...)
  if(!is.null(args$trace)){print(match.call()[[1]])}
  DF <- data.frame(DF[,"TIMESTAMP"],DF[,tree],stringsAsFactors=FALSE)
  names(DF)<-c("TIMESTAMP",tree)
  DF <- na.trim(DF)
  mv.avg <- ZLSMA.approx.na(DF,tree,n,gap=Inf,with.plot=FALSE,...)
  tree.vals <- DF[,tree]
  tree.times <- DF$TIMESTAMP
  osc <- tree.vals - mv.avg
  # If you want a plot of the oscillation
  if(!is.null(args$with.plot) && args$with.plot=='Detrend'){
    plot(tree.times,osc,type='l',
         xlab="TIMESTAMP",
         ylab=c(tree,"Daily Osciallation (mm)"),
         main=c("Oscialltions of",tree))
  }
  return(osc)
}

#t <- Detrend(Sap.All,trees[[5]],6*24,with.plot=TRUE)

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
  mm.osc <- MaxMinTreeDaily(temp.osc,tree,...)
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
  
  result <- data.frame(mm.osc.max,mm.osc.min)
  names(result) <- c(tree.names[tree,"tmax"],tree.names[tree,"max"],
                     tree.names[tree,"tmin"],tree.names[tree,"min"])
  return(result)
}

# Supposedly a clean section of data
#test <- GetTimeInt(Sap.All,"2012-04-6 00:00:00","2012-04-13 00:00:00")
#h <- MaxMin.SMA(Sap.All,trees[[1]],with.plot=TRUE)

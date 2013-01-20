require('lubridate')

CalcSEI <- function(maxmin,tree){
  
  tmin <- tree.names[tree,"tmin"]; tmax <- tree.names[tree,"tmax"]
  min <- tree.names[tree,"min"]; max <- tree.names[tree,"max"]
  tomorrows.max <- c(maxmin[2:nrow(maxmin),max],NA)
  tomorrows.tmax <- c(maxmin[2:nrow(maxmin),tmax],NA)
  
  
  shr.val <- maxmin[,min] - maxmin[,max]
  shr.dur <- difftime( maxmin[,tmin], maxmin[,tmax], units='secs')
  exp.val <- tomorrows.max - maxmin[,min]
  exp.dur <- difftime( tomorrows.tmax, maxmin[,tmin], units='secs')
  inc.val <- tomorrows.max - maxmin[,max]
  
  start.t <- floor_date(maxmin[,tmin],'day')
  
  SEI <- data.frame(start.t,
                    shr.val,
                    shr.dur,
                    exp.val,
                    exp.dur,
                    inc.val,
                    "inc.dur"=NA)
  return(SEI)
}

# DF <- Sap.All

CalcSEIdf <- function(DF){
  SEI.list <- trees
  names(SEI.list) <- trees
  for (i in trees) SEI.list[[i]] <- NA
  
  for (t in trees) {
    mmpd <- MaxMin.SMA(DF,t,with.plot=FALSE,compare=FALSE)
    SEI.list[[t]] <- CalcSEI(mmpd, t)
  }
  
  return(SEI.list)
}

GardenSEI <- function(SEI.list){
  Gardened.list <- SEI.list
  n <- 0
  for (t in Gardened.list){
    n <- n + 1
    
    i <- t
    x <- i[,"shr.dur"]
    x[x<0] <- NA # dropping NEGATIVE values for shrinkage duration
    i[,"shr.dur"] <- x
    
    x <- i[ , 'shr.val']
    x[x>0] <- NA # dropping POSITIVE values for shrinkage value
    i[,'shr.val'] <- x
        
    x <- i[ , 'exp.val']
    x[x<0] <- NA # dropping NEGATIVE values for EXPANSION value
    i[,'exp.val'] <- x
    
    x <- i[ , 'exp.dur']
    x[x<0] <- NA # dropping NEGATIVE values for EXPANSION DURATION
    i[,'exp.dur'] <- x
    
    x <- i[ , "inc.val"]
    x[x<0] <- NA # dropping NEGATIVE values for increment value
    i[ , 'inc.val'] <- x
    
    Gardened.list[[names(Gardened.list)[n]]]<-i
    }
  return(Gardened.list)
}


#usage example
# SEI.list.gdnd <- GardenSEI(SEI.list)

#test <- GetTimeInt(Sap.All,"2012-04-29 00:00:00","2012-05-22 00:00:00")
#SEI <- CalcSEIdf(Sap.All)

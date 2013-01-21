
#Sap <- Sap.All
#Sap[,1] <- as.POSIXct(Sap[,1])
#rownames(Sap) <- Sap$TIMESTAMP
#Sap[1,1]

#test <- Sap[Sap$TIMESTAMP<as.POSIXct("2012-02-05 09:50:00") & Sap$TIMESTAMP>as.POSIXct("2012-02-05 09:00:00"),]

ChooseData <- function() {
  data <- c(dir('data/'))
  cnt <- 0
  for (d in data){
    cnt <- cnt + 1
    print(paste(cnt,d,sep=' -- ')) 
  }
  s <- readline('Enter corresponding number for desired data')
  print(data[as.numeric(s)])
}

GetTrees <- function(dataframe) {
  cols <- colnames(dataframe)
  trees <- list()
  for (i in 1:length(cols)) {
    if (length(grep("Dendro",cols[i])==1)) trees <- c(trees,cols[i])}
  return(trees)
}

#a <- GetTrees(Sap)

TreeInfo <- function(dataframe){
  trees <- unlist(GetTrees(dataframe))
  result <- data.frame("name"=trees,
                  "max"=sapply(trees,paste,"max",sep="."),
                  "tmax"=sapply(trees,paste,"tmax",sep="."),
                  "min"=sapply(trees,paste,"min",sep="."),
                  "tmin"=sapply(trees,paste,"tmin",sep="."),
                  stringsAsFactors=FALSE)
  return(result)
}

#tree.names <- TreeInfo(Sap.All)

# FlagTree <- function(df,tree,start,end){
#   start <- as.POSIXct(start)
#   end <- as.POSIXct(end)
#   sub <- GetTimeInt(df,start,end)
#   
#   if (tmax after midday){}
#   if (tmin before midday){}
#   if (tmin<tmax){}
#   
#   return(flag-df)
# }

# Calculates the max and min of a tree in a given time frame, we use it in max min per day
# Could use that to find the max and min between 2 hours, 2 months, 2 days, 40 seconds etc...
MaxMinTree <- function(dataframe,tree,start,end){
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  sub <- GetTimeInt(dataframe,start,end)
  col_num_tree <- grep(tree,colnames(dataframe))
  biggest <- 0
  time_of_max <- NA
  res <- list()
  for(i in 1:nrow(sub)){
    if (!is.na(sub[i,col_num_tree])){
      if(sub[i,col_num_tree]>biggest) {
        t <- sub[i,1]
        #if (as.numeric(strftime(t,"%H"))<12){
        time_of_max <- t
        biggest <- sub[i,col_num_tree]}}}#}
  res[[1]] <- biggest
  res[[2]] <- time_of_max
  min <- biggest
  time_of_min <- NA
  for(i in 1:nrow(sub)){
    if(!is.na(sub[i,col_num_tree])){
      if(sub[i,col_num_tree]<min) {
        min <- sub[i,col_num_tree]
        time_of_min <- sub[i,1]}}}
  res[[3]] <- min
  res[[4]] <- time_of_min
  names(res) <- c(paste(tree,".max",sep=""),paste(tree,".tmax",sep=""),paste(tree,".min",sep=""),paste(tree,".tmin",sep=""))
  return(res)
}
# Output format: list(max,tmax,min,tmin)

# Here I've joined the find max and find min functions into a single function
# Period where a max is 'allowed' is only up until midday 12:00:00
#MaxMinTree(Sap.All,"Dendro_Avg.1.Sap03A","2012-02-04 09:40:00","2012-02-05 09:40:00")

# Finds the max and min of all trees in a time period, uses MaxMinTree per tree
MaxMinAllTrees <- function(dataframe,start,end) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  trees <- GetTrees(dataframe)
  result <- list(start.t=start,int.end=end)
  for(i in 1:length(trees)){
    max_min_info <- MaxMinTree(dataframe,trees[i],start,end) #Here we call MaxMinTree and give it the same start and end
    result <- c(result,max_min_info)
  }
  return(result)
}
#output format: list(start,end,Dendro1.max,Dendro1.tmax,Dendro1.min,Dendro1.tmin,Dendro2.max,Dendro2.tmax,...)

#MaxMinAllTrees(Sap.All,"2012-02-05 00:00:00","2012-02-06 00:00:00")
#View(test)

# Using MaxMinAllTrees which uses MaxMinTree, it finds the max and min of all trees for each day for 'n' days
MaxMinPerDay <- function(dataframe,start,days){
  start <- as.POSIXct(start)
  data <- data.frame(stringsAsFactors=FALSE)
  for (i in 1:days){
    day.info <- data.frame(MaxMinAllTrees(dataframe,start,start+(3600*24)),stringsAsFactors=FALSE)
    data <- rbind(data,day.info)
    start <- start + (3600*24)
    if(i%%10==0) print(c(i,"days"))
  }
  return(data)
}
#output format: data.frame(Sunday,Monday,Dendro1.max,Dendro2.tmax,Dendro2.min,Dendro2.tmin,....Dendro6.min,Dendro6.tmin,
#                          Monday,Tuesday,....)

#create timeseries ts() object, setting the season equal to a day
#adj allows adjusting of the 'season'.
DecomposeData <- function(df,tree,start,end,adj){
  sub <- GetTimeInt(df, start, end)
  #sub.na <- na.omit(sub)
  index <- grep(tree,colnames(df))
  sub.tr <- sub[,index]
  #sub.ts <- ts(na.omit(sub.tr), start = 1, frequency = round(adj*24*6))
  sub.ts <- ts(sub.tr, start = 1, frequency = round(adj*24*6))
  sub.dc <- decompose(sub.ts)
  return(sub.dc)
}

#dec <- DecomposeData(Sap.All,trees.All[[1]],'2012-02-05',as.POSIXct('2012-02-05')+3600*24*110,1)

# pull out the POSIXct timestamps from our chosen series
PullTimes <- function(df, start, end){
  sub <- GetTimeInt(df, start, end)
  #sub.na <- na.omit(sub)
  times <- sub$TIMESTAMP
  return(times)
}

#times <- PullTimes(Sap.All,'2012-02-05',as.POSIXct('2012-02-05')+3600*24*110)

# return the trend componenent of the timeseries, with the original POSIXct timestamps 
MovingAvg <- function(df,tree,start,end,adj){
  times <- PullTimes(df, start, end)
  decomp <- DecomposeData(df,tree,start,end,adj)
  Moving.Avg <- data.frame(times,decomp["trend"])
  return(Moving.Avg)
}

# return the observations minus trend, with original POSIXct timestamps. 
Oscillations <- function(df,tree,start,end,adj){
  times <- PullTimes(df, start, end)
  decomp <- DecomposeData(df,tree,start,end,adj)
  seasonal <- decomp$seasonal
  random <- decomp$random
  osc <- seasonal + random
  #osc <- data.frame(times, osc)
  return(osc)
}

#o <- Oscillations(Sap.All,trees.All[[1]],'2012-02-05',as.POSIXct('2012-02-05')+3600*24*110,1)

# Finds jumps in the data for a given sensitivity = 'sens', for the entire dataset given for a single tree.
FlagJumps <- function(df,tree,sens){
  tree.col.num <- grep(tree,colnames(df))
  prev <- df[1,tree.col.num]
  bad.data <- list()
  for (i in 1:length(df[,tree.col.num])){
    cur <- df[i,tree.col.num]
    if (is.na(cur+prev)) {prev<-cur}
    else {
      if (abs(cur-prev)>sens) {
        bad.data <- c(bad.data,list(df[i,1]))
        prev <-cur    
      }
    }
  }
  return(bad.data)
}



# Output format: list(time1,time2,...)

#test <- MaxMinPerDay(Sap,"2012-02-07 00:00:00",10)

# Not yet functional
# GenHistPdfPng <- function(data,bins,file.name,title,x,y){
#   pdf(paste(file.name,"pdf",sep="."))
#   hist(data, breaks = bins,
#        xlab = x,
#        ylab = y,
#        main = paste("Histogram of", title))
#   dev.off()
#   
#   png(paste(file.name,"png",sep="."),
#       res = 300,
#       width = 2400,
#       height = 2400)
#   hist(data, breaks = bins,
#        xlab = x,
#        ylab = y,
#        main = paste("Histogram of", title))
#   dev.off()
# }

# the following two conversion functions were copied from 'read dendrometer data - site generic.R'
# check that the first column is column 3 not 4??

millivolts_to_mm <- function(data.frame) {
  for (i in 3:ncol(data.frame)){
    data.frame[ ,i] <- data.frame[ ,i]*4*10^(-3)
  }
  return(data.frame)
}

microns_to_mm <- function(data.frame) {
  for (i in 3:ncol(data.frame)){
    data.frame[, i] <- data.frame[ ,i]*10^(-3)
  }
  return(data.frame)
}

agg_by_day <- function(data.frame){
  trees <- get_trees(data.frame)
  data.frame <- timestamp_to_date(data.frame)
  mins <- aggregate(data.frame[,c(trees)], list(data.frame$Date), min)
  maxs <- aggregate(data.frame[,c(trees)], list(data.frame$Date), max)
  mins.maxs <- merge(maxs, mins, by = "Group.1", all = TRUE, suffixes = c(".min", ".max"))
  return(mins.maxs)
}

# calculates a naive slope between two points, assumes a linear relation.
CalcSlope <- function(DF,start,end){
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  val1 <- DF[DF$TIMESTAMP==start,"Water_level_m"]
  val2 <- DF[DF$TIMESTAMP==end,"Water_level_m"]
  delta <- as.numeric(end) - as.numeric(start)
  slope <- (val2 - val1) / delta
  return(slope)
}
# output is a numeric

# Calculates the s value, assuming height of slope is 4 hours from start
Calc.s <- function(DF,start){
  start <- as.POSIXct(start)
  slope <- CalcSlope(DF,start,start+4*3600) # 4 hours, usually from midnight...
  val1 <- DF[DF$TIMESTAMP==start,"Water_level_m"]
  val2 <- DF[DF$TIMESTAMP==(start+24*3600),"Water_level_m"]
  r24 <- 24*3600*slope
  projected.val <- val1 + r24
  s <- projected.val - r24 - val2
  return(list(s, r24))
}
# output is a list

#Calc.s(gw,"2012/6/10 00:00:00")
#CalcSlope(gw,"2012/5/26 00:00:00","2012/5/26 04:00:00")

CalcET <- function(DF,start, Sy){
  start <- as.POSIXct(start)
  out <- Calc.s(DF,start)
  s <- out[[1]]
  r24 <- out[[2]]
  ET <- Sy * (r24 + s)
  return(list("TIMESTAMP"=start,"ET"=ET))
}

#CalcET(gw,"2012/6/10 00:00:00", 0.2)

#Finds the first instance of a given time, ie finds first time it is 09:40:00 in a data frame and returns the date time
FirstInstance <- function(DF, Time){
  z <- DF[strftime(DF$TIMESTAMP, format = "%H:%M:%S") == Time, ]  
  h <- as.POSIXct(min(z[,"TIMESTAMP"]))
  return("Date"=h)
}

#FirstInstance(gw,"20:40:00")

CalcETAll <- function(DF){
  date <- as.POSIXct(FirstInstance(DF, "00:00:00"))
  LastDate <- as.POSIXct(DF[nrow(DF),"TIMESTAMP"]) - 24*3600
  output <- data.frame()
  while (date<LastDate){
    newrow <- data.frame(CalcET(DF,date, 0.2),stringsAsFactors=FALSE)
    print(newrow)
    output <- rbind(output,newrow)
    date <- date+24*3600
  }
  return(output)
}

#a <- CalcETAll(gw)

timestamp_to_date <- function(data.frame){
  x <- data.frame[ ,1]
  x <- strptime(x, format = "%Y-%m-%d %H:%M:%S")
  y <- format(x, format = "%Y-%m-%d")
  data.frame$Date <- y
  return(data.frame)
}

# usage examples 
#dendro.voltages.xts <- timestamp_to_date(dendro.voltages.xts)
#Sap03A <- timestamp_to_date(Sap03A)
# 
# CalcSEI <- function(maxmin,tree){
#   SEI <- data.frame(maxmin[,1:2],shr.val=NA,shr.dur=NA,
#                     exp.val=NA,exp.dur=NA,
#                     inc.val=NA,inc.dur=NA)
#   name <- names(maxmin)
#   tree.min <- grep(paste(tree,"min",sep="."),name)
#   tree.tmin <- grep(paste(tree,"tmin",sep="."),name)
#   tree.max <- grep(paste(tree,"max",sep="."),name)
#   tree.tmax <- grep(paste(tree,"tmax",sep="."),name)
#   for (i in 2:(nrow(maxmin)-1)){
#     shr.val <- maxmin[i,tree.min] - maxmin[i,tree.max]
#     shr.dur <- as.numeric(maxmin[i,tree.tmin]) - as.numeric(maxmin[i,tree.tmax])
#     exp.val <- maxmin[i+1,tree.max] - maxmin[i,tree.min]
#     exp.dur <- as.numeric(maxmin[i+1,tree.tmax]) - as.numeric(maxmin[i,tree.tmin])
#     inc.val <- maxmin[i+1,tree.max] - maxmin[i,tree.max]
#     SEI[i,3] <- shr.val
#     SEI[i,4] <- shr.dur
#     SEI[i,5] <- exp.val
#     SEI[i,6] <- exp.dur
#     SEI[i,7] <- inc.val
#   }
#   return(SEI)
# }


# find missing rows before input to ts()
  MissingRowsPrint <- function(DF){
    DF[ ,1] <- as.POSIXct(DF[ ,1])
    for (i in 1:(nrow(DF)-1)){
      #print(names(DF)[4:length(names(DF))])
      q <- as.numeric(DF[i, 1])
      w <- as.numeric(DF[(i+1), 1])
      delta <-  w - q
      #print(delta)
      if (is.na(delta)) {print(paste('NA found at', i)); delta <- 10}
      if (delta != 10*60) {print(paste('Diff not 10 at', i))}
    }
  }
  # example usage
  #   sap.obj <- ls(pattern = 'Sap')
  #   sap.obj <- sap.obj[2:4]
  #   for (j in sap.obj) {MissingRowsPrint(j)}
  #MissingRowsPrint(Sap10A)
  #MissingRowsPrint(Sap03A)
  #MissingRowsPrint(Sap03F)
  #MissingRowsPrint(Sap.All)
# Functions relating to Date times - LOADED

FullDays <- function(DF){
  start <- as.numeric(DF[1,"TIMESTAMP"])
  end <- as.numeric(DF[nrow(DF),"TIMESTAMP"])
  return(floor((end-start)/(60*60*24)))
}

GetDailyInt <- function(DF,start,days){
  start <- as.POSIXct(start)
  start.t <- c()
  int.end <- c()
  result <- data.frame(matrix(nrow=days,ncol=2),stringsAsFactors=FALSE)
  for(i in c(1:days)){
    start.t <- c(start.t,as.character(start))
    end <- start + 3600*24
    int.end <- c(int.end,as.character(end))
    start <- end
  }
  result[,1] <- data.frame(start.t,stringsAsFactors=FALSE)
  result[,1] <- as.POSIXct(result[,1])
  result[,2] <- data.frame(int.end,stringsAsFactors=FALSE)
  result[,2] <- as.POSIXct(result[,2])
  names(result) <- c("start.t","int.end")
  return(result)
}

GetTimeInt <- function(df,start,end) {
  start <- as.POSIXct(start)
  end <- as.POSIXct(end)
  sub <- df[df$TIMESTAMP>=start & df$TIMESTAMP<=end,]
  #rownames(sub) <- sub$TIMESTAMP
  return(sub)
}
#test <- GetTimeInt(Sap.All,"2012-02-05 09:40:00","2012-02-05 12:40:00")

GetTimeIntTree <- function(dataframe,tree,start,end){
  sub <- GetTimeInt(dataframe,start,end)
  sub <- data.frame(sub$TIMESTAMP,sub[,tree])
  names(sub) <- c("TIMESTAMP",tree)
  return(sub)
}

# test <- GetTimeIntTree(Sap.All,trees[[1]],"2012-04-21","2012-04-21 00:50:00")
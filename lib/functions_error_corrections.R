SpikeCorrection <- function(df,col.name,time,n){
  result <- df
  sub <- subset(df,df$TIMESTAMP>=time-10*60*n)
  sub <- subset(sub,sub$TIMESTAMP<=time+10*60*n)
  avg <- sum(c(sub[1:n,tree],sub[(n+2):(n*2+1),col.name]))/(2*n)
  result[result$TIMESTAMP==time,col.name] <- avg
  return(result)
}

DisplayErrors <- function(errors){
  for(row in c(1:nrow(errors))){
    time <- errors[row,'TIMESTAMP']
    df.name <- errors[row,'dataframe']
    day <- floor_date(time,unit='day')
    tree <- errors[row,'tree']
    type <- errors[row,'error.type']
    fixed <- errors[row,'error.fixed']
    error <- get(df.name)
    error <- error[error$TIMESTAMP > (time - 12*10*60), ]
    error <- error[error$TIMESTAMP < (time + 12*10*60), ]
    plot(x = error$TIMESTAMP, xlab=paste(day,strftime(time,format='%H:%M:%S'),paste('fixed:',fixed),sep=' |--| '),
         y = error[,tree], ylab='Measurement',
         main=paste(df.name,tree,type,sep=' |--| '))
  }
}

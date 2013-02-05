SpikeCorrection <- function(df,tree,time,n){
  result <- df
  sub <- subset(df,df$TIMESTAMP>=time-10*60*n)
  sub <- subset(sub,sub$TIMESTAMP<=time+10*60*n)
  avg <- sum(c(sub[1:n,tree],sub[(n+2):(n*2+1),tree]))/(2*n)
  result[result$TIMESTAMP==time,tree] <- avg
  return(result)
}

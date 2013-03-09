# script to fill 'missing' rows from logger data
# edit01: 25-07-2012
#
# how it works
# 1 create synthetic time series from start to end with X-minute obs. 
# 2 use merge to combine the synethic timeseries with the true data


# 1 create synthetic time series from start to end with X-minute obs.

MergeSynth <- function(DF, obs.int) {
  synth.ts <- SynthTS(DF, obs.int)
  synth.ts <- as.data.frame.AsIs(synth.ts)
  names(synth.ts) <- 'TIMESTAMP'
  head(synth.ts)
  
  x <- DF
  y <- synth.ts
  output <- merge(x, y,
                  by = 'TIMESTAMP',
                  all.y = TRUE)
}

SynthTS <- function(data, obs.int){
 # locating the start and end of the input dataset
  ss <- data[1, 'TIMESTAMP'] # row, column 
  n <- nrow(data)
  se <- data[n, 'TIMESTAMP']

  ss <- as.POSIXct(ss)
  se <- as.POSIXct(se)
  
  if (ss > se)
    stop('Error: start date is after end date')
  
  synth <- seq(from = ss,
               to = se, 
               by = obs.int)
              
}

ZeroOrigin <- function(DF){
  
  tree.list <- GetTrees(DF)
  out <- DF 
  for (t in tree.list){
    z <- out[1, t] 
    out[ , t] <- out[ , t] - z
    }
  return(out)
}


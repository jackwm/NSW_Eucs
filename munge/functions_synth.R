# script to fill 'missing' rows from logger data
# edit01: 25-07-2012
#
# how it works
# 1 create synthetic time series from start to end with X-minute obs. 
# 2 use merge to combine the synethic timeseries with the true data


# 1 create synthetic time series from start to end with X-minute obs.

SynthTS <- function(data, obs.int){
 # locating the start and end of the input dataset
 ss <- data[1, 'TIMESTAMP'] # row, column 
 n <- nrow(data)
 se <- data[n, 'TIMESTAMP']

 
 # creating a series. 
 #ss <- "2012-06-03 23:10:00"
 #se <- "2012-07-17 12:30:00"
 
 ss <- as.POSIXct(ss)
 se <- as.POSIXct(se)
 
 ## S3 method for class 'POSIXt'
 #seq(from, to, by, length.out = NULL, along.with = NULL, ...)
 
 synth <- seq(from = ss,
              to = se,
              # by = 10*60 # for ten minute observations
              by = obs.int, # obs.int should be time in seconds
              #length.out = NULL,
              #along.with = NULL
              )
 
}


  ## TESTING THE FUNCTION
  
    #   # LOADING DATA
    #   source.with.encoding('~/Dropbox/phd/r_scripts/loading00.R', encoding='UTF-8')
    #   
    #   # testing with site 3A
    #   synth.ts <- SynthTS(data = Sap03A,
    #             obs.int = 10*60)
    #   head(Sap03A); head(synth.ts)
    #   tail(Sap03A); tail(synth.ts)
    #   nrow(Sap03A); length(synth.ts)
    #   
    #   # testing with site 3F
    #   synth.ts <- SynthTS(data = Sap03F,
    #                       obs.int = 10*60)
    #   head(Sap03F); head(synth.ts)
    #   tail(Sap03F); tail(synth.ts)
    #   nrow(Sap03F); length(synth.ts)
    #   
    #   # testing with site 10A
    #   synth.ts <- SynthTS(data = Sap10A,
    #                       obs.int = 10*60)
    #   head(Sap10A); head(synth.ts)
    #   tail(Sap10A); tail(synth.ts)
    #   nrow(Sap10A); length(synth.ts)

# 2 merging the synthetic time index and real data


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


    # 
    # # example usage 
    # Sap10A.fixedint <- MergeSynth(Sap10A, 10*60)
    # 
    # # testing the merger
    # nrow(Sap10A.fixedint)
    # nrow(Sap10A)
    # head(Sap10A.fixedint)
    # summary(Sap10A.fixedint)

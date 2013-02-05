# 1
e.time <- as.POSIXct("2012-05-17 14:30")
e.tree <- 'Dendro_Avg.3.03A'
e.df <- 'Den03A'
e.type <- 'spike'
e.fixed <- FALSE
# 2
e.time <- c(e.time, as.POSIXct("2012-08-08 13:40") )
e.tree <- c(e.tree, 'Dendro_Avg.3.03A')
e.df <- c(e.df,'Den03A')
e.type <- c(e.type,'spike')
e.fixed <- c(e.fixed,FALSE)
# 3
e.time <- c(e.time, as.POSIXct("2012-08-08 13:30") )
e.tree <- c(e.tree, 'Dendro_Avg.4.03A')
e.df <- c(e.df,'Den03A')
e.type <- c(e.type,'spike')
e.fixed <- c(e.fixed,FALSE)
# 4
e.time <- c(e.time, as.POSIXct("2012-08-08 14:00") )
e.tree <- c(e.tree, 'Dendro_Avg.4.03A')
e.df <- c(e.df,'Den03A')
e.type <- c(e.type,'spike')
e.fixed <- c(e.fixed,FALSE)
# 5
e.time <- c(e.time, as.POSIXct("2012-03-22 10:50") )
e.tree <- c(e.tree, 'Dendro_Avg.4.03A')
e.df <- c(e.df,'Den03A')
e.type <- c(e.type,'spike')
e.fixed <- c(e.fixed,FALSE)
# 6 - Cannot be dealt with automatically
e.time <- c(e.time, as.POSIXct("2012-03-22 11:10") )
e.tree <- c(e.tree, 'Dendro_Avg.2.03A')
e.df <- c(e.df,'Den03A')
e.type <- c(e.type,'mixed')
e.fixed <- c(e.fixed,FALSE)
# 7 - cannot be dealt with automatically
e.time <- c(e.time, as.POSIXct("2012-03-22 10:50") )
e.tree <- c(e.tree, 'Dendro_Avg.1.03A')
e.df <- c(e.df,'Den03A')
e.type <- c(e.type,'mixed')
e.fixed <- c(e.fixed,FALSE)
# 8
e.time <- c(e.time, as.POSIXct("2012-08-08 14:00") )
e.tree <- c(e.tree, 'Dendro_Avg.1.03A')
e.df <- c(e.df,'Den03A')
e.type <- c(e.type,'spike')
e.fixed <- c(e.fixed,FALSE)
# 9 - error is mixed
e.time <- c(e.time, as.POSIXct('2012-03-22 15:30'))
e.tree <- c(e.tree, 'Dendro_Avg.2.10A')
e.df <- c(e.df,'Den10A')
e.type <- c(e.type,'mixed')
e.fixed <- c(e.fixed,FALSE)
# 10
e.time <- c(e.time, as.POSIXct('2012-03-22 15:40'))
e.tree <- c(e.tree, 'Dendro_Avg.3.10A')
e.df <- c(e.df,'Den10A')
e.type <- c(e.type,'spike')
e.fixed <- c(e.fixed,FALSE)
# 11 - error is mixed
e.time <- c(e.time, as.POSIXct("2012-03-22 13:40"))
e.tree <- c(e.tree, 'Dendro_Avg.3.03F')
e.df <- c(e.df,'Den03F')
e.type <- c(e.type,'mixed')
e.fixed <- c(e.fixed,FALSE)
# 12 - error mixed
e.time <- c(e.time, as.POSIXct("2012-03-22 15:30"))
e.tree <- c(e.tree, 'Dendro_Avg.2.10A')
e.df <- c(e.df,'Den10A')
e.type <- c(e.type,'mixed')
e.fixed <- c(e.fixed,FALSE)
# 13 - error mixed
e.time <- c(e.time, as.POSIXct("2012-03-22 13:40"))
e.tree <- c(e.tree, 'Dendro_Avg.4.03F')
e.df <- c(e.df,'Den03F')
e.type <- c(e.type,'mixed')
e.fixed <- c(e.fixed,FALSE)

# Constructing data frame
errors <- data.frame("TIMESTAMP"=e.time,"tree"=e.tree,"dataframe"=e.df,"error.type"=e.type,"error.fixed"=e.fixed,stringsAsFactors=FALSE)

if (config$error_vis=='on'){
  for(row in c(1:nrow(errors))){
    time <- errors[row,'TIMESTAMP']
    df.name <- errors[row,'dataframe']
    day <- floor_date(time,unit='day')
    tree <- errors[row,'tree']
    error <- get(df.name)
    error <- error[error$TIMESTAMP > (time - 12*10*60), ]
    error <- error[error$TIMESTAMP < (time + 12*10*60), ]
    plot(x = error$TIMESTAMP, xlab=day,
         y = error[,tree], ylab='Measurement',
         main=paste(df.name,tree,strftime(time,format='%H:%M:%S'),sep=' |--| '))
  }
}
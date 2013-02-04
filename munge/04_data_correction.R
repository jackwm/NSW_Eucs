
    # plotting some interesting parts, to do with the tree swap
    ############################################################
    t.tree <- 'Dendro_Avg.4.03F'
    t.tree <- 'Dendro_Avg.4.10A'
    # tree swap
    ks <- as.POSIXct("2012-04-01 00:00")
    ke <- as.POSIXct("2012-06-01 00:00")
    # pre tree swap
    ks <- as.POSIXct("2012-04-21 00:00")
    ke <- as.POSIXct("2012-04-26 00:00")
    # pre tree swap - for 10A
    ks <- as.POSIXct("2012-04-18 00:00")
    ke <- as.POSIXct("2012-04-24 00:00")
    # week following tree swap, 
    ks <- as.POSIXct("2012-04-27 00:00")
    ke <- as.POSIXct("2012-05-05 00:00")
    # one week after the tree swap, interesting trough
    ks <- as.POSIXct("2012-05-27 00:00")
    ke <- as.POSIXct("2012-06-10 00:00")
    # entire period after the tree swap
    ks <- as.POSIXct("2012-04-27 00:00")
    ke <- as.POSIXct("2012-09-01 00:00")
    # most recent fortnight
    ks <- as.POSIXct("2012-08-05 00:00")
    ke <- as.POSIXct("2012-08-18 00:00")
    # most recent week
    ks <- as.POSIXct("2012-08-13 00:00")
    ke <- as.POSIXct("2012-08-18 00:00")
    #
    setwd('~/Dropbox/phd/r_output/')
    fn <- paste(t.tree, format(Sys.time(), format = "%b.%d.%H%M"),sep =".")
    ## low res
    #png(filename = paste(fn, 'web', 'png', sep = "."))
    # hi res
    #png(filename = paste(fn, 'print', 'png', sep ="."), res = 300, width = 2400, height = 1800)
    error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
    plot(error$TIMESTAMP, error[, t.tree], type = 'p', cex = .2,
         main = t.tree, ylab = "Relative stem radius (mm)", xlab = "")
    dev.off()
    #
    # #Sap.All[11862:11868,c(1,13)] # recorrecting the 
    # correction <- Sap.All[11865,13] - Sap.All[11862,13]
    # Sap.All[1:11862,13]<-Sap.All[1:11862,13]+correction
    # This was a tree swap. Need to make a new column and move the preceeding data there. 
    ############################################################
    
    
    # creating df of 'spike' errors to loop over
    spikes.df <- data.frame(e.time,e.tree)
    # running corrections on the spikes errors df
    for (i in 1:nrow(spikes.df)) print(spikes.df[i,'x'])
    for (i in 1:nrow(spikes.df)) {  
      e.time <- spikes.df[i, 'e.time']
      e.time <- as.POSIXct(e.time)
      e.tree <- spikes.df[i, 'y']
      e.tree <- as.character(e.tree)
      Sap.All[ Sap.All$TIMESTAMP == e.time, e.tree ] <- ( Sap.All[ Sap.All$TIMESTAMP == (x-10*60), e.tree ] + Sap.All[ Sap.All$TIMESTAMP == (x+10*60), y] ) / 2
    }
    
    # creating df of 're-adjustment' errors to loop over
    readju.df <- data.frame(u,w)
    # running corrections on the readjument errors df. 
    for (i in 1:nrow(readju.df)) print(readju.df[i,c('u','w')])
    
    for (i in 1:nrow(readju.df)) {  
      u <- readju.df[i, 'u']
      u <- as.POSIXct(u)
      w <- readju.df[i, 'w']
      w <- as.character(w)
      # re-adjusting
      # de.tree is delta e.tree, the difference between the two periods. 
      de.tree <- Sap.All[ Sap.All$TIMESTAMP == (u+10*60), w] - Sap.All[ Sap.All$TIMESTAMP == (u-10*60), w]
      Sap.All[ Sap.All$TIMESTAMP < u, w] <- Sap.All[ Sap.All$TIMESTAMP < u, w ] + dy
      # fixing 'spike' between the two period which also needs to be removed.
      Sap.All[ Sap.All$TIMESTAMP == u, w ] <- ( Sap.All[ Sap.All$TIMESTAMP == (u-10*60), w ] + Sap.All[ Sap.All$TIMESTAMP == (u+10*60), w ] ) / 2
    }
    
    tree.names <- TreeInfo(Sap.All) # might be worth adding the columns "site" and "tree" here. 
    trees <- GetTrees(Sap.All)
    trees.short <- c("t1s1","t2s1","t3s1","t4s1","t1s2","t2s2","t3s2","t4s2","t1s3","t2s3","t3s3","t4s4")
    
    # cleaning up
    rm(i,nam,my.col.names.obj,MyReadFunc,file.dir,my.file.list,my.obj.list)
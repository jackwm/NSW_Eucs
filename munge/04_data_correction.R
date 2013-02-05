
  # Correcting spikes
  for (n in c(1:nrow(errors))){
    if (errors[n,'error.type']=='spike'){
      
    }else print(errors[n,'error.type'])
  }

  if (FALSE) {
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
  }
  # cleaning up
  rm(i,nam,my.col.names.obj,MyReadFunc,file.dir,my.file.list,my.obj.list)
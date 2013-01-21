source('src/functions.R')
source('src/functions_date.R')
source('src/functions_max_min.R')
source('src/functions_SEI.R')
source('src/functions_synth.R')
source('src/functions_ZeroOrigin.R')


Sys.setenv(TZ='Etc/GMT-10') # sets the operating environment timezone. 

time.zone <- "Etc/GMT-10" # Will fix this later
time.origin <- "1970-01-01 00:00:00"

metRain <- met.data$Rain
met.data$Rain <- NULL
# use na.omit to drop na's at the end
# as they seem to just be at the end, we can safely remove nas

#plot(is.na(met.data$TIMESTAMP))

#met.data <- na.omit(met.data) #trim the end instead
met.data <- met.data[1:17480, ]

# converting from mV to mm
Sap.All <- millivolts_to_mm(Sap.All)

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
spikes.df <- data.frame(x,y)
# running corrections on the spikes errors df
for (i in 1:nrow(spikes.df)) print(spikes.df[i,'x'])
for (i in 1:nrow(spikes.df)) {  
  x <- spikes.df[i, 'x']
  x <- as.POSIXct(x)
  y <- spikes.df[i, 'y']
  y <- as.character(y)
Sap.All[ Sap.All$TIMESTAMP == x, y ] <- ( Sap.All[ Sap.All$TIMESTAMP == (x-10*60), y ] + Sap.All[ Sap.All$TIMESTAMP == (x+10*60), y] ) / 2
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
  # dy is delta y, the difference between the two periods. 
  dy <- Sap.All[ Sap.All$TIMESTAMP == (u+10*60), w] - Sap.All[ Sap.All$TIMESTAMP == (u-10*60), w]
  Sap.All[ Sap.All$TIMESTAMP < u, w] <- Sap.All[ Sap.All$TIMESTAMP < u, w ] + dy
# fixing 'spike' between the two period which also needs to be removed.
  Sap.All[ Sap.All$TIMESTAMP == u, w ] <- ( Sap.All[ Sap.All$TIMESTAMP == (u-10*60), w ] + Sap.All[ Sap.All$TIMESTAMP == (u+10*60), w ] ) / 2
}

tree.names <- TreeInfo(Sap.All) # might be worth adding the columns "site" and "tree" here. 
trees <- GetTrees(Sap.All)
trees.short <- c("t1s1","t2s1","t3s1","t4s1","t1s2","t2s2","t3s2","t4s2","t1s3","t2s3","t3s3","t4s4")

# cleaning up
rm(i,nam,my.col.names.obj,MyReadFunc,file.dir,my.file.list,my.obj.list)


## this script will load .dat files from three different dataloggers into memory at the same time
## use merge to create one large table
## export this large table as .csv for visualisation elsewhere & sharing with collegues. 
## history: created from "read dendrometer data - site generic - cbind to one table.R"
## forked on: 25 May 2012 by Stephen Fujiwara
## inputs: campebell scientific .dat files
## outputs: one .csv

## changelog: 
## 2012-09-13 removed all tz settings, other than setenv at the start. 
## 2012-08-16 changed file.dir to reflect new folder structure. Changed number of lines to skip when looking for filenames. 
## 2012-06-16 put the ZeroOrigin function inside the loop which loads the data from each site. 
## 2012-06-26
## loads all "SapXXX" .dat files in the wd, saves them as individual objects
## while loading, modifies column names in each "SapXXX" object to include the object name, so column names are unique
## merge three site tables into one big data table called 'SapAll'
##

Sys.setenv(TZ='Etc/GMT-10') # sets the operating environment timezone. 

time.zone <- "Etc/GMT-10" # Will fix this later
time.origin <- "1970-01-01 00:00:00"

# setting workding and output directories
file.dir <- "~/Dropbox/phd/data/Level_1/"
setwd(file.dir)

output.dir <- "~/Dropbox/phd/r_output" 
# We will change the working directory to output.dir later on after loading the data

#Loading groundwater data
  # gw <- read.csv(file = "~/Dropbox/phd/data/Level_0/Kang10A_0_knaive39m.csv", skip = 3)
  # colnames(gw) <- c("rn","TIMESTAMP","Abs Pres, kPa","Temp, ?C", "Water_level_m", "Coupler Detached","Coupler Attached", "Host Connected","End Of File")
  # 
  # gw[ ,2] <- as.POSIXct(gw[ ,2], format = "%m/%d/%y %I:%M:%S %p")



## IMPORTING MET DATA, RANDOL'S XLS FILE 'SAVED-AS' CSV FROM EXCEL IN 3 PARTS
  # Part 1/4 collecting column names
  met.data <- read.csv("~/Dropbox/phd/data/Level_1/Kanga_MetSt_10Min_ from 2012_01_01.csv",
                       header = TRUE, skip = 1)
  met.data_col.names <- c(
    names(met.data)[1:16],
    "sat_vap_press", "VPD", "Rain")
  
  #Part 2/4 collecting metadata
  met.data <- read.csv("~/Dropbox/phd/data/Level_1/Kanga_MetSt_10Min_ from 2012_01_01.csv",
                       header = FALSE)
  
  met.data_col.info<- met.data[1:4, ]
  
  #Part 3/4 reading in data and applying correct column names
  met.data <- read.csv("~/Dropbox/phd/data/Level_1/Kanga_MetSt_10Min_ from 2012_01_01.csv",
                       header = FALSE, 
                       skip = 4, 
                       col.names = met.data_col.names, 
                       as.is = TRUE)
  
  rm(met.data_col.names)
  # Part 4/4 setting TIMESTAMP as a POSIXct object
  met.data[ ,1] <- as.POSIXct(met.data[,1], format = "%y-%m-%d %H:%M")
  
  # Checking for missing rows and NAs.  
  #MissingRowsPrint(met.data) #doesn't seem to work very well
  
  # use plot to check for nas
  #plot(is.na(met.data$VPD))
  #plot(is.na(met.data$Rain))
  # dropping the na column as it is full of nas. 
  metRain <- met.data$Rain
  met.data$Rain <- NULL
  # use na.omit to drop na's at the end
  # as they seem to just be at the end, we can safely remove nas
  
  #plot(is.na(met.data$TIMESTAMP))
  
  #met.data <- na.omit(met.data) #trim the end instead
  met.data <- met.data[1:17480, ]
  
  # Some plot to check the data
  #plot(met.data$TIMESTAMP, met.data$VPD, type = 'l')

## LOADING DENDROMETER DATA

# create a list of files to process
my.file.list <- list.files(path = ".", pattern = '*_DendroTenMinOutput.dat$' )
if (length(my.file.list)>3) stop("more than three datafiles present")

# Creating a list of site-names, derived from the filenames using substring
my.obj.list <- sapply(my.file.list, substring, 7, 12)

# load the column names provided by loggernet 
#my.col.names.obj <- read.csv(my.file.list[1], skip = 1) # old
my.col.names.obj <- read.csv(my.file.list[1], skip = 0)  # new
my.col.names.obj <- head(my.col.names.obj) # we can discard most of the roads, we just need the column names

# define a function to read in a file
MyReadFunc <- function(my.file.name){
  
  x <- names(my.col.names.obj)
  x[4:7]  <- sapply(x[4:7], paste, substr(my.file.name, start = 7, stop = 12), sep="")
  names(my.col.names.obj) <- x
  
DF <-   read.csv(file = my.file.name, 
           skip = 4,
           header = FALSE,
           col.names = names(my.col.names.obj))
#DF <- DF[-c(1:10), ] # removing the first entry, to assist remove likely outliers at the start.   
#DF <- ZeroOrigin(DF)
  return(DF)
}

# use a small loop to run the above function over all my files 
for (i in seq_along(my.file.list)) {
  nam <- my.obj.list[i]
  assign(nam, MyReadFunc(my.file.list[i]))
}

# merging the multiple dataframe objects

#     how I will reference my data tables
#     my.obj.list[1]
#     my.obj.list[[1]]
#     head(get(my.obj.list[3])) 

if (length(my.obj.list) == 3) {
  Sap.All <- merge(get(my.obj.list[1]), get(my.obj.list[2]), by = 'TIMESTAMP', all = TRUE)
  Sap.All <- merge(Sap.All, get(my.obj.list[3]), by = 'TIMESTAMP', all = TRUE)
}  else
  stop("number of data tables is greater or less than 3. must write new code for this instance.")

# converting from mV to mm
Sap.All <- millivolts_to_mm(Sap.All)

# converting Sap$TIMESTAMP to a POSIXct value
# Fix the Time zones
Sap.All[,1] <- as.POSIXct(Sap.All[,1])
Sap10A[,1] <- as.POSIXct(Sap10A[,1])
Sap03A[,1] <- as.POSIXct(Sap03A[,1])
Sap03F[,1] <- as.POSIXct(Sap03F[,1])

# Filling missing times
Sap.All <- MergeSynth(Sap.All,10*60)

# Fix data errors
############################################################
# Error noticed Site 031 Tree 1 row 6785, seems that the device was bumped and there was a sudden...
# ...jump in the level, it doesn't seem to be a single point that has changed but following values also

# trees to fix:
  # Site 03A Tree 1
  # Site 03A Tree 1 - Done
  # Site 03A Tree 3 - Done
  # Site 03A Tree 4 - Done as of 27-Aug-2012
  
  # Site 03F Tree 2
  # Site 03F Tree 3
  
  # Site 10A Tree 2
  # Site 10A Tree 3

# more difficult, leave till last. 
  # Site 10A Tree 4
  # Site 03F Tree 4

# Site 03A Tree 3
  #error <- Sap.All[ Sap.All$TIMESTAMP < as.POSIXct("2012-05-21") & Sap.All$TIMESTAMP > as.POSIXct("2012-05-14"), ]
  #Sap.All[14876:14878,c(1,6)] # shows the region with the error
  # Sets this data point as the average of the neighbouring data points

 # locating an error to fix. 
# subsetting 1st error - with rows
n <- nrow(Sap.All)
  error <- Sap.All[0:n, ] 
  error <- Sap.All[14200:15400, ]
  error <- Sap.All[14700:15000, ]
  error <- Sap.All[14870:14900, ]

# subsetting 1st error - with date-times
ks <- as.POSIXct("2012-05-17 12:00")
ke <- as.POSIXct("2012-05-18 00:00")

# subsetting 2nd error 
ks <- as.POSIXct("2012-08-08 10:00")
ke <- as.POSIXct("2012-08-08 16:00")

error <- Sap.All[Sap.All$TIMESTAMP > ks, ]
error <- error[error$TIMESTAMP < ke , ]

#plotting the error
plot(error$TIMESTAMP, error$Dendro_Avg.3.Sap03A)

#  if I make x and y a dataframe, I can loop over it..
x <- as.POSIXct("2012-05-17 14:30")
y <- 'Dendro_Avg.3.Sap03A'

x <- c(x, as.POSIXct("2012-08-08 13:40") )
y <- c(y, 'Dendro_Avg.3.Sap03A')
  

# visualising 3rd error
t.tree <- 'Dendro_Avg.4.Sap03A'
ks <- as.POSIXct("2012-08-08 10:00")
ke <- as.POSIXct("2012-08-08 15:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree])
# adding 3rd error to the list
x <- c(x, as.POSIXct("2012-08-08 13:30") )
y <- c(y, 'Dendro_Avg.4.Sap03A')

# visualising 4th error
t.tree <- 'Dendro_Avg.1.Sap03A'
ks <- as.POSIXct("2012-08-08 13:00")
ke <- as.POSIXct("2012-08-08 15:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree])
# adding 4th error to the list
x <- c(x, as.POSIXct("2012-08-08 14:00") )
y <- c(y, t.tree)

# visualising 5th error
t.tree <- 'Dendro_Avg.1.Sap03A'
ks <- as.POSIXct("2012-03-22 09:00")
ke <- as.POSIXct("2012-03-22 13:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree])
# adding 5th error to the list
x <- c(x, as.POSIXct("2012-03-22 10:50") )
y <- c(y, t.tree)

# visualising 6th error
t.tree <- 'Dendro_Avg.2.Sap03A'
ks <- as.POSIXct("2012-03-22 08:00")
ke <- as.POSIXct("2012-03-22 13:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree])
# 6th error - we need a new list for this point
u <- as.POSIXct("2012-03-22 11:10")
attr(u, which = 'tz')
u
w <- t.tree

# visualising 7th error
# t.tree <- 'Dendro_Avg.1.Sap03A'
# ks <- as.POSIXct("2012-03-16 00:00")
# ke <- as.POSIXct("2012-03-30 00:00")
# ks <- as.POSIXct("2012-03-22 08:00")
# ke <- as.POSIXct("2012-03-22 13:00")
# error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
# plot(error$TIMESTAMP, error[, t.tree])
# # 7th error is a re-adjustment, rather than spike
# u <- c(u, as.POSIXct("2012-03-22 10:50"))
# u
# w <- c(w, t.tree)


# visualising 8th error
t.tree <- 'Dendro_Avg.1.Sap03A'
ks <- as.POSIXct("2012-08-08 12:00")
ke <- as.POSIXct("2012-08-08 16:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree])
# 8th error is a spike
x <- c(x, as.POSIXct("2012-08-08 14:00") )
y <- c(y, t.tree)

# visualising 9th error
t.tree <- 'Dendro_Avg.2.Sap10A'
ks <- as.POSIXct("2012-03-22 08:00")
ke <- as.POSIXct("2012-03-22 13:00")
ks <- as.POSIXct("2012-03-22 12:00")
ke <- as.POSIXct("2012-03-22 18:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree])
# 9th error is a spike
x <- c(x, as.POSIXct('2012-03-22 13:30'))
y <- c(y, t.tree)

# 10th error
t.tree <- 'Dendro_Avg.3.Sap10A'
ks <- as.POSIXct("2012-03-22 08:00")
ke <- as.POSIXct("2012-03-22 18:00")
ks <- as.POSIXct("2012-03-22 12:00")
ke <- as.POSIXct("2012-03-22 16:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree])
# 10th error is a spike only
x <- c(x, as.POSIXct('2012-03-22 15:40'))
y <- c(y, t.tree)

# 11th error
t.tree <- 'Dendro_Avg.3.Sap03F'
ks <- as.POSIXct("2012-01-01 00:00")
ke <- as.POSIXct("2012-08-10 00:00")
ks <- as.POSIXct("2012-03-22 08:00")
ke <- as.POSIXct("2012-03-22 18:00")
ks <- as.POSIXct("2012-03-22 10:00")
ke <- as.POSIXct("2012-03-22 16:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree], type = 'p', cex = .2)
# 11th error is a spike & re-adjust
u <- c(u, as.POSIXct("2012-03-22 13:40"))
w <- c(w, t.tree)

#12th error
t.tree <- 'Dendro_Avg.2.Sap10A'
ks <- as.POSIXct("2012-03-20 00:00")
ke <- as.POSIXct("2012-03-25 00:00")
ks <- as.POSIXct("2012-03-22 12:00")
ke <- as.POSIXct("2012-03-22 18:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree], type = 'p', cex = .2)
# 11th error is a spike & re-adjust
u <- c(u, as.POSIXct("2012-03-22 15:30"))
w <- c(w, t.tree)

# 13th error
t.tree <- 'Dendro_Avg.4.Sap03F'
ks <- as.POSIXct("2012-03-20 00:00")
ke <- as.POSIXct("2012-03-25 00:00")
ks <- as.POSIXct("2012-03-22 12:00")
ke <- as.POSIXct("2012-03-22 18:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree], type = 'p', cex = .2)
# this error is a spike & re-adjust
u <- c(u, as.POSIXct("2012-03-22 13:40"))
w <- c(w, t.tree)

# 14th error
t.tree <- 'Dendro_Avg.4.Sap03F'
ks <- as.POSIXct("2012-07-10 00:00")
ke <- as.POSIXct("2012-09-01 00:00")
ks <- as.POSIXct("2012-08-08 09:00")
ke <- as.POSIXct("2012-08-08 15:00")
error <- subset(Sap.All, TIMESTAMP > ks & TIMESTAMP < ke)
plot(error$TIMESTAMP, error[, t.tree], cex = .2,
     main = t.tree, ylab = "Relative stem radius (mm)", xlab = "",
     type = "h")
# this error is a spike & re-adjust
u <- c(u, as.POSIXct("2012-03-22 12:00"))
w <- c(w, t.tree)
#
############################################################


# plotting some interesting parts, to do with the tree swap
############################################################
t.tree <- 'Dendro_Avg.4.Sap03F'
t.tree <- 'Dendro_Avg.4.Sap10A'
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


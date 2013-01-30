# Until you work on this Fuji, I'm not going to run let this script run

if (FALSE){

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

# for some reason that dataset has a large negative as the first value. 
# if it hasn't been removed prior to loading, that if statement will remove it.
if(met[1, 'AirTC_Avg'] == -96.600000000) {
  n <- nrow(met)
  met <- met[2:n, ]
}

# creating column of saturated vapour pressue (es - kPa) and VPD (D - Pa)
met$es <- 0.61078 * exp(17.269 * met$AirTC_Avg / (237.3+met$AirTC_Avg))
met$D <- met$es * (1 - met$RH_Avg / 100)

############################################################

# fixing up the dataframes

if (want.save == TRUE) {
  
  # creating a new folder to save the data to
  dir.string <- paste('~/Dropbox/phd/r_output_data/', Sys.Date(), '/', sep = "")
  dir.create(dir.string, showWarnings = FALSE) 
  setwd(dir.string)
  #
  # 
  # saving CSVs - met data
  
  for (nnnn in names(df.list) ) {
    print(paste('saving', nnnn, '.csv'))
    write.csv(x = df.list[[nnnn]],
              file = paste(nnnn, '.csv', sep = ""));
  }
  # saving .Rdata
  file.string <- paste('df.list.Rdata', sep = "_")
  save(df.list, file = file.string)
  print(paste(file.string, 'saved'))
}    

# writing to Rdata, one file for each dataframe    

setwd(wd.backup)
#}
#############################################################################  
#}

}
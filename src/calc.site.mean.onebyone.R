## Calculating mean growht series for each site individually

# calculating mean growth per site
my.obj.list <- list(Den03F, Den03A, Den10A)
str(my.obj.list)
mol <- my.obj.list
head(mol[[1]])

attr(mol[[1]], 'Site') <- "03F"
attr(mol[[2]], 'Site') <- "03A"
attr(mol[[3]], 'Site') <- "10A"

# Tidying up the three dataframes to permit stacking
for (i in 1:3){
  # Dropping metadata columns
  mol[[i]][,2] <- NULL
  mol[[i]][,2] <- NULL
  # Adding a column to show the Site for each row's measurement
  mol[[i]]$Site <- attr(mol[[i]], 'Site')
  # Making the column names consistent to permit rbind
  colnames(mol[[i]]) <- sub(pattern="[[:punct:]][[:digit:]]{2}[[:alpha:]]", replacement="",x= colnames(mol[[i]]) )
  # re-running zero-origin
  mol[[i]] <- ZeroOrigin(mol[[i]])
  # subsetting as per user specification
  mol[[i]] <- mol[[i]][mol[[i]]$TIMESTAMP > spec.s, ]
  # DF <- DF[DF$TIMESTAMP < spec.e, ]  
}

# using rbind to stack one sites' data on top of anohter
den.siteonsite <- do.call(what='rbind', mol)
head(den.siteonsite)

# checking the output
summary(den.siteonsite)
sapply(FUN= class, X=den.siteonsite)
head(den.siteonsite)

# calculating the row (site) mean for each timestamp
df.sos <- den.siteonsite
df.sos$Mean <- apply(X=df.sos[2:4], FUN='mean',MARGIN=1 )
head(df.sos)
summary(df.sos)

# deleting each individual tree value
df.sos[,2]<- NULL
df.sos[,2]<- NULL
df.sos[,2]<- NULL
df.sos[,2]<- NULL



## divide radius by RBH (radius breast height)
DF2.1 <- tree.info
# dropping out dendro 4 at 3F and 10A
DF2.1 <- DF2.1[ -c(which(DF2.1$Species == 'E. globoidea')), ]
DF2.1$Dist_m <- NULL
DF2.1$Tree <- NULL
DF2.1$Bearing_degrees <- NULL
# calculating changes in radius per unit radius
DF2.1$DBH_mm <- DF2.1$DBH_cm * 10 
DF2.1$rad_mm <- DF2.1$DBH_mm / 2 
DF2.1$rad_m <- DF2.1$rad_mm / 1000
# checking each dataframe to prepare for merger
head(df.sos)
head(DF2.1)
class(df.sos$Site)
class(DF2.1$Site)
# changing to char so both cols match
DF2.1$Site <- as.character(DF2.1$Site)
# clearing out superfluous columns
DF2.1$DBH_cm <- NULL
DF2.1$DBH_mm <- NULL
DF2.1$rad_mm <- NULL
DF2.1$Species <- NULL
DF2.1$Port <- NULL

DF2.1$Site <- as.factor(DF2.1$Site)
site.rbh <- by(DF2.1[,'rad_m'], DF2.1[,'Site'], FUN=mean, simplify=FALSE)
site.rbh <- data.frame('Mean_rbh' = as.numeric(site.rbh[1:3]), 'Site' = names(site.rbh)) 

df.sos.2 <- merge(as.data.frame(df.sos), as.data.frame(site.rbh), by = c('Site'))

# Dividing by the mean radius. 
x <- df.sos.2
sapply(x, class)
x$Mean_rad_per_m <- with(x,
     Mean / Mean_rbh)

x <- x[ ,c('TIMESTAMP','Site', 'Mean_rad_per_m')]

# converting site labels to depth
x$Site_alpha <- x$Site
class(x$Site)
levels(x$Site)
x$Site <- as.factor(x$Site)
levels(x$Site) <- list("9" = "03A", "3" = "03F", "39" = "10A")
tail(x)
DF.av <- x
target.dir <- paste(data.dir,'CS',sep='/')
target.dir <- paste(target.dir,dir(target.dir)[1],sep='/')
check <- regexpr("Kanga", dir(target.dir))

# Checking the folder to see if it has the expected structure
if(max(check)==-1){
  print(paste("Expected Folder starting with 'Kanga' None were found in",getwd()))
  } else {
  target.dirs <- paste(target.dir,dir(target.dir)[check>0],sep='/')
}

# Counting the number of files for use with Progress Bar
no.files <- 0;
for (folder in target.dirs){
  for (f in dir(folder)) {no.files <- no.files + 1}
}
print('FUJI LOOK HERE')
print(no.files)
pb <- txtProgressBar(min=0,max=no.files)

reg.exp <- '[^_]{5,6}$'
df.names <- str_extract(target.dirs,reg.exp)

check <- list()
check[['Den']] <- regexpr(".{6}(Den).{2,3}",dir(target.dir)) # three sites with den
check[['Met']] <- regexpr(".{6}(Met).{2,3}",dir(target.dir)) # one site with Met
check[['TRh']] <- regexpr(".{6}(TRh).{2,3}",dir(target.dir)) # four sites with TRh

if (!exists('df.list')) {df.list <- list()}
prog <- 0
cnt <- 0
changes.made <- FALSE
print('Compiling csv files')
for (folder in target.dirs){
  cnt <- cnt + 1
  df.name <- str_extract(folder,reg.exp)
  if (exists(df.name)) {
    prog <- prog + length(dir(folder))
    setTxtProgressBar(pb,prog)
    next
  }
  changes.made <- TRUE
  output.list <- list()
  # loop over files within the working directory
  for (f in dir(folder) ) {
    prog <- prog + 1
    setTxtProgressBar(pb,prog)
    target.file <- paste(folder,f,sep='/')
    # reading in the column headers, start by skipping 1
    col.names.obj <- read.csv(file = target.file, skip = 1, header = TRUE) # need to change this to skip 1.
    # read the data
    df <- read.csv(file = target.file, skip = 4, header = FALSE,
                       col.names=names(col.names.obj), 
                       as.is = TRUE)
    # if that didn't work, skip 0
    if (names(df)[1]!='TIMESTAMP'){
      col.names.obj <- read.csv(file = target.file,
                                skip = 0, header = TRUE)
      df <- read.csv(file = target.file, skip = 1, header = FALSE, 
                         col.names=names(col.names.obj), 
                         as.is = TRUE)
      }
    df$RECORD <- NULL
    if (check$Met[cnt]==1){
      colnames(df)[2:ncol(df)] <- paste(colnames(df)[2:ncol(df)],'aws',sep='.')
      output.list[[f]] <- df
    }else {
      if (check$TRh[cnt]==1){
        site.id <- str_extract(df.names[cnt],'.{3}$')
        colnames(df)[2:ncol(df)] <- paste(colnames(df)[2:ncol(df)],site.id,sep='.')
        output.list[[f]] <- df
      }else{
        colnames(df)[2] <- paste(colnames(df)[2],'.',sep='')
        site.id <- str_extract(df.names[cnt],'.{3}$')
        colnames(df)[2:ncol(df)] <- paste(colnames(df)[2:ncol(df)],site.id,sep='')
        output.list[[f]] <- df
      }
    }
  }
  output.df <- do.call("rbind", output.list)
  # drop non-unique rows
  # I'm worried about this line. There's no record or output of what is dropped.
  output.df <- unique(output.df)
  # clearing out the junk rownames
  rownames(output.df) <- NULL
  # Set the time series as a POSIXct class
  output.df[,'TIMESTAMP'] <- as.POSIXct(output.df[,'TIMESTAMP'])
  # Assign the output df as the name extracted from the folder name
  assign(df.name,value=output.df)
  # Cache the dataframe using the extracted name
  cache(df.name)
  # Create a list
  df.list[[df.name]] <- output.df
}
# Creating a list of items to throw away after the end of the script
trash <- c('folder','pb','f','df.name','target.dir','target.dirs','no.files','prog','check','reg.exp','df.names')
if (changes.made) {
  trash <- c(trash,'df','col.names.obj','output.df','site.id','target.file','output.list')
} else print('All data loaded from cache')
cache('df.list')

rm(list=trash); rm(trash)

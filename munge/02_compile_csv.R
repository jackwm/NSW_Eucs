# Script to load multiple csv files, combine them (and save the output).

# Changelog
# 2012-11-20 AM v0.1 First working version
# 2012-11-20 PM v0.2 Added block to save files to ~/Dropbox/phd/r_output_data
# 2012-11-20 PM v0.3 changed the way column headers are read in to prevent errors. 
# 2012-11-20 PM v0.4 Added another loop - to loop over the 3 data types (met, den, temp/rh)
#                    Added if statements to the file-save block, to deal with the 3 data types
# 2012-11-24 PM v0.5 Added if statement to fix up met data output. Saves applying fixes in every
#                    script which loads the .Rdata file. Code copied from loading_kangaloon_AWS_dat_03
#                    Changed storage in the 'check' list to be use data type names rather than integers.
#                    Added VPD calculation. 
#                    Changed the counter used in the last for loop. Conflict betweent the 'i' 
#                    in nested for loops was causing an issue. 
# 2012-12-11 v0.5b   Added a bracket, trying to get the loops to start working again. Couldn't get them to.
# 2012-12-12 v0.6    De-nested two loops. Now only one level of nesting. 
#                    As a result the file output is different. One CSV for each data type * site
#                    And one big df.list.Rdata file for all data * sites
#
# To do:
# Sort the output before save


# Define which date you want to run the data from:

return

target.dir <- '~/Dropbox/phd/data/Level_1A/2012-11-15'
target.dir <- '~/Dropbox/phd/data/Level_1A/2012-12-11'
want.save <- TRUE

target.dir <- paste(target.dir,'CS',sep='/')
setwd(target.dir);
dir()

# initialising output list
df.list <- list()

# Checking to find a Kanga_Data_per_site
check <- regexpr("Kanga", dir())
if(max(check)==-1){
  print(paste("Expected Folder starting with 'Kanga' None were found in",getwd()))
  }else
  target.dir <- paste(target.dir,dir()[check>0],sep='/')
  setwd(target.dir)
  target.dir -> wd.backup
print(paste('now we are in', getwd()))
print(dir())

# The below regular expression searches for folders with either 
# Sap or Den OR Met in the name
# then queues up those folders for the for-loops to follow
check <- list()
# match.length indicates which element of dir() is a match for the seach string
check[['Den']] <- regexpr(".{6}(Den).{2,3}",dir()) # three sites with den
check[['Met']] <- regexpr(".{6}(Met).{2,3}",dir()) # one site with Met
check[['TRh']] <- regexpr(".{6}(TRh).{2,3}",dir()) # four sites with TRh

# Produce list of sub folders to work through, with a for loop.
  # NEW method 2012-12-11
  subfs.df <- list()
  for (nnn in names(check)){
  to_search <- dir()[check[[nnn]]>0]
  to_search <- data.frame(to_search)
  to_search$data.type <- nnn
  subfs.df <- rbind(subfs.df, to_search)
  }
  subfs.df
  # OLD
  #   to_search <- NULL
  #   data.type <- NULL
  #   for (i in names(check)){
  #     print(i)
  #     new.i<- check[[i]]
  #     to_search <- rbind(to_search, dir()[new.i>0])
  #     print(to_search) 
  #     n <- 0
  #     data.type <- i # this is stored to help later

  
# Using a loop, setwd into each subfolder,
# load files from each, rbind them. 
  # set wd to subfolder.
  # NEW 
  for (folder in subfs.df$to_search){
    setwd(paste(target.dir,folder,sep='/'))
    n <- n + 1      
  # OLD  
  #   for (folder in to_search){
  #   setwd(paste(target.dir,folder,sep='/'))
  #   n <- n + 1
  
  output.list <- list()
  # loop over files within the working directory
  for (f in dir() ) { 
    print(paste("reading csv file:",f))
    
    # defining target file
    target.file <- f
    
    # reading in the column headers, start by skipping 1
    col.names.obj <- read.csv(file = target.file, skip = 1, header = TRUE) # need to change this to skip 1.
    # read the data
    DF1028 <- read.csv(file = target.file, skip = 4, header = FALSE,
                       col.names=names(col.names.obj), 
                       as.is = TRUE)
    # if that didn't work, skip 0
    if (names(DF1028)[1]!='TIMESTAMP'){
      col.names.obj <- read.csv(file = target.file, skip = 0, header = TRUE)
      DF1028 <- read.csv(file = target.file, skip = 1, header = FALSE, 
                         col.names=names(col.names.obj), 
                         as.is = TRUE)
      }
    
    colnames(DF1028)[2:3] <- paste(colnames(DF1028)[2:3],'.',sep='')
    junk_length <- length(strsplit(folder,'')[[1]])-3
    reg_expr <- paste('.{',as.character(junk_length),'}',sep='')
    site.id <- gsub(reg_expr,'',folder)
    colnames(DF1028)[2:ncol(DF1028)] <- paste(colnames(DF1028)[2:ncol(DF1028)],site.id,sep='')
    
    # creating list of dataframes 
    output.list[[f]] <- DF1028
    }
    
    # rbind all files in dir
    output.df <- do.call("rbind", output.list)
    print('rbinding')   
  
    # drop non-unique rows
    output.df <- unique(output.df)
    # clearing out the junk rownames
    rownames(output.df) <- NULL

    df.list[[n]] <- output.df
  }
  #setwd(target.dir)
#}

names(df.list) <- subfs.df$to_search # NEW
#names(df.list) <- to_search # OLD
summary(df.list)


  
# fixing up the dataframes

  # if the we're working with met data, much to fix up:
want.fix.met <- TRUE
#if ( data.type == 'Met' ) { 
if (want.fix.met == TRUE){

  # Finding the dataframe with the met data
  met.id <- grep(x = names(df.list), pattern = "Met")

  # Fix column naming of met station data.
  met <- df.list[[met.id]]
  x <- colnames(met)
  colnames(met) <- sub(pattern='tSt', replacement='', x=x)
  
  # Fix NANs and NSR columns. 
  if (met$NSR_Avg[1] == "NAN") {met$NSR_Avg[1]<- NA}
  if (class(met$NSR_Avg) == 'character' && met$CNSR_Avg[1] == "NAN") {met$CNSR_Avg[1]<- NA}
  if (class(met$NSR_Avg) == 'character') {met$NSR_Avg <- as.numeric(met$NSR_Avg)}
  if (class(met$CNSR_Avg) == 'character') {met$CNSR_Avg <- as.numeric(met$CNSR_Avg)}
  
  # 
  x <- met[1, 'TIMESTAMP']
  if(class(x) == "character" ) {
    met$TIMESTAMP <- as.POSIXct(met$TIMESTAMP) # check that doesnt change the timestamps
  } else stop( "timestamp class is something other than character")
  
  
  # for some reason that dataset has a large negative as the first value. 
  # if it hasn't been removed prior to loading, that if statement will remove it.
  if(met[1, 'AirTC_Avg'] == -96.600000000) {
    n <- nrow(met)
    met <- met[2:n, ]
  }
  
  # creating column of saturated vapour pressue (es - kPa) and VPD (D - Pa)
  met$es <- 0.61078 * exp(17.269 * met$AirTC_Avg / (237.3+met$AirTC_Avg))
  met$D <- met$es * (1 - met$RH_Avg / 100)
  
  df.list[[met.id]] <- met
}
  
#############################################################################
# saving data
    
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

target.dir <- paste(data.dir,'CS',sep='/')
target.dir <- paste(target.dir,dir(target.dir)[1],sep='/')
check <- regexpr("Kanga", dir(target.dir))

if(max(check)==-1){
  print(paste("Expected Folder starting with 'Kanga' None were found in",getwd()))
  } else {
  target.dirs <- paste(target.dir,dir(target.dir)[check>0],sep='/')
}

reg.exp <- '[^_]{5,6}$'
df.names <- str_extract(target.dirs,reg.exp)

check <- list()
check[['Den']] <- regexpr(".{6}(Den).{2,3}",dir(target.dir)) # three sites with den
check[['Met']] <- regexpr(".{6}(Met).{2,3}",dir(target.dir)) # one site with Met
check[['TRh']] <- regexpr(".{6}(TRh).{2,3}",dir(target.dir)) # four sites with TRh

df.list <- list()

n <- 0
for (folder in target.dirs){
  n <- n + 1
  folder.name <- str_extract(folder,reg.exp)
  
  output.list <- list()
  # loop over files within the working directory
  for (f in dir(folder) ) {
    print(paste("reading csv file:",f))
    
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
    
    colnames(df)[2:3] <- paste(colnames(df)[2:3],'.',sep='')
    junk_length <- length(strsplit(folder,'')[[1]])-3
    reg_expr <- paste('.{',as.character(junk_length),'}',sep='')
    site.id <- gsub(reg_expr,'',folder)
    colnames(df)[2:ncol(df)] <- paste(colnames(df)[2:ncol(df)],site.id,sep='')

    output.list[[f]] <- df
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

names(df.list) <- df.names

# Saving the dataframes #

for (n in names(df.list)){
  df.list[[n]]['TIMESTAMP'] <- as.POSIXct(df.list[[n]][,'TIMESTAMP'])
  assign(n,value=df.list[[n]])
  cache(n)
}

cache('df.list')


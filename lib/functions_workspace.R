#CAN"T  grabbing the data from the cache 
  LoadCache <- function(pattern){
    file.list <- dir(path='./cache/', pattern=pattern)
    if (length(file.list) < 2) stop('no dendrometer Rdata files found')
    for (file in file.list) print(paste('./cache/', file, sep = ""))
    for (file in file.list) load( paste('./cache/', file, sep = "")); print(file) 
  }
# TEST 1 
  # LoadCache("Den{1}[[:digit:]]{2}[[:alpha:]]{1}"")

# make a list of multiple dataframes from the workspace
MakeList <- function(pattern){
  x <- ls(pattern=pattern)  
  n <- length(x)
  my.obj.list <- list("list", n)
  
  for( i in 1:n) { 
    my.obj.list[[i]] <- get(x[i])
  }
  return(my.obj.list)
}
# USAGE EXAMPLE
# my.obj.list <- MakeList(pattern = "Den{1}[[:digit:]]{2}[[:alpha:]]{1}")

# merging the dataframes in a list
MergeDataframeList <- function(target.list, timestamp.column){
if (length(my.obj.list) == 3) {
  den.wide <- merge(my.obj.list[1], my.obj.list[2], by = timestamp.column, all = TRUE)
  den.wide <- merge(den.wide, my.obj.list[3], by = timestamp.column, all = TRUE)
}  else {
  stop("number of data tables you're trying to merge is greater or less than 3. must write new code for this instance.") 
}
return(den.wide)
}
#USAGE EXAMPLE
# den.wide <- MergeDataframeList(target.list = my.obj.list, timestamp.col = 'TIMESTAMP')

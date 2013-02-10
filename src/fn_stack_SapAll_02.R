# function to stack dendrometer measurements from a given input dataframe
# 2012-AUG-12 version 0.1, tested, worked on Sap.All & Sap03A
# 2012-DEC-12 version 0.2, Now works for den.all. 
#                          Now character positions of 'Site' and 'Port' 
#                          (in the column names) are found via regexpr
#                          rather than giving substr a hard-coded start and stop.
#
# input: dataframe with dendrometer data,
#         timestamp in the first column,
#         measmnts frm individual trees in subsequent columns, 
#         number of columns >= number of trees
#
# output: dataframe with only four columns,
#         timestamp, dendrometer measmnt, Tree #, Site ID
#         POSIXct, numeric, factor, factor



## objects used for developing that function
# source.with.encoding('~/Dropbox/phd/r_scripts/loading00.R', encoding='UTF-8')
# DF <- Sap.All
# t <- trees[[1]] # for testing & development



StackTrees <- function(DF){

  # finding out which columns in the input DF contain dendrometer data
  # using the GetTrees function
  tree.list <- GetTrees(DF)
  
  # pulling out first tree's data
  # subsequent trees' data will be appended to that
  t <- tree.list[[1]]
  out <- DF[ ,c('TIMESTAMP',t)]
  colnames(out) <- c('TIMESTAMP', 'Dendro')
  out$Tree <- as.factor(1)
  # using substring to pull out the port number
  place.port <- regexpr(t, pattern='[[:digit:]]')
  out$Tree <- substr(x=t, start=place.port[[1]], stop=place.port[[1]])
  # finding location of 'site' details in column name string
  place <- str_extract(t, pattern='[[:digit:]]{2}[[:alpha:]]$')
  head(out)
  out$Site <- place
  
  # running a loop over the remaining trees
  # first we need to see how many columns of tree data there are in the DF
  n <- length(tree.list)
  # second we run the loop
  for (t in tree.list[2:n]){
    # pulling out tree 'n' data - just the timestamp and dendrometer measurement
    z <- DF[ ,c('TIMESTAMP',t)]
    # must change the column names, to make them consistent, for rbind to work
    colnames(z) <- c('TIMESTAMP', 'Dendro')
    # using substring to find out the tree and site for each column's data
    # creating extra columns to store these, which later we set as factor variables
    z$Site <- str_extract(t, pattern='[[:digit:]]{2}[[:alpha:]]$')
    z$Tree <- str_extract(t,'[[:digit:]]')
    # each run of the loop rbinds to the existing data
    out <- rbind(out, z)
  }
  
  # setting Site and Tree ID variables as factors (substr produces characters)
  out$Tree <- as.factor(out$Tree)
  out$Site <- as.factor(out$Site)
  # returning the stacked output dataframe
  return(out)

}

# # usage example
# stacked.sapall <- StackTrees(Sap.All)
# stacked.sap03a <- StackTrees(Sap03A)

# out <- stacked.sapall
# out <- stacked.sap03a
 
# # testing that it worked. 
# summary(out)
# str(out)
# subset(out, Site == '03A')
# subset(out, Tree == '1')
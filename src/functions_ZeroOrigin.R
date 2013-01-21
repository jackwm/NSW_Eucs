# function to reset all dendromter measurements so as to start at zero.
# fn_ZeroOrigin.R

# input:
# any dataframe with trees arranged in columns, with the columns named
# "Dendro_Avg.X.SapYYY"

# output:
# a dataframe of the same format, with the first value in each column set to zero
# and subsequent values relative to that zero

# changelog
# 2012-Aug-13 1.0 first working version. 

## the objects used for writing that script
#  DF <- Sap.All
#  t <- tree.list[[2]]
#  rm(t)
#  rm(DF)

ZeroOrigin <- function(DF){
  
tree.list <- GetTrees(DF)
out <- DF 
for (t in tree.list){
  z <- out[1, t] 
  out[ , t] <- out[ , t] - z
  }
return(out)
}

## usage example:
# sap.all.zeroed <- ZeroOrigin(Sap.All)

# 05_fill_missing_obs
# In case we still need to fill the missing observations with NAs


if (FALSE){
# grab the dendrometer dataframes
dfs <- ls(pattern="Den{1}[[:digit:]]{2}[[:alpha:]]{1}$")

# apply conversion function to each of those
for (i in dfs){
  x <- MergeSynth(DF=get(i), obs.int=60*10)
  assign(i, x)
}
rm(dfs,i,x)
}

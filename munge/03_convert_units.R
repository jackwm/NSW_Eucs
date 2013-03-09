#03_convert_units
# convert from mV to mm

# find those dataframes which are dendrometer ones
dfs <- ls(pattern="Den{1}[[:digit:]]{2}[[:alpha:]]{1}$")

# apply conversion function to each of those
for (i in dfs){
  x <- MillivoltsToMillimeters(get(i), 3, 6)
  assign(i, x)
  # arguments are: <dataframe>, <first data column>, <last data column> 
}

# Tidy up
rm(x,i,dfs)
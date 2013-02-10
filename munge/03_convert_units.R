#03_convert_units
# convert from mV to mm

# Gather names of dataframes
x <- names(df.list)
# Find those that are dendrometer measurements
y <- str_extract(x, 'den')

for (i in y){
  df.list[[i]] <- MillivoltsToMillimeters(df.list[[i]], 3, 6)
  # arguments are: <dataframe>, <first data column>, <last data column> 
}

# Tidy up
rm(x,y)
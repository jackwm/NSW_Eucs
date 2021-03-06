# writing all the data frames to csv's and storing in csv folder
if (grep(pattern='csv', x=dir())  == 0 )
  dir.create(path='csv/')

cnt <- 0;
if (changes.made){
  for (df in df.list){
    cnt <- cnt + 1
    write.csv(df,paste('csv/',names(df.list)[cnt],'.csv',sep=''),row.names=FALSE)
  }
  rm(df)
}
# Visualise errors, if set to on in config
if (config$error_vis=='on'){
  DisplayErrors(errors)
}

# Calculating time taken
time.taken <- round(as.numeric(difftime(now(),start,units='secs')),2)
print(paste('Time taken to load:',time.taken,'secs'))

# Cleaning up unused variables
rm(start,time.taken,cnt,changes.made)
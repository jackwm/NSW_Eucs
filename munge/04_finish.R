# writing all the data frames to csv's and storing in csv folder
cnt <- 0;
for (df in df.list){
  cnt <- cnt + 1
  write.csv(df,paste('csv/',names(df.list)[cnt],'.csv',sep=''),row.names=FALSE)
}

# Calculating time taken
time.taken <- round(as.numeric(difftime(now(),start,units='secs')),2)
print(paste('Time taken to load:',time.taken,'secs'))

# Cleaning up unused variables
rm(start,time.taken,cnt,df)
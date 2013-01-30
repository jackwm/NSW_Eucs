time.taken <- round(as.numeric(difftime(now(),start,units='secs')),2)
print(paste('Time taken to load:',time.taken,'secs'))
rm(start,time.taken)
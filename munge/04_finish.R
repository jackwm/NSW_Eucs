time.taken <- round(as.numeric(now()-start),2)
print(paste('Time taken to load:',time.taken,'secs'))
rm(start,time.taken)
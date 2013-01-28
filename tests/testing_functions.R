#### Testing ZLSMA.approx.na ###
for(t in trees){
  test1 <- ZLSMA.approx.na(na.trim(den.all),t,n=6*24,gap=Inf,with.plot='ZLSMA')
}
#### ----------------------- ###

### Testing MaxMin.SMA ###
for(t in trees){
  dataframe <- na.trim(GetTimeInt(Sap.All,start="2012-02-29",end="2012-03-06"))
  x <- dataframe$TIMESTAMP
  y <- dataframe[,t]
  dataframe <- data.frame(x,y); names(dataframe) <- c("TIMESTAMP",t)
  test2 <- MaxMin.SMA(dataframe,t,with.plot=TRUE,compare=FALSE)
}
### ------------------ ###

### Measuring NA's in duration ###
dataframe <- na.trim(Sap.All)
SEI.list <- CalcSEIdf(dataframe)
SEI.list.g <- GardenSEI(SEI.list)
sum_dur <- 0
sum_val <- 0
for(t in SEI.list.g){
  a <- is.na(t[,c(3,5)])
  b <- is.na(t[,c(2,4,6)])
  sum_dur <- sum_dur + length(a[a==TRUE])
  sum_val <- sum_val + length(b[b==TRUE])
}
print(paste("Number of NA's occuring from duration: ",sum_dur))
print(paste("Number of NA's occuring from values: ",sum_val))
### -------------------------- ###

#### Testing CalcSEIdf ###
dataframe <- na.trim(den.all)
test3 <- CalcSEIdf(dataframe,trace=TRUE)
### ------------------ ###

### Testing CalcSEI ###
dataframe <- na.trim(Sap.All)
mmpd <- MaxMin.SMA(den.all,trees[[1]],with.plot=TRUE,compare=FALSE)
test4 <- CalcSEI(mmpd,trees[[1]])
### --------------- ###

### Testing GardenSEI ###
SEI.list <- CalcSEIdf(na.trim(Sap.All))
SEI.list.gdnd <- GardenSEI(SEI.list)
### --------------- ###

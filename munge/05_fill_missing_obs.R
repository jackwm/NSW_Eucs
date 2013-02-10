# 05_fill_missing_obs
# In case we still need to fill the missing observations with NAs

for (i in df.list){
df.list[[i]] <- MergeSynth(df.list[[i]], 600)
}
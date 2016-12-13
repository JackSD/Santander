#read in 20k of the data from file
setwd("~/Santander")
df <- read.csv("train_ver2.csv", header=TRUE,nrows=200000)
nrow(df)

#read in for a particular person



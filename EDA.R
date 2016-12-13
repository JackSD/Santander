

df$count<-1
countries <- aggregate(df$count,by=list(df$pais_residencia), FUN="sum")
regions <- aggregate(df$count,by=list(df$nomprov), FUN="sum")
peopleLines <- aggregate(df$count, by=list(df$ncodpers), FUN="sum")
head(peopleLines[peopleLines$x==1,])

library(plotly)

plot_ly(df, x = ~age, type="histogram")

head(df,1)

#fold the products into a single column (note this ignores double products)
df2 <- cbind(df[1:24], product = names(df[25:48])[max.col(df[25:48] == 1L)])
df2$count<-1

products <- aggregate(df2$count,by=list(df2$product), FUN="sum")
plot(products)

head(df,1)
str(df2)

#converting the date to
df2$fecha_alta2 <- as.Date(df$fecha_alta)
df2$monthAdded <- NULL # 1 + as.POSIXlt(df2$fecha_alta2)$mon
df2$fecha_dato2   <- NULL
df2$count   <- NULL
df2$age <- as.numeric(df2$age)

head(df2[is.na(df2$monthAdded),])

head(df2[df2$monthAdded == 1 && !is.na(df2$monthAdded),])
head(df2[df2$monthAdded == 1,])

str(df2)
str(df)

head(df2)
df2$fecha_dato <- NULL
df2$ncodpers <- NULL

#convert inception date to continuous "tenure"
df2$DaysSinceInception <- as.numeric(difftime( as.Date("2016-05-28"),df2$fecha_alta2))

x <- difftime(as.Date("2015-01-15") , as.Date("2014-01-01"))
typeof(x)
as.numeric(x)

#data imputation 
nrow(df2[is.na(df2$renta) == TRUE,])

plot(df2$age, df2$renta)
hist(df2$renta , xlim=c(0,1000000), breaks=1000 )

library("fBasics")
df2_continuous <- data.frame(df2[c(4,6,5,7,9,16,17,19,20,23)])
df2_continuous$antiguedad <- as.numeric(df2_continuous$antiguedad)
df2_continuous$age <- as.numeric(df2_continuous$age)
basicStats(df2_continuous)

unique(df2$product)








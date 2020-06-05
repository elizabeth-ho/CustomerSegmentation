library(tidyverse)
library(data.table)
library(dplyr)
library(ggplot2)
library(caret)
library(factoextra)
library(NbClust)
library(haven)
library(clustertend)
library(LCA)
library(forecast)
library(mclust)
library(xts)
library(tseries)
library(astsa)
library(lmtest)
library(TSA)
library(timeSeries)
library(prophet)


rm(list=ls())
salesdf <- fread('SalesData.csv', header = T)
head(salesdf)
salesdf$V1 <- NULL
str(salesdf)
salesdf$TransactionDate <- as.Date(salesdf$TransactionDate, "%Y-%m-%d")

##  Remove transactions that were free samples or had price or quantity of zero.
count <- salesdf[salesdf$SellPrice==0 | salesdf$SellQty==0,]
sales2 <- salesdf[salesdf$SellPrice!=0 & salesdf$SellQty!=0,]
# if both price and quantity is negative then remove
sales2 <- sales2[!(sales2$SellPrice < 0 & sales2$SellQty<0),]
sales2$revenue <- sales2$SellPrice*sales2$SellQty
max(salesdf$TransactionDate)
# number of unique transactions
length(unique(sales2$TransNo))  #  329554
# number of unique customers
length(unique(sales2$CustomerNo))  #  1300
#  14 product categories
length(unique(sales2$Category))
# 293 Sub Categories
length(unique(sales2$SubCategory))
# 40,885 parts
length(unique(sales2$Part))


sales2$total_cogs <- sales2$COGS*sales2$SellQty
sales2$profit <- sales2$revenue-sales2$total_cogs

product_sales  <- sales2[,.(avgPrice=mean(SellPrice),
                        totalRev=sum(revenue),
                        totalQty=sum(SellQty)),
                        by=.(Category, SubCategory, Part) ]

summary(product_sales$totalRev)

# setorder by top revenue generating products and select top 5
product_sales <- setorder(product_sales, -totalRev)

top5 <- product_sales[1:5,]


########################################################################################
#
# create customer profile based on purchase behavior
cust_profile  <- sales2[,.(firstdate=min(TransactionDate),
                          lastdate=max(TransactionDate),
                          total_spend=sum(revenue),
                          profit_margin=sum(profit)/sum(revenue),
                          total_txns=length(unique(TransNo))),
                          by=CustomerNo ]

cust_profile$tenure <- cust_profile$lastdate-cust_profile$firstdate
cust_profile$recency <- as.Date("2017-10-19")-cust_profile$lastdate
cust_profile$spend_per_txn <- cust_profile$total_spend/cust_profile$total_txns



table(sales2$Category)
length(unique(sales2$Category))  #14 Categories
table(sales2$SubCategory)
length(unique(sales2$SubCategory))  #293 Sub Categories
table(sales2$Part)
length(unique(sales2$Part))  # 40,885 unique parts

## Segment based on spending in category - Which categories are significant?
test <- sales2[sales2$Category== 'CAT1005',]
table(test$CustomerNo)
test <- sales2[sales2$Category== 'CAT4004',]
table(test$CustomerNo)

# Get proportional spend by category
cust_category_sales  <- sales2[,.(CAT1005= ifelse(Category=='CAT1005', sum(revenue), 0),
                                  CAT2001= ifelse(Category=='CAT2001', sum(revenue), 0),
                                  CAT2002= ifelse(Category=='CAT2002', sum(revenue), 0),
                                  CAT2003= ifelse(Category=='CAT2003', sum(revenue), 0),
                                  CAT3001= ifelse(Category=='CAT3001', sum(revenue), 0),
                                  CAT3002= ifelse(Category=='CAT3002', sum(revenue), 0),
                                  CAT3003= ifelse(Category=='CAT3003', sum(revenue), 0),
                                  CAT3004= ifelse(Category=='CAT3004', sum(revenue), 0),
                                  CAT3005= ifelse(Category=='CAT3005', sum(revenue), 0),
                                  CAT4001= ifelse(Category=='CAT4001', sum(revenue), 0),
                                  CAT4002= ifelse(Category=='CAT4002', sum(revenue), 0),
                                  CAT4003= ifelse(Category=='CAT4003', sum(revenue), 0),
                                  CAT4004= ifelse(Category=='CAT4004', sum(revenue), 0)),
                                  by=c("CustomerNo", "Category") ]


cust_category_sales$Category <- NULL

cust_category_sales_total  <- cust_category_sales[,.(CAT1005=max(CAT1005),
                                                          CAT2001=max(CAT2001),
                                                          CAT2002=max(CAT2002),
                                                          CAT2003=max(CAT2003),
                                                          CAT3001=max(CAT3001),
                                                          CAT3002=max(CAT3002),
                                                          CAT3003=max(CAT3003),
                                                          CAT3004=max(CAT3004),
                                                          CAT3005=max(CAT3005),
                                                          CAT4001=max(CAT4001),
                                                          CAT4002=max(CAT4002),
                                                          CAT4003=max(CAT4003),
                                                          CAT4004=max(CAT4004)),
                                    by=CustomerNo ]

cust_category_sales_total[, total := rowSums(.SD), .SDcols = 2:14][]

cust_category_spend_proportion <- cust_category_sales_total[,.(CAT1005=CAT1005/total,
                                                               CAT2001=CAT2001/total,
                                                               CAT2002=CAT2002/total,
                                                               CAT2003=CAT2003/total,
                                                               CAT3001=CAT3001/total,
                                                               CAT3002=CAT3002/total,
                                                               CAT3003=CAT3003/total,
                                                               CAT3004=CAT3004/total,
                                                               CAT3005=CAT3005/total,
                                                               CAT4001=CAT4001/total,
                                                               CAT4002=CAT4002/total,
                                                               CAT4003=CAT4003/total,
                                                               CAT4004=CAT4004/total),
                                                            by=CustomerNo ]

cust_final <- merge(cust_profile,cust_category_spend_proportion,by="CustomerNo")

cust_final$profit_margin <- ifelse(cust_final$total_spend ==0, 0, cust_final$profit_margin)
colnames(cust_final)
cust_final

cust_final[is.na(cust_final)] <- 0
new_DF <- cust_final[rowSums(is.na(cust_final)) > 0,]
write.csv(cust_final,file='cust.csv',row.names = F)
##############################################################################################
#
#            Segment Customers

####  check math  15874

check <- sales2[sales2$CustomerNo %in% c(33415, 	43130, 	63230, 15874),]
write.csv(check,file='check.csv',row.names = F)

# no revenue
norev <- sales2[sales2$CustomerNo %in% c(19201, 	24684, 	63458, 64040),]
write.csv(norev,file='norev.csv',row.names = F)

#### check how many customers have a negative or low profit margin
cust_final[cust_final$profit_margin <0.05,]
summary(cust_final$total_txns)

# check total transactions outliers above ~400 are outliers
ggplot(data = cust_final, aes(x = "", y = total_txns)) + 
  geom_boxplot() +
   coord_cartesian(ylim = c(0, 1000))

str(cust_final)
cust_final$tenure <- as.numeric(cust_final$tenure)
cust_final$recency <- as.numeric(cust_final$recency)
cor(cust_final$total_spend, cust_final$tenure)
cor(cust_final$total_txns, cust_final$tenure)
colnames(cust_final)
variables_picked_for_Kmeans_idx <- colnames(cust_final)[c(seq(4,22))]
variables_picked_for_Kmeans_idx

cust <- cust_final[,variables_picked_for_Kmeans_idx, with=FALSE]



# check correlations
correlationMatrix <- cor(cust)
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.7)
# print indexes of highly correlated attributes
print(highlyCorrelated)

###### scale data

cust.sc <- scale(cust)


############# cluster test
# Compute Hopkins statistic for  dataset
# Hopkins statistic: If the value of Hopkins statistic is close to zero  
# then we can conclude that the dataset is significantly clusterable.

print(get_clust_tendency(cust.sc, graph=FALSE,n=nrow(cust.sc)-1,seed = 123))

# $hopkins_stat
#[1] 0.04806671

#############   Cluster model - kmeans

set.seed(9)  

wss <- 0
# Look over 1 to 10 possible clusters
for (i in 1:10) {
  km.out <- kmeans(cust.sc, centers = i, nstart = 20, iter.max = 50)
  wss[i] <- km.out$tot.withinss
}

# Produce a scree plot
plot(1:10, wss, type = "b", 
     xlab = "Number of Clusters", 
     ylab = "Within groups sum of squares")

eratio <- function(wss) {
  # Creates the eigenvalue ratio estimator for the number of clusters
  n <- NROW(wss)
  dss <- -diff(wss) # Create differences in wss (eigenvalues)
  dss <- c(wss[1]/log(n),dss) # Assign a zero case
  erat <- dss[1:(n-1)]/dss[2:n] # Build the eigenvalue ratio statistic
  gss <- log(1+dss/wss) # Create growth rates
  grat <- gss[1:(n-1)]/gss[2:n] # Calucluate the growth rate statistic
  return(c(which.max(erat),which.max(grat))) # Find the maximum number for each estimator
}

eratio(wss)  #suggested 1 cluster

# Select number of clusters
k <- 3

set.seed(888)
km.i <- kmeans(cust.sc, centers = k, nstart = 20, iter.max = 50)
#Get means
centers <- as.data.table(km.i$centers)
cluster_label <- km.i$cluster

#Put cluster label to original data

cust_final$segment <- cluster_label
clustered <- subset(cust_final, select = -c(CustomerNo,firstdate,lastdate) ) 

cust.means <- clustered[, lapply(.SD, mean), by=.(segment)]

ggplot(cust_final, aes(factor(segment), total_spend)) + 
  geom_boxplot(aes(fill = factor(segment))) +
  coord_cartesian(ylim=c(0, 3000000))

######### Cluster model NbClust

res <-NbClust(cust.sc, distance = "euclidean", min.nc=2, max.nc=10, method = "ward.D", index = "all")

res$All.index 
res$Best.nc 
res$All.CriticalValues 
res$Best.partition

# According to the majority rule, the best number of clusters is  3

#Put cluster label to original data

cust_final$segment2 <- res$Best.partition
clustered2 <- subset(cust_final, select = -c(CustomerNo,firstdate,lastdate, segment) ) 

cust.means2 <- clustered2[, lapply(.SD, mean), by=.(segment2)]

ggplot(cust_final, aes(factor(segment2), total_spend)) + 
  geom_boxplot(aes(fill = factor(segment2))) +
  coord_cartesian(ylim=c(0, 3000000))

######## Cluster model - mclust
str(cust.sc)
BIC <- mclustBIC(cust.sc)
mod1 <- Mclust(cust.sc, x = BIC)
summary(mod1, parameters = TRUE)
mod1$classification

cust_final$segment3 <- as.array(mod1$classification)
clustered3 <- subset(cust_final, select = -c(CustomerNo,firstdate,lastdate, segment, segment2) ) 

cust.means3 <- clustered3[, lapply(.SD, mean), by=.(segment3)]


ggplot(cust_final, aes(factor(segment3), total_spend)) + 
  geom_boxplot(aes(fill = factor(segment3))) +
  coord_cartesian(ylim=c(0, 3000000))

ggplot(cust_final, aes(factor(segment3), tenure)) + 
  geom_boxplot(aes(fill = factor(segment3)))

ggplot(cust_final, aes(factor(segment3), recency)) + 
  geom_boxplot(aes(fill = factor(segment3)))

ggplot(cust_final, aes(factor(segment3), spend_per_txn)) + 
  geom_boxplot(aes(fill = factor(segment3)))+
  coord_cartesian(ylim=c(0, 5000))

ggplot(cust_final, aes(factor(segment3), total_txns)) + 
  geom_boxplot(aes(fill = factor(segment3)))+
  coord_cartesian(ylim=c(0, 500))

###############################################################
#     Time Series
#
#  convert data to time series
head(sales2)
dailysales <- sales2[, .(dailysales=sum(SellQty)), by=TransactionDate]
ts.plot(dailysales$dailysales, xlab="Daily", ylab="Sales Quantity", main="Daily Sales Qty")

daily <- xts(dailysales$dailysales, dailysales$TransactionDate)
daily
names(daily) <- "sales"

dev.off()

#  -	No sales for Sundays
weekly <- apply.weekly(daily,sum)
plot(weekly)
summary(weekly)

# remove first and last week
weekly <- weekly[-1,]
weekly <- weekly[-146,]

plot(weekly)

##  shows there are trend and seasonal components
weekly_ts <- ts(weekly$sales, start=c(2015, 1), frequency=52) 
components.ts = decompose(weekly_ts)
plot(components.ts)

ggAcf(weekly, main ="original y")
ggAcf(diff(weekly), main ="first difference")

## plot acf and pacf - looks like MA(1) 
acf2(weekly)
acf2(diff(weekly))

## use prophet library that accounts for weekly and yearly seasonality and holidays

# convert xts data to data.frame
weekly.sales <- data.frame(Date = index(weekly), coredata(weekly) )
names(weekly.sales) <- c("ds", "y")
m.prophet <- prophet(weekly.sales)

# forecast
future <- make_future_dataframe(m.prophet, periods=52, freq = "week")
tail(future)
forecast.values <- predict(m.prophet, future)
plot(m.prophet, forecast.values)
prophet_plot_components(m.prophet, forecast.values)

######################## old time series code below
library("fUnitRoots")
urkpssTest(weekly, type = c("tau"), lags = c("short"),use.lag = 2, doplot = TRUE)
tsstationary = diff(weekly, differences=2)
plot(tsstationary)
#  need to difference data with 1 lag
acf2(tsstationary)

# MA(1) with differences=1

fit <- auto.arima(weekly)  
print(fit_RSq<-cor(fit$fitted,fit$x)^2)  #gets r-squared
checkresiduals(fit)  # checks for white noise
tsdisplay(weekly,lag.max = 90,plot.type = c("histogram"))
summary(fit)
coeftest(fit)
horizon <- seq(as.Date("2017-01-07"), length=41, by="weeks")
pred.auto <- forecast(fit, h=horizon)
plot(pred.auto$mean)

                 
########  forecast

# drop weeks with partial data
weekly <- weekly[-1,]
weekly <- weekly[-146,]

train_ts <- weekly[index(weekly) < '2017-01-01']
test_ts <- weekly[index(weekly) >= '2017-01-01']

model.ts <- auto.arima(train_ts)
forecast_ts <- forecast(model.ts, h=41)

for_dates <- seq(as.Date("2017-01-07"), length=41, by="weeks")
forecast_sales <- xts(forecast_ts$mean, order.by = for_dates)
class(forecast_sales)
plot(test_ts, main="Forecast Comparison")
lines(forecast_sales, col="red")
forecast_ts$mean

plot(test_ts)
lines(forecast_sales, col='red')


fitARIMA <- Arima(train_ts, order=c(0,1,1), seasonal=list(order=c(1,0,0), period=52))
coeftest(fitARIMA)
forecast.a <- predict(fitARIMA,n.ahead = 41)
forecast_ts <- ts(forecast.a, start=c(2017, 1), frequency=52) 

for_dates <- seq(as.Date("2017-01-07"), length=41, by="weeks")
forecast.a.ts <- xts(forecast_ts$pred, order.by = for_dates)
plot(test_ts)
lines(forecast.a.ts, col='red')


######################################################
#  Price elasticity

# join sales data with customer segments

sales3 <- subset(sales2, select = c(TransactionDate, TransNo, CustomerNo,Category,SubCategory,Part,SellPrice, SellQty, COGS, total_cogs, revenue) ) 
sales3<-merge(x=sales3,y=cust_final,by="CustomerNo",all.x=TRUE)
sales3 <- subset(sales3, select = c(TransactionDate, TransNo, CustomerNo,Category,SubCategory,Part,SellPrice, SellQty, COGS, total_cogs, revenue, segment3) ) 
write.csv(sales3,file='sales3.csv',row.names = F)

sales_el <- subset(sales2, select = c(TransactionDate, TransNo, CustomerNo,Category,SubCategory,Part,SellPrice, SellQty, COGS, total_cogs, revenue) ) 
sales_el<-merge(x=sales_el,y=cust_final,by="CustomerNo",all.x=TRUE)
sales_el <- subset(sales_el, select = c(TransactionDate, TransNo, CustomerNo,Category,SubCategory,Part,SellPrice, SellQty, COGS, total_cogs, revenue, segment3) ) 


####  need to remove rows with negative sales price or negative sales quantity
sales_el <- sales_el[sales_el$SellPrice > 0,]
sales_el <- sales_el[sales_el$SellQty > 0,]
sales_el$Category  <- as.factor(sales_el$Category)
sales_el$SubCategory  <- as.factor(sales_el$SubCategory)
sales_el$Part  <- as.factor(sales_el$Part)

write.csv(sales_el,file='elasticity.csv',row.names = F)

## get top selling product for each segment
head(sales_el)

Seg1 <- sales_el[sales_el$segment3==1,]
Seg2 <- sales_el[sales_el$segment3==2,]
Seg3 <- sales_el[sales_el$segment3==3,]
Seg4 <- sales_el[sales_el$segment3==4,]
Seg5 <- sales_el[sales_el$segment3==5,]
Seg6 <- sales_el[sales_el$segment3==6,]

Seg1sales  <- Seg1[,.(avgPrice=mean(SellPrice),
                            totalRev=sum(revenue),
                            totalQty=sum(SellQty)),
                         by=.(Category, Part) ]
# seg1 top selling part P500PA817 in CAT3002

Seg2sales  <- Seg2[,.(avgPrice=mean(SellPrice),
                      totalRev=sum(revenue),
                      totalQty=sum(SellQty)),
                   by=.(Category, Part)  ]
# seg2 top selling part P64PA16383  CAT3002


Seg3sales  <- Seg3[,.(avgPrice=mean(SellPrice),
                      totalRev=sum(revenue),
                      totalQty=sum(SellQty)),
                   by=.(Category, Part) ]
# seg3 top selling part P64PA16383/CAT3003 (avg $75) and P62PA30868/CAT3004 (avg $830)
mean(product_sales$avgPrice[product_sales$Part=='P62PA30868'])

Seg4sales  <- Seg4[,.(avgPrice=mean(SellPrice),
                      totalRev=sum(revenue),
                      totalQty=sum(SellQty)),
                   by=.(Category, Part) ]
# seg4 top selling part P62PA30811 CAT3004

Seg5sales  <- Seg5[,.(avgPrice=mean(SellPrice),
                      totalRev=sum(revenue),
                      totalQty=sum(SellQty)),
                   by=.(Category, Part) ]
# seg5 top selling part P230PA19583 CAT3001

Seg6sales  <- Seg6[,.(avgPrice=mean(SellPrice),
                      totalRev=sum(revenue),
                      totalQty=sum(SellQty)),
                   by=.(Category, Part) ]
# seg6 top selling part P385PA11996  	CAT2002	

m.Seg3.P62PA30868 <- lm(log(SellQty)~log(SellPrice), data=Seg3[Seg3$Part=='P62PA30868',])
summary(m.Seg3.P62PA30868)

mean(sales_el$SellPrice[sales_el$Part=='P62PA30868'])
m.Seg3.P64PA16383 <- lm(log(SellQty)~log(SellPrice), data=Seg3[Seg3$Part=='P64PA16383',])
summary(m.Seg3.P64PA16383)

m.Seg2.P64PA16383 <- lm(log(SellQty)~log(SellPrice), data=Seg2[Seg2$Part=='P64PA16383',])
summary(m.Seg2.P64PA16383)

m.Seg1.P500PA817 <- lm(log(SellQty)~log(SellPrice), data=Seg1[Seg1$Part=='P500PA817',])
summary(m.Seg1.P500PA817)

m.Seg4.P62PA30811 <- lm(log(SellQty)~log(SellPrice), data=Seg4[Seg4$Part=='P62PA30811',])
summary(m.Seg4.P62PA30811)

m.Seg5.P230PA19583 <- lm(log(SellQty)~log(SellPrice), data=Seg5[Seg5$Part=='P230PA19583',])
summary(m.Seg5.P230PA19583)

m.Seg6.P385PA11996 <- lm(log(SellQty)~log(SellPrice), data=Seg6[Seg6$Part=='P385PA11996',])
summary(m.Seg6.P385PA11996)


#####  Get price elasticity for group

m.Seg1<- lm(log(SellQty)~log(SellPrice)+Category, data=Seg1)
summary(m.Seg1)

m.Seg2<- lm(log(SellQty)~log(SellPrice)+Category, data=Seg2)
summary(m.Seg2)

m.Seg3<- lm(log(SellQty)~log(SellPrice)+Category, data=Seg3)
summary(m.Seg3)

m.Seg4 <- lm(log(SellQty)~log(SellPrice)+Category, data=Seg4)
summary(m.Seg4)

m.Seg5 <- lm(log(SellQty)~log(SellPrice)+Category, data=Seg5)
summary(m.Seg5)

m.Seg6 <- lm(log(SellQty)~log(SellPrice)+Category, data=Seg6)
summary(m.Seg6)



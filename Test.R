rm(list=ls())
library(plyr)
library(ggplot2)
library(dummy)
# train <- read.csv("/media/3TB/kag/rossman/data/train.csv")
# #yearMonthDate <- do.call(rbind,lapply(as.Date(train$Date),function(x){cbind(as.integer(format(x,"%Y")),as.integer(format(x,"%m")),as.integer(format(x,"%d")))}))
# load("/media/3TB/kag/rossman/yearmonthdate.RData")
# trainWODate <- train[,-which(grepl("Date",colnames(train)))]
# trainWODate <- cbind(trainWODate,yearMonthDate)
load("/media/3TB/kag/rossman/data/trainWithDates.rdata")
stores <- read.csv("/media/3TB/kag/rossman/data/store.csv")
colnames(trainWODate)[9:11] <- c("Year","Month","Day")
storeSalesMerged <- merge(trainWODate,stores,by=c("Store"))
normalizedSalesByMonthByStore <- ddply(storeSalesMerged,.(Store,Month),function(x){data.frame(NormalizedSalePerDay=mean(x$Sales/x$Customers,na.rm = T),
                                                                                         NormalizedSale=sum(x$Sales)/sum(x$Customers))}) 
# normalizedSalesByMonthByStore$Month <- as.factor(normalizedSalesByMonthByStore$Month)
# normalizedSalesByMonthByStore$Store <- as.factor(normalizedSalesByMonthByStore$Store)
# ggplot(normalizedSalesByMonthByStore,aes(x=Month,y=NormalizedSale,group=Store,color=Store))+geom_line()
# normalizedSalesByMonthByStoreType <- ddply(storeSalesMerged,.(StoreType,Month),function(x){data.frame(NormalizedSalePerDay=mean(x$Sales/x$Customers,na.rm = T),
#                                                                                          NormalizedSale=sum(x$Sales)/sum(x$Customers))}) 
# normalizedSalesByMonthByStoreType$Month <- as.factor(normalizedSalesByMonthByStoreType$Month)
# ggplot(normalizedSalesByMonthByStoreType,aes(x=Month,y=NormalizedSale,group=StoreType,color=StoreType))+geom_line()
# normalizedSalesByDayOfWeekByStoreType <- ddply(storeSalesMerged,.(StoreType,DayOfWeek),function(x){data.frame(NormalizedSalePerDay=mean(x$Sales/x$Customers,na.rm = T),
#                                                                                               NormalizedSale=sum(x$Sales)/sum(x$Customers))}) 
# normalizedSalesByDayOfWeekByStoreType$DayOfWeek <- as.factor(normalizedSalesByDayOfWeekByStoreType$DayOfWeek)
# normalizedSalesByDayOfWeekByStoreType$StoreType <- as.factor(normalizedSalesByDayOfWeekByStoreType$StoreType)
# ggplot(normalizedSalesByDayOfWeekByStoreType,aes(x=DayOfWeek,y=NormalizedSale,group=StoreType,color=StoreType))+geom_line()

#try predicting customers and scaling by mean sale/customre
colClasses <- lapply(storeSalesMerged,function(x){class(x)})
factorClasses <-which(grepl("factor",colClasses))
factorCols <- storeSalesMerged[,factorClasses]
storeSalesMergedWOFactors <- storeSalesMerged[,-factorClasses]
#factorDummies <- dummy(factorCols)
#storeSalesMergedWithDummies <- cbind(storeSalesMergedWOFactors,factorDummies)
#wDummies <- model.matrix()
lmFit <- glm(formula = log(I(Sales+1))~.,data=storeSalesMerged[,-which(grepl("Customers",colnames(storeSalesMerged)))])
rmse <- function(error)
{
  sqrt(mean(error^2))
}
lmRMSE <- rmse(lmFit$residuals)
test <- read.csv("/media/3TB/kag/rossman/data/test.csv")
yearMonthDateTest <- do.call(rbind,lapply(as.Date(test$Date),function(x){cbind(as.integer(format(x,"%Y")),as.integer(format(x,"%m")),as.integer(format(x,"%d")))}))
testWODate <- test[,-which(grepl("Date",colnames(test)))]
testWODate <- cbind(testWODate,yearMonthDateTest)
colnames(testWODate)[8:10] <- c("Year","Month","Day")
storeSalesMergedTest <- merge(testWODate,stores,by=c("Store"))
submission <- data.frame(Id=test$Id,Sales=NA)
submission$Sales <- exp(predict(lmFit,storeSalesMergedTest[,c(1,3:ncol(storeSalesMergedTest))]))

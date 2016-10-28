rm(list=ls())

library(xgboost)
library(zoo)
library(plyr)
set.seed(13)

startTime <- Sys.time()
factorsAsIntegers <- F
scriptName <- "LMPerStore"
set.seed(13)
os <- Sys.info()[["sysname"]]
nodename <- Sys.info()[["nodename"]]
trainFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/rossman/data/trainWithDates.rdata"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/rossman/data/trainWithDates.rdata"),
                           ("/media/3TB/kag/rossman/data/trainWithDates.rdata")))
load(trainFile)
print("Training data set loaded...")
storeFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/rossman/data/store.csv"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/rossman/data/store.csv"),
                           ("/media/3TB/kag/rossman/data/store.csv")))
stores <- read.csv(storeFile)
testFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/rossman/data/test.csv"),
                   ifelse(os=="Darwin",
                          ("/Users/sthiagar/Kaggle/rossman/data/test.csv"),
                          ("/media/3TB/kag/rossman/data/test.csv")))
test <- read.csv(testFile)
print("Test data set loaded")
outputFolder <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/rossman/result/"),
                       ifelse(os=="Darwin",
                              ("/Users/sthiagar/Kaggle/rossman/result/"),
                              ("/media/3TB/kag/rossman/result/")))
colnames(trainWODate)[9:11] <- c("Year","Month","Day")
# CompetitionOpenSince <- as.yearmon(paste(stores$CompetitionOpenSinceYear, 
#                                          stores$CompetitionOpenSinceMonth, sep = "-"))
# Promo2Since <- as.POSIXct(paste(stores$Promo2SinceYear, 
#                                 stores$Promo2SinceWeek, 1, sep = "-"),
#                           format = "%Y-%U-%u")
# stores <- stores[,!colnames(stores)%in% c("Promo2SinceYear","Promo2SinceWeek","CompetitionOpenSinceYear","CompetitionOpenSinceMonth")]
# stores <- cbind(stores,data.frame(CompetitionOpenSince=CompetitionOpenSince,Promo2Since=Promo2Since))
# train <- merge(trainWODate,stores,by=c("Store"))
train <- merge(trainWODate,stores)

train <- train[ which(train$Open=='1'),]
train <- train[ which(train$Sales!=0),]
train$HalfOfMonth <-ifelse(train$Day>15,2,1)
#sales distribution per store,month,dayofweek
# salesDistributionPerStore <- ddply(train,.(Store,Month,DayOfWeek,HalfOfMonth),function(x){data.frame(MeanSales=log(mean(x$Sales)),
#                                                                                                      MeanCustomers=log(mean(x$Customers)),
#                                                                                                      MeanSalesPerCustomer=log(sum(x$Sales)/sum(x$Customers)))})
salesDistributionPerStore <- ddply(train,.(Store,Month,DayOfWeek,HalfOfMonth),function(x){data.frame(MeanSales=log(mean(x$Sales)))})

# salesDistributionPerStorePerMonth <- ddply(train,.(Store,Month),function(x){data.frame(MeanSalesM=log(mean(x$Sales)),
#                                                                                                              MeanCustomersM=log(mean(x$Customers)),
#                                                                                                              MeanSalesPerCustomerM=log(sum(x$Sales)/sum(x$Customers)))})
# salesDistributionPerStorePerDayOfWeek <- ddply(train,.(Store,DayOfWeek),function(x){data.frame(MeanSalesDOW=log(mean(x$Sales)),
#                                                                                        MeanCustomersDOW=log(mean(x$Customers)),
#                                                                                        MeanSalesPerCustomerDOW=log(sum(x$Sales)/sum(x$Customers)))})
# salesDistributionPerStorePerHalfOfMonth <- ddply(train,.(Store,HalfOfMonth),function(x){data.frame(MeanSalesHOM=log(mean(x$Sales)),
#                                                                                        MeanCustomersHOM=log(mean(x$Customers)),
#                                                                                        MeanSalesPerCustomerHOM=log(sum(x$Sales)/sum(x$Customers)))})
#salesDistributionPerStore <- merge(salesDistributionPerStorePerMonth,salesDistributionPerStorePerDayOfWeek)
#salesDistributionPerStore <- merge(salesDistributionPerStore,salesDistributionPerStorePerHalfOfMonth)
train <- merge(train,salesDistributionPerStore,by=c("Store","Month","DayOfWeek","HalfOfMonth"))
yearMonthDateTest <- do.call(rbind,lapply(as.Date(test$Date),function(x){cbind(as.integer(format(x,"%Y")),as.integer(format(x,"%m")),as.integer(format(x,"%d")))}))
testWODate <- test[,-which(grepl("Date",colnames(test)))]
testWODate <- cbind(testWODate,yearMonthDateTest)
colnames(testWODate)[8:10] <- c("Year","Month","Day")
testWODate$HalfOfMonth <- ifelse(testWODate$Day>15,2,1)
test <- merge(testWODate,stores)
test1 <- merge(test,salesDistributionPerStore,by=c("Store","Month","DayOfWeek","HalfOfMonth"),all.x=T)
narows <- test1[is.na(test1$MeanSales),]
narows <- narows[,-which(colnames(narows)%in%c("MeanSales","MeanCustomers","MeanSalesPerCustomer"))]
nonNaRows <- test1[!is.na(test1$MeanSales),]
# imputedValues <- ddply(narows,.(Store,Month,DayOfWeek,HalfOfMonth),function(x){data.frame(MeanSales=log(mean(train[train$Store==x$Store &
#                                                                                                                      train$Month==x$Month &
#                                                                                                                      train$HalfOfMonth==x$HalfOfMonth,]$MeanSales)),
#                                                                                           MeanCustomers=log(mean(train[train$Store==x$Store &
#                                                                                                                          train$Month==x$Month &
#                                                                                                                          train$HalfOfMonth==x$HalfOfMonth,]$MeanCustomers)),
#                                                                                           MeanSalesPerCustomer=log(mean(train[train$Store==x$Store &
#                                                                                                                                 train$Month==x$Month &
#                                                                                                                                 train$HalfOfMonth==x$HalfOfMonth,]$MeanSalesPerCustomer)))})
imputedValues <- ddply(narows,.(Store,Month,DayOfWeek,HalfOfMonth),function(x){data.frame(MeanSales=log(mean(train[train$Store==x$Store &
                                                                                                                     train$Month==x$Month &
                                                                                                                     train$HalfOfMonth==x$HalfOfMonth,]$MeanSales)))})

mergeNARowsAndImputedValues <- merge(narows,imputedValues,by=c("Store","Month","DayOfWeek","HalfOfMonth"))
test1 <- rbind(nonNaRows,mergeNARowsAndImputedValues)

feature.names <- names(train)[c(1:4,8:ncol(train))]
cat("Feature Names\n")
feature.names

if(factorsAsIntegers){
  cat("assuming text variables are categorical & replacing them with numeric ids\n")
  for (f in feature.names) {
    if(f=="StoreType"){
      next
    }
    if (class(train[[f]])=="factor") {
      print(f)
      levels <- unique(c(train[[f]], test[[f]]))
      train[[f]] <- as.integer(factor(train[[f]], levels=levels))
      test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    }
  }
}
train[is.na(train)]   <- 0
test1[is.na(test1)]   <- 0
# cat("train data column names after slight feature engineering\n")
# names(train)
# cat("test data column names after slight feature engineering\n")
# names(test)
RMPSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  #   elab<-(as.numeric(labels))
  #   epreds<-(as.numeric(preds))
  #   print(length(which(epreds==0)))
  #   print(length(which(elab==0)))
  #   print(mean((epreds/elab-1)^2))
  err <- sqrt(mean((epreds/elab-1)^2))
  #print(err)
  return(list(metric = "RMPSE", value = err))
}
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
yearMonthDateTest <- do.call(rbind,lapply(as.Date(test$Date),function(x){cbind(as.integer(format(x,"%Y")),as.integer(format(x,"%m")),as.integer(format(x,"%d")))}))
testWODate <- test[,-which(grepl("Date",colnames(test)))]
testWODate <- cbind(testWODate,yearMonthDateTest)
colnames(testWODate)[8:10] <- c("Year","Month","Day")
testWODate$HalfOfMonth <- ifelse(testWODate$Day>15,2,1)
test <- merge(testWODate,stores)
testA <- test[test$StoreType=='a',]$Id
withStoreFeatures <- read.csv("/Users/sthiagar/Kaggle/rossman/result/XgBoostPerStoreWithSalesFeatures1_suresh-le_2015-11-0216_23_42 .csv")
withStoreFeaturesA <- withStoreFeatures[withStoreFeatures$Id %in% testA,]
baseline <- read.csv("/Users/sthiagar/Kaggle/rossman/result/XgBoostBaseline_2015-10-2414_34_50 .csv")
baseline <- baseline[!baseline$Id %in% testA,]
s <- rbind(baseline,withStoreFeaturesA)
#11168 for meanBaelineBDBaselineAC
write.csv(s,file="/Users/sthiagar/Kaggle/rossman/result/XgBoostBaselineBCDStoreFeaturesA.csv",row.names=F)
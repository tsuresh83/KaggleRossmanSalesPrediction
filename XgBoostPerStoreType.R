rm(list=ls())

library(xgboost)
library(zoo)
set.seed(13)

startTime <- Sys.time()
factorsAsIntegers <- F
scriptName<-ifelse(factorsAsIntegers,"XgBoostPerStoreTypeFactorsAsIntegers","XgBoostPerStoreType")
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
# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)



# There are some NAs in the integer columns so conversion to zero


# looking at only stores that were open in the train set
# may change this later
train <- merge(trainWODate,stores)

train <- train[ which(train$Open=='1'),]
train <- train[ which(train$Sales!=0),]

yearMonthDateTest <- do.call(rbind,lapply(as.Date(test$Date),function(x){cbind(as.integer(format(x,"%Y")),as.integer(format(x,"%m")),as.integer(format(x,"%d")))}))
testWODate <- test[,-which(grepl("Date",colnames(test)))]
testWODate <- cbind(testWODate,yearMonthDateTest)
colnames(testWODate)[8:10] <- c("Year","Month","Day")
test <- merge(testWODate,stores)

feature.names <- names(train)[c(1,2,5:ncol(train))]
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
test[is.na(test)]   <- 0
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

samples <- list()
storeTypesOrderInTrain <- character()
trainPerStoreType <- function(df){
  set.seed(13)
  print(paste("nrow",nrow(df),"store type",unique(df$StoreType)))
  h<-sample(nrow(df),0.2*nrow(df))
  samples[[as.character(unique(df$StoreType))]] <<- h
  storeTypesOrderInTrain <<- c(storeTypesOrderInTrain,as.character(unique(df$StoreType)))
  tra <- df[,feature.names]
  print(dim(tra))
  dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(df$Sales+1)[h])
  dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(df$Sales+1)[-h])
#   dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=(df$Sales)[h])
#   dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=(df$Sales)[-h])
  watchlist<-list(val=dval,train=dtrain)
  param <- list(  objective           = "reg:linear", 
                  booster = "gbtree",
                  eta                 = 0.02, # 0.06, #0.01,
                  max_depth           = 10, #changed from default of 8
                  subsample           = 0.9, # 0.7
                  colsample_bytree    = 0.7 # 0.7
                  #eval_metric = "rmse"
                  #num_parallel_tree   = 2
                  # alpha = 0.0001, 
                  # lambda = 1
  )
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 3000, #300, #280, #125, #250, # changed from 300
                      verbose             = 0,
                      early.stop.round    = 100,
                      watchlist           = watchlist,
                      maximize            = FALSE
                      ,feval=RMPSE
  )
  return(clf)
}
dataPerStoreType<- lapply(unique(train$StoreType),function(x){train[train$StoreType==x,]})
dataPerStoreTypeNames <- unlist(lapply(dataPerStoreType,function(x){unique(x$StoreType)}))
models <- lapply(dataPerStoreType,trainPerStoreType)
names(models) <- dataPerStoreTypeNames
names(dataPerStoreType) <- dataPerStoreTypeNames
predictPerStoreType <- function(df){
  Id=df$Id
  t <- df[,feature.names]
  return(data.frame(Id, Sales=exp(predict(models[[as.character(unique(t$StoreType))]],data.matrix(t)))-1))
}
dataPerStoreTypeTest<- lapply(unique(test$StoreType),function(x){test[test$StoreType==x,]})
submission <- do.call(rbind,lapply(dataPerStoreTypeTest,predictPerStoreType))
# submission <- data.frame()
# for(st in 1 : length(dataPerStoreTypeTest)){
#   submission <- rbind(submission,predictPerStoreType(dataPerStoreTypeTest[[st]]))
# }
prefix <- paste(outputFolder,scriptName,"_",nodename,"_",gsub(" ","",gsub(":","_",startTime)),sep="")
write.csv(submission, paste(prefix,".csv"),row.names = F)
save(list=ls(),file=paste(prefix,".rdata"))
#Store Type b has differential distribution per assortment
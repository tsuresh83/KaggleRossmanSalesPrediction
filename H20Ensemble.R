rm(list=ls())

library(h2o)
library(h2oEnsemble)
library(plyr)
set.seed(13)

startTime <- Sys.time()
scriptName<-"H2OEnsemble"
set.seed(13)
os <- Sys.info()[["sysname"]]
nodename <- Sys.info()[["nodename"]]
trainFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/rossman/data/train.csv"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/rossman/data/train.csv"),
                           ("/media/3TB/kag/rossman/data/train.csv")))
train <- read.csv(trainFile)
print("Training data set loaded...")
storeFile <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/rossman/data/store.csv"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/rossman/data/store.csv"),
                           ("/media/3TB/kag/rossman/data/store.csv")))
store <- read.csv(storeFile)
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
salesDist <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/rossman/result/salesDistributionPerStore.rdata"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/rossman/result/salesDistributionPerStore.rdata"),
                           ("/media/3TB/kag/rossman/result/salesDistributionPerStore.rdata")))
load(salesDist)
impValues <- ifelse(nodename=="bigtumor",("/home/tumor/MLExperimental/rossman/result/imputedValues.rdata"),
                    ifelse(os=="Darwin",
                           ("/Users/sthiagar/Kaggle/rossman/result/imputedValues.rdata"),
                           ("/media/3TB/kag/rossman/result/imputedValues.rdata")))
load(impValues)
memory <- ifelse(nodename=="bigtumor","128g",
                 ifelse(os=="Darwin","5g",
                        "50g"))
train <- merge(train,store)
test <- merge(test,store)


#feature.names <- names(train)[c(1,2,6:ncol(train))]


# looking at only stores that were open in the train set
# may change this later
train <- train[ which(train$Open=='1'),]
train <- train[ which(train$Sales!='0'),]
# seperating out the elements of the date column for the train set
train$month <- as.integer(format(as.Date(train$Date), "%m"))
train$year <- as.integer(format(as.Date(train$Date), "%Y"))
train$day <- as.integer(format(as.Date(train$Date), "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
train <- train[,-c(3)]

# seperating out the elements of the date column for the test set
test$month <- as.integer(format(as.Date(test$Date), "%m"))
test$year <- as.integer(format(as.Date(test$Date), "%Y"))
test$day <- as.integer(format(as.Date(test$Date), "%d"))

# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
test <- test[,-c(4)]
train$Weekend <- 0
train[train$DayOfWeek %in% c(6,7),]$Weekend  <- 1
train$PromoInterval <- as.character(train$PromoInterval)
train[train$PromoInterval=="",]$PromoInterval <-0
train[train$PromoInterval=="Feb,May,Aug,Nov",]$PromoInterval <-"2,5,8,11"
train[train$PromoInterval=="Jan,Apr,Jul,Oct",]$PromoInterval <-"1,4,7,10"
train[train$PromoInterval=="Mar,Jun,Sept,Dec",]$PromoInterval <-"3,6,9,12"
promo2 <- rep(F,nrow(train))
for(i in c(1:12)){
  promo2[grepl(paste("\\b",i,"\\b",sep=""),train$PromoInterval) & train$month==i & train$year>=train$Promo2SinceYear] <-T
}
train <- train[,-grep("PromoInterval",colnames(train))]
train <- cbind(train,Promo2On = promo2)
train$HalfOfMonth <-ifelse(train$Day>15,2,1)
storeSalesColNames <- colnames(salesDistributionPerStore)
storeSalesColNames[2] <- "month"
colnames(salesDistributionPerStore) <- storeSalesColNames
train <- merge(train,salesDistributionPerStore,by=c("Store","month","DayOfWeek","HalfOfMonth"))

#prepare test
test$Weekend <- 0
test[test$DayOfWeek %in% c(6,7),]$Weekend  <- 1
test$PromoInterval <- as.character(test$PromoInterval)
test[test$PromoInterval=="",]$PromoInterval <-0
test[test$PromoInterval=="Feb,May,Aug,Nov",]$PromoInterval <-"2,5,8,11"
test[test$PromoInterval=="Jan,Apr,Jul,Oct",]$PromoInterval <-"1,4,7,10"
test[test$PromoInterval=="Mar,Jun,Sept,Dec",]$PromoInterval <-"3,6,9,12"
promo2Test <- rep(F,nrow(test))
for(i in c(1:12)){
  promo2Test[grepl(paste("\\b",i,"\\b",sep=""),test$PromoInterval) & test$month==i & test$year>=test$Promo2SinceYear] <-T
}
test <- test[,-grep("PromoInterval",colnames(test))]
test <- cbind(test,Promo2On=promo2Test)
test$HalfOfMonth <-ifelse(test$Day>15,2,1)
test1 <- merge(test,salesDistributionPerStore,by=c("Store","month","DayOfWeek","HalfOfMonth"),all.x = T)
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
# imputedValues <- ddply(narows,.(Store,month,DayOfWeek,HalfOfMonth),function(x){data.frame(MeanSales=log(mean(train[train$Store==unique(x$Store) &
#                                                                                                                         train$month==unique(x$month) &
#                                                                                                                         train$HalfOfMonth==unique(x$HalfOfMonth),]$Sales)),
#                                                                                              MeanCustomers=log(mean(train[train$Store==unique(x$Store) &
#                                                                                                                             train$month==unique(x$month) &
#                                                                                                                             train$HalfOfMonth==unique(x$HalfOfMonth),]$Customers)),
#                                                                                              MeanSalesPerCustomer=log(sum(train[train$Store==unique(x$Store) &
#                                                                                                                                   train$month==unique(x$month) &
#                                                                                                                                   train$HalfOfMonth==unique(x$HalfOfMonth),]$Sales)/
#                                                                                                                         sum(train[train$Store==unique(x$Store) &
#                                                                                                                                     train$month==unique(x$month) &
#                                                                                                                                     train$HalfOfMonth==unique(x$HalfOfMonth),]$Customers)))})
mergeNARowsAndImputedValues <- merge(narows,imputedValues,by=c("Store","month","DayOfWeek","HalfOfMonth"))
test1 <- rbind(nonNaRows,mergeNARowsAndImputedValues)

train[is.na(train)]<-0
test1[is.na(test1)]<-0
#trainBkup <- train
#train <- train[sample(1:nrow(train),5000),]
feature.names <- names(train)[c(1:4,8:ncol(train))]
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
independent <- feature.names
dependent <- "Sales"
localH2O = h2o.init(nthreads=-1,max_mem_size = memory)
print(memory)
trainh20 <- as.h2o(train[,c(feature.names,dependent)],conn=localH2O)
h2o.randomForest.1 <- function(..., ntrees = 3000, max_depth=9) h2o.randomForest.wrapper(..., ntrees = ntrees, max_depth=max_depth)
h2o.gbm.1 <- function(..., ntrees = 2000, max_depth=10,min_rows=50,learn_rate=0.02) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth=max_depth,min_rows=min_rows,learn_rate=0.01)
h2o.gbm.2 <- function(..., ntrees = 3000, max_depth=8,min_rows=50,learn_rate=0.02) h2o.gbm.wrapper(..., ntrees = ntrees, max_depth=max_depth,min_rows=min_rows,learn_rate = 0.01)
# h20.glm = h2o.glm(x=independent, y=dependent, training_frame=part1h20, validation_frame = part2h20,family="binomial", standardize=F,
#                  lambda_search=TRUE)
# h2o.saveModel(my.glm,path = outputFolder)
h2o.deeplearning.1 <- function(..., hidden = c(500,500), activation = "Rectifier", seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
h2o.deeplearning.2 <- function(..., hidden = c(200,200,200), activation = "Tanh", seed = 1)  h2o.deeplearning.wrapper(..., hidden = hidden, activation = activation, seed = seed)
learner <- c("h2o.randomForest.1","h2o.gbm.1","h2o.gbm.2","h2o.deeplearning.1","h2o.deeplearning.2")
metalearner <- "h2o.deeplearning.wrapper"
#metalearner2 <- c("h2o.deeplearning.wrapper")
family <- "gaussian"
modelStart <- Sys.time()
fit <- h2o.ensemble(x = independent, y = dependent, 
                    training_frame = trainh20, 
                    family = family, 
                    learner = learner, 
                    metalearner = metalearner,
                    cvControl = list(V = 2, shuffle = TRUE))
testH20 <- as.h2o(test1[,feature.names],conn=localH2O)
pred <- predict.h2o.ensemble(fit,testH20)
submission <- data.frame(Id=test1$Id, Sales=as.data.frame(pred$pred)[,1])
write.csv(submission, paste(prefix,".csv"),row.names = F)
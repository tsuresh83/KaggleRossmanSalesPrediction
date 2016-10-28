# removing the date column (since elements are extracted) and also StateHoliday which has a lot of NAs (may add it back in later)
#10720 LB rmse @3000
#10728 LB rmse @5000 (4544)
#11104 LB rmse @6000 (overfitting)#addl features were all 0
#10903 LB rmse @3000 - addl features were correct
rm(list=ls())

library(xgboost)

set.seed(13)

startTime <- Sys.time()
scriptName<-"XgBoostBaselineWithAddlFeatures"
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
Promo2Cols <- data.frame()
for(i in c(1:12)){
  promo2 <- rep(0,nrow(train))
  promo2[grepl(paste("\\b",i,"\\b",sep=""),train$PromoInterval) & train$month==i & train$year>=train$Promo2SinceYear] <-1
  if(i==1){
    Promo2Cols <- promo2
  }else{
    Promo2Cols <- cbind(Promo2Cols,promo2)
  }
  
}
Promo2Cols <- as.data.frame(Promo2Cols)
colnames(Promo2Cols) <- paste("Promo2",c(1:12),sep="_")
train <- train[,-grep("PromoInterval",colnames(train))]
train <- cbind(train,Promo2Cols)
test$Weekend <- 0
test[test$DayOfWeek %in% c(6,7),]$Weekend  <- 1
test$PromoInterval <- as.character(test$PromoInterval)
test[test$PromoInterval=="",]$PromoInterval <-0
test[test$PromoInterval=="Feb,May,Aug,Nov",]$PromoInterval <-"2,5,8,11"
test[test$PromoInterval=="Jan,Apr,Jul,Oct",]$PromoInterval <-"1,4,7,10"
test[test$PromoInterval=="Mar,Jun,Sept,Dec",]$PromoInterval <-"3,6,9,12"
Promo2ColsTest <- data.frame()
for(i in c(1:12)){
  promo2 <- rep(0,nrow(test))
  promo2[grepl(paste("\\b",i,"\\b",sep=""),test$PromoInterval) & test$month==i & test$year>=test$Promo2SinceYear] <-1
  if(i==1){
    Promo2ColsTest <- promo2
  }else{
    Promo2ColsTest <- cbind(Promo2ColsTest,promo2)
  }
  
}
Promo2ColsTest <- as.data.frame(Promo2ColsTest)
colnames(Promo2ColsTest) <- paste("Promo2",c(1:12),sep="_")
test <- test[,-grep("PromoInterval",colnames(test))]
test <- cbind(test,Promo2ColsTest)
train[is.na(train)]<-0
test[is.na(test)]<-0
feature.names <- names(train)[c(1,2,6:ncol(train))]
feature.names

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("train data column names after slight feature engineering\n")
names(train)
cat("test data column names after slight feature engineering\n")
names(test)
tra<-train[,feature.names]
RMPSE<- function(preds, dtrain) {
  labels <- getinfo(dtrain, "label")
  elab<-exp(as.numeric(labels))-1
  epreds<-exp(as.numeric(preds))-1
  err <- sqrt(mean((epreds/elab-1)^2))
  return(list(metric = "RMPSE", value = err))
}
nrow(train)
h<-sample(nrow(train),10000)

dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=log(train$Sales+1)[h])
dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=log(train$Sales+1)[-h])
watchlist<-list(val=dval,train=dtrain)
param <- list(  objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.02, # 0.06, #0.01,
                max_depth           = 10, #changed from default of 8
                subsample           = 0.5, # 0.7
                colsample_bytree    = 0.7 # 0.7
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
                    maximize            = FALSE,
                    feval=RMPSE
)
pred1 <- exp(predict(clf, data.matrix(test[,feature.names]))) -1
prefix <- paste(outputFolder,scriptName,"_",nodename,"_",gsub(" ","",gsub(":","_",startTime)),sep="")
submission <- data.frame(Id=test$Id, Sales=pred1)
write.csv(submission, paste(prefix,".csv"),row.names = F)
save(list=ls(),file=paste(prefix,".rdata"))
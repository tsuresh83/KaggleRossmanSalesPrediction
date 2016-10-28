rm(list=ls())

library(xgboost)

set.seed(13)

startTime <- Sys.time()
scriptName<-"PerStoreLM"
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
# Promo2Cols <- data.frame()
# for(i in c(1:12)){
#   promo2 <- rep(0,nrow(train))
#   promo2[grepl(paste("\\b",i,"\\b",sep=""),train$PromoInterval) & train$month==i & train$year>=train$Promo2SinceYear] <-1
#   if(i==1){
#     Promo2Cols <- promo2
#   }else{
#     Promo2Cols <- cbind(Promo2Cols,promo2)
#   }
#   
# }
# Promo2Cols <- as.data.frame(Promo2Cols)
# colnames(Promo2Cols) <- paste("Promo2",c(1:12),sep="_")
train <- train[,-grep("PromoInterval",colnames(train))]
train <- cbind(train,Promo2On = promo2)
test$Weekend <- 0
test[test$DayOfWeek %in% c(6,7),]$Weekend  <- 1
test$PromoInterval <- as.character(test$PromoInterval)
test[test$PromoInterval=="",]$PromoInterval <-0
test[test$PromoInterval=="Feb,May,Aug,Nov",]$PromoInterval <-"2,5,8,11"
test[test$PromoInterval=="Jan,Apr,Jul,Oct",]$PromoInterval <-"1,4,7,10"
test[test$PromoInterval=="Mar,Jun,Sept,Dec",]$PromoInterval <-"3,6,9,12"
# Promo2ColsTest <- data.frame()
# for(i in c(1:12)){
#   promo2 <- rep(0,nrow(test))
#   promo2[grepl(paste("\\b",i,"\\b",sep=""),test$PromoInterval) & test$month==i & test$year>=test$Promo2SinceYear] <-1
#   if(i==1){
#     Promo2ColsTest <- promo2
#   }else{
#     Promo2ColsTest <- cbind(Promo2ColsTest,promo2)
#   }
#   
# }
# Promo2ColsTest <- as.data.frame(Promo2ColsTest)
# colnames(Promo2ColsTest) <- paste("Promo2",c(1:12),sep="_")
promo2Test <- rep(F,nrow(test))
for(i in c(1:12)){
  promo2Test[grepl(paste("\\b",i,"\\b",sep=""),test$PromoInterval) & test$month==i & test$year>=test$Promo2SinceYear] <-T
}
test <- test[,-grep("PromoInterval",colnames(test))]
test <- cbind(test,Promo2On=promo2Test)
train[is.na(train)]<-0
test[is.na(test)]<-0
feature.names <- names(train)[c(1,2,6:ncol(train))]
feature.names
# 
# for (f in feature.names) {
#   if (class(train[[f]])=="character") {
#     levels <- unique(c(train[[f]], test[[f]]))
#     train[[f]] <- as.integer(factor(train[[f]], levels=levels))
#     test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
#   }
# }
store1And3 <- train[train$Store %in% c(1,3),]
store1And3 <- store1And3[,-grep("Customers",colnames(store1And3))]
fits <- regsubsets(Sales~.-Store,store1And3[store1And3$Store==1,],method="forward")
summ <- summary(fits)
x <- model.matrix(Sales~.,data=store1And3[store1And3$Store==1,])
y<- store1And3[store1And3$Store==1,]$Sales
library(glmnet)
grid = 10^seq(10,-2,length=100)
lassomodel <- glmnet(x,y,alpha=1,lambda=grid)
cv.out = cv.glmnet(x,y,alpha=1)
bestlambda <- cv.out$lambda.min
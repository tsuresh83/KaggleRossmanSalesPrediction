rm(list=ls())
load("/media/3TB/kag/rossman/result/XgBoostPerStoreType_suresh-le_2015-10-2314_28_14_nrounds6K_depth8.rdata")
names(dataPerStoreType) <- as.character(dataPerStoreTypeNames)
getEvalSet <- function(df){
  return(df[unlist(samples[[unique(as.character(df$StoreType))]]),])
}
evalSetDF <- do.call(rbind,lapply(dataPerStoreType,function(x){getEvalSet(x)}))
evalSetDF$Id <- 1:nrow(evalSetDF)
evalSet <- lapply(unique(evalSetDF$StoreType),function(x){evalSetDF[evalSetDF$StoreType==x,]} )

pr <- do.call(rbind,lapply(evalSet,predictPerStoreType))
mergedEvalSetResult <- merge(evalSetDF,pr,by="Id")
mergedEvalSetResult <- mergedEvalSetResult[,c("Id","StoreType","Sales.x","Sales.y")]
colnames(mergedEvalSetResult) <- c("Id","StoreType","Act","Est")
err <- function(est,act){ 
  sqrt(mean((est/act-1)^2))
}
rmse <-ddply(mergedEvalSetResult,.(StoreType),function(x){data.frame(rmse=err(x$Est,x$Act))})
print(mean(rmse$rmse))
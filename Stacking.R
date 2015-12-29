setwd("/home/svu/e0008572/Walmart/7dec")

sample_pred_xgb <- read.csv("G:/Geek World/Kaggle/Walmart/7th Dec/models/New data sim mat refined/xgb/22218 iterations/submission_sample_xgb_075_3_005_best.csv")

head(sample_pred_xgb)



train <- read.csv("G:/Geek World/Kaggle/Walmart/Input/train.csv")

actuals <- train[which(train$VisitNumber %in% sample_pred_xgb$VisitNumber), c("VisitNumber", "TripType")] 
actuals <- actuals[order(actuals$VisitNumber),]
actuals <- unique(actuals)

set.seed(321)

load("G:/Geek World/Kaggle/Walmart/7th Dec/Similarity_matrix/train449_plus_fine_rep_upc.RData")
load("G:/Geek World/Kaggle/Walmart/7th Dec/features_500_all_models.RData")

train449_plus_fine_rep_upc <- train449_plus_fine_rep_upc[which(train449_plus_fine_rep_upc$VisitNumber %in% sample_pred_xgb$VisitNumber), c("TripType", "VisitNumber", feature.names)]
train449_plus_fine_rep_upc <- train449_plus_fine_rep_upc[order(train449_plus_fine_rep_upc$VisitNumber, decreasing = FALSE),]
summary(train449_plus_fine_rep_upc$VisitNumber)

train_pred2 <- cbind.data.frame(train449_plus_fine_rep_upc, sample_pred_xgb[,-1])

head(sample_pred_xgb[,1],100)
head(train449_plus_fine_rep_upc[,2],100)

train_pred2 <- as.data.frame(lapply(train_pred2, function(x) ifelse(is.na(x), 0,x)))
which(is.na(train_pred2) == TRUE)

library(randomForest)
rf_train2 <- randomForest(as.factor(TripType) ~ ., data=train_pred2, importance=TRUE, ntree = 25, do.trace = 1)


save(rf_train, file = "rftrain_75_sim_mat.RData")


load("test449_plus_fine_rep_upc.RData")
test <- test449_plus_fine_rep_upc  
rm(test449_plus_fine_rep_upc)


load("G:/Geek World/Kaggle/Walmart/7th Dec/models/New data sim mat refined/xgb/22218 iterations/submission_xgb_075_3_005_best_fulldata_s65561.csv")

test <- as.data.frame(lapply(test, function(x) ifelse(is.na(x), 0,x)))

`%ni%` <- Negate(`%in%`) 

rf_test <- predict(rf_train, newdata = test[ ,feature.names], type = "prob")
save(rf_test, file = "rf_test_75_sim_mat.RData")

write.csv(rf_test, "rf_75_sim_mat_fulldata.csv")





setwd("G:/Geek World/Kaggle/Walmart/7th Dec/models/xgb/sim mat modified")


# 
# LogLoss<-function(actual, predicted)
# {
#   y_pred <- rep(0,nrow(actual))
#   i <- 1 
#   
#   for(i in 1 : nrow(actual)){
#     y_pred[i] <- sum(actual[i,] * log(predicted[i,]))
#   }
#   result <- -1/nrow(actual) * sum(y_pred)
#   
#   return(result)
# }

G:\Geek World\Kaggle\Walmart\7th Dec\models\New data sim mat refined\rf tuned\New

upsample <- read.csv("G:/Geek World/Kaggle/Walmart/7th Dec/models/xgb/submission_sample_xgb_075_3_005_5k.csv")
sample1 <- read.csv("G:/Geek World/Kaggle/Walmart/7th Dec/models/New data sim mat refined/xgb/submission_sample_xgb_075_3_005_best.csv")
sample2 <- read.csv("G:/Geek World/Kaggle/Walmart/2nd Dec data/Models/xgb_model/submission_sample_xgb_075_3_005_best.csv")
#load("G:/Geek World/Kaggle/Walmart/2nd Dec data/Models/nnet/prediction_vector_nnet.RData")

#load("G:/Geek World/Kaggle/Walmart/7th Dec/Models/New data sim mat refined/rf tuned/New/rftest_tuned_25sample.RData")

load("G:/Geek World/Kaggle/Walmart/7th Dec/Models/New data sim mat refined/rftest_566var_5dec.RData")

head(rf_test)

dim(sample1)
dim(upsample)
dim(nnet)


train <- read.csv("G:/Geek World/Kaggle/Walmart/Input/train.csv")

summary(sample1$VisitNumber)

actuals <- train[which(train$VisitNumber %in% rf_test$VisitNumber), c("VisitNumber", "TripType")] 
actuals <- actuals[order(actuals$VisitNumber),]

library(reshape2)
library(plyr)
actuals <- dcast(actuals, formula = VisitNumber ~ TripType, value.var = "VisitNumber", fun.aggregate = function(x) length(unique(x))) 
head(actuals)
head(rf_test)

colnames(actuals) <- colnames(rf_test)

# unique(actuals$TripType)
# unique(train$TripType)

actuals_m <- as.matrix(actuals[,-1])


pred_2_m <- as.matrix(sample1[,-1])
pred_3_m <- as.matrix(sample2[,-1])
pred_rf <- as.matrix(rf_test[,-1])

head(pred_2_m)
head(pred_1_m)
head(pred_rf)
head(actuals_m)

weights_list <- c(weight1 = 0, weight2 = 0, weight3 = 0, logloss = 999)

#weights_list <- c(weight1 = 0, logloss = 999)

library(MLmetrics)
LogLoss(actuals_m,pred_2_m)
LogLoss(actuals_m,pred_3_m)
LogLoss(actuals_m,pred_rf)

i <- 0

for(i in seq(0.5,1,0.01)) {
  for(j in seq(0,0.3,0.01)) {
      for(k in seq(0,0.2,0.01)){
        
        if((i + j + k) == 1) {
        ensemble_pred <- i * pred_2_m + j  * pred_3_m + k * pred_rf
        logloss <- LogLoss(actuals_m, ensemble_pred)
        weights_list <- rbind.data.frame(weights_list, data.frame(weight1 = i, weight2 = j, weight3 = k, logloss = logloss))
        #weights_list <- rbind.data.frame(weights_list, data.frame(weight1 = i, logloss = logloss))  
        }
      }
  }
}

weights_list[282,]

which.min(weights_list$logloss) 






sample1 <- read.csv("G:/Geek World/Kaggle/Walmart/7th Dec/models/New data sim mat refined/xgb/submission_xgb_075_3_005_best_fulldata_s65749.csv")
sample2 <- read.csv("G:/Geek World/Kaggle/Walmart/2nd Dec data/Models/xgb_model/submission_xgb_075_3_005_best_fulldata_s067486.csv")


head(sample1)

pred_2_m <- as.matrix(sample1[,-1])
pred_3_m <- as.matrix(sample2[,-1])

ensemble_pred <- 0.7 * pred_2_m + 0.3  * pred_3_m
  
ensemble_pred_df <- cbind.data.frame(VisitNumber = sample1$VisitNumber, data.frame(ensemble_pred))
head(ensemble_pred_df)

write.csv(ensemble_pred_df, file = "G:/Geek World/Kaggle/Walmart/7th Dec/models/New data sim mat refined/xgb/submission_674_657xgb_ensemble_7_3.csv", row.names = FALSE)

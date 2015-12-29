# This is for the data with the similarity matrix calculated for both percentage and absolute values of categories bought

setwd("/home/svu/e0008572/Walmart/7dec")

require(xgboost, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")
require(splitstackshape, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")

#setwd("G:/Geek World/Kaggle/Walmart/2nd Dec data")
load("train449_plus_fine_rep_upc.RData")
train <- train449_plus_fine_rep_upc
rm(train449_plus_fine_rep_upc)
#head(train)


# Ordering trip types
x <- train[,1:2]
y1 <- unique(x$TripType)
y <- rank(y1)
y <- y - 1

z <- as.data.frame(cbind(y1,RankedTrip = y))
merged <- merge.data.frame(x,z,by.x = "TripType", by.y = "y1",all.x = TRUE)

train$TripType <- merged$RankedTrip

rm(x,y,y1,z,merged)
# --------------------------


train <- as.data.frame(lapply(train, function(x) ifelse(is.na(x), 0,x)))
which(is.na(train) == TRUE)


`%ni%` <- Negate(`%in%`) 

feature.names <- names(train)[which(names(train) %ni% c("TripType", "VisitNumber", "perc_t_HEALTH_AND_BEAUTY_AIDS", "t_HEALTH_AND_BEAUTY_AIDS", "b_HEALTH_AND_BEAUTY_AIDS", "perc_b_HEALTH_AND_BEAUTY_AIDS", "r_HEALTH_AND_BEAUTY_AIDS", "perc_r_HEALTH_AND_BEAUTY_AIDS", "t_99999"))]

gc()

set.seed(321)
train_sample <- as.data.frame(stratified(train, group = "TripType", size = 0.75, replace = FALSE))
train_label <- train_sample$TripType

dtrain   <- xgb.DMatrix(data.matrix(train_sample[,feature.names]), label=train_label)
gc()


cat("set parameters\n")
ds <- c(3)
lambdas <- c(0.005)
d.size <- length(ds)
l.size <- length(lambdas)

n.opt = 30000

tune.out <- data.frame()
for (i in 1:d.size) {
  for (j in 1:l.size) {
    d <- ds[i]
    lambda <- lambdas[j]
    for (n in c(30000)) {
      set.seed(321)
      gbm.mod <- xgb.cv(data = dtrain, num_class = 38 ,nround=n, nfold = 5, metrics="mlogloss",
                        max.depth =d, eta = lambda, objective = "multi:softprob")
      
      #Creating matrix of logloss errors
      xgb_cv_perf <- matrix(as.numeric(unlist(gbm.mod)), n ,4)
      # 3rd column gives logloss error for test data
      n.opt <- which.min(xgb_cv_perf[,3])
      
      cat("n =", n, " n.opt =", n.opt, "\n")
      if (n.opt / n < 0.95) break
    }
    cv.err <- xgb_cv_perf[n.opt,3]
    out <- data.frame(d=d, lambda=lambda, n=n, n.opt=n.opt, cv.err=cv.err)
    print(out)
    tune.out <- rbind(tune.out, out)
  }
}

save(tune.out, file = "tune_out032_sample.RData")



set.seed(321)
xgb_fit <- xgb.train(data = dtrain, 
                     num_class = 38 ,
                     nround = n.opt, 
                     metrics="mlogloss",
                     max.depth = 3, 
                     eta = 0.005,
                     verbose = TRUE,
                     objective = "multi:softprob")

save(xgb_fit, file = "xgb_075_3_005_best_trained.RData")



#-----------------------PREDICTION ----------------
test_sample <- train[which(train$VisitNumber %ni% train_sample$VisitNumber),]
test_sample <- test_sample[order(test_sample$VisitNumber),]

prediction_vector <- predict(xgb_fit, data.matrix(test_sample[ ,feature.names]))

m <- as.data.frame(matrix(prediction_vector,nrow = nrow(test_sample),ncol = 38, byrow = TRUE))
submission_sample <- cbind.data.frame(VisitNumber = test_sample$VisitNumber, m)

submission_sample <- submission_sample[order(submission_sample$VisitNumber),]

colnames(submission_sample) <- c('VisitNumber',	'TripType_3',	'TripType_4',	'TripType_5',	'TripType_6',	'TripType_7',	'TripType_8',	'TripType_9',	'TripType_12',	'TripType_14',	'TripType_15',	'TripType_18',	'TripType_19',	'TripType_20',	'TripType_21',	'TripType_22',	'TripType_23',	'TripType_24',	'TripType_25',	'TripType_26',	'TripType_27',	'TripType_28',	'TripType_29',	'TripType_30',	'TripType_31',	'TripType_32',	'TripType_33',	'TripType_34',	'TripType_35',	'TripType_36',	'TripType_37',	'TripType_38',	'TripType_39',	'TripType_40',	'TripType_41',	'TripType_42',	'TripType_43',	'TripType_44',	'TripType_999')
write.csv(submission_sample, "submission_sample_xgb_075_3_005_best.csv",row.names = FALSE)


#----------------Prediction on Test -------------------------

load("test449_plus_fine_rep_upc.RData")
test <- test449_plus_fine_rep_upc
rm(test449_plus_fine_rep_upc)

test <- as.data.frame(lapply(test, function(x) ifelse(is.na(x), 0,x)))

prediction_vector <- predict(xgb_fit, data.matrix(test[ ,feature.names]))
m <- as.data.frame(matrix(prediction_vector,nrow = nrow(test),ncol = 38, byrow = TRUE))
submission <- cbind.data.frame(VisitNumber = test$VisitNumber, m)

save(submission, file = "submission_xgb_075_3_005_best_fulldata.Ddata")
#colnames(submission) <- c('VisitNumber',	'TripType_3',	'TripType_4',	'TripType_5',	'TripType_6',	'TripType_7',	'TripType_8',	'TripType_9',	'TripType_12',	'TripType_14',	'TripType_15',	'TripType_18',	'TripType_19',	'TripType_20',	'TripType_21',	'TripType_22',	'TripType_23',	'TripType_24',	'TripType_25',	'TripType_26',	'TripType_27',	'TripType_28',	'TripType_29',	'TripType_30',	'TripType_31',	'TripType_32',	'TripType_33',	'TripType_34',	'TripType_35',	'TripType_36',	'TripType_37',	'TripType_38',	'TripType_39',	'TripType_40',	'TripType_41',	'TripType_42',	'TripType_43',	'TripType_44',	'TripType_999')
write.csv(submission, "submission_xgb_075_3_005_best_fulldata.csv",row.names = FALSE)


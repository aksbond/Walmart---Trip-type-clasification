setwd("/home/svu/e0008572/Walmart/21dec")

require(Rtsne, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")
require(xgboost, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")
require(splitstackshape, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")

load("train_13k_true.RData")

length(unique(train$VisitNumber))
nrow(train)
length(train)

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


`%ni%` <- Negate(`%in%`) 

feature.names <- names(train)[which(names(train) %ni% c("TripType", "VisitNumber", "perc_t_HEALTH_AND_BEAUTY_AIDS", "t_HEALTH_AND_BEAUTY_AIDS", "b_HEALTH_AND_BEAUTY_AIDS", "perc_b_HEALTH_AND_BEAUTY_AIDS", "r_HEALTH_AND_BEAUTY_AIDS", "perc_r_HEALTH_AND_BEAUTY_AIDS", "t_99999"))]


keep_features <- names(train[1:580])
keep_features <- keep_features[which(keep_features %ni% c("TripType", "VisitNumber", "perc_t_HEALTH_AND_BEAUTY_AIDS", "t_HEALTH_AND_BEAUTY_AIDS", "b_HEALTH_AND_BEAUTY_AIDS", "perc_b_HEALTH_AND_BEAUTY_AIDS", "r_HEALTH_AND_BEAUTY_AIDS", "perc_r_HEALTH_AND_BEAUTY_AIDS", "t_99999"))]
keep_features <- keep_features[ which(keep_features %ni% keep_features[which(regexpr("b_*|t_*|r_*|u_*",keep_features) == 1)])]

keep_features

tsne_sample <- train[, keep_features]

load("tsne_train_test_best.RData")

tsne_pred <- as.data.frame(tsne$Y)
colnames(tsne_pred) <- c("tsnex", "tsney")

#SUBSET TSNE
train <- cbind.data.frame(train, tsne_pred[1:95674,]) 

feature.names <- names(train)[which(names(train) %ni% c("TripType", "VisitNumber", "perc_t_HEALTH_AND_BEAUTY_AIDS", "t_HEALTH_AND_BEAUTY_AIDS", "b_HEALTH_AND_BEAUTY_AIDS", "perc_b_HEALTH_AND_BEAUTY_AIDS", "r_HEALTH_AND_BEAUTY_AIDS", "perc_r_HEALTH_AND_BEAUTY_AIDS", "t_99999"))]


set.seed(321)
train_label <- train$TripType
dtrain   <- xgb.DMatrix(data.matrix(train[,feature.names]), label=train_label)
gc()

n.opt <- 7500

set.seed(321)
xgb_fit <- xgb.train(data = dtrain, 
                     num_class = 38 ,
                     nround = n.opt, 
                     metrics="mlogloss",
                     max.depth = 6, 
                     eta = 0.005,
                     verbose = TRUE,
                     objective = "multi:softprob")

save(xgb_fit, file = "xgb_full_tsne_7500_trained.RData")


#----------------Prediction on Test -------------------------
load("test_13k.RData")

test <- cbind.data.frame(test, tsne_pred[95675:nrow(tsne_pred),]) 

prediction_vector <- predict(xgb_fit, data.matrix(test[ ,feature.names]))
m <- as.data.frame(matrix(prediction_vector,nrow = nrow(test),ncol = 38, byrow = TRUE))
submission <- cbind.data.frame(VisitNumber = test$VisitNumber, m)

save(submission, file = "submission_xgb_full_6_tsne_best_fulldata.RData")
colnames(submission) <- c('VisitNumber',	'TripType_3',	'TripType_4',	'TripType_5',	'TripType_6',	'TripType_7',	'TripType_8',	'TripType_9',	'TripType_12',	'TripType_14',	'TripType_15',	'TripType_18',	'TripType_19',	'TripType_20',	'TripType_21',	'TripType_22',	'TripType_23',	'TripType_24',	'TripType_25',	'TripType_26',	'TripType_27',	'TripType_28',	'TripType_29',	'TripType_30',	'TripType_31',	'TripType_32',	'TripType_33',	'TripType_34',	'TripType_35',	'TripType_36',	'TripType_37',	'TripType_38',	'TripType_39',	'TripType_40',	'TripType_41',	'TripType_42',	'TripType_43',	'TripType_44',	'TripType_999')
write.csv(submission, "submission_xgb_full_6_tsne_7500_fulldata.csv",row.names = FALSE)


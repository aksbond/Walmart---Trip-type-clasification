setwd("/home/svu/e0008572/Walmart/2dec")

require(reshape2, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")
require(plyr, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")


`%ni%` <- Negate(`%in%`) 

#setwd("G:/Geek World/Kaggle/Walmart")
train <- read.csv("/home/svu/e0008572/Walmart/Input/train.csv", stringsAsFactors = FALSE)


week_days <- cbind.data.frame(week_num = c(1,2,3,4,5,6,7), Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
train <- merge.data.frame(train, week_days, by.x = "Weekday", by.y = "Weekday")
head(train)
train$Weekday <- train$week_num
head(train)
train <- train[,c(2,3,1,4,5,6,7)]
str(train)
# returns column  
train <- mutate(train, Returns = ifelse(ScanCount < 0, abs(ScanCount),0))

# Treating NAs 
train[which(is.na(train$FinelineNumber)==TRUE), "FinelineNumber"] <- 99999


cat("Aggregation time ..... \n")

# MAIN CATEGORIES TRANSACTED
# Find total number of MAIN categories transacted 
train_main_cat_transacted <- aggregate(x = train[,6], by = list(train$TripType,train$VisitNumber,train$Weekday), FUN = function(x) length(unique(x)))
head(train_main_cat_transacted)
colnames(train_main_cat_transacted) <- c("TripType", "VisitNumber", "Weekday", "Dist_main_Cat_transacted")
dim(train_main_cat_transacted)

# SUB CATEGORIES transacted
# Find total number of sub categories transacted -- Remove RETURN entries
train_sub_cat_transacted <- aggregate(x = train[,7], by = list(train$TripType,train$VisitNumber,train$Weekday), FUN = function(x) length(unique(x)))
head(train_sub_cat_transacted)
colnames(train_sub_cat_transacted) <- c("TripType", "VisitNumber", "Weekday", "Dist_sub_Cat_transacted")
dim(train_sub_cat_transacted)

# # UPCs transacted
# Find total number of UPCs transacted - Remove RETURN entries
train_upc_transacted <- aggregate(x = train[,4], by = list(train$TripType,train$VisitNumber,train$Weekday), FUN = function(x) length(unique(x)))
head(train_upc_transacted)
colnames(train_upc_transacted) <- c("TripType", "VisitNumber", "Weekday", "Dist_upc_transacted")
dim(train_upc_transacted)


# Merge Cateogry datasets
agg_main_sub_dist_trans <- merge.data.frame(train_main_cat_transacted, train_sub_cat_transacted, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist_trans <- merge.data.frame(train_upc_transacted, agg_main_sub_dist_trans, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
dim(agg_main_sub_upc_dist_trans)


# MAIN CATEGORIES BOUGHT
# Find total number of MAIN categories bought -- Remove RETURN entries
train_no_return <- train[which(train$ScanCount > 0), ]
train_main_cat_bought <- aggregate(x = train_no_return[,6], by = list(train_no_return$TripType,train_no_return$VisitNumber,train_no_return$Weekday), FUN = function(x) length(unique(x)))
head(train_main_cat_bought)
colnames(train_main_cat_bought) <- c("TripType", "VisitNumber", "Weekday", "Dist_main_Cat_bought")
dim(train_main_cat_bought)

# SUB CATEGORIES BOUGHT
# Find total number of sub categories bought -- Remove RETURN entries
train_sub_cat_bought <- aggregate(x = train_no_return[,7], by = list(train_no_return$TripType,train_no_return$VisitNumber,train_no_return$Weekday), FUN = function(x) length(unique(x)))
head(train_sub_cat_bought)
colnames(train_sub_cat_bought) <- c("TripType", "VisitNumber", "Weekday", "Dist_sub_Cat_bought")
dim(train_sub_cat_bought)

# # UPCs BOUGHT
# Find total number of UPCs bought - Remove RETURN entries
train_upc_bought <- aggregate(x = train_no_return[,4], by = list(train_no_return$TripType,train_no_return$VisitNumber,train_no_return$Weekday), FUN = function(x) length(unique(x)))
head(train_upc_bought)
colnames(train_upc_bought) <- c("TripType", "VisitNumber", "Weekday", "Dist_upc_bought")
dim(train_upc_bought)


# NOW FOR RETURN ITEMS----------------

# MAIN CATEGORIES RETURNED
# Find total number of MAIN categories bought -- Remove RETURN entries
train_return <- train[which(train$ScanCount < 0), ]
#head(train_return)
train_main_cat_return <- aggregate(x = train_return[,6], by = list(train_return$TripType,train_return$VisitNumber,train_return$Weekday), FUN = function(x) length(unique(x)))
head(train_main_cat_return)
colnames(train_main_cat_return) <- c("TripType", "VisitNumber", "Weekday", "Dist_main_Cat_Return")
dim(train_main_cat_return)

# SUB CATEGORIES return
# Find total number of sub categories return -- Remove RETURN entries
train_sub_cat_return <- aggregate(x = train_return[,7], by = list(train_return$TripType,train_return$VisitNumber,train_return$Weekday), FUN = function(x) length(unique(x)))
head(train_sub_cat_return)
colnames(train_sub_cat_return) <- c("TripType", "VisitNumber", "Weekday", "Dist_sub_Cat_Return")
dim(train_sub_cat_return)

# # UPCs return
# Find total number of UPCs return - Remove RETURN entries
train_upc_return <- aggregate(x = train_return[,4], by = list(train_return$TripType,train_return$VisitNumber,train_return$Weekday), FUN = function(x) length(unique(x)))
head(train_upc_return)
colnames(train_upc_return) <- c("TripType", "VisitNumber", "Weekday", "Dist_Upc_Return")
dim(train_upc_return)

# Merge return category datasets
agg_main_sub_dist_ret <- merge.data.frame(train_main_cat_return, train_sub_cat_return, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist_ret <- merge.data.frame(train_upc_return, agg_main_sub_dist_ret, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
dim(agg_main_sub_upc_dist_ret)



# Merge all the datasets
cat("Merging aggregated datasets..... \n")
# Bought datasets
agg_main_sub_dist <- merge.data.frame(train_main_cat_bought, train_sub_cat_bought, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist_buy <- merge.data.frame(train_upc_bought, agg_main_sub_dist, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist1 <- merge.data.frame(agg_main_sub_upc_dist_buy, agg_main_sub_upc_dist_ret, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist <- merge.data.frame(agg_main_sub_upc_dist_trans, agg_main_sub_upc_dist1, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)

dim(agg_main_sub_upc_dist)

rm(train_no_return, train_return, train_upc_transacted, train_sub_cat_transacted, train_main_cat_transacted, train_upc_return, train_sub_cat_return, train_main_cat_return, train_sub_cat_bought, train_upc_bought, train_main_cat_bought)

# --------- MAJOR DATASET ----------------------------- 
tail(agg_main_sub_upc_dist)
agg_main_sub_upc_dist <- as.data.frame(lapply(agg_main_sub_upc_dist, function(x) ifelse(is.na(x), 0,x)))

agg_main_sub_upc_dist <- mutate(agg_main_sub_upc_dist, Upc_bought_perc = Dist_upc_bought / (Dist_upc_bought + Dist_Upc_Return ))
agg_main_sub_upc_dist <- mutate(agg_main_sub_upc_dist, Upc_return_perc = Dist_Upc_Return / (Dist_upc_bought + Dist_Upc_Return ))

agg_main_sub_upc_dist <- mutate(agg_main_sub_upc_dist, Sub_cat_bought_perc = Dist_sub_Cat_bought / (Dist_sub_Cat_bought + Dist_sub_Cat_Return ))
agg_main_sub_upc_dist <- mutate(agg_main_sub_upc_dist, Sub_cat_return_perc = Dist_sub_Cat_Return / (Dist_sub_Cat_bought + Dist_sub_Cat_Return ))


agg_main_sub_upc_dist <- mutate(agg_main_sub_upc_dist, Main_cat_bought_perc = Dist_main_Cat_bought / (Dist_main_Cat_bought + Dist_main_Cat_Return ))
agg_main_sub_upc_dist <- mutate(agg_main_sub_upc_dist, Main_cat_return_perc = Dist_main_Cat_Return / (Dist_main_Cat_bought + Dist_main_Cat_Return ))

dim(agg_main_sub_upc_dist)
tail(agg_main_sub_upc_dist)
str(agg_main_sub_upc_dist)
#--XXXXXXXXXXxXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXxxxxxxxxxxxxxxxxxxxx



#---- Same category return purchase - Product Replacement flags
train_return <- train[which(train$ScanCount < 0),]
train_return_only <- aggregate(x = train_return[,"ScanCount"], by = list(train_return$TripType,train_return$VisitNumber,train_return$Weekday, train_return$DepartmentDescription), FUN = function(x) sum(abs(x)))
colnames(train_return_only) <- c("TripType", "VisitNumber", "Weekday", "DepartmentDescription", "returned")
train_buy_only <- train[which(train$ScanCount > 0),]
train_buy_only <- aggregate(x = train_buy_only[,"ScanCount"], by = list(train_buy_only$TripType,train_buy_only$VisitNumber,train_buy_only$Weekday, train_buy_only$DepartmentDescription), FUN = sum)
colnames(train_buy_only) <- c("TripType", "VisitNumber", "Weekday", "DepartmentDescription", "bought")

train_main_cat_replacement <- merge.data.frame(train_return_only, train_buy_only, by.x = c("TripType", "VisitNumber", "Weekday", "DepartmentDescription"), by.y = c("TripType", "VisitNumber", "Weekday" , "DepartmentDescription"), all.x = TRUE, all.y = TRUE)
train_main_cat_replacement <- as.data.frame(lapply(train_main_cat_replacement, function(x) ifelse(is.na(x), 0,x)))

head(train_main_cat_replacement)
train_main_cat_replacement <- mutate(train_main_cat_replacement, main_cat_replacement = ifelse(bought == returned,1,0))
train_main_cat_replacement <- mutate(train_main_cat_replacement, main_cat_replacement_pdts = ifelse(bought == returned, bought,0))

train_main_cat_replacement_agg <- aggregate(x = train_main_cat_replacement[,c("main_cat_replacement","main_cat_replacement_pdts")], by = list(train_main_cat_replacement$TripType,train_main_cat_replacement$VisitNumber,train_main_cat_replacement$Weekday), FUN = sum)
colnames(train_main_cat_replacement_agg) <- c("TripType", "VisitNumber", "Weekday", "main_cat_replacement", "main_cat_replacement_pdts")
train_main_cat_replacement_agg <- mutate(train_main_cat_replacement_agg, main_cat_rep_pdts_per_cat = ifelse(main_cat_replacement != 0, main_cat_replacement_pdts/main_cat_replacement,0))

head(train_main_cat_replacement_agg)


#----------------------------------------------------

train_return <- train[which(train$ScanCount < 0),]
train_return <- train[which(is.na(train$FinelineNumber) == FALSE),]
train_return_only <- aggregate(x = train_return[,"ScanCount"], by = list(train_return$TripType,train_return$VisitNumber,train_return$Weekday, train_return$FinelineNumber), FUN = function(x) sum(abs(x)))
colnames(train_return_only) <- c("TripType", "VisitNumber", "Weekday", "FinelineNumber", "returned")
train_buy_only <- train[which(train$ScanCount > 0),]
train_buy_only <- aggregate(x = train_buy_only[,"ScanCount"], by = list(train_buy_only$TripType,train_buy_only$VisitNumber,train_buy_only$Weekday, train_buy_only$FinelineNumber), FUN = sum)
colnames(train_buy_only) <- c("TripType", "VisitNumber", "Weekday", "FinelineNumber", "bought")

train_sub_cat_replacement <- merge.data.frame(train_return_only, train_buy_only, by.x = c("TripType", "VisitNumber", "Weekday", "FinelineNumber"), by.y = c("TripType", "VisitNumber", "Weekday" , "FinelineNumber"), all.x = TRUE, all.y = TRUE)
train_sub_cat_replacement <- as.data.frame(lapply(train_sub_cat_replacement, function(x) ifelse(is.na(x), 0,x)))

head(train_sub_cat_replacement)
train_sub_cat_replacement <- mutate(train_sub_cat_replacement, sub_cat_replacement = ifelse(bought == returned,1,0))
train_sub_cat_replacement <- mutate(train_sub_cat_replacement, sub_cat_replacement_pdts = ifelse(bought == returned, bought,0))

train_sub_cat_replacement_agg <- aggregate(x = train_sub_cat_replacement[,c("sub_cat_replacement","sub_cat_replacement_pdts")], by = list(train_sub_cat_replacement$TripType,train_sub_cat_replacement$VisitNumber,train_sub_cat_replacement$Weekday), FUN = sum)
colnames(train_sub_cat_replacement_agg) <- c("TripType", "VisitNumber", "Weekday", "sub_cat_replacement", "sub_cat_replacement_pdts")
train_sub_cat_replacement_agg <- mutate(train_sub_cat_replacement_agg, sub_cat_rep_pdts_per_cat = ifelse(sub_cat_replacement != 0, sub_cat_replacement_pdts/sub_cat_replacement,0))

head(train_sub_cat_replacement_agg)
summary(train_sub_cat_replacement_agg)


#----------------------------------------------------

train_return <- train[which(train$ScanCount < 0),]
train_return_only <- aggregate(x = train_return[,"ScanCount"], by = list(train_return$TripType,train_return$VisitNumber,train_return$Weekday, train_return$Upc), FUN = function(x) sum(abs(x)))
colnames(train_return_only) <- c("TripType", "VisitNumber", "Weekday", "Upc", "returned")
train_buy_only <- train[which(train$ScanCount > 0),]
train_buy_only <- aggregate(x = train_buy_only[,"ScanCount"], by = list(train_buy_only$TripType,train_buy_only$VisitNumber,train_buy_only$Weekday, train_buy_only$Upc), FUN = sum)
colnames(train_buy_only) <- c("TripType", "VisitNumber", "Weekday", "Upc", "bought")

train_upc_replacement <- merge.data.frame(train_return_only, train_buy_only, by.x = c("TripType", "VisitNumber", "Weekday", "Upc"), by.y = c("TripType", "VisitNumber", "Weekday" , "Upc"), all.x = TRUE, all.y = TRUE)
train_upc_replacement <- as.data.frame(lapply(train_upc_replacement, function(x) ifelse(is.na(x), 0,x)))

head(train_upc_replacement)
train_upc_replacement <- mutate(train_upc_replacement, upc_replacement = ifelse(bought == returned,1,0))
train_upc_replacement <- mutate(train_upc_replacement, upc_replacement_pdts = ifelse(bought == returned, bought,0))

train_upc_replacement_agg <- aggregate(x = train_upc_replacement[,c("upc_replacement","upc_replacement_pdts")], by = list(train_upc_replacement$TripType,train_upc_replacement$VisitNumber,train_upc_replacement$Weekday), FUN = sum)
colnames(train_upc_replacement_agg) <- c("TripType", "VisitNumber", "Weekday", "upc_replacement", "upc_replacement_pdts")
train_upc_replacement_agg <- mutate(train_upc_replacement_agg, upc_rep_pdts_per_cat = ifelse(upc_replacement != 0, upc_replacement_pdts/upc_replacement,0))

head(train_upc_replacement_agg)
summary(train_upc_replacement_agg)
#----------------------------------------------------------


summary(train_main_cat_replacement)
summary(train_sub_cat_replacement_agg)
summary(train_upc_replacement_agg)

# Merge the replacement datasets now
rep_main_sub <- merge.data.frame(train_main_cat_replacement_agg, train_sub_cat_replacement_agg, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
replacement_all <- merge.data.frame(rep_main_sub, train_upc_replacement_agg, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
head(replacement_all)

#save(replacement_all, file="G:/Geek World/Kaggle/Walmart/30th Nov/Replacement_cols_30nov.RData")

#------xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

#Get the Upc initials first 2 numbers
# Read test data to compare columns

test <- read.csv("/home/svu/e0008572/Walmart/Input/test.csv")

nzeros <- nrow(test)
test <- cbind.data.frame(TripType = rep(0,nzeros), test) 

week_days <- cbind.data.frame(week_num = c(1,2,3,4,5,6,7), Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
test <- merge.data.frame(test, week_days, by.x = "Weekday", by.y = "Weekday")
head(test)
test$Weekday <- test$week_num
head(test)
test <- test[,c(2,3,1,4,5,6,7)]
str(test)


test1 <- test[which(is.na(test$Upc) == FALSE),] 
upc2 <- mutate(test1, new_var = substr(as.character(sprintf("%012.0f", Upc)),1,2))
head(upc2)
upc2cast_test <- dcast(upc2, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x)))


upc3 <- mutate(test1, new_var = substr(as.character(sprintf("%012.0f", Upc)),1,3))
head(upc3)
upc3cast_test <- dcast(upc3, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x)))
dim(upc3cast_test)
head(upc3cast_test)
names(upc3cast_test)


#Get the Upc initials first 2 numbers

train1 <- train[which(is.na(train$Upc) == FALSE),] 
upc2 <- mutate(train1, new_var = substr(as.character(sprintf("%012.0f", Upc)),1,2))
head(upc2)
upc2cast_train <- dcast(upc2, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x)))
dim(upc2cast_train)
head(upc2cast_train)

rm_cols <- which(names(upc2cast_train) %ni% names(upc2cast_test))
upc2cast_train <- upc2cast_train[,-rm_cols]


upc3 <- mutate(train1, new_var = substr(as.character(sprintf("%012.0f", Upc)),1,3))
head(upc3)
upc3cast_train <- dcast(upc3, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x)))
dim(upc3cast_train)
head(upc3cast_train)
names(upc3cast_train)

rm_cols <- which(names(upc3cast_train) %ni% names(upc3cast_test))
upc3cast_train <- upc3cast_train[,-rm_cols]

colnames(upc2cast_train) <- c(names(upc2cast_train[,1:3]), paste0("u_", names(upc2cast_train[,4:length(upc2cast_train)])))
colnames(upc3cast_train) <- c(names(upc3cast_train[,1:3]), paste0("u_", names(upc3cast_train[,4:length(upc3cast_train)])))

# ----XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXx---------------------------------------------

# TOTAL PRODUCTS BOUGHT AND RETURNED
# Aggregate by keeping only Scan Count 
train_no_upc_sub_main <- aggregate(x = train[,c(5,8)], by = list(train$TripType,train$VisitNumber,train$Weekday), FUN = function(x) sum(abs(x)))
colnames(train_no_upc_sub_main) <- c(names(train[,c(1,2,3)]), "Total_products_transacted", "Total_products_returned")
head(train_no_upc_sub_main)

train_no_upc_sub_main <- mutate(train_no_upc_sub_main, Total_products_bought = Total_products_transacted  - Total_products_returned)
train_no_upc_sub_main <- mutate(train_no_upc_sub_main, Product_bought_perc = Total_products_bought / Total_products_transacted)
train_no_upc_sub_main <- mutate(train_no_upc_sub_main, Product_return_perc = Total_products_returned / Total_products_transacted)

head(train_no_upc_sub_main)
dim(train_no_upc_sub_main)
str(train_no_upc_sub_main)
which(is.na(train_no_upc_sub_main) == TRUE)


Visit_level_metrics <-  merge.data.frame(train_no_upc_sub_main, agg_main_sub_upc_dist, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
new_features <- merge.data.frame(replacement_all, upc2cast_train, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE) 
new_features <- merge.data.frame(new_features, upc3cast_train, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE) 

Visit_level_metrics <-  merge.data.frame(Visit_level_metrics, new_features, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)

head(Visit_level_metrics)
str(Visit_level_metrics)
names(Visit_level_metrics)

Visit_level_metrics <- mutate(Visit_level_metrics, Weekend = ifelse(Weekday %in% c(6,7), 1,0))
Visit_level_metrics <- mutate(Visit_level_metrics, ext_Weekend = ifelse(Weekday %in% c(5,6,7), 1,0))

Visit_level_metrics <- mutate(Visit_level_metrics, Products_transacted_per_main_cat = ifelse(Dist_main_Cat_transacted != 0, Total_products_transacted / Dist_main_Cat_transacted,0))
Visit_level_metrics <- mutate(Visit_level_metrics, Products_transacted_per_sub_cat = ifelse(Dist_sub_Cat_transacted != 0, Total_products_transacted / Dist_sub_Cat_transacted,0))
Visit_level_metrics <- mutate(Visit_level_metrics, Products_transacted_per_upc = ifelse(Dist_upc_transacted != 0, Total_products_transacted / Dist_upc_transacted,0))

Visit_level_metrics <- mutate(Visit_level_metrics, Products_bought_per_main_cat = ifelse(Dist_main_Cat_bought != 0, Total_products_bought / Dist_main_Cat_bought, 0))
Visit_level_metrics <- mutate(Visit_level_metrics, Products_bought_per_sub_cat = ifelse(Dist_sub_Cat_bought != 0, Total_products_bought / Dist_sub_Cat_bought,0))
Visit_level_metrics <- mutate(Visit_level_metrics, Products_bought_per_upc = ifelse(Dist_upc_bought !=0, Total_products_bought / Dist_upc_bought,0))

Visit_level_metrics <- mutate(Visit_level_metrics, Products_returned_per_main_cat = ifelse(Dist_main_Cat_Return != 0 , Total_products_returned / Dist_main_Cat_Return, 0))
Visit_level_metrics <- mutate(Visit_level_metrics, Products_returned_per_sub_cat = ifelse(Dist_sub_Cat_Return != 0, Total_products_returned / Dist_sub_Cat_Return, 0))
Visit_level_metrics <- mutate(Visit_level_metrics, Products_returned_per_upc = ifelse(Dist_Upc_Return != 0 , Total_products_returned / Dist_Upc_Return,0))



Visit_level_metrics <- mutate(Visit_level_metrics, Upc_transacted_per_main_cat = ifelse(Dist_main_Cat_transacted != 0, Dist_upc_transacted / Dist_main_Cat_transacted,0))
Visit_level_metrics <- mutate(Visit_level_metrics, Upc_transacted_per_sub_cat = ifelse(Dist_sub_Cat_transacted != 0, Dist_upc_transacted / Dist_sub_Cat_transacted,0))

Visit_level_metrics <- mutate(Visit_level_metrics, Upc_bought_per_main_catt = ifelse(Dist_main_Cat_bought != 0, Dist_upc_bought / Dist_main_Cat_bought,0))
Visit_level_metrics <- mutate(Visit_level_metrics, Upc_bought_per_sub_cat = ifelse(Dist_sub_Cat_bought !=0, Dist_upc_bought / Dist_sub_Cat_bought,0))

Visit_level_metrics <- mutate(Visit_level_metrics, Upc_returned_per_main_cat = ifelse(Dist_main_Cat_Return != 0, Dist_Upc_Return / Dist_main_Cat_Return, 0))
Visit_level_metrics <- mutate(Visit_level_metrics, Upc_returned_per_sub_cat = ifelse(Dist_sub_Cat_Return != 0 , Dist_Upc_Return / Dist_sub_Cat_Return,0))

Visit_level_metrics <- mutate(Visit_level_metrics, Sub_cat_transacted_per_main_cat = ifelse(Dist_main_Cat_transacted != 0, Dist_sub_Cat_transacted / Dist_main_Cat_transacted,0))
Visit_level_metrics <- mutate(Visit_level_metrics, Sub_cat_bought_per_main_catt = ifelse(Dist_main_Cat_bought != 0, Dist_sub_Cat_bought / Dist_main_Cat_bought,0))
Visit_level_metrics <- mutate(Visit_level_metrics, Sub_cat_returned_per_main_cat = ifelse(Dist_main_Cat_Return != 0, Dist_sub_Cat_Return / Dist_main_Cat_Return, 0))


summary(Visit_level_metrics)

################****** MAJOR DATASET ************#############################
head(Visit_level_metrics)
dim(Visit_level_metrics)
#################################################################################


cat("Casting starts here ..... \n")

# GET MAIN CATEGORY COLUMNS
# Aggregate at Trip level - Removing UPC and SUB CATEGORY
# Calculate Total products transacted in each category
train_no_upc_sub_trans1 <- aggregate(x = train[,5], by = list(train$TripType,train$VisitNumber,train$Weekday,train$DepartmentDescription), FUN = function(x) sum(abs(x)))
colnames(train_no_upc_sub_trans1) <- names(train[,c(1,2,3,6,5)])
train_no_upc_sub_trans1$DepartmentDescription <- paste0("t_", gsub(" ", "_", train_no_upc_sub_trans1$DepartmentDescription))
unique(train_no_upc_sub_trans1$DepartmentDescription)
head(train_no_upc_sub_trans1)


train_no_upc_sub_trans <- merge.data.frame(train_no_upc_sub_trans1, Visit_level_metrics[,c("TripType", "VisitNumber", "Weekday", "Total_products_transacted")], by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
train_no_upc_sub_trans <- mutate(train_no_upc_sub_trans, trans_perc = ifelse(Total_products_transacted != 0, ScanCount/Total_products_transacted))
head(train_no_upc_sub_trans)

main_cat_level_trans1 <- dcast(train_no_upc_sub_trans1, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "ScanCount", fun.aggregate = sum) 
head(main_cat_level_trans1)
#---------Similarity matrix calculation---------------------------------------------------------------------------

install.packages("qlcMatrix")
library(qlcMatrix)
category <- main_cat_level_trans1[,4:length(main_cat_level_trans1)]
#"cosSparse" from qlcMatrix package

install.packages("dgCMatrix-class")
library(Matrix)

category_m <- as.matrix(category)
m <- as(category_m, "dgCMatrix")


#normL <- function(x,s) { abs(drop(crossprod(x,s))) ^ (1/2) }
#cosSparse(X, Y = NULL, norm = norm2, weight = NULL)

dist_m <- cosSparse(X = m, weight = "idf")
# head(dist_m)
# str(dist_m)

dist_mat <- as.matrix(dist_m)
melt_dist_m <- as.data.frame(dist_mat)

head(melt_dist_m)
melt_dist_m$var <- row.names(melt_dist_m)

library(reshape2)
x <- melt(data = melt_dist_m, id.vars = "var")
#head(x,60)

#x$rank_var <- rank(x$value, ties.method = "first", na.last = TRUE) 

ranked_x <- ddply(x, .(variable), transform, rank=rank(-value, ties.method="first"))
head(ranked_x,10)
ranked_x[which(ranked_x$rank > 5), "value"] <- 0
head(ranked_x,10)

cast_x <- dcast(ranked_x, variable ~ var, value.var = "value")
head(cast_x)
cast_x <- as.data.frame(lapply(cast_x, function(x) ifelse(is.na(x), 0,x)))
final_dist_m <- as.matrix(cast_x[,-1])

category_m
i <- 1
j <- 1

for(i in 1 : 50)
{
  temp <- matrix(rep(NA,69*69), nrow = 69, ncol = 69)
  temp_cntr <- 1
  for(j in which(category_m[i,] > 0))
  {
    
    temp[temp_cntr,] <- category_m[i,]
    temp[temp_cntr,] <- category_m[i,j] * final_dist_m[j,]
    temp[temp_cntr,which(category_m[i,] > 0 )] <- category_m[i,which(category_m[i,] > 0)]
    temp_cntr <- temp_cntr + 1
  }
  category_m[i,] <- colMeans(temp,na.rm = TRUE)
  
}



# cccd(x = NULL, y = NULL, dxx = NULL, dyx = NULL, method = NULL,
#      k = NA, algorithm = 'cover_tree')
# 
# cccd(dxx = dist_m, algorithm = 'cover_tree')
# 
# graph_cat_dom <- dominate(graph_cat, method = "greedy",proportion=1.0)
# head(graph_cat_dom)

# install.packages("cccd")
# library(cccd)
# 
# graph_cat <- nng(x = NULL, dx = final_dist_m, k = 4, mutual = FALSE, method = NULL, use.fnn = FALSE, algorithm = 'cover_tree')
# head(graph_cat)
# 
# 
# names <- colnames(dist_mat)
# dim(final_dist_m)
# row.names(final_dist_m) <- colnames(final_dist_mat)
# str()
# 
# 
# head(dist_mat)
# getwd()
# png(file="mygraphic.png",width=1500,height=4000)
# plot(graph_cat)
# dev.off()


plot(graph_cat)

#------------------------------------------------------------------------------------
train_no_upc_sub_trans$DepartmentDescription <- paste0("perc_", train_no_upc_sub_trans$DepartmentDescription)
main_cat_level_trans2 <- dcast(train_no_upc_sub_trans, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "trans_perc", fun.aggregate = sum) 
head(main_cat_level_trans2)

main_cat_level_trans <- merge.data.frame(main_cat_level_trans1, main_cat_level_trans2, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
head(main_cat_level_trans)

which(is.na(main_cat_level_trans) == TRUE)
dim(main_cat_level_trans)


# Calculate Total products bought in each category
train1 <- train[which(train$ScanCount > 0),]
train_no_upc_sub_bought1 <- aggregate(x = train1[,5], by = list(train1$TripType,train1$VisitNumber,train1$Weekday,train1$DepartmentDescription), FUN = sum)
colnames(train_no_upc_sub_bought1) <- names(train1[,c(1,2,3,6,5)])
train_no_upc_sub_bought1$DepartmentDescription <- paste0("b_", gsub(" ", "_", train_no_upc_sub_bought1$DepartmentDescription))
unique(train_no_upc_sub_bought1$DepartmentDescription)

train_no_upc_sub_bought <- merge.data.frame(train_no_upc_sub_bought1, Visit_level_metrics[,c("TripType", "VisitNumber", "Weekday", "Total_products_transacted")], by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
train_no_upc_sub_bought <- mutate(train_no_upc_sub_bought, bought_perc = ifelse(Total_products_transacted != 0, ScanCount/Total_products_transacted))
head(train_no_upc_sub_bought)

main_cat_level_bought1 <- dcast(train_no_upc_sub_bought, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "ScanCount", fun.aggregate = sum) 
train_no_upc_sub_bought$DepartmentDescription <- paste0("perc_", train_no_upc_sub_bought$DepartmentDescription)

main_cat_level_bought2 <- dcast(train_no_upc_sub_bought, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "bought_perc", fun.aggregate = sum) 
head(main_cat_level_bought2)

main_cat_level_bought <- merge.data.frame(main_cat_level_bought1, main_cat_level_bought2, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
head(main_cat_level_bought)

which(is.na(main_cat_level_bought) == TRUE)
dim(main_cat_level_bought)



# Calculate Total products returned in each category
train1 <- train[which(train$ScanCount < 0),]
train_no_upc_sub_return1 <- aggregate(x = train1[,5], by = list(train1$TripType,train1$VisitNumber,train1$Weekday,train1$DepartmentDescription), FUN = function(x) sum(abs(x)))
colnames(train_no_upc_sub_return1) <- names(train1[,c(1,2,3,6,5)])
train_no_upc_sub_return1$DepartmentDescription <- paste0("r_", gsub(" ", "_", train_no_upc_sub_return1$DepartmentDescription))
unique(train_no_upc_sub_return1$DepartmentDescription)
head(train_no_upc_sub_return1)

train_no_upc_sub_return <- merge.data.frame(train_no_upc_sub_return1, Visit_level_metrics[,c("TripType", "VisitNumber", "Weekday", "Total_products_transacted")], by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
train_no_upc_sub_return <- mutate(train_no_upc_sub_return, return_perc = ifelse(Total_products_transacted != 0, ScanCount/Total_products_transacted))
head(train_no_upc_sub_return)


main_cat_level_return1 <- dcast(train_no_upc_sub_return, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "ScanCount", fun.aggregate = sum) 
train_no_upc_sub_return$DepartmentDescription <- paste0("perc_", train_no_upc_sub_return$DepartmentDescription)

main_cat_level_return2 <- dcast(train_no_upc_sub_return, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "return_perc", fun.aggregate = sum) 
head(main_cat_level_return2)

main_cat_level_return <- merge.data.frame(main_cat_level_return1, main_cat_level_return2, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
head(main_cat_level_return)

which(is.na(main_cat_level_return) == TRUE)
dim(main_cat_level_return)


#Merging all the main category data together 
main_cat_level1 <- merge.data.frame(main_cat_level_trans, main_cat_level_bought, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
main_cat_level <- merge.data.frame(main_cat_level1, main_cat_level_return, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)

rm(train1,main_cat_level1, main_cat_level_trans,main_cat_level_trans1,main_cat_level_trans2, main_cat_level_bought1, main_cat_level_bought2, main_cat_level_bought, main_cat_level_return1,main_cat_level_return2,main_cat_level_return)


################****** MAJOR DATASET ************#############################
head(main_cat_level)
#################################################################################



# Lets merge them all
# First merge UPCs and Sub Categories
train_visit_level_data <- merge.data.frame(Visit_level_metrics, main_cat_level, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
dim(train_visit_level_data)
head(train_visit_level_data)

train_visit_level_data <- as.data.frame(lapply(train_visit_level_data, function(x) ifelse(is.na(x), 0,x)))

save(train_visit_level_data, file = "train_visit_level_data.RData")



# Append finelines

train[which(is.na(train$FinelineNumber)==TRUE), "FinelineNumber"] <- 99999
test[which(is.na(test$FinelineNumber)==TRUE), "FinelineNumber"] <- 99999

x <- unique(train$FinelineNumber)
y <- unique(test$FinelineNumber)

`%ni%` <- Negate(`%in%`) 
rm_fines <- x[which(x %ni% y)]

rm(x,y)

train <- train[which(train$FinelineNumber %ni% rm_fines),]

train$FinelineNumber <- paste0("t_", as.character(sprintf("%04d", train$FinelineNumber)))


sub_cat_level <- dcast(train, formula = TripType + VisitNumber + Weekday ~ FinelineNumber, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x))) 

#head(sub_cat_level[,c(1:8)])
length(sub_cat_level$VisitNumber)
dim(sub_cat_level)

which(is.na(sub_cat_level) == TRUE)

# Merge with Trip level data
train449_plus_fine_rep_upc <- merge.data.frame(train_visit_level_data, sub_cat_level, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)


save(train449_plus_fine_rep_upc, file = "train449_plus_fine_rep_upc.RData")






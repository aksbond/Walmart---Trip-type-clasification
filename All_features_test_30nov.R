setwd("/home/svu/e0008572/Walmart/2dec")

`%ni%` <- Negate(`%in%`) 

require(reshape2, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")
require(plyr, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")

#setwd("G:/Geek World/Kaggle/Walmart")
test <- read.csv("/home/svu/e0008572/Walmart/Input/test.csv", stringsAsFactors = FALSE)


nzeros <- nrow(test)
test <- cbind.data.frame(TripType = rep(0,nzeros), test) 

week_days <- cbind.data.frame(week_num = c(1,2,3,4,5,6,7), Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
test <- merge.data.frame(test, week_days, by.x = "Weekday", by.y = "Weekday")
head(test)
test$Weekday <- test$week_num
head(test)
test <- test[,c(2,3,1,4,5,6,7)]
str(test)
# returns column  
test <- mutate(test, Returns = ifelse(ScanCount < 0, abs(ScanCount),0))

# Treating NAs 
test[which(is.na(test$FinelineNumber)==TRUE), "FinelineNumber"] <- 99999


cat("Aggregation time ..... \n")

# MAIN CATEGORIES TRANSACTED
# Find total number of MAIN categories transacted 
test_main_cat_transacted <- aggregate(x = test[,6], by = list(test$TripType,test$VisitNumber,test$Weekday), FUN = function(x) length(unique(x)))
head(test_main_cat_transacted)
colnames(test_main_cat_transacted) <- c("TripType", "VisitNumber", "Weekday", "Dist_main_Cat_transacted")
dim(test_main_cat_transacted)

# SUB CATEGORIES transacted
# Find total number of sub categories transacted -- Remove RETURN entries
test_sub_cat_transacted <- aggregate(x = test[,7], by = list(test$TripType,test$VisitNumber,test$Weekday), FUN = function(x) length(unique(x)))
head(test_sub_cat_transacted)
colnames(test_sub_cat_transacted) <- c("TripType", "VisitNumber", "Weekday", "Dist_sub_Cat_transacted")
dim(test_sub_cat_transacted)

# # UPCs transacted
# Find total number of UPCs transacted - Remove RETURN entries
test_upc_transacted <- aggregate(x = test[,4], by = list(test$TripType,test$VisitNumber,test$Weekday), FUN = function(x) length(unique(x)))
head(test_upc_transacted)
colnames(test_upc_transacted) <- c("TripType", "VisitNumber", "Weekday", "Dist_upc_transacted")
dim(test_upc_transacted)


# Merge return datasets
agg_main_sub_dist_trans <- merge.data.frame(test_main_cat_transacted, test_sub_cat_transacted, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist_trans <- merge.data.frame(test_upc_transacted, agg_main_sub_dist_trans, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
dim(agg_main_sub_upc_dist_trans)


# MAIN CATEGORIES BOUGHT
# Find total number of MAIN categories bought -- Remove RETURN entries
test_no_return <- test[which(test$ScanCount > 0), ]
test_main_cat_bought <- aggregate(x = test_no_return[,6], by = list(test_no_return$TripType,test_no_return$VisitNumber,test_no_return$Weekday), FUN = function(x) length(unique(x)))
head(test_main_cat_bought)
colnames(test_main_cat_bought) <- c("TripType", "VisitNumber", "Weekday", "Dist_main_Cat_bought")
dim(test_main_cat_bought)

# SUB CATEGORIES BOUGHT
# Find total number of sub categories bought -- Remove RETURN entries
test_sub_cat_bought <- aggregate(x = test_no_return[,7], by = list(test_no_return$TripType,test_no_return$VisitNumber,test_no_return$Weekday), FUN = function(x) length(unique(x)))
head(test_sub_cat_bought)
colnames(test_sub_cat_bought) <- c("TripType", "VisitNumber", "Weekday", "Dist_sub_Cat_bought")
dim(test_sub_cat_bought)

# # UPCs BOUGHT
# Find total number of UPCs bought - Remove RETURN entries
test_upc_bought <- aggregate(x = test_no_return[,4], by = list(test_no_return$TripType,test_no_return$VisitNumber,test_no_return$Weekday), FUN = function(x) length(unique(x)))
head(test_upc_bought)
colnames(test_upc_bought) <- c("TripType", "VisitNumber", "Weekday", "Dist_upc_bought")
dim(test_upc_bought)


# NOW FOR RETURN ITEMS----------------

# MAIN CATEGORIES RETURNED
# Find total number of MAIN categories bought -- Remove RETURN entries
test_return <- test[which(test$ScanCount < 0), ]
#head(test_return)
test_main_cat_return <- aggregate(x = test_return[,6], by = list(test_return$TripType,test_return$VisitNumber,test_return$Weekday), FUN = function(x) length(unique(x)))
head(test_main_cat_return)
colnames(test_main_cat_return) <- c("TripType", "VisitNumber", "Weekday", "Dist_main_Cat_Return")
dim(test_main_cat_return)

# SUB CATEGORIES return
# Find total number of sub categories return -- Remove RETURN entries
test_sub_cat_return <- aggregate(x = test_return[,7], by = list(test_return$TripType,test_return$VisitNumber,test_return$Weekday), FUN = function(x) length(unique(x)))
head(test_sub_cat_return)
colnames(test_sub_cat_return) <- c("TripType", "VisitNumber", "Weekday", "Dist_sub_Cat_Return")
dim(test_sub_cat_return)

# # UPCs return
# Find total number of UPCs return - Remove RETURN entries
test_upc_return <- aggregate(x = test_return[,4], by = list(test_return$TripType,test_return$VisitNumber,test_return$Weekday), FUN = function(x) length(unique(x)))
head(test_upc_return)
colnames(test_upc_return) <- c("TripType", "VisitNumber", "Weekday", "Dist_Upc_Return")
dim(test_upc_return)

# Merge return datasets
agg_main_sub_dist_ret <- merge.data.frame(test_main_cat_return, test_sub_cat_return, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist_ret <- merge.data.frame(test_upc_return, agg_main_sub_dist_ret, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
dim(agg_main_sub_upc_dist_ret)



# Merge all the datasets
cat("Merging aggregated datasets..... \n")
# Bought datasets
agg_main_sub_dist <- merge.data.frame(test_main_cat_bought, test_sub_cat_bought, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist_buy <- merge.data.frame(test_upc_bought, agg_main_sub_dist, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist1 <- merge.data.frame(agg_main_sub_upc_dist_buy, agg_main_sub_upc_dist_ret, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
agg_main_sub_upc_dist <- merge.data.frame(agg_main_sub_upc_dist_trans, agg_main_sub_upc_dist1, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)

dim(agg_main_sub_upc_dist)

rm(test_no_return, test_return, test_upc_transacted, test_sub_cat_transacted, test_main_cat_transacted, test_upc_return, test_sub_cat_return, test_main_cat_return, test_sub_cat_bought, test_upc_bought, test_main_cat_bought)

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
test_return <- test[which(test$ScanCount < 0),]
test_return_only <- aggregate(x = test_return[,"ScanCount"], by = list(test_return$TripType,test_return$VisitNumber,test_return$Weekday, test_return$DepartmentDescription), FUN = function(x) sum(abs(x)))
colnames(test_return_only) <- c("TripType", "VisitNumber", "Weekday", "DepartmentDescription", "returned")
test_buy_only <- test[which(test$ScanCount > 0),]
test_buy_only <- aggregate(x = test_buy_only[,"ScanCount"], by = list(test_buy_only$TripType,test_buy_only$VisitNumber,test_buy_only$Weekday, test_buy_only$DepartmentDescription), FUN = sum)
colnames(test_buy_only) <- c("TripType", "VisitNumber", "Weekday", "DepartmentDescription", "bought")

test_main_cat_replacement <- merge.data.frame(test_return_only, test_buy_only, by.x = c("TripType", "VisitNumber", "Weekday", "DepartmentDescription"), by.y = c("TripType", "VisitNumber", "Weekday" , "DepartmentDescription"), all.x = TRUE, all.y = TRUE)
test_main_cat_replacement <- as.data.frame(lapply(test_main_cat_replacement, function(x) ifelse(is.na(x), 0,x)))

head(test_main_cat_replacement)
test_main_cat_replacement <- mutate(test_main_cat_replacement, main_cat_replacement = ifelse(bought == returned,1,0))
test_main_cat_replacement <- mutate(test_main_cat_replacement, main_cat_replacement_pdts = ifelse(bought == returned, bought,0))

test_main_cat_replacement_agg <- aggregate(x = test_main_cat_replacement[,c("main_cat_replacement","main_cat_replacement_pdts")], by = list(test_main_cat_replacement$TripType,test_main_cat_replacement$VisitNumber,test_main_cat_replacement$Weekday), FUN = sum)
colnames(test_main_cat_replacement_agg) <- c("TripType", "VisitNumber", "Weekday", "main_cat_replacement", "main_cat_replacement_pdts")
test_main_cat_replacement_agg <- mutate(test_main_cat_replacement_agg, main_cat_rep_pdts_per_cat = ifelse(main_cat_replacement != 0, main_cat_replacement_pdts/main_cat_replacement,0))

head(test_main_cat_replacement_agg)


#----------------------------------------------------

test_return <- test[which(test$ScanCount < 0),]
test_return <- test[which(is.na(test$FinelineNumber) == FALSE),]
test_return_only <- aggregate(x = test_return[,"ScanCount"], by = list(test_return$TripType,test_return$VisitNumber,test_return$Weekday, test_return$FinelineNumber), FUN = function(x) sum(abs(x)))
colnames(test_return_only) <- c("TripType", "VisitNumber", "Weekday", "FinelineNumber", "returned")
test_buy_only <- test[which(test$ScanCount > 0),]
test_buy_only <- aggregate(x = test_buy_only[,"ScanCount"], by = list(test_buy_only$TripType,test_buy_only$VisitNumber,test_buy_only$Weekday, test_buy_only$FinelineNumber), FUN = sum)
colnames(test_buy_only) <- c("TripType", "VisitNumber", "Weekday", "FinelineNumber", "bought")

test_sub_cat_replacement <- merge.data.frame(test_return_only, test_buy_only, by.x = c("TripType", "VisitNumber", "Weekday", "FinelineNumber"), by.y = c("TripType", "VisitNumber", "Weekday" , "FinelineNumber"), all.x = TRUE, all.y = TRUE)
test_sub_cat_replacement <- as.data.frame(lapply(test_sub_cat_replacement, function(x) ifelse(is.na(x), 0,x)))

head(test_sub_cat_replacement)
test_sub_cat_replacement <- mutate(test_sub_cat_replacement, sub_cat_replacement = ifelse(bought == returned,1,0))
test_sub_cat_replacement <- mutate(test_sub_cat_replacement, sub_cat_replacement_pdts = ifelse(bought == returned, bought,0))

test_sub_cat_replacement_agg <- aggregate(x = test_sub_cat_replacement[,c("sub_cat_replacement","sub_cat_replacement_pdts")], by = list(test_sub_cat_replacement$TripType,test_sub_cat_replacement$VisitNumber,test_sub_cat_replacement$Weekday), FUN = sum)
colnames(test_sub_cat_replacement_agg) <- c("TripType", "VisitNumber", "Weekday", "sub_cat_replacement", "sub_cat_replacement_pdts")
test_sub_cat_replacement_agg <- mutate(test_sub_cat_replacement_agg, sub_cat_rep_pdts_per_cat = ifelse(sub_cat_replacement != 0, sub_cat_replacement_pdts/sub_cat_replacement,0))

head(test_sub_cat_replacement_agg)
summary(test_sub_cat_replacement_agg)


#----------------------------------------------------

test_return <- test[which(test$ScanCount < 0),]
test_return_only <- aggregate(x = test_return[,"ScanCount"], by = list(test_return$TripType,test_return$VisitNumber,test_return$Weekday, test_return$Upc), FUN = function(x) sum(abs(x)))
colnames(test_return_only) <- c("TripType", "VisitNumber", "Weekday", "Upc", "returned")
test_buy_only <- test[which(test$ScanCount > 0),]
test_buy_only <- aggregate(x = test_buy_only[,"ScanCount"], by = list(test_buy_only$TripType,test_buy_only$VisitNumber,test_buy_only$Weekday, test_buy_only$Upc), FUN = sum)
colnames(test_buy_only) <- c("TripType", "VisitNumber", "Weekday", "Upc", "bought")

test_upc_replacement <- merge.data.frame(test_return_only, test_buy_only, by.x = c("TripType", "VisitNumber", "Weekday", "Upc"), by.y = c("TripType", "VisitNumber", "Weekday" , "Upc"), all.x = TRUE, all.y = TRUE)
test_upc_replacement <- as.data.frame(lapply(test_upc_replacement, function(x) ifelse(is.na(x), 0,x)))

head(test_upc_replacement)
test_upc_replacement <- mutate(test_upc_replacement, upc_replacement = ifelse(bought == returned,1,0))
test_upc_replacement <- mutate(test_upc_replacement, upc_replacement_pdts = ifelse(bought == returned, bought,0))

test_upc_replacement_agg <- aggregate(x = test_upc_replacement[,c("upc_replacement","upc_replacement_pdts")], by = list(test_upc_replacement$TripType,test_upc_replacement$VisitNumber,test_upc_replacement$Weekday), FUN = sum)
colnames(test_upc_replacement_agg) <- c("TripType", "VisitNumber", "Weekday", "upc_replacement", "upc_replacement_pdts")
test_upc_replacement_agg <- mutate(test_upc_replacement_agg, upc_rep_pdts_per_cat = ifelse(upc_replacement != 0, upc_replacement_pdts/upc_replacement,0))

head(test_upc_replacement_agg)
summary(test_upc_replacement_agg)
#----------------------------------------------------------


summary(test_main_cat_replacement)
summary(test_sub_cat_replacement_agg)
summary(test_upc_replacement_agg)

# Merge the replacement datasets now
rep_main_sub <- merge.data.frame(test_main_cat_replacement_agg, test_sub_cat_replacement_agg, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
replacement_all <- merge.data.frame(rep_main_sub, test_upc_replacement_agg, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
head(replacement_all)

#------xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx



# Read test data to compare columns

train <- read.csv("/home/svu/e0008572/Walmart/Input/train.csv")

week_days <- cbind.data.frame(week_num = c(1,2,3,4,5,6,7), Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
train <- merge.data.frame(train, week_days, by.x = "Weekday", by.y = "Weekday")
head(train)
train$Weekday <- train$week_num
head(train)
train <- train[,c(2,3,1,4,5,6,7)]
str(train)


train1 <- train[which(is.na(train$Upc) == FALSE),] 
upc2 <- mutate(train1, new_var = substr(as.character(sprintf("%012.0f", Upc)),1,2))
head(upc2)
upc2cast_train <- dcast(upc2, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x)))


upc3 <- mutate(train1, new_var = substr(as.character(sprintf("%012.0f", Upc)),1,3))
head(upc3)
upc3cast_train <- dcast(upc3, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x)))


#Get the Upc initials first 2 numbers

library(reshape2)
test1 <- test[which(is.na(test$Upc) == FALSE),] 
upc2 <- mutate(test1, new_var = substr(as.character(sprintf("%012.0f", Upc)),1,2))
head(upc2)
upc2cast_test <- dcast(upc2, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x)))
dim(upc2cast_test)
head(upc2cast_test)
names(upc2cast_test)

keep_cols <- which(names(upc2cast_test) %in% names(upc2cast_train))
upc2cast_test <- upc2cast_test[,keep_cols]




upc3 <- mutate(test1, new_var = substr(as.character(sprintf("%012.0f", Upc)),1,3))
head(upc3)
upc3cast_test <- dcast(upc3, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x)))
dim(upc3cast_test)
head(upc3cast_test)
names(upc3cast_test)

keep_cols <- which(names(upc3cast_test) %in% names(upc3cast_train))
upc3cast_test <- upc3cast_test[,keep_cols]



colnames(upc2cast_test) <- c(names(upc2cast_test[,1:3]), paste0("u_", names(upc2cast_test[,4:length(upc2cast_test)])))
colnames(upc3cast_test) <- c(names(upc3cast_test[,1:3]), paste0("u_", names(upc3cast_test[,4:length(upc3cast_test)])))

# ----------xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx


# TOTAL PRODUCTS BOUGHT AND RETURNED
# Aggregate by keeping only Scan Count 
test_no_upc_sub_main <- aggregate(x = test[,c(5,8)], by = list(test$TripType,test$VisitNumber,test$Weekday), FUN = function(x) sum(abs(x)))
colnames(test_no_upc_sub_main) <- c(names(test[,c(1,2,3)]), "Total_products_transacted", "Total_products_returned")
head(test_no_upc_sub_main)
test_no_upc_sub_main <- mutate(test_no_upc_sub_main, Total_products_bought = Total_products_transacted  - Total_products_returned)
test_no_upc_sub_main <- mutate(test_no_upc_sub_main, Product_bought_perc = Total_products_bought / Total_products_transacted)
test_no_upc_sub_main <- mutate(test_no_upc_sub_main, Product_return_perc = Total_products_returned / Total_products_transacted)

head(test_no_upc_sub_main)
dim(test_no_upc_sub_main)
str(test_no_upc_sub_main)
which(is.na(test_no_upc_sub_main) == TRUE)




Visit_level_metrics <-  merge.data.frame(test_no_upc_sub_main, agg_main_sub_upc_dist, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)

new_features <- merge.data.frame(replacement_all, upc2cast_test, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE) 
new_features <- merge.data.frame(new_features, upc3cast_test, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE) 

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
test_no_upc_sub_trans1 <- aggregate(x = test[,5], by = list(test$TripType,test$VisitNumber,test$Weekday,test$DepartmentDescription), FUN = function(x) sum(abs(x)))
colnames(test_no_upc_sub_trans1) <- names(test[,c(1,2,3,6,5)])
test_no_upc_sub_trans1$DepartmentDescription <- paste0("t_", gsub(" ", "_", test_no_upc_sub_trans1$DepartmentDescription))
unique(test_no_upc_sub_trans1$DepartmentDescription)

test_no_upc_sub_trans <- merge.data.frame(test_no_upc_sub_trans1, Visit_level_metrics[,c("TripType", "VisitNumber", "Weekday", "Total_products_transacted")], by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
test_no_upc_sub_trans <- mutate(test_no_upc_sub_trans, trans_perc = ifelse(Total_products_transacted != 0, ScanCount/Total_products_transacted))
head(test_no_upc_sub_trans)

main_cat_level_trans1 <- dcast(test_no_upc_sub_trans, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "ScanCount", fun.aggregate = sum) 
test_no_upc_sub_trans$DepartmentDescription <- paste0("perc_", test_no_upc_sub_trans$DepartmentDescription)

main_cat_level_trans2 <- dcast(test_no_upc_sub_trans, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "trans_perc", fun.aggregate = sum) 
head(main_cat_level_trans2)

main_cat_level_trans <- merge.data.frame(main_cat_level_trans1, main_cat_level_trans2, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
head(main_cat_level_trans)

which(is.na(main_cat_level_trans) == TRUE)
dim(main_cat_level_trans)


# Calculate Total products bought in each category
test1 <- test[which(test$ScanCount > 0),]
test_no_upc_sub_bought1 <- aggregate(x = test1[,5], by = list(test1$TripType,test1$VisitNumber,test1$Weekday,test1$DepartmentDescription), FUN = sum)
colnames(test_no_upc_sub_bought1) <- names(test1[,c(1,2,3,6,5)])
test_no_upc_sub_bought1$DepartmentDescription <- paste0("b_", gsub(" ", "_", test_no_upc_sub_bought1$DepartmentDescription))
unique(test_no_upc_sub_bought1$DepartmentDescription)

test_no_upc_sub_bought <- merge.data.frame(test_no_upc_sub_bought1, Visit_level_metrics[,c("TripType", "VisitNumber", "Weekday", "Total_products_transacted")], by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
test_no_upc_sub_bought <- mutate(test_no_upc_sub_bought, bought_perc = ifelse(Total_products_transacted != 0, ScanCount/Total_products_transacted))
head(test_no_upc_sub_bought)

main_cat_level_bought1 <- dcast(test_no_upc_sub_bought, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "ScanCount", fun.aggregate = sum) 
test_no_upc_sub_bought$DepartmentDescription <- paste0("perc_", test_no_upc_sub_bought$DepartmentDescription)

main_cat_level_bought2 <- dcast(test_no_upc_sub_bought, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "bought_perc", fun.aggregate = sum) 
head(main_cat_level_bought2)

main_cat_level_bought <- merge.data.frame(main_cat_level_bought1, main_cat_level_bought2, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
head(main_cat_level_bought)

which(is.na(main_cat_level_bought) == TRUE)
dim(main_cat_level_bought)



# Calculate Total products returned in each category
test1 <- test[which(test$ScanCount < 0),]
test_no_upc_sub_return1 <- aggregate(x = test1[,5], by = list(test1$TripType,test1$VisitNumber,test1$Weekday,test1$DepartmentDescription), FUN = function(x) sum(abs(x)))
colnames(test_no_upc_sub_return1) <- names(test1[,c(1,2,3,6,5)])
test_no_upc_sub_return1$DepartmentDescription <- paste0("r_", gsub(" ", "_", test_no_upc_sub_return1$DepartmentDescription))
unique(test_no_upc_sub_return1$DepartmentDescription)
head(test_no_upc_sub_return1)

test_no_upc_sub_return <- merge.data.frame(test_no_upc_sub_return1, Visit_level_metrics[,c("TripType", "VisitNumber", "Weekday", "Total_products_transacted")], by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
test_no_upc_sub_return <- mutate(test_no_upc_sub_return, return_perc = ifelse(Total_products_transacted != 0, ScanCount/Total_products_transacted))
head(test_no_upc_sub_return)


main_cat_level_return1 <- dcast(test_no_upc_sub_return, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "ScanCount", fun.aggregate = sum) 
test_no_upc_sub_return$DepartmentDescription <- paste0("perc_", test_no_upc_sub_return$DepartmentDescription)

main_cat_level_return2 <- dcast(test_no_upc_sub_return, formula = TripType + VisitNumber + Weekday ~ DepartmentDescription, value.var = "return_perc", fun.aggregate = sum) 
head(main_cat_level_return2)

main_cat_level_return <- merge.data.frame(main_cat_level_return1, main_cat_level_return2, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
head(main_cat_level_return)

which(is.na(main_cat_level_return) == TRUE)
dim(main_cat_level_return)


#Merging all the main category data together 
main_cat_level1 <- merge.data.frame(main_cat_level_trans, main_cat_level_bought, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
main_cat_level <- merge.data.frame(main_cat_level1, main_cat_level_return, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)

rm(test1,main_cat_level1, main_cat_level_trans,main_cat_level_trans1,main_cat_level_trans2, main_cat_level_bought1, main_cat_level_bought2, main_cat_level_bought, main_cat_level_return1,main_cat_level_return2,main_cat_level_return)


################****** MAJOR DATASET ************#############################
head(main_cat_level)
#################################################################################



# Lets merge them all
# First merge UPCs and Sub Categories
test_visit_level_data <- merge.data.frame(Visit_level_metrics, main_cat_level, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
dim(test_visit_level_data)
head(test_visit_level_data)

test_visit_level_data <- as.data.frame(lapply(test_visit_level_data, function(x) ifelse(is.na(x), 0,x)))

save(test_visit_level_data, file = "test_visit_level_data.RData")


#----------------xxxxxxxxxxxxxx-------------------------------------------

x <- unique(train$FinelineNumber)
y <- unique(test$FinelineNumber)


train[which(is.na(train$FinelineNumber)==TRUE), "FinelineNumber"] <- 99999
test[which(is.na(test$FinelineNumber)==TRUE), "FinelineNumber"] <- 99999

rm_fines <- y[which(y %ni% x)]

rm(x,y)

test <- test[which(test$FinelineNumber %ni% rm_fines),]

test$FinelineNumber <- paste0("t_", as.character(sprintf("%04d", test$FinelineNumber)))


sub_cat_level <- dcast(test, formula = TripType + VisitNumber + Weekday ~ FinelineNumber, value.var = "ScanCount", fun.aggregate = function(x) sum(abs(x))) 

#head(sub_cat_level[,c(1:8)])
length(sub_cat_level$VisitNumber)
dim(sub_cat_level)

which(is.na(sub_cat_level) == TRUE)

# Merge with Trip level data
test449_plus_fine_rep_upc <- merge.data.frame(test_visit_level_data, sub_cat_level, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
dim(test449_plus_fine_rep_upc)


save(test449_plus_fine_rep_upc, file = "test449_plus_fine_rep_upc.RData")





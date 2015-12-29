setwd("/home/svu/e0008572/Walmart/20dec")

require(reshape2, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")
require(plyr, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")


#setwd("G:/Geek World/Kaggle/Walmart")
test <- read.csv("/home/svu/e0008572/Walmart/Input/test.csv", stringsAsFactors = FALSE)

test$TripType <- rep(0,nrow(test))
week_days <- cbind.data.frame(week_num = c(1,2,3,4,5,6,7), Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
test <- merge.data.frame(test, week_days, by.x = "Weekday", by.y = "Weekday")
head(test)
test$Weekday <- test$week_num
head(test)

test <- test[,c("TripType", "VisitNumber", "Weekday", "Upc", "ScanCount")]
str(test)

# Treating NAs 
test[which(is.na(test$FinelineNumber)==TRUE), "FinelineNumber"] <- 99999

test_base <- unique(test[,c("TripType", "VisitNumber", "Weekday")])
head(test_base)

test <- unique(test[,c("TripType", "VisitNumber", "Weekday", "Upc", "ScanCount")])
test <- test[which(is.na(test$Upc) == FALSE),]

length(unique(test$VisitNumber))

iter <- 1

for(i in 1 : 12)
{
  for(j in i : 12)
  {
    if((j-i) > 2){
      next
    }
    
    print(paste0("\ni ", i, " j ",j, " iter = ",iter, "\n"))
    test <- mutate(test, new_var = substr(as.character(sprintf("%012.0f", Upc)),i,j))
    head(test_base)
    
    test_cast <- dcast(test, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = sum)
    nrow(test_cast)
    head(test_cast)
    
    colnames(test_cast) <- c(names(test_cast[,1:3]), paste0("u_",i,j, "_val", names(test_cast[,4:length(test_cast)])))
    print(length(test_cast$VisitNumber))
    
    test_base <- merge.data.frame(test_base, test_cast, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
    
    iter = iter + 1
    
  }
}  

save(test_base, file = "Upc_all_perms_base_test.RData")


load("Upc_all_perms_base.RData")
train_base <- as.data.frame(lapply(train_base, function(x) ifelse(is.na(x), 0,x)))


train_base <- train_base[,which(names(train_base) %in% names(test_base))]
test_base <- test_base[,which(names(test_base) %in% names(train_base))]


load("/home/svu/e0008572/Walmart/17dec/train449_plus_fine_rep_upc.RData")
load("/home/svu/e0008572/Walmart/17dec/test449_plus_fine_rep_upc.RData")

train <- train449_plus_fine_rep_upc
test <- test449_plus_fine_rep_upc


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

#load("G:/Geek World/Kaggle/Walmart/17th Dec/train449_plus_fine_rep_upc.RData")
rm_cols <- grep("u_",names(train))
rm_cols <- c(rm_cols, which(names(train) %in% c("perc_t_HEALTH_AND_BEAUTY_AIDS", "t_HEALTH_AND_BEAUTY_AIDS", "b_HEALTH_AND_BEAUTY_AIDS", "perc_b_HEALTH_AND_BEAUTY_AIDS", "r_HEALTH_AND_BEAUTY_AIDS", "perc_r_HEALTH_AND_BEAUTY_AIDS", "t_99999")))
train <- train[,-rm_cols]

rm_cols <- grep("u_",names(test))
test <- test[,-rm_cols]

feature.names <- names(train)[which(names(train) %ni% c("TripType", "VisitNumber", "perc_t_HEALTH_AND_BEAUTY_AIDS", "t_HEALTH_AND_BEAUTY_AIDS", "b_HEALTH_AND_BEAUTY_AIDS", "perc_b_HEALTH_AND_BEAUTY_AIDS", "r_HEALTH_AND_BEAUTY_AIDS", "perc_r_HEALTH_AND_BEAUTY_AIDS", "t_99999"))]

# rm_cols <- which(names(train_base) %in% colnames(train_base[,sapply(train_base, function(v) var(v, na.rm=TRUE)==0)]))
# train_base <- train_base[,-rm_cols]
# rm_cols <- which(names(test_base) %in% colnames(test_base[,sapply(test_base, function(v) var(v, na.rm=TRUE)==0)]))
# test_base <- test_base[,-rm_cols]

save(train_base, file = "Upc_all_perms_base_1.RData")
save(test_base, file = "Upc_all_perms_base_test_1.RData")

train <- merge.data.frame(train, train_base, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)
test <- merge.data.frame(test, test_base, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE, all.y = TRUE)

nrow(train)
nrow(test)


length(unique(train$VisitNumber))
length(unique(test$VisitNumber))

train <- as.data.frame(lapply(train, function(x) ifelse(is.na(x), 0,x)))
which(is.na(train) == TRUE)

test <- as.data.frame(lapply(test, function(x) ifelse(is.na(x), 0,x)))
which(is.na(test) == TRUE)

save(train, file = "train_13k.RData")
save(test, file = "test_13k.RData")

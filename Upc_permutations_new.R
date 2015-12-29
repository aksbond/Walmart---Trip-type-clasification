setwd("/home/svu/e0008572/Walmart/20dec")

require(reshape2, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")
require(plyr, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")
require(randomForest, lib.loc = "/home/svu/e0008572/R/x86_64-pc-linux-gnu-library/3.2")

#setwd("G:/Geek World/Kaggle/Walmart")
train <- read.csv("/home/svu/e0008572/Walmart/Input/train.csv", stringsAsFactors = FALSE)

week_days <- cbind.data.frame(week_num = c(1,2,3,4,5,6,7), Weekday = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
train <- merge.data.frame(train, week_days, by.x = "Weekday", by.y = "Weekday")
head(train)
train$Weekday <- train$week_num
head(train)
train <- train[,c(2,3,1,4,5,6,7)]
str(train)

# Treating NAs 
train[which(is.na(train$FinelineNumber)==TRUE), "FinelineNumber"] <- 99999

train <- unique(train[,c("TripType", "VisitNumber", "Weekday", "Upc", "ScanCount")])
train <- train[which(is.na(train$Upc) == FALSE),]

train_base <- unique(train[,c("TripType", "VisitNumber", "Weekday")])
head(train_base)

length(unique(train$VisitNumber))

iter <- 1

for(i in 1 : 12)
{
  for(j in i : 12)
  {
    if((j-i) > 2){
      next
    }
    
    print(paste0("\ni ", i, " j ",j, " iter = ",iter, "\n"))
    train <- mutate(train, new_var = substr(as.character(sprintf("%012.0f", Upc)),i,j))
    head(train_base)
    
    train_cast <- dcast(train, formula = TripType + VisitNumber + Weekday ~ new_var, value.var = "ScanCount", fun.aggregate = sum)
    nrow(train_cast)
    head(train_cast)
    
    colnames(train_cast) <- c(names(train_cast[,1:3]), paste0("u_",i,j, "_val", names(train_cast[,4:length(train_cast)])))
    print(length(train_cast$VisitNumber))
    
    train_base <- merge.data.frame(train_base, train_cast, by.x = c("TripType", "VisitNumber", "Weekday"), by.y = c("TripType", "VisitNumber", "Weekday"), all.x = TRUE)
    
    iter = iter + 1
    
    
  }
  
}  

save(train_base, file = "Upc_all_perms_base.RData")

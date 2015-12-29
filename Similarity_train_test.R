setwd("G:/Geek World/Kaggle/Walmart/2nd Dec data")


load("train449_plus_fine_rep_upc.RData")
load("test449_plus_fine_rep_upc.RData")

names(train449_plus_fine_rep_upc)[1:500]
names(test449_plus_fine_rep_upc)[1:500]

category_train_b <- train449_plus_fine_rep_upc[,313:381]
category_train_b <- category_train_b[,-which(names(category_train_b) == "b_HEALTH_AND_BEAUTY_AIDS")]
dim(category_train_b)


category_test_b <- test449_plus_fine_rep_upc[,311:378]
dim(category_test_b)

category_both_b <- rbind.data.frame(category_train_b, category_test_b) 
dim(category_both_b)

#install.packages("qlcMatrix")
library(qlcMatrix)

#install.packages("dgCMatrix-class")
library(Matrix)

category_m <- as.matrix(category_both_b)
m <- as(category_m, "dgCMatrix")
dist_m <- cosSparse(X = m, weight = "idf")
# head(dist_m)
# dim(dist_m)
dist_mat <- as.matrix(dist_m)
melt_dist_m <- as.data.frame(dist_mat)

head(melt_dist_m)
melt_dist_m$var <- row.names(melt_dist_m)

library(reshape2)
library(plyr)
x <- melt(data = melt_dist_m, id.vars = "var")

ranked_x <- ddply(x, .(variable), transform, rank=rank(-value, ties.method="first"))
head(ranked_x,10)
ranked_x[which(ranked_x$rank > 5), "value"] <- 0
head(ranked_x,10)

cast_x <- dcast(ranked_x, variable ~ var, value.var = "value")
head(cast_x)
cast_x <- as.data.frame(lapply(cast_x, function(x) ifelse(is.na(x), 0,x)))
final_dist_m <- as.matrix(cast_x[,-1])

dim(final_dist_m)

i <- 1
j <- 1

which(is.nan(category_m))

for(i in 1 : nrow(category_m))
{
  temp <- matrix(rep(NA,68*68), nrow = 68, ncol = 68)
  temp_cntr <- 1
  for(j in which(category_m[i,] > 0))
  {
    
    temp[temp_cntr,] <- category_m[i,]
    temp[temp_cntr,] <- (category_m[i,j] * final_dist_m[j,])/10
    temp[temp_cntr,which(category_m[i,] > 0 )] <- category_m[i,which(category_m[i,] > 0)]
    temp_cntr <- temp_cntr + 1
  }
  if(temp_cntr > 1)
  {
    category_m[i,] <- colMeans(temp,na.rm = TRUE)
  }
  
  if(length(which(is.nan(category_m[i,]) == TRUE)) > 1)
  {
    i
    temp  
    break
  }
  
}

head(category_m)
dim(category_m)

#category_both_b <- rbind.data.frame(category_train_b, category_test_b) 

# Remove health and beauty aids 
train <- train449_plus_fine_rep_upc
train449_plus_fine_rep_upc <- train449_plus_fine_rep_upc[,-which(names(train449_plus_fine_rep_upc) == "b_HEALTH_AND_BEAUTY_AIDS")]
names(train449_plus_fine_rep_upc)[1:500]
train449_plus_fine_rep_upc[,313:380] <- category_m[1:95674,] 

head(train[,313:380])
head(train449_plus_fine_rep_upc[,313:380])

tail(train[,313:380])
tail(train449_plus_fine_rep_upc[,313:380])




test <- test449_plus_fine_rep_upc
test449_plus_fine_rep_upc[,311:378] <- category_m[95675:nrow(category_m),] 

head(test[,311:378])
head(test449_plus_fine_rep_upc[,311:378])

tail(test[,311:378])
tail(test449_plus_fine_rep_upc[,311:378])


dim(train449_plus_fine_rep_upc)
dim(test449_plus_fine_rep_upc)

names(train449_plus_fine_rep_upc)[which(names(train449_plus_fine_rep_upc) %ni% names(test449_plus_fine_rep_upc))]






# NOW DO THE SAME FOR PERCENTAGE BOUGHT


names(train449_plus_fine_rep_upc)[1:500]
names(test449_plus_fine_rep_upc)[1:500]

category_train_b <- train449_plus_fine_rep_upc[,381:449]
category_train_b <- category_train_b[,-which(names(category_train_b) == "perc_b_HEALTH_AND_BEAUTY_AIDS")]
dim(category_train_b)


category_test_b <- test449_plus_fine_rep_upc[,379:446]
dim(category_test_b)

category_both_b <- rbind.data.frame(category_train_b, category_test_b) 
dim(category_both_b)

#install.packages("qlcMatrix")
library(qlcMatrix)

#install.packages("dgCMatrix-class")
library(Matrix)

category_m <- as.matrix(category_both_b)
m <- as(category_m, "dgCMatrix")
dist_m <- cosSparse(X = m, weight = "idf")
# head(dist_m)
# dim(dist_m)
dist_mat <- as.matrix(dist_m)
melt_dist_m <- as.data.frame(dist_mat)

head(melt_dist_m)
melt_dist_m$var <- row.names(melt_dist_m)

library(reshape2)
library(plyr)
x <- melt(data = melt_dist_m, id.vars = "var")

ranked_x <- ddply(x, .(variable), transform, rank=rank(-value, ties.method="first"))
head(ranked_x,10)
ranked_x[which(ranked_x$rank > 5), "value"] <- 0
head(ranked_x,10)

cast_x <- dcast(ranked_x, variable ~ var, value.var = "value")
head(cast_x)
cast_x <- as.data.frame(lapply(cast_x, function(x) ifelse(is.na(x), 0,x)))
final_dist_m <- as.matrix(cast_x[,-1])

dim(final_dist_m)

i <- 1
j <- 1

which(is.nan(category_m))

for(i in 1 : nrow(category_m))
{
  temp <- matrix(rep(NA,68*68), nrow = 68, ncol = 68)
  temp_cntr <- 1
  for(j in which(category_m[i,] > 0))
  {
    
    temp[temp_cntr,] <- category_m[i,]
    temp[temp_cntr,] <- (category_m[i,j] * final_dist_m[j,])/10
    temp[temp_cntr,which(category_m[i,] > 0 )] <- category_m[i,which(category_m[i,] > 0)]
    temp_cntr <- temp_cntr + 1
  }
  if(temp_cntr > 1)
  {
    category_m[i,] <- colMeans(temp,na.rm = TRUE)
  }
  
  if(length(which(is.nan(category_m[i,]) == TRUE)) > 1)
  {
    i
    temp  
    break
  }
  
}

head(category_m)
dim(category_m)

#category_both_b <- rbind.data.frame(category_train_b, category_test_b) 

# Remove health and beauty aids 
#train <- train449_plus_fine_rep_upc
train449_plus_fine_rep_upc <- train449_plus_fine_rep_upc[,-which(names(train449_plus_fine_rep_upc) == "perc_b_HEALTH_AND_BEAUTY_AIDS")]
names(train449_plus_fine_rep_upc)[1:500]
train449_plus_fine_rep_upc[,381:448] <- category_m[1:95674,] 

head(train[,381:448])
head(train449_plus_fine_rep_upc[,381:448])

tail(train[,381:448])
tail(train449_plus_fine_rep_upc[,381:448])



names(test449_plus_fine_rep_upc)[1:500]
#test <- test449_plus_fine_rep_upc
test449_plus_fine_rep_upc[,379:446] <- category_m[95675:nrow(category_m),] 

head(test[,379:446])
head(test449_plus_fine_rep_upc[,379:446])

tail(test[,379:446])
tail(test449_plus_fine_rep_upc[,379:446])


dim(train449_plus_fine_rep_upc)
dim(test449_plus_fine_rep_upc)

names(train449_plus_fine_rep_upc)[which(names(train449_plus_fine_rep_upc) %ni% names(test449_plus_fine_rep_upc))]

setwd("G:/Geek World/Kaggle/Walmart/2nd Dec data")

save(train449_plus_fine_rep_upc, file = "G:/Geek World/Kaggle/Walmart/7th Dec/Similarity_matrix/train449_plus_fine_rep_upc.RData")
save(test449_plus_fine_rep_upc, file = "G:/Geek World/Kaggle/Walmart/7th Dec/Similarity_matrix/test449_plus_fine_rep_upc.RData")

# load dataset - dat_raw to current working directory from github repository: https://github.com/Henryisagoodguy/EDX-Capstone-Project-Mobile-Price-prediction/blob/main/dat_raw.Rdata

# install packages
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(gam)) install.packages("gam", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(gam)
library(rpart)
library(randomForest)

# read dataset - dat_raw
load("dat_raw.rda")
dim(dat_raw)
head(dat_raw)

# Preprocessing: clean data to remove unuseful data, keep data for mobile and features of Brand, RAM, and ROM
dat <- dat_raw %>% filter(Product == "Mobile Phone") %>% select(Brand, Price, RAM, ROM)
head(dat)
dim(dat)

# Generate data sets for train and test
set.seed(1)
test_index <- createDataPartition(dat$Price, times = 1, p = 0.2, list = FALSE)
temp <- dat[test_index, ]
train_set <- dat[-test_index, ]
test_set <- temp %>% semi_join(train_set, by = "Brand") %>% semi_join(train_set, by ="RAM") %>% semi_join(train_set, by = "ROM")

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# Use group average price as predict price
mu <- train_set %>% group_by(Brand, RAM, ROM) %>% summarize(mu = mean(Price))

predicted_price_mu <- test_set %>% left_join(mu, by=c("Brand", "RAM", "ROM")) %>% pull(mu)

mu_rmse <- RMSE(test_set$Price, predicted_price_mu)

mu_rmse

mean(test_set$Price)

# Use KNN model to predict price.
train_knn <- train(Price ~ ., method = "knn", data = train_set)

predicted_price_knn <- predict(train_knn, test_set, type = "raw")

knn_rmse <- RMSE(test_set$Price, predicted_price_knn)

knn_rmse 

# fine tune KNN model
data.frame(k = seq(50, 70, 5))

train_knn_tunegrid <- train(Price ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(50, 70, 5)))

predicted_price_knntunegird <- predict(train_knn_tunegrid, test_set, type = "raw")

knntunegrid_rmse <- RMSE(test_set$Price, predicted_price_knntunegird)

knntunegrid_rmse

train_knn_tunegrid$bestTune

train_knn_tunegrid$finalModel

# Adjust cross validation parameters to speed up computation
control <- trainControl(method = "cv", number = 10, p = .9)

train_knn_cv <- train(Price ~ ., method = "knn", data = train_set, tuneGrid = data.frame(k = seq(50, 80, 10)), trControl = control)

predicted_price_cv <- predict(train_knn_cv, test_set, type = "raw")

knncv_rmse <- RMSE(test_set$Price, predicted_price_cv)

knncv_rmse

# Use Loess model to predict price

library(gam)
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(Price ~ ., method = "gamLoess", tuneGrid = grid, data = train_set)

predicted_price_loess <- predict(train_loess, test_set, type = "raw")

loess_rmse <- RMSE(test_set$Price, predicted_price_loess)

loess_rmse

# Use decision tree model to predict price

library(rpart)

train_dt <- train(Price ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0.1, 1, len = 25)), data = train_set)

predicted_price_dt <- predict(train_dt, test_set, type = "raw")

dt_rmse <- RMSE(test_set$Price, predicted_price_dt)

dt_rmse

# predict prices with random forest model
library(randomForest)

set.seed(1)

train_rf <- randomForest(Price ~ ., data = train_set)

predicted_price_rf <- predict(train_rf, test_set)

rf_rmse <- RMSE(test_set$Price, predicted_price_rf)

rf_rmse

# use cross validation to choose parameter to optimize random forest model.
set.seed(1)

train_rf_2 <- train(Price ~ ., method = "Rborist", tuneGrid = data.frame(predFixed = 2, minNode = c(3, 50)), data = train_set)

predicted_price_rf_2 <- predict(train_rf_2, test_set)

rf_rmse_2 <- RMSE(test_set$Price, predicted_price_rf_2)

rf_rmse_2





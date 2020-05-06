#libraries

library(caTools)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

#load data & first check

train <- read.csv('D:/Data science/Kaggle/RestaurantRevenuePredict/train.csv')

str(train)
summary(train)
head(train)

#EDA

unique(train$City)
unique(train$City.Group)
unique(train$Type)


city <- unique(train$City)

cityLabel <- 1:34

city_label <- data.frame(city,cityLabel)

city_label


train_new <- merge(x=train,y=city_label,by.x = 'City',by.y = 'city',all.x = TRUE)
head(train_new)

train_new$cityGroupLabel <- if_else(train_new$City.Group == 'Big Cities',1,0)
head(train_new)

train_new$typeLabel <- if_else(train_new$Type == 'IL',1
                               ,if_else(train_new$Type == 'FC',2,3))

head(train_new)

str(train)

train_new$year <- strtoi(substr(train_new$Open.Date,7,10))
str(train_new)

train_new$age <- 2020 - train_new$year
str(train_new)

drop <- c('City','Id','Open.Date','City.Group','Type','year')

train_final <- train_new[,!names(train_new) %in% drop]

head(train_final)

#EDA v.2

head(train_final)

train_finalLong <- gather(train_final,key='measure',value='value',-revenue)
train_finalLong
unique(train_finalLong$measure)

revenue <- train_final$revenue
revenue_rep <- rep(revenue, 41)

  #Try 1 plot
ggplot(train_final,aes(x=P2,y=revenue))+
  geom_point()+
  ylim(0,1.0e+07)

  #Facet plot
ggplot(train_finalLong,aes(x=value,y=revenue_rep))+
  geom_line()+
  facet_wrap(~measure)+
  ylim(0,1.0e+07)
  
#EDA v.3 Big/Other cities

train_BigCityLong <- gather(train_final[train_final$cityGroupLabel == 1,],key='measure',value='value',-revenue)
str(train_BigCityLong)

revenueBigCity <- train_final[train_final$cityGroupLabel == 1,'revenue']
head(revenueBigCity)
length(revenueBigCity)

revenueBigCity_rep <- rep(revenueBigCity, 41)

  #Facet plot
ggplot(train_BigCityLong,aes(x=value,y=revenueBigCity_rep))+
  geom_line()+
  facet_wrap(~measure)+
  ylim(0,1.0e+07)

#Try fit model

train_final_trim <- train_final[train_final$revenue<1.0e+07,]
summary(train_final_trim)

mod_test <- lm(revenue ~ .,data=train_final_trim)
summary(mod_test)

mod_test2 <- lm(revenue ~ P20+P28,data=train_final_trim)
summary(mod_test2)

#Train model

set.seed(101)
sample = sample.split(train_final_trim, SplitRatio = 0.7)
head(sample)

tr = subset(train_final_trim, sample == TRUE)
te = subset(train_final_trim, sample == FALSE)

mod_final <- lm(revenue ~ P20+P28,data=tr)
summary(mod)

pred_final <- predict(mod_final,newdata = te)
head(pred_final)

SSE = sum((pred_final - te$revenue)^2)
SST = sum((mean(te$revenue) - te$revenue)^2)
R2 = 1-SSE/SST
R2

RMSE = sqrt(SSE/nrow(te))
RMSE

mod_final2 <- lm(revenue ~ P20+P28,data = train_final)

#Predict on test data

test_df <- read.csv('D:/Data science/Kaggle/RestaurantRevenuePredict/test.csv')

pred_test <- predict(mod_final2,newdata = test_df)

length(pred_test)
nrow(test_df)

head(pred_test)

Id <- test_df[,'Id']
Prediction <- pred_test
filename <- 'result.csv'

result <- data.frame(Id,Prediction)
head(result)
write.csv(result,paste('D:/Data science/Kaggle/RestaurantRevenuePred_R/',filename),row.names = FALSE)

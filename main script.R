#libraries

library(caTools)
library(dplyr)

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


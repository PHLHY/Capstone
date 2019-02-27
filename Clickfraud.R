#Used for quicker dataset loading due to the big datasets 
library(data.table)

library(plyr)

#data visualization 
library(ggplot2)

#loading up datasets 
train <- fread("all/train.csv", showProgress = T)
test <- fread("all/test.csv", showProgress = T)

#quick look at the data 
head(train)
tail(train)
str(train)

#checking for missing values broken down by variables  
colSums(is.na(test))
colSums(is.na(train))

#Note attribute_time having blank entries which makes sense since they did not download app (target variable). Proven below where the number matches 
colSums(train=="")
table(train$is_attributed)

#taking a look at the dataset of target variable. Noted that it is skewed (0.24% shows target attribute) 
table(train$is_attributed)

#to control randomization for future processing 
set.seed(575)

#sampling to make this datasets smaller for easier computation. Note computer limitations and crashing on R. 
#Would usually do a 70/30 split, however, original percentage differences between test and train is 90/10 split 
s.train <- train[sample(nrow(train), 1000000), ]
s.test <- test[sample(nrow(test), 100000), ]

#target variable. Still skewed. 0.25% shows target attribute. Similar to original dataset.
#will need to balance dataset (undersample/oversample)
table(s.train$is_attributed) 

#splitting click_time into different columns for better analysis 
#removing click_time and year and month since they are the same for all 
#consider adding in seconds?
s.train$click_time<-as.POSIXct(s.train$click_time, format = "%Y-%m-%d %H:%M")
s.train$year=year(s.train$click_time)
s.train$month=month(s.train$click_time)
s.train$days=weekdays(s.train$click_time)
s.train$hour=hour(s.train$click_time)
table(s.train$year)
table(s.train$month)
s.train$click_time=NULL
s.train$year=NULL
s.train$month=NULL

#changing is_attributed and to factor
s.train$is_attributed = factor(s.train$is_attributed)

#variables frequency, need to look at ggplot2 for desc and top 15 
count.trainip <- count(s.train, "ip")
ggplot(s.train, aes(x=ip), color="steelblue") + geom_bar()
count.trainapp <- count(s.train, "app")
ggplot(s.train, aes(x=app), color="steelblue") + geom_bar()
count.traindevice <- count(s.train, "device")
ggplot(s.train, aes(x=device), color="steelblue") + geom_bar()
count.trainos <- count(s.train, "os")
ggplot(s.train, aes(x=os), color="steelblue") + geom_bar()
count.trainchannel <- count(s.train, "channel")
ggplot(s.train, aes(x=channel), color="steelblue") + geom_bar()

#Libraries used 
library(data.table)
library(plyr)
library(corrplot)
library(MASS)
library(caret)
install.packages("ROSE")
library(ROSE)
library(DMwR)
library(caret)
library(xgboost)
install.packages("pROC")
library(pROC)

#Quicker loading of datasets
train <- fread("all/train.csv", showProgress = T)
test <- fread("all/test.csv", showProgress = T)

#Quick look at the data 
head(train)
tail(train)
str(train)

#Checking for missing values broken down by variables  
colSums(is.na(test))
colSums(is.na(train))

#Note attribute_time having blank entries which makes sense since they did not download app (target variable). Proven below where the number matches 
colSums(train=="")
table(train$is_attributed)

#Taking a look at the dataset of target variable. Noted that it is skewed (0.0025% shows target attribute) 
#456846/184447044
table(train$is_attributed)

#To control randomization for future processing 
set.seed(575)

#Sampling to make this dataset smaller for easier computation. Note computer limitations and crashing on R. 
reduced.set <- train[sample(nrow(train), 25000), ]

#Ensuring reduced set has similar distribution to original dataset for target variable
#60/24940 = 0.0024%
table(reduced.set$is_attributed)

#Splitting click_time into different columns for better analysis 
#Removing click_time and year and month since they are the same for all 
reduced.set$click_time<-as.POSIXct(reduced.set$click_time, format = "%Y-%m-%d %H:%M")
reduced.set$year=year(reduced.set$click_time)
reduced.set$month=month(reduced.set$click_time)
reduced.set$days=weekdays(reduced.set$click_time)
reduced.set$hour=hour(reduced.set$click_time)
table(reduced.set$year)
table(reduced.set$month)
reduced.set$click_time=NULL
reduced.set$year=NULL
reduced.set$month=NULL

#Changing days to numeric (monday = 1, Tuesday =2, wednesday-3, thursday = 4). 
reduced.set$days <- gsub("Thursday", "4", reduced.set$days)
reduced.set$days <- gsub("Wednesday", "3", reduced.set$days)
reduced.set$days <- gsub("Tuesday", "2", reduced.set$days)
reduced.set$days <- gsub("Monday", "1", reduced.set$days)

#Will remove attributed_time as it is dependent on is_attributed meaning there is a value if is_attriuted = 1 
reduced.set$attributed_time=NULL

#Plotting graphs 
count.target <- table(reduced.set$is_attributed)
barplot(count.target, main="Barplot for Target Attribute", xlab="Target Attribute")

#Split by target attribute 
split0 <- reduced.set[which(reduced.set$is_attributed == "0"),]
split1 <- reduced.set[which(reduced.set$is_attributed == "1"),]

#Top 10 frequency for ips (not target/target)
count.ip0 <- sort(table(split0$ip), decreasing = TRUE)[1:10]
count.ip1 <- sort(table(split1$ip), decreasing = TRUE)[1:10]

#Top 10 frequency for apps (not target/target)
count.app0 <- sort(table(split0$app), decreasing = TRUE)[1:10]
count.app1 <- sort(table(split1$app), decreasing = TRUE)[1:10]

#Top 10 frequency for devices (not target/target)
count.device0 <- sort(table(split0$device), decreasing = TRUE)[1:10]
count.device1 <- sort(table(split1$device), decreasing = TRUE)[1:10]

#Top 10 frequency for os's (not target/target)
count.os0 <- sort(table(split0$os), decreasing = TRUE)[1:10]
count.os1 <- sort(table(split1$os), decreasing = TRUE)[1:10]

#Top 10 frequency for channels (not target/target)
count.channel0 <- sort(table(split0$channel), decreasing = TRUE)[1:10]
count.channel1 <- sort(table(split1$channel), decreasing = TRUE)[1:10]

#Top frequency for days (not target/target)
count.day0 <- sort(table(split0$days), decreasing = TRUE)[1:4]
count.day1 <- sort(table(split1$days), decreasing = TRUE)[1:4]

#Top frequency for hours (not target/target)
count.hour0 <- sort(table(split0$hour), decreasing = TRUE)[1:24]
count.hour1 <- sort(table(split1$hour), decreasing = TRUE)[1:24]

#Barplots  
par(mfrow=c(2,2))
barplot(count.ip0, main="Top 10 for ips (not target)", xlab="ips", col="darkblue", las=2)
barplot(count.ip1, main="Top 10 for ips (target)", xlab="ips", col="red", las=2)
barplot(count.app0, main="Top 10 for apps (not target)", xlab="apps", col="darkblue", las=2)
barplot(count.app1, main="Top 10 for apps (target)", xlab="apps", col="red", las=2)

par(mfrow=c(2,2))
barplot(count.device0, main="Top 10 for devices (not target)", xlab="devices", col="darkblue", las=2)
barplot(count.device1, main="Top 10 for devices (target)", xlab="devices", col="red", las=2)
barplot(count.os0, main="Top 10 for os's (not target)", xlab="os's", col="darkblue", las=2)
barplot(count.os1, main="Top 10 for os's (target)", xlab="os's", col="red", las=2)

par(mfrow=c(2,2))
barplot(count.channel0, main="Top 10 for channels (not target)", xlab="channels", col="darkblue", las=2)
barplot(count.channel1, main="Top 10 for channels (target)", xlab="channels", col="red", las=2)
barplot(count.day0, main="Top days (not target)", xlab="days", col="darkblue", las=2)
barplot(count.day1, main="Top days (target)", xlab="days", col="red", las=2)

par(mfrow=c(1,1))
barplot(count.hour0, main="Top hours (not target)", xlab="hours", col="darkblue", las=2)
barplot(count.hour1, main="Top hours (target)", xlab="hours", col="red", las=2)

#Correlation 
#Changing factor to numeric for correlation. Will need to change back to target attribute back to factor later.
reduced.set$is_attributed <- as.numeric(as.character(reduced.set$is_attributed))
reduced.set$days <- as.numeric(as.character(reduced.set$days))
#cor (pearson), note negative weak correlation for channel and app and for days and hour
corrplot(cor(reduced.set, method="spearman"), method="number")

#Feature selection 
#Forward 
full <- lm(is_attributed~., data=reduced.set)
null <- lm(is_attributed~1, data=reduced.set)
forward <- stepAIC(null,scope=list(lower=null, upper=full), direction ="forward", trace=TRUE)
summary(forward)
#Results shows ip, app, channel, os should be selected only 

#Target variable back to factor 
reduced.set$is_attributed  = factor(reduced.set$is_attributed)

#Removal of variables for modelling as determined by feature selection
reducedvar.set <- reduced.set[,c(-3,-7,-8)]

#To split given training set to training/testing sets for modelling
check_index <- sample(1:nrow(reducedvar.set ), 0.7 * nrow(reducedvar.set ))
train.set <- reducedvar.set[check_index,]
test.set <- reducedvar.set [-check_index,]

#Target variable. Still skewed. 0.0025% shows target attribute. Closely similar to original dataset.
#49/17451 = 0.0028%
table(train.set$is_attributed) 

#Cross Validation 
train_control<- trainControl(method="cv", number=10, summaryFunction=twoClassSummary, classProbs=T, savePredictions = T)

#To fix Classprobs error for names 
levels(train.set$is_attributed) <- c("No", "Target")
levels(test.set$is_attributed) <- c("No", "Target")

#No data normalization as all models are Tree-based 
#RandomForest via Caret (No balancing) 
NB_RF.train = train(is_attributed ~ ., data = train.set, trControl = train_control, method = "rf", metric="ROC")
NB_RF.train
NB_RF.predict=predict(NB_RF.train, test.set, type="prob")
roc(test.set$is_attributed, NB_RF.predict$Target)$auc

#DecisionTree via Caret (No balancing) 
NB_DT.train = train(is_attributed ~ ., data = train.set, trControl = train_control, method = "rpart", metric="ROC")
NB_DT.train
NB_DT.predict=predict(NB_DT.train, test.set, type="prob")
roc(test.set$is_attributed, NB_DT.predict$Target)$auc

#XGboost via Caret (No balancing) 
NB_XGB.train = train(is_attributed ~ ., data = train.set, trControl = train_control, method = "xgbTree", metric="ROC")
NB_XGB.train
NB_XGB.predict=predict(NB_XGB.train, test.set, type="prob")
roc(test.set$is_attributed, NB_XGB.predict$Target)$auc

#Balancing training set (ROSE Oversample/Undersample)
balanced_train.set <- ovun.sample(is_attributed ~ ., data = train.set, method = "both", p=0.5, N=17500, seed = 575)$data
table(balanced_train.set$is_attributed)

#RandomForest via Caret (ROSE) 
ROSE_RF.train = train(is_attributed ~ ., data = balanced_train.set, trControl = train_control, method = "rf", metric="ROC")
ROSE_RF.train
ROSE_RF.predict=predict(ROSE_RF.train, test.set, type="prob")
roc(test.set$is_attributed, ROSE_RF.predict$Target)$auc

#DecisionTree via Caret (ROSE) 
ROSE_DT.train = train(is_attributed ~ ., data = balanced_train.set, trControl = train_control, method = "rpart", metric="ROC")
ROSE_DT.train
ROSE_DT.predict=predict(ROSE_DT.train, test.set, type="prob")
roc(test.set$is_attributed, ROSE_DT.predict$Target)$auc

#XGboost via Caret (ROSE) 
ROSE_XGB.train = train(is_attributed ~ ., data = balanced_train.set, trControl = train_control, method = "xgbTree", metric="ROC")
ROSE_XGB.train
ROSE_XGB.predict=predict(ROSE_XGB.train, test.set, type="prob")
roc(test.set$is_attributed, ROSE_RF.predict$Target)$auc

#Balancing training set (SMOTE)
SMOTE_train.set <- SMOTE(is_attributed ~ ., data = train.set, perc.over=17500, perc.under=100)
table(SMOTE_train.set$is_attributed)

#RandomForest via Caret (SMOTE)
SMOTE_RF.train = train(is_attributed ~ ., data = SMOTE_train.set, trControl = train_control, method = "rf", metric="ROC")
SMOTE_RF.train
SMOTE_RF.predict=predict(SMOTE_RF.train, test.set, type="prob")
roc(test.set$is_attributed, SMOTE_RF.predict$Target)$auc

#DecisionTree via Caret (SMOTE) 
SMOTE_DT.train = train(is_attributed ~ ., data = SMOTE_train.set, trControl = train_control, method = "rpart", metric="ROC")
SMOTE_DT.train
SMOTE_DT.predict=predict(SMOTE_DT.train, test.set, type="prob")
roc(test.set$is_attributed, SMOTE_DT.predict$Target)$auc

#XGboost via Caret (SMOTE) 
SMOTE_XGB.train = train(is_attributed ~ ., data = SMOTE_train.set, trControl = train_control, method = "xgbTree", metric="ROC")
SMOTE_XGB.train
SMOTE_XGB.predict=predict(SMOTE_XGB.train, test.set, type="prob")
roc(test.set$is_attributed, SMOTE_XGB.predict$Target)$auc

install.packages('caTools')
install.packages('caret')
install.packages('rpart.plot')
install.packages('rattle')
install.packages('ROCR')
install.packages('pROC')
install.packages('olsrr')
install.packages('e1071')
install.packages('Hmisc')
install.packages("survival")
install.packages('corrplot')
install.packages('kernlab')
install.packages("Survival")

#clearing working environment
rm(list=ls())

library(caTools)
library(corrplot)
library(kernlab)
library(caTools)
library(caret) # Accuracy
library(ggplot2)
library(rpart)
library(rpart.plot)
library(rattle)
library(ROCR)
library(pROC)
library(olsrr)
library(e1071)
library(Hmisc)
#library(Survival)

#Using Local Dataset location
data <- read.csv(file="C:/Users/HP/Desktop/DataMining_Project/OnlineNewsPopularity/OnlineNewsPopularity.csv", header=TRUE, sep=",")

#summary before cleaning
summary(data)

dim(data)

# Exploratory Data Analysis & Cleaning Data:
# Check for any missing values:
sum(is.na(data))

#Removing outlier
data=data[!data$n_non_stop_words==1042,]
summary(data)

#Removing non predictive variables 
data <- subset( data, select = -c(url ,is_weekend ) )

dim(data)

#Combining Plots for EDA for visual analysis
par(mfrow=c(3,4))
for(i in 2:length(data)){hist(data[,i],
                              xlab=names(data)[i] , main = paste("[" , i , "]" ,
                                                                 "Histogram of", names(data)[i])  )}

#read in data
raw<-read.csv("OnlineNewsPopularity.csv")
dim(raw)

head(raw)

#sapply(raw, class)
raw[,61] <- as.numeric(raw[,61])
raw <- raw[,-1]

suppressWarnings(library(ggplot2))
suppressWarnings(library(reshape2))
#suppressWarnings(library(DAAG))

raw <- raw[raw[,4]<1,]

for(i in c(11,20,44,45,46,48,49,50,53))raw <- raw[raw[,i]!=0,]

for(i in c(3,7,8,9,10,22,26:30,39:43,47, 60)){
  if(!sum(raw[,i]==0)){raw[,i] <- log(raw[,i]); names(raw)[i] <- paste("log_",names(raw)[i], sep="")}
  else{raw[,i] <- sqrt(raw[,i]); names(raw)[i] <- paste("sqrt_",names(raw)[i], sep="")}
}

raw <- raw[, -c(19,21,23,25)]

raw$news_sub <- rep("other", nrow(raw))
raw$news_sub[raw$data_channel_is_lifestyle==1] <- "lifeStyle"
raw$news_sub[raw$data_channel_is_entertainment==1] <- "entertainment"
raw$news_sub[raw$data_channel_is_bus==1] <- "bus"
raw$news_sub[raw$data_channel_is_socmed==1] <- "socmed"
raw$news_sub[raw$data_channel_is_tech==1] <- "tech"
raw$news_sub[raw$data_channel_is_world==1] <- "world"
# plot
p1 <- ggplot(data=raw, aes(as.factor(news_sub), log_shares))
p1 + geom_boxplot()

raw$news_day <- rep("Sunday", nrow(raw))
raw$news_day[raw$weekday_is_monday==1] <- "Monday"
raw$news_day[raw$weekday_is_tuesday==1] <- "Tuesday"
raw$news_day[raw$weekday_is_wednesday==1] <- "Wednesday"
raw$news_day[raw$weekday_is_thursday==1] <- "Thursday"
raw$news_day[raw$weekday_is_friday==1] <- "Friday"
raw$news_day[raw$weekday_is_saturday==1] <- "Saturday"
#Check 
p1 <- ggplot(data=raw, aes(as.factor(news_day), log_shares))
p1 + geom_boxplot()

raw2 <- raw[,-c(13:18, 27:33, 57,58)]

x <- as.matrix(scale(raw2[,-43]))
dim(x)

dim(x)

corx <- cor(x)
dim(corx)

evd<-svd(corx)
w <- x %*% evd$u
pca2 <- as.data.frame(cbind(w[,1:4], raw2$log_shares))
names(pca2) <- c("Component_1", "Component_2", "Component_3", "Component_4", "log_shares")
#Map share on first two components
pcaplot <- ggplot(aes(Component_1, Component_2, colour=log_shares), data=pca2)
pcaplot + geom_point(alpha = 0.5) + scale_colour_gradient(limits=c(0, 14), low="white", high="red")

#Map share on 3rd and 4th components
pcaplot <- ggplot(aes(Component_3, Component_4, colour=log_shares), data=pca2)
pcaplot + geom_point(alpha = 0.5) + scale_colour_gradient(limits=c(0, 14), low="white", high="red")

    corm<-1-cor(raw2)
heatmap(corm)

summary(corm[43,-43])
qplot(1-corm[43,-43], binwidth=0.01, fill=..count.., geom="histogram", xlab="correlation")

hc <- hclust(dist(corm))
par(mfrow=c(1,1))
plot(hc)

#Converting categorical values from numeric to factor - Weekdays
for (i in 31:37){
  data[,i] <- factor(data[,i])
}

#Converting categorical values from numeric to factor - News subjects
for (i in 13:18){
  data[,i] <- factor(data[,i])
}


sapply(data, class)


#Checking importance of news subjects(categorical) on shares
for (i in 13:18){
  
  boxplot(log(data$shares) ~ (data[,i]), xlab=names(data)[i] , ylab="shares")
}

#Checking importance of weekdays on news shares
for (i in 31:37){
  
  boxplot(log(data$shares) ~ (data[,i]), xlab=names(data)[i] , ylab="shares")
}


# Sampling the dataset into training data and test data:

splitdata<- sample.split(data,SplitRatio = 0.7)
train_data <- subset(data, splitdata == TRUE)
test_data <- subset(data, splitdata == FALSE)





#Using Linear Model

fit_lm <- lm(shares ~ ., data = train_data)
layout(matrix(1:4, 2, 2))
plot(fit_lm)


summary(fit_lm)

#Forward stepwise regression to select variables
#using stepwise regression
#Taking important variables


d2 <- subset( data, select = c(n_tokens_title,timedelta, kw_avg_avg, self_reference_min_shares,
                             kw_min_avg, num_hrefs, kw_max_max, avg_negative_polarity,
                             data_channel_is_entertainment, weekday_is_monday, 
                             LDA_02, kw_min_max, average_token_length, global_subjectivity,
                             kw_max_min, global_rate_positive_words, 
                             n_tokens_content, n_non_stop_unique_tokens,
                             min_positive_polarity, weekday_is_saturday,
                             data_channel_is_lifestyle, kw_avg_max,
                             kw_avg_min, title_sentiment_polarity,
                             num_self_hrefs, self_reference_max_shares,
                             n_tokens_title, LDA_01, kw_min_min, shares) )
summary(d2$shares)
dim(d2)

# Sampling the dataset based on best variables

splitdata<- sample.split(d2,SplitRatio = 0.7)
traindata <- subset(d2, splitdata == TRUE)
testdata <- subset(d2, splitdata == FALSE)


# Now, we fit a model with all the variables;
fit_lmbest <- lm(shares ~ ., data = traindata)

summary(fit_lmbest)
layout(matrix(1:4, 2, 2))
plot(fit_lmbest)

#taking log shares to optimize model
####################################
d2$shares <- log(d2$shares)
summary(d2$shares)
fit_lmlog <- lm(shares ~ ., data = traindata)
summary(fit_lmlog)

sum(is.na(d2))

# Define articles with shares larger than 7.244 (median) as popular article
#######################################################################
d2$shares <- as.factor(ifelse(d2$shares > 7.244,1,0))
hist(log(data$shares), col = c("black", "gray") 
     , xlab="Shares", main="Shares Frequency" )


# Sampling the dataset based on best variables
splitdata<- sample.split(d2,SplitRatio = 0.7)
traindata <- subset(d2, splitdata == TRUE)
testdata <- subset(d2, splitdata == FALSE)

#Implementing Nayive bayes 
nav.mod <- naiveBayes(shares~.,traindata)
#pred <- predict(nav.mod,testdata)
newsnb.pred<-predict(nav.mod,testdata,type="class" )
newsnb.prob<-predict(nav.mod,testdata,type="raw" )

confusionMatrix(newsnb.pred,testdata$shares)




#ROC Curve for Naive Bayes
newsnb.roc <- roc(testdata$shares,newsnb.prob[,2])
plot(newsnb.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="grey", print.thres=TRUE , main="ROC for Naive Bayes")

#Implementing KNN

kNN3 <- train(shares~., data = traindata, method = "knn", 
              maximize = TRUE,
              trControl = trainControl(method = "cv", number = 10),
              preProcess=c("center", "scale"))
ggplot(kNN3) + geom_line() + geom_smooth() + theme_light()

kNN3pred<-predict(kNN3,testdata,type="raw")
KNN3prob<-predict(kNN3,testdata,type="prob")

#predictedkNN3 <- predict(kNN3,newdata=testdata)
confusionMatrix(kNN3pred, testdata$shares)




#ROC kNN
newsKnnRoc <- roc(testdata$shares,KNN3prob[,2])
plot(newsKnnRoc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="grey", print.thres=TRUE , main="ROC for kNN")

#  Implementing CART

# Classification and Regression Trees
news.cart<-rpart(shares ~.,traindata,method='class')

par(mfrow=c(1,1))
fancyRpartPlot(news.cart, digits=2, palettes = c("Purples", "Oranges"))

#predict
cart_pred<-predict( news.cart,testdata ,type="class")
cart_prob<-predict( news.cart,testdata ,type="prob")

# Confusion matrix
confusionMatrix(cart_pred, testdata$shares)

# ROC Curve
newsCartRoc <- roc(testdata$shares,cart_prob[,2])
plot(newsCartRoc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="grey", print.thres=TRUE , main="ROC for CART")

#Correlation matrix

cm <- d2
dim(cm)
for (i in 1:30){
  cm[,i] <- as.numeric(cm[,i])
}

sapply(cm, class)
library(Hmisc)

cormat <- cor(cm,use = "everything",
              method = c("pearson","kendall","spearman"))
plot(cormat)



#visualizing Correllation matrix
library(corrplot)
corrplot(cormat, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt =90,main="Visualizing Correlation Matrix")


install.packages("C50")
library(C50)

#Implementing C5.0


newsc50<-C5.0(shares ~.,traindata,trials=20)
newsc50.pred<-predict(newsc50,testdata,type="class" )
newsc50.prob<-predict(newsc50,testdata,type="prob" )
# Confusion matrix
confusionMatrix(newsc50.pred, testdata$shares)
plot(newsc50)



#ROC Curve for C5.0
newsc50.roc <- roc(testdata$shares,newsc50.prob[,2])
plot(newsc50.roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     grid.col=c("green", "red"), max.auc.polygon=TRUE,
     auc.polygon.col="grey", print.thres=TRUE, main="ROC for C5.0")



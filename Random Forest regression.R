# Random Forest Regression Model 
rm(list=ls())

library(randomForest)
library(readxl)
#setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
setwd("C:\\Users\\akane\\Dropbox\\sir david")
mydata<-read_excel("master spreadsheet.xlsx", sheet=1)

head(mydata)

#drops <- c("wiki")
#mydata<-mydata[ , !(names(mydata) %in% drops)]
head(mydata)
names(mydata)


newdata <- mydata[c(3,7,8,11,12,15,18,20,26,29,34)]
names(newdata)
names(newdata)[names(newdata) == 'accuracy (1 high; 2 name covers several sp eg sifaka or skua; 3 very generic eg mouse or crab; 4 confusion name)'] <- 'accuracy'
names(newdata)[names(newdata) == 'viewers (citation: BARB)'] <- 'viewers'
names(newdata)[names(newdata) == 'baseline 01/07/15 to 30/06/16 - sum median'] <- 'baseline'
names(newdata)[names(newdata) == 'wiki diff median sum'] <- 'wiki'
names(newdata)[names(newdata) == 'summed anomaly 1%'] <- 'anomaly'
names(newdata)[names(newdata) == 'tweet count'] <- 'tweet'



newdata$anomaly[newdata$anomaly==2] <- 1

newdata$wiki <- scale(newdata$wiki)
newdata$baseline <- scale(newdata$baseline)


head(newdata)
# replace nas with 0
newdata$multiple[is.na(newdata$multiple)] <- 0
newdata$tweet[is.na(newdata$tweet)] <- 0
newdata$tweet[newdata$tweet=="na"] <- 0


newdata<-newdata[complete.cases(newdata), ]
length(newdata$accuracy)

# there is a relationship between seconds on screen and magnitude of boost seen on wikipedia 
plot(newdata$seconds,newdata$wiki)
mTest <- lm (newdata$wiki ~ newdata$seconds)
summary(mTest)

# categorical response 
newdata$anomaly <- as.factor(newdata$anomaly)
levels(newdata$anomaly)

# change class of variables for random forest analysis 
library(dplyr)
newdata=newdata %>% mutate_if(is.character, as.factor)
newdata$tweet <- as.numeric(newdata$tweet)


# continuous response 
output.forest1 <- randomForest(wiki~accuracy+viewers+multiple+seconds+taxa_gen+baseline + tweet + status2, 
                               data = newdata, ntree=1000 ,mtry=4)
print(output.forest1) 
print(importance(output.forest1,type = 2)) 
varImpPlot(output.forest1,type=2)


# categorical response 
output.forest2 <- randomForest(anomaly~accuracy+viewers+multiple+seconds+taxa_gen+baseline + tweet + status2, 
                               data = newdata, ntree=1000 ,mtry=4)

print(output.forest2) 
print(importance(output.forest2,type = 2)) 
varImpPlot(output.forest2,type=2)

# lm 
m1 <- lm(wiki~accuracy+viewers+multiple+seconds+taxa_gen+baseline + tweet + status2,data = newdata)
summary(m1)

# plot(m1)

# for twitter as response

# continuous response 
output.forest3 <- randomForest(tweet~accuracy++multiple+seconds+taxa_gen+baseline + status2, 
                               data = newdata, ntree=1000 ,mtry=4)
print(output.forest3) 
print(importance(output.forest3,type = 2)) 
varImpPlot(output.forest3,type=2)

# lm 
m2 <- lm(tweet~accuracy+multiple+seconds+taxa_gen+baseline + status2,data = newdata)
summary(m2)



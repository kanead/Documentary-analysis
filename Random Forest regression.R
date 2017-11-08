# Random Forest Regression Model 
rm(list=ls())

library(randomForest)

setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
mydata<-read.csv("masterSpreadsheet.csv", header = T,sep=",")

head(mydata)

drops <- c("wiki")
mydata<-mydata[ , !(names(mydata) %in% drops)]
head(mydata)
names(mydata)
newdata <- mydata[c(1,4,5,7,8,11,18,21,23)]
head(newdata)
names(newdata)[names(newdata) == 'accuracy_.1_high._2_name_covers_several_sp_eg_sifaka_or_skua._3_very_generic_eg_mouse_or_crab._4_confusion_name.'] <- 'accuracy'
names(newdata)[names(newdata) == 'viewers_.citation._BARB.'] <- 'viewers'
names(newdata)[names(newdata) == 'baseline_mobile_median'] <- 'baseline'
names(newdata)[names(newdata) == 'wiki_diff_median'] <- 'wiki'
names(newdata)[names(newdata) == 'mobile_anomaly_2.'] <- 'anomaly'
newdata$anomaly[newdata$anomaly==2] <- 1

newdata$wiki <- scale(newdata$wiki)
newdata$baseline <- scale(newdata$baseline)


head(newdata)
newdata<-newdata[complete.cases(newdata), ]
length(newdata$accuracy)

# categorical response 
newdata$anomaly <- as.factor(newdata$anomaly)
levels(newdata$anomaly)

output.forest2 <- randomForest(anomaly~accuracy+viewers+multiple+last+seconds+taxa+baseline, 
                               data = newdata, ntree=10000 ,mtry=2)

print(output.forest2) 
print(importance(output.forest2,type = 2)) 
varImpPlot(output.forest2,type=2)

# continuous response 
output.forest1 <- randomForest(wiki~accuracy+viewers+multiple+last+seconds+taxa+baseline, 
                               data = newdata, ntree=10000 ,mtry=6)
print(output.forest1) 
print(importance(output.forest1,type = 2)) 
varImpPlot(output.forest1,type=2)


# glm 
m1 <- lm(wiki~accuracy+multiple+seconds+taxa+baseline,data = newdata)
summary(m1)

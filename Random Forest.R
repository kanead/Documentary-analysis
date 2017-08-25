# Random Forest Regression Model 
rm(list=ls())

library(randomForest)

setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
# mydata<-read.csv("randomForestData.csv", header = T,sep=",")
mydata<-read.csv("baselineMod.csv", header = T,sep=",")

head(mydata)

drops <- c("status")
mydata<-mydata[ , !(names(mydata) %in% drops)]
head(mydata)

length(mydata$accuracy)
mydata<-mydata[complete.cases(mydata), ]
length(mydata$accuracy)
class(mydata$anomaly)
mydata$anomaly <- as.factor(mydata$anomaly)
levels(mydata$anomaly)
mydata$standBaseline <- scale(mydata$baseline)

output.forest1 <- randomForest(anomaly~accuracy+seco+taxa+standBaseline, 
                             data = mydata, ntree=10000 ,mtry=2)
print(output.forest1) 
print(importance(output.forest1,type = 2)) 
varImpPlot(output.forest1,type=2)

# confusion matrix is giving high false positives
# giving 1s for what should be 0s 

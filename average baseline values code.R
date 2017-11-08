######################################################################################################
# Code to extract average baseline values for species covered in Planet Earth 2 
######################################################################################################
rm(list=ls())
library(AnomalyDetection)
# library(dplyr)
# detach("package:dplyr", unload=TRUE)
library(ggplot2)
library(pageviews)
library(plyr)
library(gtools)
setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
# read in either the data for the conservation messages or for the species names 
output<-read.csv("output2015.csv",header = T,sep = ",")
head(output)
# Mobile
mobileOutput <- output[output$access=="mobile-web" , ]
mobileOutput<-droplevels(mobileOutput)
head(mobileOutput)
newdata <- mobileOutput[c(3,7:8)]
newdata$date<-as.POSIXct(strptime(newdata$date,"%d/%m/%Y")) # "%d-%m-%Y" "%d/%m/%Y" "%Y-%m-%d"
head(newdata)

meanMobViewsBase<-ddply(newdata, "article", function(x) {
  mean(x$views)
})
meanMobViewsBase

meidanMobViewsBase<-ddply(newdata, "article", function(x) {
  median(x$views)
})
meidanMobViewsBase
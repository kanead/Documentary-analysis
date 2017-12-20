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
output<-read.csv("bornfree.csv",header = T,sep = ",")

head(output)
output$Day.Index<-as.POSIXct(strptime(output$Day.Index,"%d/%m/%Y"))
head(output)

# raw donation data 
res = AnomalyDetectionTs(data.frame(output[1:2]), max_anoms=0.06, direction='both', plot=TRUE)
res$plot
res$anoms

plot(output$Day.Index, output$Product.Revenue,xlab="date", ylab="donations", pch=16)

# mean standardised data 
cols <- c(1, 3)
meanStdOutput<-output[,cols]
res = AnomalyDetectionTs(data.frame(meanStdOutput), max_anoms=0.01, direction='both', plot=TRUE)
res$plot
res$anoms

plot(meanStdOutput$Day.Index, meanStdOutput$standardised,xlab="date", ylab="donations", pch=16, main="Born Free Foundation")

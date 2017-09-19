# Planet Earth 2 Analysis
# UK run 6 November to 11 December 2016
# This encompasses week 44 to week 50
# US run 18 February to 25 March 2017
# This encompasses week 7 to week 12

# clean everything first
rm(list=ls())
library(dplyr)
library(pageviews)
library(data.table)

# load in the data which is a vector of species names 
setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
data<-read.csv("mentionedNames.csv", header = TRUE, sep = ",")
# data<-read.csv("messages.csv", header = TRUE, sep = ",")

head(data)
length(data$name)

# remove problem species 
#errorSpecies <- c("Ring-tailed lemur")
#`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))
#data<-data[data$name %not in% errorSpecies,] 
#data<-droplevels(data)

# test data for function
# data.new<-head(data,1)

# remove the duplicated values 
data.new<-data[!duplicated(data), ]
length(data.new)
class(data.new)
#data.test<-head(data.new,1)

# drop levels that may be problematic
#data.new<-data.new[70:80]
#droplevels(data.new)

# modify the function from pageviews package to collect data for each species - UK air dates
get_wiki <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                          , start = as.Date('2015-07-01'), end = as.Date("2016-06-30")
                                          , user_type = "user", platform = c("desktop", "mobile-web"))
}

# US air dates
get_wikiUSA <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                          , start = as.Date('2017-01-01'), end = as.Date("2017-04-30")
                                          , user_type = "user", platform = c("desktop", "mobile-web"))
}




# loop over each species 
output<-data.new %>%  get_wiki
output
length(output)
head(output)
tail(output)

# save the collected data, output is UK, outputUSA is US
write.csv(output, file = "output2015.csv",row.names=FALSE)
output<-read.csv("output2015.csv", header = T, sep = ",")

# mean values of hits for each species 
# for mobile data
mobileOutput <- output[output$access=="mobile-web" , ]
mobileMean<-with(mobileOutput, tapply(views, article, mean))
round(mobileMean,0)

# for desktop data
desktopOutput <- output[output$access=="desktop" , ]
desktopMean<-with(desktopOutput, tapply(views, article, mean))
round(desktopMean,0)


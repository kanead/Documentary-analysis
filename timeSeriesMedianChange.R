####################################################################################
# Time Series Analysis in R
####################################################################################
rm(list=ls())
# https://rpubs.com/richkt/269908

library(pageviews)
# library(changepoint)
library(dplyr)
# library(data.table)

# USA airdates 
# 18th Feb, 25th Feb, 4th March, 11th March, 18th March, 25th March 
# UK airdates 
# 6th Nov, 13th Nov, 20th Nov, 27th Nov, 4th Dec, 11th Dec

# load in the data which is a vector of species names 
setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
# load in PE1mentionedNames.csv or mentionedNames.csv
data<-read.csv("mentionedNames.csv", header = TRUE, sep = ",")

# articles <- c("Rugby_union","Football") # test using these articles 
articles <- data$name

# the dates before airing 
before <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                        , start = as.Date('2016-05-01'), 
                                        end = as.Date("2016-10-31")
                                        , user_type = "user", platform = c("mobile-web"))
}

# the dates after airing 
after <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                       , start = as.Date('2017-05-01'), 
                                       end = as.Date("2017-10-31")
                                       , user_type = "user", platform = c("mobile-web"))
}

# collect the wiki hits for before 
outputBefore <-articles %>% before
# collect the wiki hits for after 
outputAfter <-articles %>% after

# split the dataset by article and find the median wiki hits for each before the date
splitArticlesBefore<-with(outputBefore, tapply(views, outputBefore$article, median))
#splitArticlesBefore<-data.frame(splitArticlesBefore)

# split the dataset by article and find the median wiki hits for each 
splitArticlesAfter<-with(outputAfter, tapply(views, outputAfter$article, median))
#splitArticlesAfter<-data.frame(splitArticlesAfter)

# combine the arrays 
combinedHits <- cbind(splitArticlesBefore, splitArticlesAfter)
combinedHits<-data.frame(combinedHits)

# which one is bigger? Before or after? 
combinedHits$change<-ifelse(combinedHits$splitArticlesAfter>
                              combinedHits$splitArticlesBefore,1,0)
combinedHits
sum(combinedHits$change) /length(combinedHits$change)
# calculate percentage change
combinedHits$percChange <- ((combinedHits$splitArticlesAfter-
                               combinedHits$splitArticlesBefore) / 
                              abs(combinedHits$splitArticlesBefore)) * 100
head(combinedHits)
# export as medianHitsPE1.csv or medianHitsPE2.csv
write.csv(combinedHits,file="medianHitsPE2.csv",row.names = T)

####################################################################################
# Read in Combined Data with PE1 and PE2 Wiki Hits
####################################################################################
mydata<-read.csv("medianHitsPE1_PE2.csv",header = T)
head(mydata)
# remove the infinite values (going for 0 to something)
mydata$percChange[!is.finite(mydata$percChange)] <- NA
# remove the yak, its article was created in 2016
# seems to have redirected to Yak before then!
mydata<-mydata[!(mydata$name=="Wild_yak"),]
# remove ostrich, similar issue 
mydata<-mydata[!(mydata$name=="Ostrich"),]
# summary statistics 
median(mydata$percChange[mydata$show=="PE1"],na.rm = T)
median(mydata$percChange[mydata$show=="PE2"],na.rm = T)
mean(mydata$percChange[mydata$show=="PE1"],na.rm = T)
mean(mydata$percChange[mydata$show=="PE2"],na.rm = T)
sd(mydata$percChange[mydata$show=="PE1"],na.rm = T)
sd(mydata$percChange[mydata$show=="PE2"],na.rm = T)
# plot the data
par(mfrow=c(1,2))
hist(mydata$percChange[mydata$show=="PE1"],xlab = "% change",main = "PE1")
hist(mydata$percChange[mydata$show=="PE2"],xlab = "% change",main="PE2")
par(mfrow=c(1,1))
boxplot(mydata$percChange[mydata$show=="PE1"],mydata$percChange[mydata$show=="PE2"])
# test the significance of the percentage change between the two groups 
wilcox.test(mydata$percChange[mydata$show=="PE1"],mydata$percChange[mydata$show=="PE2"]
       ,na.rm = T)
t.test(mydata$percChange[mydata$show=="PE1"],mydata$percChange[mydata$show=="PE2"]
            ,na.rm = T)

# test species with anomlaies (1%) against the others
# subset to get anomaly data first 
anomData <- mydata[mydata$anomalies==1,]
# summary statistics 
median(anomData$percChange,na.rm = T)
mean(anomData$percChange,na.rm = T)
sd(anomData$percChange,na.rm = T)

par(mfrow=c(1,1))

hist(anomData$percChange,xlab = "% change",main="PE2 Anomalies")
# compare the PE2 anomalies with PE1 data
boxplot(mydata$percChange[mydata$show=="PE1"],anomData$percChange
            ,na.rm = T,names=c("PE1","PE2 Anomalies"))
wilcox.test(mydata$percChange[mydata$show=="PE1"],anomData$percChange
            ,na.rm = T)
t.test(mydata$percChange[mydata$show=="PE1"],anomData$percChange
            ,na.rm = T)

####################################################################################
# test for timeseries function
####################################################################################
# Yak is weird because of new Wiki page in Dec 2016
test<- article_pageviews(project = "en.wikipedia", article = "Wild_yak"
                                        , start = as.Date('2016-05-01'), 
                                        end = as.Date("2016-12-31")
                                        , user_type = "user", platform = c("mobile-web"))
tail(test)



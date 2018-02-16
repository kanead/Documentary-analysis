# Changepoint Analysis in R
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
data<-read.csv("mentionedNames.csv", header = TRUE, sep = ",")

# articles <- c("Rugby_union","Football") # test using these articles 
articles <- data$name

# the dates before airing 
before <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                          , start = as.Date('2016-05-01'), end = as.Date("2016-10-31")
                                          , user_type = "user", platform = c("mobile-web"))
}

# the dates after airing 
after <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                        , start = as.Date('2017-05-01'), end = as.Date("2017-10-31")
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
combinedHits$change<-ifelse(combinedHits$splitArticlesAfter>combinedHits$splitArticlesBefore,1,0)
combinedHits
sum(combinedHits$change) /length(combinedHits$change)


combinedHits <- merge(splitArticlesBefore,splitArticlesAfter)

splitArticles$change<-ifelse(splitArticles$TRUE.>splitArticles$FALSE.,1,0)
splitArticles















output$date <- as.Date(output$date)
ls<- split(output,output$date<as.Date("2017-06-01"))

ls

splitData<-output %>% group_by(article) %>%
  
  


output %>% 
group_by(article) %>% 
do(as.data.frame(cpt.mean(as.numeric(as.character(output$views)), method="BinSeg",Q=2)))


test









output %>% 
  select(article, views) %>% 
  mean(views)
  do(cpt.mean(.)) 

class(dat.grp$views)


# data.new<-data[!duplicated(data), ]



dat.grp <- group_by(output, article)

dat.grp %>% 
  select(article, views) %>% 
  do(cpt.mean(.)) 






set.seed(10)
m.data <- c(rnorm(100, 0, 1), rnorm(100, 1, 1), rnorm(100, 0, 1),
               + rnorm(100, 0.2, 1))
ts.plot(m.data, xlab = "Index")

# PELT and binary segmentation search methods
m.pelt <- cpt.mean(m.data, method = "PELT")
plot(m.pelt, type = "l", cpt.col = "blue", xlab = "Index",
         cpt.width = 4)
cpts(m.pelt)

# binary segmentation search methods
m.binseg <- cpt.mean(m.data, method = "BinSeg")
plot(m.binseg, type = "l", xlab = "Index", cpt.width = 4)
cpts(m.binseg)

# number of discoveries over time 
data("discoveries", package = "datasets")
dis.pelt <- cpt.meanvar(discoveries, test.stat = "Poisson",
                           method = "PELT")
plot(dis.pelt, cpt.width = 3)
cpts.ts(dis.pelt)

class(discoveries)


tsData<-article_pageviews(project = "en.wikipedia", article = "Rugby_union"
                                         , start = as.Date('2016-01-01'), end = as.Date("2017-12-31")
                                         , user_type = "user", platform = c("desktop"))

tsData<-(tsData[8])
tsData<-as.numeric(as.character(tsData$views))
mvalue = cpt.mean(tsData, method="BinSeg",Q=2)
cpts(mvalue)
plot(mvalue)



# tsData<-as.ts(tsData)
m.pelt <- cpt.mean(tsData, method = "PELT")
plot(m.pelt, type = "l", cpt.col = "blue", xlab = "Index",
     cpt.width = 1)
cpts(m.pelt)
length(cpts(m.pelt))



tsData<-as.ts(tsData)
dis.pelt <- cpt.meanvar(tsData, test.stat = "Poisson",
                        method = "PELT")
plot(dis.pelt, cpt.width = 3)
cpts(dis.pelt)




mvalue = cpt.mean(tsData, method="BinSeg",Q=2)
cpts(mvalue)
plot(mvalue)

# Changepoint Analysis in R
rm(list=ls())
# https://rpubs.com/richkt/269908

library(pageviews)
library(changepoint)
library(dplyr)
library(pageviews)
library(data.table)

# load in the data which is a vector of species names 
setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
data<-read.csv("mentionedNames.csv", header = TRUE, sep = ",")


articles <- c("Rugby_union", "Football")

blah <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                          , start = as.Date('2017-01-01'), end = as.Date("2017-12-31")
                                          , user_type = "user", platform = c("mobile-web"))
}

output<-articles %>% blah

dat.grp %>% 
  select(article, views) %>% 
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


tsData<-article_pageviews(project = "en.wikipedia", article = "Golden_mole"
                                         , start = as.Date('2016-01-01'), end = as.Date("2017-12-31")
                                         , user_type = "user", platform = c("desktop"))

tsData<-(tsData[8])
tsData<-as.numeric(as.character(tsData$views))

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

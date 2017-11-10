######################################################################################################
# Arkive webpage hits 
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
output<-read.csv("arkivePageViews.csv",header = T,sep = ",")
head(output)

names(output)[names(output) == 'page.views'] <- 'views'
output$day<-as.POSIXct(strptime(output$day,"%d/%m/%Y"))
newdata<-output
head(newdata)

# find the anomalies for the Arkive data on the Planet Earth 2 airdates 
arkiveAnom <-ddply(newdata, "species", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.01, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-06"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-07"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-13"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-14"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-20"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-21"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-27"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-28"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-04"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-05"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-11"), format = "%Y-%m-%d") |
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-12"), format = "%Y-%m-%d") ,1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})

arkiveAnom


######################################################################################################
# Can test individual species here 
# generic function to loop over the whole data frame requires two columns, one for the date and
# one for the number of hits. 

par(mfrow=c(2,3))

testRun <- newdata[newdata$species=="African_elephant" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "African_elephant",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Tiger" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Tiger",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Grey wolf (Canis lupus)" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Grey wolf (Canis lupus)",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Lion" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Lion",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Barn_owl" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Barn_owl",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Brown bear (Ursus arctos)" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Brown bear (Ursus arctos)",
     # main = testRun$article[1],
     pch=16)

######################################################################################################
par(mfrow=c(2,3))

testRun <- newdata[newdata$species=="Bamboo_lemur" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Bamboo_lemur",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Desert_long-eared_bat" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Desert_long-eared_bat",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Jackson's_widowbird" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Jackson's_widowbird",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Serval" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Serval",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Southern viscacha (Lagidium viscacia)" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Southern viscacha (Lagidium viscacia)",
     # main = testRun$article[1],
     pch=16)

testRun <- newdata[newdata$species=="Sword-billed_hummingbird" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
#res$plot
res$anoms

plot(testRun$day, testRun$views, xlim=range(testRun$day), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Sword-billed_hummingbird",
     # main = testRun$article[1],
     pch=16)

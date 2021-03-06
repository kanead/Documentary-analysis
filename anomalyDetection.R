######################################################################################################
# Analysis of Wikipedia Trends for Documentary Analysis 
######################################################################################################
# https://github.com/twitter/AnomalyDetection/blob/master/R/detect_anoms.R
# https://news.ycombinator.com/item?id=8855962
# https://www.r-bloggers.com/anomaly-detection-in-r/
# involves taking the max absolute difference from the detrended sample mean in 
# terms of standard deviations, remove it and repeat until you have your collection 
# of x outliers.

# deleted some species because of data length: cheetah, darkling beetles, red crab 
# ddply(newdata, "article", function(x) {length(x$article)})

# Air dates
# episode 1 - 6 November 2016   2016-11-06
#                               2016-11-07

# episode 2 - 13 November 2016	2016-11-13
#                               2016-11-14

# episode 3 - 20 November 2016	2016-11-20
#                               2016-11-21

# episode 4 - 27 November 2016	2016-11-27
#                               2016-11-28

# episode 5 - 4 December 2016 	2016-12-04
#                               2016-12-05

# episode 6 - 11 December 2016	2016-12-11
#                               2016-12-12
######################################################################################################
rm(list=ls())
library(AnomalyDetection)
# library(dplyr)
# detach("package:dplyr", unload=TRUE)
library(ggplot2)
library(pageviews)
library(plyr)
library(gtools)
library(data.table)
setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
# read in either the data for the conservation messages or for the species names 
output<-read.csv("output.csv",header = T,sep = ",")
# output<-read.csv("messages.csv",header = T,sep = ",")

# select either mobile or desktop or combined access
# Mobile
# mobileOutput <- output[output$access=="mobile-web" , ]
# mobileOutput<-droplevels(mobileOutput)
# head(mobileOutput)
# newdata <- mobileOutput[c(3,7:8)]
# newdata$date<-as.POSIXct(strptime(newdata$date,"%d/%m/%Y")) # "%d-%m-%Y" "%d/%m/%Y" "%Y-%m-%d"
# head(newdata)
 
# Desktop
# deskOutput <- output[output$access=="desktop" , ]
# deskOutput<-droplevels(deskOutput)
# head(deskOutput)
# newdata <- deskOutput[c(3,7:8)]
# newdata$date<-as.POSIXct(strptime(newdata$date,"%d/%m/%Y"))

# Combined
# can group the mobile and desktop data 
 dataTableOutput<-setDT(output)[, .(sumy=sum(views)), by = .(article,date)]
# 271 + 370 for African elephant 01/01/16
# 101 + 98 for hawksbill sea turtle 31/12/16
 newdata<-data.frame(dataTableOutput)
 names(newdata)[names(newdata) == 'sumy'] <- 'views'
 newdata$date<-as.POSIXct(strptime(newdata$date,"%d/%m/%Y"))
 newdata<-droplevels(newdata)

######################################################################################################
episode1 <- c("Pygmy_three-toed_sloth","Komodo_dragon","Lemur","Indri","Ring-tailed_lemur","Bamboo_lemur",
              "Sifaka","Marine_iguana","Crab","Lizard", "Colubridae" ,"Snares_penguin","Shearwater","Buller's_albatross",
              "Fairy_tern","Seychelles_fody","Noddy_(tern)","Yellow_crazy_ant","Chinstrap_penguin",
              "Skua", "Christmas_Island_red_crab")

# keep only the species featured in this episode 
newdata1<-newdata[newdata$article %in% episode1,] 
newdata1<-droplevels(newdata1)

# episode 1 function
airedAnomDataDays1<-ddply(newdata1, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.1, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-06"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-07"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
######################################################################################################
episode2 <- c("Snow_Leopard","Nubian_ibex","Red_fox","Golden_eagle","Corvidae",
              "Grizzly_bear","Marmot","Bobcat","Mouse","Coyote", "Goldeneye_(duck)","Squirrel","Lagidium",
              "Flamingo")

# keep only the species featured in this episode 
newdata2<-newdata[newdata$article %in% episode2,] 
newdata2<-droplevels(newdata2)

# episode 2 function
airedAnomDataDays2<-ddply(newdata2, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.1, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-13"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-14"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
######################################################################################################
episode3 <- c("Indri","Spider_monkey","Draco_(genus)","Hummingbird", "Sword-billed_hummingbird","River_dolphin",
              "Capybara","Giant_otter","Caiman","Jaguar","Uroplatus","Click_beetle","Railroad_worm","Glass_frog",
              "Millipede","Red_bird-of-paradise","Wilson's_bird-of-paradise", "Rat", "Spider", "Fire_ant", "Wasp")

# keep only the species featured in this episode 
newdata3<-newdata[newdata$article %in% episode3,] 
newdata3<-droplevels(newdata3)

# episode 3 function
airedAnomDataDays3<-ddply(newdata3, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.1, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-20"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-21"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
######################################################################################################
episode4 <- c("Lion","Oryx","Giraffe","Harris's_hawk","Ground_squirrel","Butcherbird","Locust","Zebra",
              "Elephant","Sandgrouse","Pale_chanting_goshawk","Golden_mole","Desert_long-eared_bat",
              "Deathstalker", "Darkling_beetle", "Pachydactylus_rangei" ,"Namaqua_chameleon")

# keep only the species featured in this episode 
newdata4<-newdata[newdata$article %in% episode4,] 
newdata4<-droplevels(newdata4)

# episode 4 function
airedAnomDataDays4<-ddply(newdata4, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.1, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-11-27"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-11-28"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
######################################################################################################
episode5 <- c("Saiga_antelope","Lion","African_buffalo","Micromys","Barn_owl","Southern_carmine_bee-eater",
              "Kori_bustard","Ostrich","Elephant","Serval","Southern_African_vlei_rat",
              "Wildebeest","Jackson's_widowbird","Atta_(genus)", "Amitermes_meridionalis", "Termite","Giant_anteater","Bison","Fox",
              "Vole", "Caribou","Arctic_wolf","Rhinoceros","Wild_water_buffalo","Sloth_bear", "Tiger","Elephant")

# keep only the species featured in this episode 
newdata5<-newdata[newdata$article %in% episode5,] 
newdata5<-droplevels(newdata5)

# episode 5 function
airedAnomDataDays5<-ddply(newdata5, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.1, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-12-04"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-05"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
######################################################################################################
episode6 <- c("Colobinae","Peregrine_falcon","Leopard","Starling","Great_bowerbird","Raccoon",
              "Rhesus_macaque","Spotted_hyena","Pigeon","Wels_catfish","Hawksbill_sea_turtle",
              "Crab", "Smooth-coated_otter")

# keep only the species featured in this episode 
newdata6<-newdata[newdata$article %in% episode6,] 
newdata6<-droplevels(newdata6)

# episode 6 function
airedAnomDataDays6<-ddply(newdata6, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.1, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                    == strptime(as.Date("2016-12-11"), format = "%Y-%m-%d") | 
                      strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                    == strptime(as.Date("2016-12-12"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})
######################################################################################################

# loop over all species to find the total number of anomalies for each species over the course of the year
lengthAnomData<-ddply(newdata, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.1, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") <= strptime(as.Date("2016-12-11"), format = "%Y-%m-%d") 
                    & strptime((res$anoms$timestamp), format = "%Y-%m-%d") >= strptime(as.Date("2016-11-06"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  length(anomalies)
})
######################################################################################################
lengthAnomData
airedAnomDataDays1
airedAnomDataDays2
airedAnomDataDays3
airedAnomDataDays4
airedAnomDataDays5
airedAnomDataDays6

testDataCombo<-rbind.fill(airedAnomDataDays1,airedAnomDataDays2,airedAnomDataDays3,airedAnomDataDays4,airedAnomDataDays5,
      airedAnomDataDays6)

######################################################################################################
# Messages Instead of Species 
######################################################################################################
airedAnomDataDaysMessages <-ddply(newdata, "article", function(x) {
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

######################################################################################################
# loop over all species to find the number of anomalies within the time frame of the air dates
airedAnomData<-ddply(newdata, "article", function(x) {
  res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.01, direction='both', plot=TRUE)
  # determine if the anomalies are between 2 dates
  anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") <= strptime(as.Date("2016-12-11"), format = "%Y-%m-%d") 
                    & strptime((res$anoms$timestamp), format = "%Y-%m-%d") >= strptime(as.Date("2016-11-06"), format = "%Y-%m-%d"),1,0)
  # find the sum total of the ones that are
  sum(anomalies)
})



# combine the data
anomalyData<-cbind(airedAnomData,lengthAnomData$V1)
# rename the columns 
names(anomalyData)[names(anomalyData) == 'V1'] <- 'airedAnomalies'
names(anomalyData)[names(anomalyData) == 'lengthAnomData$V1'] <- 'totalAnomalies'
head(anomalyData)
######################################################################################################
# Can test individual species here 
# generic function to loop over the whole data frame requires two columns, one for the date and
# one for the number of hits. 
par(mfrow=c(1,1))

testRun <- newdata[newdata$article=="Caribou" , ]
res = AnomalyDetectionTs(data.frame(testRun[2:3]), max_anoms=0.01, direction='both', plot=TRUE)
res$plot
res$anoms

plot(testRun$date, testRun$views, xlim=range(testRun$date), ylim=range(testRun$views), 
     xlab="date", ylab="page hits", main = "Caribou",
    # main = testRun$article[1],
     pch=16)
 
#     , col = ifelse(testRun$date == anomalies,'red','black'),
#     cex = ifelse(testRun$date == res$anoms$timestamp,2,1) )

# lines(testRun$date[order(testRun$date)], testRun$views[order(testRun$date)], xlim=range(testRun$date), 
#      ylim=range(testRun$views), pch=16)

# colour the points by clicking on them if the ifelse statement refuses to work
pnt <- identify(testRun$date, testRun$views, plot = F)

# This colors those points red
points(testRun$date[pnt], testRun$views[pnt], col = "red")

# summary stats for the time series 
median(testRun$views)
mean(testRun$views)
max(testRun$views)
######################################################################################################

finalData<-read.csv("results of anomaly analysis.csv",header = T,sep = ",")
head(finalData)

# find the number of animals that have an anomaly during UK broadcast using mobile access @ 1%
sum(finalData$airDateAnomaliesOne.[finalData$access=="mobile" & finalData$location=="UK"] > 0) /
  length(finalData$airDateAnomaliesOne.[finalData$access=="mobile" & finalData$location=="UK"])

# find the number of animals that have an anomaly during US broadcast using mobile access @ 1%
sum(finalData$airDateAnomaliesOne.[finalData$access=="mobile" & finalData$location=="USA"] > 0) /
length(finalData$airDateAnomaliesOne.[finalData$access=="mobile" & finalData$location=="USA"])

# find the number of animals that have an anomaly during UK broadcast using desktop access @ 1%
sum(finalData$airDateAnomaliesOne.[finalData$access=="desktop" & finalData$location=="UK"] > 0) /
  length(finalData$airDateAnomaliesOne.[finalData$access=="desktop" & finalData$location=="UK"])

# find the number of animals that have an anomaly during US broadcast using desktop access @ 1%
sum(finalData$airDateAnomaliesOne.[finalData$access=="desktop" & finalData$location=="USA"] > 0) /
  length(finalData$airDateAnomaliesOne.[finalData$access=="desktop" & finalData$location=="USA"])

# find the number of animals that have an anomaly during UK broadcast using combined access @ 1%
sum(finalData$airDateAnomaliesOne.[finalData$access=="combined" & finalData$location=="UK"] > 0) /
  length(finalData$airDateAnomaliesOne.[finalData$access=="combined" & finalData$location=="UK"])

# find the number of animals that have an anomaly during US broadcast using combined access @ 1%
sum(finalData$airDateAnomaliesOne.[finalData$access=="combined" & finalData$location=="USA"] > 0) /
  length(finalData$airDateAnomaliesOne.[finalData$access=="combined" & finalData$location=="USA"])

# 2%

# find the number of animals that have an anomaly during UK broadcast using mobile access @ 2%
sum(finalData$airDateAnomaliesTwo.[finalData$access=="mobile" & finalData$location=="UK"] > 0) /
  length(finalData$airDateAnomaliesTwo.[finalData$access=="mobile" & finalData$location=="UK"])

# find the number of animals that have an anomaly during US broadcast using mobile access @ 2%
sum(finalData$airDateAnomaliesTwo.[finalData$access=="mobile" & finalData$location=="USA"] > 0) /
  length(finalData$airDateAnomaliesTwo.[finalData$access=="mobile" & finalData$location=="USA"])

# find the number of animals that have an anomaly during UK broadcast using desktop access @ 2%
sum(finalData$airDateAnomaliesTwo.[finalData$access=="desktop" & finalData$location=="UK"] > 0) /
  length(finalData$airDateAnomaliesTwo.[finalData$access=="desktop" & finalData$location=="UK"])

# find the number of animals that have an anomaly during US broadcast using desktop access @ 2%
sum(finalData$airDateAnomaliesTwo.[finalData$access=="desktop" & finalData$location=="USA"] > 0) /
  length(finalData$airDateAnomaliesTwo.[finalData$access=="desktop" & finalData$location=="USA"])

# find the number of animals that have an anomaly during UK broadcast using combined access @ 2%
sum(finalData$airDateAnomaliesTwo.[finalData$access=="combined" & finalData$location=="UK"] > 0) /
  length(finalData$airDateAnomaliesTwo.[finalData$access=="combined" & finalData$location=="UK"])

# find the number of animals that have an anomaly during US broadcast using combined access @ 2%
sum(finalData$airDateAnomaliesTwo.[finalData$access=="combined" & finalData$location=="USA"] > 0) /
  length(finalData$airDateAnomaliesTwo.[finalData$access=="combined" & finalData$location=="USA"])
######################################################################################################
# summary stats on the article hits 
######################################################################################################
library(dplyr)

# turn off exponential notation 
options(scipen = 999)

# mean 
summaryDataMean<-newdata %>%
  group_by(article) %>%
  summarise_each(funs(mean(., na.rm=TRUE)))

summaryDataMean<-data.frame(summaryDataMean)
summaryDataMean$views<-round(summaryDataMean$views,0)

# median 
summaryDataMedian<-newdata %>%
  group_by(article) %>%
  summarise_each(funs(median(., na.rm=TRUE)))

summaryDataMedian<-data.frame(summaryDataMedian)
summaryDataMedian$views<-round(summaryDataMedian$views,0)

# maximum 
summaryDataMax<-newdata %>%
  group_by(article) %>%
  summarise_each(funs(max(., na.rm=TRUE)))

summaryDataMax<-data.frame(summaryDataMax)

# standard deviation  
summaryDataSD<-newdata %>%
  group_by(article) %>%
  summarise_each(funs(sd(., na.rm=TRUE)))

summaryDataSD<-data.frame(summaryDataSD)
summaryDataSD$views<-round(summaryDataSD$views,0)



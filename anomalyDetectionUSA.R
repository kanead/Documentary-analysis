######################################################################################################
# Analysis of Wikipedia Trends for Documentary Analysis USA
######################################################################################################
# https://github.com/twitter/AnomalyDetection/blob/master/R/detect_anoms.R
# https://news.ycombinator.com/item?id=8855962
# https://www.r-bloggers.com/anomaly-detection-in-r/
# involves taking the max absolute difference from the detrended sample mean in 
# terms of standard deviations, remove it and repeat until you have your collection 
# of x outliers.

# deleted some species because of data length: cheetah, darkling beetles, red crab 
# ddply(newdata, "article", function(x) {length(x$article)})

# 18th Feb, 25th Feb, 4th March, 11th March, 18th March, 25th March 
# Islands, Mountains, Jungles, Deserts, Grasslands, Cities

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
output<-read.csv("outputMentionedNamesUSA.csv",header = T,sep = ",")
head(output)

# select either mobile or desktop or combined access
# Mobile
 #mobileOutput <- output[output$access=="mobile-web" , ]
 #mobileOutput<-droplevels(mobileOutput)
 #head(mobileOutput)
 #newdata <- mobileOutput[c(3,7:8)]
 #newdata$date<-as.POSIXct(strptime(newdata$date,"%d/%m/%Y")) # "%d-%m-%Y" "%d/%m/%Y" "%Y-%m-%d"
 #head(newdata)

# Desktop
# deskOutput <- output[output$access=="desktop" , ]
# deskOutput<-droplevels(deskOutput)
# head(deskOutput)
# newdata <- deskOutput[c(3,7:8)]
# newdata$date<-as.POSIXct(strptime(newdata$date,"%d/%m/%Y"))
# head(newdata)
 
# Combined
# can group the mobile and desktop data 
 dataTableOutput<-setDT(output)[, .(sumy=sum(views)), by = .(article,date)]
# 271 + 370 for African elephant 01/01/16
# 101 + 98 for hawksbill sea turtle 31/12/16
 newdata<-data.frame(dataTableOutput)
 names(newdata)[names(newdata) == 'sumy'] <- 'views'
 newdata$date<-as.POSIXct(strptime(newdata$date,"%d/%m/%Y"))
 newdata<-droplevels(newdata)
 head(newdata)
 
 ######################################################################################################
 # removed red crab from list of episode 1 species 
 episode1 <- c("Pygmy_three-toed_sloth","Komodo_dragon","Indri","Ring-tailed_lemur","Bamboo_lemur",
               "Sifaka","Marine_iguana","Racer_snake","Snares_penguin","Shearwater","Buller's_albatross",
               "Fairy_tern","Seychelles_fody","Noddy_(tern)","Yellow_crazy_ant","Chinstrap_penguin",
               "Skua")
 
 # keep only the species featured in this episode 
 newdata1<-newdata[newdata$article %in% episode1,] 
 newdata1<-droplevels(newdata1)
 
 # episode 1 function
 airedAnomDataDays1<-ddply(newdata1, "article", function(x) {
   res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
   # determine if the anomalies are between 2 dates
   anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                     == strptime(as.Date("2017-02-18"), format = "%Y-%m-%d") | 
                       strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                     == strptime(as.Date("2017-02-19"), format = "%Y-%m-%d"),1,0)
   # find the sum total of the ones that are
   sum(anomalies)
 })
 ######################################################################################################
 # peregrine is only mentioned in passing and isn't on screen in episode 2 
 episode2 <- c("Snow_leopard","Nubian_ibex","Red_fox","Golden_eagle","Peregrine_falcon","Crow",
               "Grizzly_bear","Marmot","Bobcat","Mouse","Goldeneye_(duck)","Squirrel","Mountain_viscacha",
               "Flamingo")
 
 # keep only the species featured in this episode 
 newdata2<-newdata[newdata$article %in% episode2,] 
 newdata2<-droplevels(newdata2)
 
 # episode 2 function
 airedAnomDataDays2<-ddply(newdata2, "article", function(x) {
   res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
   # determine if the anomalies are between 2 dates
   anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                     == strptime(as.Date("2017-02-25"), format = "%Y-%m-%d") | 
                       strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                     == strptime(as.Date("2017-02-26"), format = "%Y-%m-%d"),1,0)
   # find the sum total of the ones that are
   sum(anomalies)
 })
 ######################################################################################################
 # leaf-tailed gecko left out 
 episode3 <- c("Indri","Spider_monkey","Draco_lizard","Sword-billed_hummingbird","River_dolphin",
               "Capybara","Giant_otter","Caiman","Jaguar","Glass_frog","Click_beetle","Railroad_worm",
               "Millipede","Red_bird-of-paradise","Wilson's_bird-of-paradise")
 
 # keep only the species featured in this episode 
 newdata3<-newdata[newdata$article %in% episode3,] 
 newdata3<-droplevels(newdata3)
 
 # episode 3 function
 airedAnomDataDays3<-ddply(newdata3, "article", function(x) {
   res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
   # determine if the anomalies are between 2 dates
   anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                     == strptime(as.Date("2017-03-04"), format = "%Y-%m-%d") | 
                       strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                     == strptime(as.Date("2017-03-05"), format = "%Y-%m-%d"),1,0)
   # find the sum total of the ones that are
   sum(anomalies)
 })
 ######################################################################################################
 # shovel-snouted lizared, darkling beetles, web-footed gecko left out
 episode4 <- c("Lion","Oryx","Giraffe","Harris's_hawk","Ground_squirrel","Butcherbird","Locust",
               "Elephant","Sandgrouse","Mustang","Golden_mole","Termite","Desert_long-eared_bat",
               "Deathstalker_scorpion","Namaqua_chameleon")
 
 # keep only the species featured in this episode 
 newdata4<-newdata[newdata$article %in% episode4,] 
 newdata4<-droplevels(newdata4)
 
 # episode 4 function
 airedAnomDataDays4<-ddply(newdata4, "article", function(x) {
   res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
   # determine if the anomalies are between 2 dates
   anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                     == strptime(as.Date("2017-03-11"), format = "%Y-%m-%d") | 
                       strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                     == strptime(as.Date("2017-03-12"), format = "%Y-%m-%d"),1,0)
   # find the sum total of the ones that are
   sum(anomalies)
 })
 ######################################################################################################
 # cheetah left out because it's only mentioned in passing as comparison to the serval, grass-cutter ants
 episode5 <- c("Saiga_antelope","Lion","Buffalo","Harvest_mouse","Barn_owl","Carmine_bee-eater",
               "Kori_bustard","Ostrich","African_elephant","Serval","Southern_African_vlei_rat",
               "Wildebeest","Jackson's_widowbird","Termite","Giant_anteater", "Giant_otter","Bison","Fox",
               "Caribou","Arctic_wolf","Tiger","Rhino","Sloth_bear","Asian_water_buffalo")
 
 # keep only the species featured in this episode 
 newdata5<-newdata[newdata$article %in% episode5,] 
 newdata5<-droplevels(newdata5)
 
 # episode 5 function
 airedAnomDataDays5<-ddply(newdata5, "article", function(x) {
   res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
   # determine if the anomalies are between 2 dates
   anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                     == strptime(as.Date("2017-03-18"), format = "%Y-%m-%d") | 
                       strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                     == strptime(as.Date("2017-03-19"), format = "%Y-%m-%d"),1,0)
   # find the sum total of the ones that are
   sum(anomalies)
 })
 ######################################################################################################
 # add Hawksbill sea turtle
 episode6 <- c("Langur","Peregrine_falcon","Leopard","Pig","Starling","Great_bowerbird","Raccoon",
               "Rhesus_macaque","Spotted_hyena","Pigeon","Wels_catfish","Hawksbill_sea_turtle",
               "Smooth-coated_otter")
 
 # keep only the species featured in this episode 
 newdata6<-newdata[newdata$article %in% episode6,] 
 newdata6<-droplevels(newdata6)
 
 # episode 6 function
 airedAnomDataDays6<-ddply(newdata6, "article", function(x) {
   res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
   # determine if the anomalies are between 2 dates
   anomalies<-ifelse(strptime((res$anoms$timestamp), format = "%Y-%m-%d") 
                     == strptime(as.Date("2017-03-25"), format = "%Y-%m-%d") | 
                       strptime((res$anoms$timestamp), format = "%Y-%m-%d")
                     == strptime(as.Date("2017-03-26"), format = "%Y-%m-%d"),1,0)
   # find the sum total of the ones that are
   sum(anomalies)
 })
 ######################################################################################################
 
 # loop over all species to find the total number of anomalies for each species 
 lengthAnomData<-ddply(newdata, "article", function(x) {
   res = AnomalyDetectionTs((data.frame(x[2:3])), max_anoms=0.02, direction='both', plot=TRUE)
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
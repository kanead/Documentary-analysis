# Planet Earth 2 Analysis
# UK run 6 November to 11 December 2016
# This encompasses week 44 to week 50
# US run 18 February to 25 March 2017
# This encompasses week 7 to week 12

# leaf-tailed gecko and web-footed gecko don't have a wiki page

# clean everything first
rm(list=ls())
library(dplyr)
library(pageviews)
library(data.table)

# load in the data which is a vector of species names 
setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
data<-read.csv("specificNames.csv", header = T, sep = ",")
head(data)
length(data$name)

# remove the duplicated values 
data.new<-data[!duplicated(data), ]
length(data.new)
class(data.new)
#data.test<-tail(data.new,25)

# keep the column we're interested in 
data[,(3)]
head(data)

# modify the function from pageviews package to collect data for each species - UK air dates
get_wiki <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                          , start = as.Date('2016-01-01'), end = as.Date("2016-12-31")
                                          , user_type = "user", platform = c("desktop", "mobile-web"))
}

# US air dates
get_wikiUSA <- function(x){article_pageviews(project = "en.wikipedia", article = x
                                          , start = as.Date('2017-01-01'), end = as.Date("2017-04-20")
                                          , user_type = "user", platform = c("desktop", "mobile-web"))
}

# loop over each species 
output<-data.new %>%  get_wikiUSA
output
length(output)
tail(output)
head(output)

# save the collected data, output is UK, outputUSA is US
write.csv(output, file = "outputUSA.csv",row.names=FALSE)
output<-read.csv("output.csv", header = T, sep = ",")

boxplot(output$views[output$article=="Sloth"], lwd = 2,pch=16,range=2, outline=FALSE,  ylim = c(min(output$views[output$article=="Sloth"]), max(output$views[output$article=="Sloth"])))
stripchart(output$views[output$article=="Sloth"], vertical = TRUE,  
           method = "jitter", add = TRUE, pch = 20, col = 'blue')



# access the months in our dataset and add them to our original output data 
# output$month<-strftime(output$date, format = "%m")

# access the weeks in our dataset and add them to our original output data 
# output$week<-strftime(output$date, format = "%W")

# mobile data only
mobileOutput <- output[output$access=="mobile-web" , ]
mobileOutput<-droplevels(mobileOutput)
head(mobileOutput)

# group views by week and species
mobileOutputSummaryWeekly<-mobileOutput %>%
  group_by(week, article) %>%
  summarise(views = sum(views))
mobileOutputSummaryWeekly<-as.data.frame(mobileOutputSummaryWeekly)

plot(mobileOutputSummaryWeekly$views ~ mobileOutputSummaryWeekly$week, pch = 16, xlab="week", ylab="hits")
dev.new()
plot(mobileOutputSummaryWeekly$views[mobileOutputSummaryWeekly$article=="Chilean_flamingo"] ~ 
       mobileOutputSummaryWeekly$week[mobileOutputSummaryWeekly$article=="Chilean_flamingo"], 
     pch = 16, xlab="week", ylab="hits")


# find the the week with the highest number of hits for each article 
DT <- data.table(mobileOutputSummaryWeekly)
maxWeekSpecies<-DT[,.SD[which.max(views)],by=article]
maxWeekSpecies<-data.frame(maxWeekSpecies)
head(maxWeekSpecies)

# were these weeks during the air dates of Planet Earth 2 in the UK? 
length(maxWeekSpecies$article)
sum(maxWeekSpecies$week > 44 & maxWeekSpecies$week < 51)
sum(maxWeekSpecies$week > 44 & maxWeekSpecies$week < 51)/length(maxWeekSpecies$article)

# find out which species are the ones that have the highest values then
whichSpecies<-maxWeekSpecies[maxWeekSpecies$week > 44 & maxWeekSpecies$week < 51, "article"]
whichSpecies<-droplevels(data.frame(whichSpecies))
head(whichSpecies)
whichSpecies
write.csv(whichSpecies, file = "topSpecies.csv",row.names=FALSE)

# calculate the 75th quantile for page hits by article

quantileData<-mobileOutputSummaryWeekly %>%
  group_by(article) %>%
 summarize(q3=quantile(views, 0.75))
quantileData<-data.frame(quantileData)


# subset the data by the 90th quantile (or whatever you want)
# you can set this to .99 which should show the number of species whose max was during the airing
quantileDataSub <- mobileOutputSummaryWeekly %>% 
  group_by(article) %>% 
  filter(week, views > quantile(views, .9))
quantileDataSub<-data.frame(quantileDataSub)
head(quantileDataSub)

length(quantileDataSub$views)
length(mobileOutputSummaryWeekly$views)

# count the number of weeks that fall within the original airing above 90th percentile
totalWeeks<-quantileDataSub %>% 
  group_by(article) %>% 
  count(week > 44 & week < 51)
totalWeeks<-data.frame(totalWeeks)
names(totalWeeks)[names(totalWeeks) == 'week...44...week...51'] <- 'percentile_T_F'
length(totalWeeks$percentile_T_F[totalWeeks$percentile_T_F=="TRUE"])/length(totalWeeks$percentile_T_F)

sum(totalWeeks$n[totalWeeks$percentile_T_F=="TRUE"])/sum(totalWeeks$n)









##########################################################################################
# USA DATA
##########################################################################################
# were these weeks during the air dates of Planet Earth 2 in the USA? 
length(maxWeekSpecies$article)
sum(maxWeekSpecies$week > 7 & maxWeekSpecies$week < 12)
sum(maxWeekSpecies$week > 7 & maxWeekSpecies$week < 12)/length(maxWeekSpecies$article)

# find out which species are the ones that have the highest values then
whichSpecies<-maxWeekSpecies[maxWeekSpecies$week > 7 & maxWeekSpecies$week < 12, "article"]
whichSpecies<-droplevels(data.frame(whichSpecies))
head(whichSpecies)
##########################################################################################
# plot the data
output$access<-as.factor(output$access)
class(output$access)
#plot(mydata$date,mydata$views, col=mydata$access, pch = 16)
plot(output$date[output$agent=="user"],output$views[output$agent=="user"], col=output$access[output$agent=="user"], 
     pch = 16, xlab = "time", ylab = "hits")

# add a legend
legend("topright", legend=c("desktop", "mobile-web"),
       col=c("black", "red"), cex=0.8, pch = 16)

# plot individual species
plot(output$date[output$agent=="user" & output$article=="Bee-eater"],output$views[output$agent=="user"& output$article=="Bee-eater"], col=output$access[output$agent=="user"& output$article=="Bee-eater"], 
     pch = 16, xlab = "time", ylab = "hits")

# add a legend
legend("topright", legend=c("desktop", "mobile-web"),
       col=c("black", "red"), cex=0.8, pch = 16)

##########################################################################################
# group views by month
mobileOutputSummary<-mobileOutput %>%
  group_by(month) %>%
  summarise(views = sum(views))
mobileOutputSummary<-as.data.frame(mobileOutputSummary)

# plot views by month for mobile access 
plot(mobileOutputSummary$views ~ mobileOutputSummary$month, pch = 16, xlab="month", ylab="hits")

##########################################################################################
# desktop data only
desktopOutput <- output[output$access=="desktop" , ]
desktopOutput<-droplevels(desktopOutput)
head(desktopOutput)

# group views by month
desktopOutputSummary<-desktopOutput %>%
  group_by(month) %>%
  summarise(views = sum(views))
desktopOutputSummary<-as.data.frame(desktopOutputSummary)

# plot views by month for mobile access 
plot(desktopOutputSummary$views ~ desktopOutputSummary$month, pch = 16, xlab="month", ylab="hits")

# we can also group views by month for each species rather than a total grouping for all species 
# mobile data
mobileOutputSummarySpecies<-mobileOutput %>%
  group_by(month, article) %>%
  summarise(views = sum(views))
mobileOutputSummarySpecies<-as.data.frame(mobileOutputSummarySpecies)

plot(mobileOutputSummarySpecies$views[mobileOutputSummarySpecies$article=="Desert_long-eared_bat"] ~ 
       mobileOutputSummarySpecies$month[mobileOutputSummarySpecies$article=="Desert_long-eared_bat"], 
     pch = 16, xlab="month", ylab="hits")

# desktop data
desktopOutputSummarySpecies<-desktopOutput %>%
  group_by(month, article) %>%
  summarise(views = sum(views))
desktopOutputSummarySpecies<-as.data.frame(desktopOutputSummarySpecies)




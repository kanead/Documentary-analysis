# clean everything first
rm(list=ls())
library(dplyr)
library(pageviews)

# load in the data which is a vector of species names 
setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")
data<-read.csv("names.csv", header = T, sep = ",")
head(data)
length(data$name)

# remove the duplicated values 
data.new<-data[!duplicated(data), ]
length(data.new)
class(data.new)
#data.test<-tail(data.new,25)

# modify the function from pageviews package to collect data for each species 
get_wiki <- function(x){article_pageviews(project = "en.wikipedia", article = x
                           , start = as.Date('2016-01-01'), end = as.Date("2016-12-31")
                           , user_type = "user", platform = c("desktop", "mobile-web"))
}

# loop over each species 
output<-data.new %>%  get_wiki
output
length(output)
tail(output)
head(output)

# save the collected data
write.csv(output, file = "output.csv",row.names=FALSE)

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

# access the months in our dataset and add them to our original output data 
output$month<-strftime(output$date, format = "%m")

# access the weeks in our dataset and add them to our original output data 
output$week<-strftime(output$date, format = "%W")

# mobile data only
mobileOutput <- output[output$access=="mobile-web" , ]
mobileOutput<-droplevels(mobileOutput)
head(mobileOutput)

# group views by month
mobileOutputSummary<-mobileOutput %>%
  group_by(month) %>%
  summarise(views = sum(views))
mobileOutputSummary<-as.data.frame(mobileOutputSummary)

# plot views by month for mobile access 
plot(mobileOutputSummary$views ~ mobileOutputSummary$month, pch = 16, xlab="month", ylab="hits")

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

# group views by week and species
mobileOutputSummaryWeekly<-mobileOutput %>%
  group_by(week, article) %>%
  summarise(views = sum(views))
mobileOutputSummaryWeekly<-as.data.frame(mobileOutputSummaryWeekly)

plot(mobileOutputSummaryWeekly$views ~ mobileOutputSummaryWeekly$week, pch = 16, xlab="month", ylab="hits")

plot(mobileOutputSummaryWeekly$views[mobileOutputSummaryWeekly$article=="Desert_long-eared_bat"] ~ 
       mobileOutputSummaryWeekly$week[mobileOutputSummaryWeekly$article=="Desert_long-eared_bat"], 
     pch = 16, xlab="week", ylab="hits")

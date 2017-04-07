# clean everything first
rm(list=ls())

install.packages("pageviews")



library(pageviews)
mydata<-(article_pageviews(project = "en.wikipedia", article = "Sloth"
                      , start = as.Date('2016-05-01'), end = as.Date("2016-12-31")
                      , user_type = c("user", "bot"), platform = c("desktop", "mobile-web")))

head(mydata)
mydata$access<-as.factor(mydata$access)
class(mydata$access)
#plot(mydata$date,mydata$views, col=mydata$access, pch = 16)
plot(mydata$date[mydata$agent=="user"],mydata$views[mydata$agent=="user"], col=mydata$access[mydata$agent=="user"], pch = 16)
hist(mydata$views[mydata$agent=="user"])

hist(log10(mydata$views)[mydata$agent=="user"])
mean(log10(mydata$views)[mydata$agent=="user"])
sd(log10(mydata$views)[mydata$agent=="user"])

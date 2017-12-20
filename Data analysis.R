# Random Forest Regression Model 
rm(list=ls())

library(randomForest)
library(dplyr)
library(olsrr)

setwd("C:\\Users\\akane\\Dropbox\\sir david")
mydata<-read.csv("master spreadsheet.csv", header = T,sep=",")
head(mydata)

#drops <- c("wiki")
#mydata<-mydata[ , !(names(mydata) %in% drops)]
head(mydata)
names(mydata)

mydata <- mydata[which(mydata$onlydiaries=='0'),]
tail(mydata)

names(mydata[c(1,3,7,8,9,10,11,12,15,18,20,22,25,29,34)])
newdata <- mydata[c(1,3,7,8,9,10,11,12,15,18,20,22,26,29,34,42)]
head(newdata)
names(newdata)[names(newdata) == 'accuracy..1.high..2.name.covers.several.sp.eg.sifaka.or.skua..3.very.generic.eg.mouse.or.crab..4.confusion.name.'] <- 'accuracy'
names(newdata)[names(newdata) == 'viewers..citation..BARB.'] <- 'viewers'
names(newdata)[names(newdata) == 'baseline.01.07.15.to.30.06.16...sum.median'] <- 'baseline'
names(newdata)[names(newdata) == 'wiki.diff.median.sum'] <- 'wiki'
names(newdata)[names(newdata) == 'summed.anomaly.1.'] <- 'anomaly'
names(newdata)[names(newdata) == 'status2'] <- 'status'
names(newdata)[names(newdata) == 'tweet.count'] <- 'twitter'
names(newdata)[names(newdata) == 'max.wiki.hit.sum'] <- 'maxWiki'
names(newdata)[names(newdata) == 'taxa_gen'] <- 'taxa'

head(newdata)

newdata$anomaly[newdata$anomaly==2] <- 1
newdata$multiple[is.na(newdata$multiple)] <- 0



class(newdata$status)
newdata$twitter <- as.numeric(newdata$twitter)

#newdata$wiki <- scale(newdata$wiki)
#newdata$baseline <- scale(newdata$baseline)

# which columns have missing data
head(newdata)
apply(newdata, 2, function(x){any(is.na(x))})
# drop incomplete cases for random forest 
# newdata<-newdata[complete.cases(newdata), ]
length(newdata$accuracy)

# categorical response 
newdata$anomaly <- as.factor(newdata$anomaly)
levels(newdata$anomaly)

output.forest1 <- randomForest(anomaly~accuracy+viewers+multiple+last+first+seconds+taxa+baseline+
                                 diaries+status, data = newdata, ntree=10000 ,mtry=4)

print(output.forest1) 
print(importance(output.forest1,type = 2)) 
varImpPlot(output.forest1,type=2)

# continuous response 
output.forest2 <- randomForest(maxWiki~accuracy+viewers+multiple+last+first+seconds+taxa+baseline+
                                 diaries+status+twitter, data = newdata, 
                               ntree=10000 ,mtry=3)
print(output.forest2) 
print(importance(output.forest2,type = 2)) 
varImpPlot(output.forest2,type=2)

###################################################################################
# glm 
###################################################################################
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(~accuracy+multiple+seconds+taxa+baseline+twitter+wiki,data = newdata, 
      lower.panel=panel.smooth, upper.panel=panel.cor)

boxplot(newdata$baseline~newdata$taxa)
boxplot(newdata$seconds~newdata$taxa)

boxplot(newdata$twitter~newdata$taxa)
plot(newdata$twitter~newdata$baseline)
plot(log10(newdata$twitter)~log10(newdata$baseline))
plot(newdata$twitter~newdata$seconds)


#levels(newdata$taxa)[levels(newdata$taxa)=="1mammalia"] <- "aaamammalia"


m1 <- glm(wiki~accuracy+multiple+seconds+taxa+baseline+twitter,data = newdata)
summary(m1)
step(m1)

m2<-glm(wiki~seconds+twitter,data = newdata)
summary(m2)
plot(m2)

modBase <- lm(wiki ~ seconds + baseline + twitter, data=newdata)
summary(modBase)



m2<-glm(wiki~seconds+twitter,data = newdata)
summary(m2)
plot(m2)

# remove the values that go below the median
moddata<-newdata[newdata$wiki>0,]

m3 <- glm(sqrt(wiki)~accuracy+viewers+seconds+taxa+baseline+
            +status+twitter,data=moddata)

summary(m3)
step(m3)


m4<-glm(formula = sqrt(wiki) ~ seconds + baseline + twitter, data = moddata)
summary(m4)
plot(m4)

m5<-glm(log(wiki)~twitter+baseline,data=moddata)
summary(m5)
plot(m5)

m6<-glm(log(wiki)~twitter+baseline+seconds,data=moddata)
summary(m6)
plot(m6)

ols_vif_tol(m6)


m7<-glm(log(wiki)~accuracy+viewers+seconds+taxa+baseline+
          +status+twitter,data=moddata)
summary(m7)

step(m7)

m8<-lm(formula = log10(wiki) ~ baseline + status + twitter, data = moddata)
summary(m8)

ols_vif_tol(m8)


mlog <- glm(log(wiki) ~ log(baseline) + log(twitter) + log(seconds),data=moddata)

NAdata<-moddata[complete.cases(moddata), ]


######################################################
# twitter as y 
######################################################

wrong <- glm(twitter~seconds+cord+baseline,data=newdata)
summary(wrong)
plot(wrong)

plot(newdata$twitter~newdata$seconds)
boxplot(newdata$twitter~newdata$cord)


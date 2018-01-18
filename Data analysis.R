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

#mydata <- mydata[which(mydata$onlydiaries=='0'),]
#tail(mydata)

#names(mydata[c(1,3,7,8,9,10,11,12,15,18,20,22,25,29,34)])
newdata <- mydata[c(5:21)]
VIF.test <- newdata[c(1,4,5)]
corvif(VIF.test)
#names(newdata)[names(newdata) == 'accuracy..1.high..2.name.covers.several.sp.eg.sifaka.or.skua..3.very.generic.eg.mouse.or.crab..4.confusion.name.'] <- 'accuracy'
#names(newdata)[names(newdata) == 'viewers..citation..BARB.'] <- 'viewers'
#names(newdata)[names(newdata) == 'baseline.01.07.15.to.30.06.16...sum.median'] <- 'baseline'
#names(newdata)[names(newdata) == 'wiki.diff.median.sum'] <- 'wiki'
#names(newdata)[names(newdata) == 'summed.anomaly.1.'] <- 'anomaly'
#names(newdata)[names(newdata) == 'status2'] <- 'status'
#names(newdata)[names(newdata) == 'tweet.count'] <- 'twitter'
#names(newdata)[names(newdata) == 'max.wiki.hit.sum'] <- 'maxWiki'
#names(newdata)[names(newdata) == 'taxa_gen'] <- 'taxa'

# which columns have missing data
head(newdata)
apply(newdata, 2, function(x){any(is.na(x))})

newdata$Anom1 <- as.factor(newdata$Anom1)
levels(newdata$Anom1)
newdata$Anom1[newdata$Anom1==2] <- 1
newdata$Anom1<-droplevels(newdata$Anom1)
levels(newdata$Anom1)

newdata$status<-as.factor(newdata$status)

#class(newdata$status)
#newdata$twitter <- as.numeric(newdata$twitter)

#newdata$wiki <- scale(newdata$wiki)
#newdata$baseline <- scale(newdata$baseline)


# drop incomplete cases for random forest 
newdata<-newdata[complete.cases(newdata), ]
length(newdata$accuracy)

# categorical response 
output.forest1 <- randomForest(Anom1~accuracy+viewers+multiple+last+first+seconds+taxa_gen+baseline_2015+
                                 diaries+status+tweet, data = newdata, ntree=10000 ,mtry=4)

print(output.forest1) 
print(importance(output.forest1,type = 2)) 
varImpPlot(output.forest1,type=2)

# continuous response 
#output.forest2 <- randomForest(diff~accuracy+viewers+multiple+last+first+seconds+taxa_gen+baseline_2015+
#                                 diaries+status+tweet, data = newdata, ntree=10000 ,mtry=4)

output.forest2 <- randomForest(diff~accuracy+viewers+seconds+taxa_gen+baseline_2015+
                                  diaries+status+tweet, data = newdata, ntree=10000 ,mtry=3)

print(output.forest2) 
print(importance(output.forest2,type = 2)) 
varImpPlot(output.forest2,type=2)


# remove data that have no anomalies 
subdata <-newdata[newdata$Anom1==1,]
length(subdata$article)

# continuous response 
output.forest2 <- randomForest(diff~accuracy+viewers+multiple+last+first+seconds+taxa_gen+baseline_2015+
                                 diaries+status+tweet, data = subdata, 
                               ntree=10000 ,mtry=3)
print(output.forest2) 
print(importance(output.forest2,type = 2)) 
varImpPlot(output.forest2,type=2)

###################################################################################
# glm 
###################################################################################
# drop the differences that are negative 
moddata<-newdata[newdata$diff>0,]
# find out which explanatory variables have NAs
apply(moddata, 2, function(x){any(is.na(x))})

# check for correlations between the variables 

cor.test(moddata$seconds,moddata$baseline_2015)
#####################################################################################
# fit negative binomial regression for wiki hits on with the negatives removed  
#####################################################################################
m.NB.wiki <- glm.nb(diff~seconds + baseline_2015,data = moddata)
summary(m.NB.wiki)
#step(m.NB.wiki,direction = "both")
plot(m.NB.wiki)

# check for overdispersion, should be around 1, one suggested cutoff is < 1.5 
summary(m.NB.wiki)$deviance / summary(m.NB.wiki)$df.residual

# get the coefficients back in units we can understand by undoing the link function
exp(coef(m.NB.wiki))

# get pseudo R^2 values 
library(modEvA)
modEvA::RsqGLM(model=m.NB.wiki)

# drop problem value Giant Otter
moddata_otter <- moddata[-19, ]

m.NB.wiki <- glm.nb(diff~seconds + baseline_2015 + status ,data = moddata_otter)
summary(m.NB.wiki)
# step(m.NB.wiki,direction = "both")
plot(m.NB.wiki)

# check for overdispersion, should be around 1, one suggested cutoff is < 1.5 
summary(m.NB.wiki)$deviance / summary(m.NB.wiki)$df.residual

# get the coefficients back in units we can understand by undoing the link function
exp(coef(m.NB.wiki))

# get pseudo R^2 values 
library(modEvA)
modEvA::RsqGLM(model=m.NB.wiki)

# boxplot of diff as a function of IUCN status 
boxplot(moddata_otter$diff~moddata_otter$status)

# count the number of species per IUCN status 
moddata_otter %>% 
  group_by(status) %>%
  summarise(no_rows = length(status))


# count the number of species per taxonomic group 
moddata_otter %>% 
  group_by(taxa_gen) %>%
  summarise(no_rows = length(taxa_gen))


# check for multicolinearity 
# multicollinearity is a quality of the linear predictor - a linear combination of the explanatory variables. 


#####################################################################################
# fit negative binomial regression for tweet counts 
#####################################################################################
library(MASS)
m.NB.twitter<-glm.nb(tweet~seconds,data=newdata)
summary(m.NB.twitter)
plot(m.NB.twitter)

# check for overdispersion, should be around 1, one suggested cutoff is < 1.5 
summary(m.NB.twitter)$deviance / summary(m.NB.twitter)$df.residual

# get the coefficients back in units we can understand by undoing the link function
exp(coef(m.NB.twitter))

# get pseudo R^2 values 
modEvA::RsqGLM(model=m.NB.twitter)

#####################################################################################
# Fit square root transformed regression for tweet counts 
#####################################################################################
m.sqrt.twitter <- lm(sqrt(tweet) ~ sqrt(seconds), data = newdata)
summary(m.sqrt.twitter)
plot(m.sqrt.twitter)
#####################################################################################













modBase <- lm(wiki ~ seconds + baseline + twitter, data=newdata)
summary(modBase)





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


m1 <- lm(diff~accuracy+viewers+multiple+last+first+seconds+taxa_gen+baseline_2015+
           status+tweet, data = newdata)
summary(m1)
step(m1,direction = "both")

m2<-lm(diff~seconds,data = newdata)
summary(m2)
plot(m2)

m3 <- lm(diff~seconds+baseline_2015+tweet,data=newdata)
summary(m3)

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


##########################################################################
# Rolling Correlation between Arkive and Wikipedia page hits 
##########################################################################
rm(list=ls()) 
library(data.table)
library(tidyquant)  # Loads tidyverse, tidyquant, financial pkgs, xts/zoo
library(cranlogs)   # For inspecting package downloads over time
library(corrr)      # Tidy correlation tables and correlation plotting
library(cowplot)    # Multiple plots with plot_grid()
library(ggplot2)

setwd("C:\\Users\\akane\\Desktop\\Science\\Manuscripts\\Documentary analysis\\Documentary-analysis")

# load in the web hits for Wiki and Arkive 
arkive<-read.csv("arkivePageViews.csv",header = T,sep = ",")
wiki<-read.csv("output.csv",header = T,sep = ",")

head(arkive)
head(wiki)
##########################################################################
# combine mobile and desktop access for wiki hits 
##########################################################################
dataTableOutput<-setDT(wiki)[, .(sumy=sum(views)), by = .(article,date)]
# 271 + 370 for African elephant 01/01/16
# 101 + 98 for hawksbill sea turtle 31/12/16
newdata<-data.frame(dataTableOutput)
names(newdata)[names(newdata) == 'sumy'] <- 'views'
newdata$date<-as.POSIXct(strptime(newdata$date,"%d/%m/%Y"))
wiki<-droplevels(newdata)

head(arkive)
head(wiki)
##########################################################################
# change column names to match 
##########################################################################
names(arkive)[names(arkive) == 'species'] <- 'article'
names(arkive)[names(arkive) == 'day'] <- 'date'
names(arkive)[names(arkive) == 'page.views'] <- 'views'

# set the date column as a date class 
head(arkive)
arkive$date<-as.POSIXct(strptime(arkive$date,"%d/%m/%Y"))
head(arkive)

# we'll compare a subset of the data that matches exactly 
arkiveSub<-arkive[arkive$article %in% wiki$article,]
arkiveSub<-droplevels(arkiveSub)

# pare the wiki data back so we're dealing with the same number of factor levels 
wikiSub<-wiki[wiki$article %in% arkiveSub$article,]
wikiSub<-droplevels(wikiSub)

levels(wikiSub$article)
levels(arkiveSub$article)
##########################################################################
# some of the wiki entries don't have data for the full year for some reason 
##########################################################################
wikiLength<-wikiSub %>% 
  group_by(article) %>%
  summarise(no_rows = length(article))

arkiveLength<-arkiveSub %>% 
  group_by(article) %>%
  summarise(no_rows = length(article))

which(wikiLength$no_rows!=366)
which(arkiveLength$no_rows!=366)

wikiLength[28,]
wikiLength[55,]

idx<-c("Jackson's_widowbird", "Seychelles_fody")
##########################################################################
# remove the tracks with a fewer than 366 results for web hits as set out with idx
##########################################################################
# write a function that is the opposite of the match function %in%
`%not in%` <- function (x, table) is.na(match(x, table, nomatch=NA_integer_))

wikiSub<-wikiSub[wikiSub$article %not in% idx,] 
wikiSub<-droplevels(wikiSub)

arkiveSub<-arkiveSub[arkiveSub$article %not in% idx,] 
arkiveSub<-droplevels(arkiveSub)

# check to see if the levels are removed 
levels(wikiSub$article)

# check the class of the factor levels 
levels(arkiveSub$article)

# join the dataframes 
returns_joined <- left_join(wikiSub, 
                            arkiveSub,
                            by = c("date","article")) 
##########################################################################
# date causes an issue, change it to as.Date
##########################################################################
returns_joined$date<-as.Date(returns_joined$date)
head(returns_joined)
tail(returns_joined)
levels(returns_joined$article)
class(returns_joined$date)
##########################################################################
# the object that the running correlation is performed on is a tibble 
##########################################################################
returns_joined<-returns_joined %>%
  group_by(article)

returns_joined<-as.tibble(returns_joined)
returns_joined

# create zoo object
# library(timeSeries)
##########################################################################
# static correlation 
##########################################################################
staticCorr<-returns_joined %>%
  group_by(article) %>%
  summarize(COR=cor(views.x,views.y))
# hist(staticCorr$COR)
median(staticCorr$COR)

# subset by date
substaticCorr<- returns_joined %>% 
  select(article,date,views.x, views.y) %>%
  filter(date >= "2016-11-06" & date <="2016-12-12")

substaticCorr<-substaticCorr %>%
  group_by(article) %>%
  summarize(COR=cor(views.x,views.y))
# hist(substaticCorr$COR)
median(substaticCorr$COR)

##########################################################################
# perform a running correlation 
##########################################################################
# n = the resolution of the running correlation 
# an n of 7 performs a running correlation at the level of a week
testCorr<-returns_joined %>%
tq_mutate_xy(
  x          = views.x,
  y          = views.y,
  mutate_fun = runCor, 
  # runCor args
  n          = 60,
  use        = "pairwise.complete.obs",
  # tq_mutate args
  col_rename = "rolling_corr"
)

testCorr

# plot it 
library(scales)
testCorr$date<-as.Date(testCorr$date)
testCorr %>%
  ggplot(aes(x = date, y = rolling_corr, color = article)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  labs(title = "Rolling Correlation",
       x = "", y = "Correlation", color = "") +   scale_x_date(labels = date_format("%d-%b")) +
  facet_wrap(~ article, ncol = 10) + theme(legend.position="none") #+
 # scale_x_date(date_breaks = "1 month", 
#               labels=  date_format("%m-%y"),
#               limits = as.Date(c('2016-10-01','2016-12-12'))) 

##########################################################################
# split the data into smaller chunks and do the same analysis 
##########################################################################
testCorrSub <- returns_joined %>%
  group_by(article) %>%
  filter(date >= "2016-10-27" & date <="2016-11-09")

testCorrSub<-testCorrSub %>%
  tq_mutate_xy(
    x          = views.x,
    y          = views.y,
    mutate_fun = runCor, 
    # runCor args
    n          = 3,
    use        = "pairwise.complete.obs",
    # tq_mutate args
    col_rename = "rolling_corr"
  )

# testCorrSub

# plot it 
library(scales)
testCorrSub$date<-as.Date(testCorrSub$date)
testCorrSub %>%
  ggplot(aes(x = date, y = rolling_corr, color = article)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  labs(title = "Rolling Correlation",
        y = "Correlation", color = "") +   scale_x_date(labels = date_format("%d-%b")) +
  facet_wrap(~ article, ncol = 10) + theme(legend.position="none") 

##########################################################################
# summarise the rolling correlations overall
##########################################################################
rollCorrSumm<-testCorrSub %>%
    group_by(article) %>%
  summarize(median(rolling_corr, na.rm=T))
 summary(rollCorrSumm$`median(rolling_corr, na.rm = T)`)

# and by a subset of dates over whole broadcast 
rollCorrSummSub<-testCorrSub %>%
  group_by(article) %>%
  filter(date >= "2016-10-27" & date <="2016-11-09")   %>%
  summarize(median(rolling_corr, na.rm=T))
summary(rollCorrSummSub$`median(rolling_corr, na.rm = T)`)

# finally subset by just the broadcast dates 
#rollCorrSummBroad<-testCorr %>%
#  group_by(article) %>%
#  filter(date == "2016-11-06" & date =="2016-11-07" &
#         date == "2016-11-13" & date =="2016-11-14" &
#         date == "2016-11-20" & date =="2016-11-21" &
#         date == "2016-11-27" & date =="2016-11-28" &
#         date == "2016-12-04" & date =="2016-12-05" &
#         date == "2016-12-11" & date =="2016-12-12")   %>%
#  summarize(median(rolling_corr, na.rm=T))
#summary(rollCorrSummSub$`median(rolling_corr, na.rm = T)`)

##########################################################################
# Subset the data by episode
##########################################################################

episode1 <- c("Pygmy_three-toed_sloth","Komodo_dragon","Indri","Ring-tailed_lemur","Bamboo_lemur",
              "Sifaka","Marine_iguana","Snares_penguin","Shearwater","Buller's_albatross",
              "Fairy_tern","Noddy_(tern)","Chinstrap_penguin",
              "Skua")

# keep only the species featured in this episode 
returns_joinedEp1<-returns_joined[returns_joined$article %in% episode1,] 
returns_joinedEp1<-droplevels(returns_joinedEp1)

returns_joinedEp1 <- returns_joinedEp1 %>%
  group_by(article) %>%
  filter(date >= "2016-10-27" & date <="2016-11-12")

returns_joinedEp1<-returns_joinedEp1 %>%
  tq_mutate_xy(
    x          = views.x,
    y          = views.y,
    mutate_fun = runCor, 
    # runCor args
    n          = 3,
    use        = "pairwise.complete.obs",
    # tq_mutate args
    col_rename = "rolling_corr"
  )

# testCorrSub

# plot it 
library(scales)
returns_joinedEp1$date<-as.Date(returns_joinedEp1$date)
returns_joinedEp1 %>%
  ggplot(aes(x = date, y = rolling_corr, color = article)) +
  geom_hline(yintercept = 0) +
  geom_line(size = 1) +
  labs(title = "Rolling Correlation",
       y = "Correlation", color = "") +   scale_x_date(labels = date_format("%d-%b")) +
  facet_wrap(~ article, ncol = 4) + theme(legend.position="none") 

##########################################################################
# summarise the rolling correlations overall
##########################################################################
rollCorrSumm<-returns_joinedEp1 %>%
  group_by(article) %>%
  summarize(median(rolling_corr, na.rm=T))
summary(rollCorrSumm$`median(rolling_corr, na.rm = T)`)

# and by a subset of dates over whole broadcast 
rollCorrSummSub<-returns_joinedEp1 %>%
  group_by(article) %>%
  filter(date >= "2016-10-27" & date <="2016-11-12")   %>%
  summarize(median(rolling_corr, na.rm=T))
summary(rollCorrSummSub$`median(rolling_corr, na.rm = T)`)

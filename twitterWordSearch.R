# Twitter word search 
# excel returns the number of cells with a match for a string, str_count returns the absolute number of matches
# so the value can be higher in R if a tweet mentions an animal more than once
rm(list=ls())
library(dplyr)
library(stringr)
require(stringi)

setwd("C:/Users/dfernandezbellon/Dropbox/work/documentaries/sir david/twittah/")
data <- read.csv("twittah.csv",header = T, sep = ",")
head(data)
######################################################################################################
# Count the number of tweets we recovered by episode
######################################################################################################

broadcastLength<-data.frame(data %>% group_by(broadcast)%>%summarise(no_rows = length(broadcast)))
broadcastLength

# episode 1 = 6086 tweets        
# episode 2 = 5693 tweets        
# episode 3 = 6473 tweets        
# episode 4 = 5458 tweets        
# episode 5 = 5385 tweets        
# episode 6 = 5584 tweets

######################################################################################################
# Subset tweets (use=1) to obtain 5k per episode and count again
######################################################################################################

mydata<-data[which(data$use==1),]


broadcastLength<-data.frame(mydata %>% group_by(broadcast) %>% summarise(no_rows = length(broadcast)))
broadcastLength

# all episodes 5k

######################################################################################################
# Count number of occurrences of species in tweets
######################################################################################################
# don't forget the crab!

#what fucking crab??

#
#
#
#
#
#
#
#I'm working on this - D
#
#need to:
#         finalise species list
#         figure out a way to input vector of string variables into count formula
#         ONLY do sp count for the episode in which they appeared (as opposed to tweets from all episodes)
#
#
#frome Kane-O:
#
#       x<-dplyr::filter(mydata, grepl('sloth|Komodo', mydata$content,ignore.case = TRUE))
#       length(x$content)
#Need to find out how to input the names from the vector of string variables rather than typing them all out. But it works. 




#Episode 1
episode1 <- c("sloth","Komodo","Indri","lemur","Sifaka","iguana","snake","penguin","Shearwater","albatross",
              "tern","fody","Noddy","crazy ant", "Skua")

grepl(episode1, mydata$content,fixed=TRUE,ignore.case = TRUE)


x<-dplyr::filter(mydata, grepl(episode1, mydata$content,fixed=TRUE,ignore.case = TRUE))
length(x$content)









#
# PREVIOUS SCRIPT
#




# collapse all the rows of tweets into a single block of text
tweetText<-paste(unlist(mydata$content), collapse =" ")

# Episode 1 Species 
# create the vector of words we want to search for, simplified for Twitter
episode1 <- c("sloth","Komodo","Indri","lemur","Sifaka","iguana","snake","penguin","Shearwater","albatross",
              "tern","fody","Noddy","crazy ant", "Skua")
# search for them while ignoring the case 
str_count(tweetText, ignore.case(episode1))

# Episode 2 Species 
# create the vector of words we want to search for, simplified for Twitter
episode2 <- c("Snow leopard","ibex","Red fox","Golden eagle","Peregrine falcon","Crow",
              "Grizzly bear","Marmot","Bobcat","Mouse","Duck","Squirrel","viscacha",
              "Flamingo")
# search for them while ignoring the case 
str_count(tweetText, ignore.case(episode2))

# Episode 3 Species
episode3 <- c("Indri","Spider monkey","Draco lizard","hummingbird","River dolphin",
              "Capybara","Giant otter","Caiman","Jaguar","Glass frog","beetle","worm",
              "Millipede","bird of paradise")
# search for them while ignoring the case 
str_count(tweetText, ignore.case(episode3))

# Episode 4 species 
episode4 <- c("Lion","Oryx","Giraffe","hawk","squirrel","Butcherbird","Locust",
              "Elephant","Sandgrouse","Mustang","Golden mole","Termite","bat",
              "scorpion","chameleon")
# search for them while ignoring the case 
str_count(tweetText, ignore.case(episode4))

# Episode 5 species 
episode5 <- c("Saiga","Lion","Buffalo","mouse","Barn owl","bee eater",
              "bustard","Ostrich","elephant","Serval","rat",
              "Wildebeest","widowbird","Termite","anteater", "Giant otter","Bison","Fox",
              "Caribou","wolf","Tiger","Rhino","Sloth bear","buffalo")
# search for them while ignoring the case 
str_count(tweetText, ignore.case(episode5))

# Episode 6 species 
episode6 <- c("Langur","Peregrine falcon","Leopard","Pig","Starling","bowerbird","Raccoon",
              "macaque","hyena","Pigeon","catfish","turtle","otter")
# search for them while ignoring the case 
str_count(tweetText, ignore.case(episode6))

######################################################################################################
# Subset tweets based on matches 
######################################################################################################

mydataSubset <- mydata[grep("indri", mydata$content, ignore.case = TRUE), ]
length(mydataSubset$content)
some_txt<- mydataSubset$content
# some_txt<-paste(unlist(mydataSubset$content), collapse =" ")



if (!require("devtools")) install.packages("devtools")
devtools::install_github("PMassicotte/gtrendsR")

library(gtrendsR)

res <- gtrends(c("nhl", "nba"), geo = c("CA", "US"))
plot(res)






searchTrends<-(gtrends(c("planet earth 2", "conservation"),geo = c('GB'), time = "2016-01-01 2016-12-31")) 
plot(searchTrends)

searchTrends<-(gtrends(c("planet earth 2", "Attenborough"),geo = c('GB'), time = "2016-01-01 2016-12-31")) 
plot(searchTrends)

searchTrends<-(gtrends(c("planet earth 2", "iguana"),geo = c('GB'), time = "2016-01-01 2016-12-31")) 
plot(searchTrends)

searchTrends<-(gtrends(c("planet earth 2", "IUCN"),geo = c('GB'), time = "2016-01-01 2016-12-31")) 
plot(searchTrends)

searchTrends<-(gtrends(c("planet earth 2", "world wildlife fund"),geo = c('GB'), time = "2016-01-01 2016-12-31")) 
plot(searchTrends)

searchTrends<-(gtrends(c("planet earth 2", "climate change", "land use", "invasive species", "extinction"),geo = c('GB'), time = "2016-01-01 2016-12-31")) 
plot(searchTrends)

searchTrends<-(gtrends(c("planet earth 2"),geo = c('GB'), time = "2016-01-01 2016-12-31")) 
plot(searchTrends)
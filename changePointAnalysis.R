# Changepoint Analysis in R

library(pageviews)
library(changepoint)
set.seed(10)
m.data <- c(rnorm(100, 0, 1), rnorm(100, 1, 1), rnorm(100, 0, 1),
               + rnorm(100, 0.2, 1))
ts.plot(m.data, xlab = "Index")

# PELT and binary segmentation search methods
m.pelt <- cpt.mean(m.data, method = "PELT")
plot(m.pelt, type = "l", cpt.col = "blue", xlab = "Index",
         cpt.width = 4)
cpts(m.pelt)

# binary segmentation search methods
m.binseg <- cpt.mean(m.data, method = "BinSeg")
plot(m.binseg, type = "l", xlab = "Index", cpt.width = 4)
cpts(m.binseg)

# number of discoveries over time 
data("discoveries", package = "datasets")
dis.pelt <- cpt.meanvar(discoveries, test.stat = "Poisson",
                           method = "PELT")
plot(dis.pelt, cpt.width = 3)
cpts.ts(dis.pelt)

class(discoveries)


tsData<-article_pageviews(project = "en.wikipedia", article = "Golden_mole"
                                         , start = as.Date('2016-01-01'), end = as.Date("2017-12-31")
                                         , user_type = "user", platform = c("desktop", "mobile-web"))





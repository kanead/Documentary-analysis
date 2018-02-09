library(devtools)
install_github("pablobarbera/Rfacebook/Rfacebook")
library(Rfacebook)


token <- 'EAACEdEose0cBALnfN4HRfBFA5co8XGH0Rxqaoh9ZAQ7poZAO1pwwIJIkvKAghL7GXFFEplKSxzRZA5U8FIQycQ6hb2A1FuyFRq4uALEIvPtdbmmpCqDGRQUIj01oPKQiVzXpCXllbAaO9lRNH7muMtC3QQJsgkvinVDAoYWGXCvz3w68eHUa8c1hZCtJl38ZD'

me <- getUsers("me", token, private_info=TRUE)
me$name # my name
# [1] "Pablo Barberá"
me$hometown # my hometown
# [1] "Cáceres, Spain"

# https://www.facebook.com/WWF/
page <- getPage("WWF", token, n = 5000, since='2016/01/01', until='2016/12/31')

page <- getPage("WWF", token, n = 5000)
page[which.max(page$likes_count), ]

getLikes("bbcearth", token = token)

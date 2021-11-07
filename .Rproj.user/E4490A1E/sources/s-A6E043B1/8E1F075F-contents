library(tidyverse)
library(RSelenium)
library(rvest)




setwd("C:/Users/HW van der Waal/projects/SDG_tax")


url <- "https://finance.yahoo.com/quote/"
rD <- rsDriver(browser="firefox", port=4566L, verbose=FALSE)
remDr <- rD[["client"]]

#let browser go to URL
remDr$navigate(url)

#extract number of hits



#term <- "chad"
#remDr$findElement(using = "css selector", value = "body > div.content-wrapper > div > main > article > section > div.explore-list__search > input")$sendKeysToElement(list(term))
remDr$findElement(using = "css selector", value = "#yfin-usr-qry")
remDr$findElements("css", "button.explore-list__load-more.button")[[1]]$clickElement()


#this returns the URLS of articles with search term "chad", but only the first page. 
html <- remDr$getPageSource()[[1]]


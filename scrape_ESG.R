library(RSelenium)
library(tidyverse)
library(rvest)
library(purrr)

getwd()
ctry <- "TD"
url <- paste("https://www.business-humanrights.org/en/latest-news/?&country=",ctry, sep="")
url
rD <- rsDriver(browser="chrome", port=4545L, verbose=F)
remDr <- rD[["client"]]

#let browser go to URL
remDr$navigate(url)

#extract number of hits

#term <- "chad"
#remDr$findElement(using = "css selector", value = "body > div.content-wrapper > div > main > article > section > div.explore-list__search > input")$sendKeysToElement(list(term))
remDr$findElement(using = "css selector", value = "a.card__main-link")



#this returns the URLS of articles with search term "chad", but only the first page. 
html <- remDr$getPageSource()[[1]]

# this returns the number of hits
hits <- read_html(html) %>%
  html_nodes("p.explore-list__result-count") %>%
  html_text() %>%
  str_extract("\\d+") #("\\d{1,3}(,\\d{3})")
hits <- as.numeric(gsub(",", "", hits))

eind <- as.integer(hits/10)+1

#now we need to hit the explore hit load more button
for (teller in c(1:eind)){
  html <- remDr$getPageSource()[[1]] #returns first page 
  remDr$findElements("css", "button.explore-list__load-more.button")[[1]]$clickElement() #clicks on load more and appends
}

signals <- read_html(html) %>% # parse HTML
  html_nodes("a.card__main-link") %>%
map(xml_attrs) %>% 
  map_df(~as.list(.))


#create dynamic filename
file_name <- paste("url_", ctry, ".csv", sep="")
write.table(signals, file_name, row.names=TRUE, col.names=TRUE) 



#dit bestand analyseert de Stoxx3000 

#inlezen bibliotheken

library(readr)
library(tidyverse)
library(fuzzyjoin)
library(MatchIt)

#inlezen Stoxx 3000 data
stoxx3000 <- stoxx3000 <- read_delim("stoxx3000.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)%>% 
  select(-1)
colnames(stoxx3000)[9] <- "mcap"


#verdeling van bedrijven over categorieën

stoxx3000 %>% filter(Component == "Large") %>%
  group_by(Country) %>%
  summarize(country= Country, n = n()) %>% 
  filter(n>5) %>%
  ggplot(aes(x = reorder(country, -n), y = n)) + geom_col()

large <- stoxx3000 %>% filter(Component == "Large") %>%
  group_by(Country) %>%
  summarize(n = n())
stoxx <- stoxx3000 %>% mutate(., ctrygrp = with(., case_when(
  (Country == "US") ~ "USA",
  (Country %in% c("SE", "DE", "FR", "CH", "NL", "BE", "DK", "ES", "IT")) ~ 'EUR',
  (Country %in% c("KR", "JP", "TW")) ~ 'JKT',
  (TRUE ~ "OTH")
)))
stoxx %>% filter(Component == "Large") %>%
  group_by(ctrygrp) %>%
  summarize(n = n())

stoxx_Large <- stoxx %>% filter(Component == "Large") 

stoxx$ctrygrp <- as.factor(stoxx$ctrygrp)
#Propensity score matching
if(!require(MatchIt)){
  install.packages("MatchIt")
  library(MatchIt)
}

# aselectively draw 100 observations from JKT sample
stoxx_JKT <- stoxx_Large %>% filter(ctrygrp == "JKT")
set.seed(1, sample.kind = "Rounding")
JKT_sample <- sample_n(stoxx_JKT, 100)


`%!in%` <- Negate(`%in%`) # create NotIn operator
stoxx_USA <- stoxx %>% filter(Component == "Large" & ctrygrp == "USA")
stoxx_USA_JKT <- bind_rows(JKT_sample, stoxx_USA) %>%  #retains only USA
  mutate(treat = ifelse(ctrygrp == "JKT", 1 ,0)) 
  
  

# No matching; constructing a pre-match matchit object
m.out0_USA <- matchit(treat ~ mcap, data = stoxx_USA_JKT,
                  method = NULL, distance = "glm")


# Checking balance prior to matching
summary(m.out0_USA)

#create a matched dataset of USA on JKT (as the smallest subset)
set.seed(1234, sample.kind = "Rounding")
match.it.USA <- matchit(treat ~ mcap, data = stoxx_USA_JKT, method="nearest", ratio=1)
a <- summary(match.it.USA)
a

data.match.USA <- match.data(match.it.USA)


#create a matched dataset of EUR on JKT (as the smallest subset)
stoxx_EUR <- stoxx %>% filter(Component == "Large" & ctrygrp == "EUR") #retains only EUR
stoxx_EUR_JKT <- bind_rows(JKT_sample, stoxx_EUR) %>%  #retains only EUR
  mutate(treat = ifelse(ctrygrp == "JKT", 1 ,0))   


# No matching; constructing a pre-match matchit object
m.out0_EUR <- matchit(treat ~ mcap, data = stoxx_EUR_JKT,
                      method = NULL, distance = "glm")

# Checking balance prior to matching
summary(m.out0_EUR)

set.seed(1234, sample.kind = "Rounding")
match.it.EUR <- matchit(treat ~ mcap, data = stoxx_EUR_JKT, method="nearest", ratio=1)
a <- summary(match.it.EUR)
a

data.match.EUR <- match.data(match.it.EUR)
# remove JKT from one of the datasets
data.match.EUR <- data.match.EUR %>% filter(ctrygrp == "EUR")

#append the two datasets two get a dataset of equal number of ctrygrps
data_matched <- bind_rows(data.match.USA, data.match.EUR)

#compare mcap-means of the dataframes
mean(stoxx_Large$mcap) #overall mean
stoxx_Large %>% filter(ctrygrp =="EUR") %>% summarize(mean(mcap)) %>% pull()
stoxx_Large %>% filter(ctrygrp =="USA") %>% summarize(mean(mcap)) %>% pull()
stoxx_Large %>% filter(ctrygrp =="JKT") %>% summarize(mean(mcap)) %>% pull()
JKT_sample %>% summarize(mean(mcap)) %>% pull()
data_matched %>% summarize(mean(mcap)) %>% pull()

#save the data set to csv
write.csv(data_matched, "sample.csv")


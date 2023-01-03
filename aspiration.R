library(readr)
library(tidyverse)
library(dplyr)
aspiration <- read_delim("aspiration.csv", 
                         delim = ";", escape_double = FALSE, 
                         col_types = cols(ROA = col_double()), 
                         trim_ws = TRUE) %>% select(-c(Column1, Column2)) %>% 
                         distinct()
# calculate average performances per Factset_sector
a <- aspiration %>% filter(!(Factset_sector == "#N/B")) %>%
  group_by(Factset_sector, Year) %>% dplyr::summarise(mean_ROA = mean(ROA, na.rm=T), n= n(),
                                                      median_ROA = median(ROA, na.rm=T))
a
#calculate average performance over a 5-year average using rolling mean from zoo
#we are not using this, because it is not compared to the average industry performance, and
#it is questionable what it would mean. Timbate uses year relative performance and cetr. 
library(zoo)
aspiration <- aspiration %>% mutate(yrgrp16 = ifelse(Year %in% c(2012:2015), 1,0),
                      yrgrp20 = ifelse(Year %in% c(2016:2020), 1,0))  %>%
group_by(ISIN) %>% arrange(ISIN) %>%
  mutate(mPerf = 
           rollmean(ROA, k = 5, fill = NA, align = "left")) %>%
  select(ISIN, Year, mPerf, ROA, Factset_sector)


table(aspiration$ctrygrp, aspiration$Factset_sector)


aspiration_levels <- left_join(aspiration, a, by=c("Factset_sector" = "Factset_sector", "Year" = "Year")) %>%
  mutate(performance = ROA-mean_ROA) %>%
  mutate(performance = Winsorize(performance, probs = c(0.01, 0.99), na.rm = TRUE))
#
e <- aspiration_levels %>% 
  right_join(.,longlist_3, by = c("ISIN" = "ISIN", "Year" = "year")) %>% 
  mutate(cetr = CTP/PTI) %>% 
  select(ISIN, performance, cetr, Year, Factset_sector)
save(e, file = "performance.Rdata")

#===============================================================================
#de rest is wat plotten


aspiration_levels %>% ggplot(aes(Year, performance, fill = Factset_sector)) +
  geom_boxplot() 

aspiration_levels %>% ggplot(aes(factor(Year), performance) +
  geom_boxplot() +
  facet_wrap( . ~Factset_sector)

etr_perf <- aspiration_levels %>% 
  right_join(.,longlist_3, by = c("ISIN" = "ISIN", "Year" = "year"))




e %>% filter(cetr<1 & cetr > 0) %>%
  ggplot(aes(performance, cetr, col = ctrygrp)) +
    geom_point() + geom_smooth(method = "lm") +
  facet_grid(. ~ Factset_sector)

library(dynlm)
library(zoo)
e$Year <- as.Date(as.yearmon(e$Year))

stargazer(e[c("performance", "cetr")], type = "text", title = "Table 1: Summary statistics", out = "table1.txt")

te <- e %>% drop_na() %>% as.ts(e)
performance <- ts(e$performance, start=2012,end = 2020)
cetr <- ts(e$cetr, start = 2012, end = 2020)

dm <- dynlm(cetr ~ L(performance,2) + L(performance, 0) )
summary(dm)

lm <- lm(cetr ~ performance + ctrygrp + Factset_sector , data = e)  
summary(lm)
plot(cetr)


library(readr)
library(tidyverse)
library(dplyr)
aspiration <- read_delim("aspiration.csv", 
                         delim = ";", escape_double = FALSE, col_types = cols(ROA = col_double()), 
                         trim_ws = TRUE)

a <- aspiration %>% filter(!(Factset_sector == "#N/B")) %>%
  group_by(Factset_sector, Year) %>% dplyr::summarise(mean_ROA = mean(ROA, na.rm=T), n= n(),
                                                      median_ROA = median(ROA, na.rm=T))
a

table(aspiration$ctrygrp, aspiration$Factset_sector)


aspiration_levels <- left_join(aspiration, a, by=c("Factset_sector" = "Factset_sector", "Year" = "Year")) %>%
  mutate(performance = ROA-mean_ROA)

aspiration_levels %>% ggplot(aes(performance, Factset_sector)) +
  geom_boxplot() +
  facet_grid(. ~ ctrygrp)

aspiration_levels %>% ggplot(aes(factor(Year), performance)) +
  geom_boxplot() +
  facet_wrap( . ~Factset_sector)

etr_perf <- aspiration_levels %>% 
  right_join(.,longlist_3, by = c("ISIN" = "ISIN", "Year" = "year"))

e <- etr_perf %>% mutate(cetr = CTP/PTI) %>% select(ISIN, performance, cetr, Year, ctrygrp, Factset_sector)


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


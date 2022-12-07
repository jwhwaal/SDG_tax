library(tidyverse)

library(readr)
th <- read_csv("th.csv") %>% select(-'...1') %>%
  mutate(year = as.double(str_sub(doc_id, 14,17)))


dfth <- df %>% left_join(.,th, 
                             by = c( "ISIN" = "company"), c("Year" = "year")) %>% 
  mutate(th = as.factor(if_else(eu_noncoop > 0,1,0))) 

dfth %>% filter(etr_5_s < 0.1 & etr_5_s > -0.35 & year %in% c(2016,2020)) %>%
  ggplot(aes(etr_5_s, ctrygrp)) +
  geom_boxplot(na.rm = T) +
  facet_grid( th ~ year)

dfth %>% 
  filter(etr_5_s < 0.1 & etr_5_s > -0.35 & year %in% c(2016,2020)) %>%
  group_by(year, th) %>%
  summarize(m = median(etr_5_s, na.rm = T),
            sum = sum(eu_noncoop))

model4 <- lm(etr_5_s ~ cetr_5_s + th + factor(year) + SIZE + ROA + LEV + factor(FOREIGN), data = dfth)
summary(model4)

dfth %>% filter(etr_5_s < 0.5 & etr_5_s > -0.5 & year %in% c(2016,2020)) %>%
  ggplot(aes(etr_5_s, cetr_5_s)) +
  geom_point(na.rm = T) +
  geom_smooth(formula = y ~ x)


dfth %>% filter(etr_5_s < 0.5 & etr_5_s > -0.5 & year %in% c(2016,2020)) %>%
  ggplot(aes(etr_5_s, log(eu_noncoop+1))) +
  geom_point(na.rm = T) +
  geom_smooth(formula = y ~ x)

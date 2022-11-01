library(readr)
library(tidyverse)
sample_cetr_scaled <- read_delim("sample_cetr_scaled.txt", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
longlist <- sample_cetr_scaled %>% pivot_longer(., cols = starts_with("m"),
                                    names_to = "year") %>%
  select(-year) %>% rename(year = value)

library(readxl)
tr <- read_excel("taxrates_81_21.xlsx") 
tr <- tr %>%  
  select(iso_2, year, rate) %>%
  drop_na(rate) %>%
  mutate(rate = as.numeric(rate)) 


longlist <- longlist %>% left_join(., tr, by = c("year" = "year", "Country" = "iso_2"))
longlist <- longlist %>%
  mutate(str = rate/100) %>%
  seelct(-rate)

write.csv(longlist, "longlist.csv")

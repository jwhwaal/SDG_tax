a_t <- aspiration_levels %>% select(Year, ISIN, performance) %>%
  distinct(ISIN, Year, .keep_all = TRUE)


a_t_w <- a_t%>% 
  pivot_wider(., names_from = "Year", 
                                   values_from = "performance" )


a_t_w <- a_t_w %>% replace(is.na(.), 0) %>%
  mutate(mPerf_20 = rowMeans(.[2:5]), mPerf_16 = rowMeans(.[5:9]))
a_mp <- a_t_w %>% pivot_longer(.,cols = starts_with("mPerf"), 
                        values_to = "mPerf") %>%
  mutate(year = as.double(recode(name,
                          "mPerf_20" = "2020",
                          "mPerf_16" = "2016"
                          ))) %>%
  select(ISIN, year, mPerf)

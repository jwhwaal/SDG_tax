a_t <- aspiration_levels %>% select(Year, ISIN, performance) %>%
  distinct(ISIN, Year, .keep_all = TRUE)


a_t_w <- a_t%>% 
  pivot_wider(., names_from = "Year", 
                                  names_prefix = "Y", values_from = "performance" )


a_t_w <- a_t_w %>% replace(is.na(.), 0) %>%
  mutate(mPerf_20 = rowMeans(.[2:5]), mPerf_16 = rowMeans(.[5:9]))

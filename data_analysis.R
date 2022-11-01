library(readr)
library(tidyverse)
library(readxl)
data <- read_excel("sample_report.xlsx", 
                                    sheet = "sample_long") %>%
  filter(Year %in% c(2016, 2020))


# import statutory tax rates from Tax Foundation.org
#taxrates_81_21 <- read_excel("taxrates_81_21.xlsx")
#tr <- taxrates_81_21 %>% filter(iso_2 %in% unique(data$Country) & year %in% c(2016:2020)) %>%
#  mutate(stat_rate=as.numeric(rate)) %>%
#  select(iso_2, country, year, stat_rate)
#unique(data$Country)

# import Cetr_5_scaled file
library(readr)
longlist_3 <- read_delim("longlist_3.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE) %>%
  filter(year %in% c(2016, 2020))

data1 <- data %>% left_join(.,longlist_3, by = c("Year" = "year", "ISIN" = "ISIN", "Country" = "Country" )) %>%
  select(ISIN, Sector, Year, LTD, RD, PTI_FOREIGN, PTI, CTP, cetr_5, cetr_5_s, ASSETS, Country) %>%
  mutate(LTD = as.numeric(LTD), RD = as.numeric(RD), PTI_FOREIGN = as.numeric(PTI_FOREIGN))
data1 


#create new variables
data2 <- data1 %>% mutate(LEV = LTD/ASSETS,
                      RDS = RD/ASSETS,
                      SIZE = log(ASSETS),
                      FOREIGN = case_when(
                        PTI_FOREIGN > 1 ~ 1,
                        is.na(PTI_FOREIGN)  ~ 0,
                        TRUE ~ 0))
# SHELTER = -4.30 + 6.63*BTD -1.72*LEV + 0.66*SIZE + 2.26*ROA + 1.62*FOREIGN + 1.56*RDS)

load("C:/Users/HW van der Waal/projects/NLP-test/outdata.Rdata")
View(outdata)
summary(data2$CETR_scaled)

out <- outdata %>% select(company, year, sdg01:sdg, doc_id)
df <- data2 %>% left_join(.,out, by = c("ISIN" = "company", "Year"= "year"))
write.csv(df, "df_table.txt")

#calculate Cash Effective Tax Rates en Henry-Sansing measures
# The H&S measure, indicated as ∆, is defined as the difference between cash taxes paid, 
# adjusted for tax refunds receivable, and the product of its pre-tax book income 
# and the statutory tax rate. 
# The ∆ is then scaled by the market value of the firm assets to make the measure comparable across firms of different sectors. The market value of assets is the book value of assets plus the difference between market value of equity minus the book value of equity (Henry & Sansing, 2018). 

#The market value of assets is the book value of assets plus the difference 
#between market value of equity minus the book value of equity 

unique(df$country)
table(df$country, df$ctrygrp)
which(df$ctrygrp=="EUR")

#create country groups based on country

df <- df %>% mutate(., ctrygrp = with(., case_when(
  (Country %in% c("US", "JE")) ~ "USA",
  (Country %in% c("SE", "DE", "FR", "CH", "NL", "BE", "DK", "ES", "IT", "FI")) ~ 'EUR',
  (Country %in% c("KR", "JP", "TW")) ~ 'JKT',
  (TRUE ~ "OTH")
))) 

unique(df$ctrygrp)

# create report dummy variable - if no document, no report.
df <- df %>% mutate(report = as.factor(ifelse(is.na(doc_id), 0,1)))


df %>% filter(is.na(doc_id))
df <- df %>%
  mutate(across(PTI:cetr_5_s, as.numeric))

#make a boxplot of cash tax paid
df %>% 
  ggplot(aes(report, as.numeric(CTP))) +
  geom_boxplot()

#make a boxplot of  Cash ETR_5 >0
df %>% filter(Year %in% c(2016,2020) & cetr_5 >0 & cetr_5 <1) %>%
  ggplot(., aes(x=report, y=as.numeric(cetr_5), col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ Year)

#make a boxplot of scaled Cash ETR
df %>% filter(cetr_5_s < 2.5 & cetr_5_s > -2.5) %>%
  ggplot(., aes(x=report, y=cetr_5_s, col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ Year)




#make a boxplot of  BTD
df %>% filter(Year %in% c(2016,2020) & CETR_5 >0 & CETR_5 < 1) %>%
  ggplot(., aes(x=report, y=CETR_5, col = ctrygrp)) +
  geom_boxplot()

#make a boxplot of  SHELTER
df %>% filter(Year %in% c(2016,2020) & CETR_5 >0) %>%
  ggplot(., aes(x=report, y=log(SHELTER), col = ctrygrp)) +
  geom_boxplot()



#make a boxplot of pre_tax income
df %>% filter(Year %in% c(2016,2020)) %>%
  ggplot(., aes(x=report, y=PTI, col = ctrygrp)) +
  geom_boxplot()


#Unpaired Wilcoxon 
# Subset weight data before treatment
report <- subset(df,  report == 1, cetr_5_s,
                 drop = TRUE)
# subset weight data after treatment
noreport <- subset(df,  report == 0, cetr_5_s,
                drop = TRUE)
wilcox.test(report, noreport, alternative = "greater")


#Unpaired Wilcoxon 
# Subset weight data before treatment
report1 <- subset(df,  report == 1, CASH_TAX_PAID,
                 drop = TRUE)
# subset weight data after treatment
noreport1 <- subset(df,  report == 0, CASH_TAX_PAID,
                   drop = TRUE)
wilcox.test(report1, noreport1, alternative = "less")

data3 <- df %>%
 mutate(sdg = ifelse(is.na(sdg), 0, sdg)) 

data3 %>% filter(cetr_5 <1 & cetr_5 > -1) %>%
  ggplot(., aes(x=cetr_5, y=sdg, col = ctrygrp)) +
  geom_point() +
  facet_grid(. ~ Year)
 

r1 <- lm(data3, formula = cetr_5_s ~ sdg07 + sdg13 + sdg16 + ctrygrp)
summary(r1)

r2 <- glm(report ~ cetr_5_s + RDS + SIZE + LEV + ctrygrp, family = binomial, data = data3)
summary(r2)

summary(data3$BTD)

r3 <- lm(cetr_5_s ~ log(sdg01+1) + log(sdg02+1) + log(sdg03+1) + log(sdg04+1) + log(sdg05+1) + log(sdg06+1) + log(sdg07+1) + log(sdg08+1) + 
           +log(sdg09+1) + log(sdg10+1) + log(sdg11+1) + log(sdg12+1) + log(sdg13+1) + log(sdg14+1) + log(sdg15+1) + log(sdg16+1) + RDS  + SIZE + LEV +
            FOREIGN + Sector + ctrygrp, data = data3)
summary(r3)


# fit some PLM models on BTD
library(plm)

fixed1 <- plm(CETR_5 ~ sdg03 + sdg07 +sdg13 , data=data3, index=c("Year"), model="within")
summary(fixed1)

 data4 <- data3 %>% filter(as.integer(report)==1) %>% mutate(SDG = log(sdg03 + sdg08 + sdg13 + sdg16 +1))


fixed3 <- plm(CETR_scaled ~ log(sdg+1) + RDS + SIZE + LEV + 
                FOREIGN + ctrygrp, data=data3, index=c("Year"), model="within")
summary(fixed3)

fixed4 <- plm(cetr_5 ~ log(sdg01+1) + log(sdg02+1) + log(sdg03+1) + log(sdg04+1) + log(sdg05+1) + log(sdg06+1) + log(sdg07+1) + log(sdg08+1) + 
                +log(sdg09+1) + log(sdg10+1) + log(sdg11+1) + log(sdg12+1) + log(sdg13+1) + log(sdg14+1) + log(sdg15+1) + log(sdg16+1) + RDS + SIZE + LEV + 
                FOREIGN + ctrygrp + Sector, data=data3, index=c("Year"), model="within")
summary(fixed4)

fixed5 <- lm(CETR_5 ~ log(sdg01+1) + log(sdg02+1) + log(sdg03+1) + log(sdg04+1) + log(sdg05+1) + log(sdg06+1) + log(sdg07+1) + log(sdg08+1) + 
                +log(sdg09+1) + log(sdg10+1) + log(sdg11+1) + log(sdg12+1) + log(sdg13+1) + log(sdg14+1) + log(sdg15+1) + log(sdg16+1) + RDS + SIZE + LEV + 
                FOREIGN + ctrygrp + Sector, data=data3)
summary(fixed5)




data3 %>% filter(CETR_5 > -50 & CETR_scaled < 50 & Year %in% c(2016,2020)) %>%
  ggplot(., aes(x=log(sdg02+1), y=CETR_5, col = Sector)) +
  geom_point()  +
  facet_grid(ctrygrp ~Year) +
  coord_flip()

data4 %>% filter(CETR_5 > 0 & CETR_5 < 1 & Year %in% c(2016,2020)) %>%
  ggplot(., aes(x=ROA, y=CETR_5, col = Sector)) +
  geom_point() +
  facet_grid(. ~Year)


x <- df$CETR_5[df$report==1]
y <- df$CETR_5[df$report==0]
mean(x, na.rm = TRUE)
mean(y, na.rm = TRUE)

# run a t-test on the means
t.test(x, y = NULL,
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

test <- wilcox.test(x, y, alternative = "greater")
test


#make a boxplot of delta-scaled versus sector
df %>% filter(Year %in% c(2016,2020)) %>%
  ggplot(., aes(x=report, y=delta_scaled, col = Sector)) +
  geom_boxplot()
df %>% group_by(Sector, Year) %>%
  summarize(mean_delta = mean(delta_scaled, na.rm=TRUE),
            mean_ROA = mean(PRETAX_INCOME/ASSETS, na.rm =TRUE)) %>%
  arrange(mean_delta) %>%
  ggplot(aes(mean_ROA, mean_delta, col = Year)) +
  geom_point()

#make a plot of sdg mentioning versus sector
# make a plot of delta scaled versus sdg mentioning frequency
df %>% filter(Year %in% c(2016,2020) & ctrygrp != "OTH") %>% ggplot(aes(Sector, log(sdg+1))) +
  geom_boxplot() +
  facet_grid(Year ~ ctrygrp) +
  coord_flip()


#make a plot of sdg  versus sector

colors <- colorRampPalette(c("blue", "green", "yellow", "red"))(10)

df %>%  dplyr::select(Year, Sector, sdg01:sdg16, ctrygrp) %>%
  filter(Year %in% c(2016,2020)) %>% 
     pivot_longer(cols = sdg01:sdg16,
                 names_to = "SDG",
                 values_to = "count") %>%
    ggplot(aes(Sector, SDG)) +
  geom_tile(aes(fill = log(count)), na.rm = TRUE) +
  scale_fill_distiller(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.text.y=element_text(size=5)) + 
  coord_flip() +
  facet_grid(ctrygrp ~ Year)

# make a plot of CETR_scaled versus sdg mentioning frequency
df %>% filter(Year %in% c(2016,2020) & ctrygrp != "OTH") %>% 
  ggplot(aes(log(sdg+1), CETR_5, color = ctrygrp)) +
  geom_point() +
  facet_grid(report ~ Year)

df %>% filter(Year %in% c(2016,2020) & ctrygrp != "OTH" ) %>% 
  filter(CETR_scaled < 25 & CETR_scaled > -25) %>%
  ggplot(aes(report, CETR_5)) +
  geom_boxplot() +
  facet_grid(ctrygrp ~ Year)
summary(df$CETR_scaled)

df %>% filter(Year %in% c(2016,2020) & ctrygrp != "OTH" ) %>% 
  filter(CETR_scaled < 25 & CETR_scaled > -25) %>%
  ggplot(aes(log(sdg+1), CETR_scaled)) +
  geom_point() +
  facet_grid(ctrygrp ~ Year)

df_sdg <- df %>% filter(Year %in% c(2016,2020) & ctrygrp != "OTH" ) %>% 
  filter(CETR_scaled < 25 & CETR_scaled > -25) %>%
  mutate(SDG = case_when(
    sdg > 0 ~ 1,
    is.na(sdg)  ~ 0,
    TRUE ~ 0)) %>%
  select(SDG, sdg, CETR_scaled, CETR_5, ctrygrp, Year, report, SIZE, ROA, LEV, FOREIGN, sdg01:sdg16, RDS, Sector, Country) 
df_sdg %>%
  ggplot(aes(as.factor(SDG), CETR_5, color = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ Year)

x <- df_sdg$CETR_5[df_sdg$SDG==1]
y <- df_sdg$CETR_5[df_sdg$SDG==0]
mean(x, na.rm = TRUE)
mean(y, na.rm = TRUE)

# run a t-test on the means
t.test(x, y = NULL,
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

test <- wilcox.test(x, y, alternative = "greater")
test





df %>% ggplot(aes(CETR_scaled)) + geom_histogram(binwidth = 10)
df %>% ggplot(aes(CETR_scaled, CETR_5)) + geom_point()

df_sdg$sdg[is.na(df_sdg$sdg)] <-0
unique(df_sdg$sdg)
df_sdg %>% filter(Year %in% c(2016,2020) & ctrygrp != "OTH") %>% ggplot(aes(CETR_scaled, log(sdg+1), color = ctrygrp)) +
  geom_point() +
  facet_grid(report ~ Year)



fixed5 <- plm(CETR_5 ~  log(sdg+1)  + RDS +  ROA + SIZE + LEV + 
                FOREIGN + Country, data=df_sdg, index=c("Year"), model="within")
summary(fixed5)

library(broom)
df_sdg20 <- df_sdg %>% filter(Year == 2020)

fit <- lm(CETR_scaled ~  log(sdg+1) + report + RDS +  SIZE + LEV + 
     FOREIGN, data = df_sdg20)
summary(fit)



#US environmentally sensitive industries, including oil and gas extraction, mining (except oil and gas), support activities for mining, utilities, food manufacturing, beverage and tobacco product manufacturing, paper manufacturing, petroleum and coal products manufacturing, chemical manufacturing, and fabricated metal product manufacturing.
# International Review of Accounting, Banking & Finance . Summer2012, Vol. 4 Issue 2, p61-99. 39p

#make a heatmap of BTD versus Sector and ctrygrp

df_windsor <- df  %>%  dplyr::select(Year, Sector, ctrygrp, CETR_5, CETR_scaled, BTD, sdg, report, gc, gri, int) %>%
  filter(Year %in% c(2016,2020) & CETR_scaled > -25 & CETR_scaled < 25) %>% 
  group_by(Sector, ctrygrp, report) %>% summarize(mCETR_scaled = mean(CETR_scaled, na.rm=TRUE), Year = Year, report = report)

df_windsor %>%
  ggplot(aes(Sector, ctrygrp)) +
  geom_tile(aes(fill = mCETR_scaled)) +
  scale_fill_continuous(type = "viridis") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.text.y=element_text(size=5)) + 
  coord_flip() +
  facet_grid(report ~ Year)

df %>% filter(Year %in% c(2016,2020) & ctrygrp != "OTH") %>% ggplot(aes(CETR_5, SIZE, color = ctrygrp)) +
  geom_point() +
  facet_grid(. ~ Year)

library(readr)
library(tidyverse)
library(readxl)


# DATA LOADING AND WRANGLING

# import financial data
longlist_3  <- read_delim("longlist_3.csv", 
                                       delim = ";", 
                                       escape_double = FALSE, 
                                       na = c("", "NA", "#N/B"))

#df1 is voor regressies op alle jaren met alleen CETR en FTSE_ESG
df1 <- read_delim("longlist_3.csv", 
                   delim = ";", 
                   escape_double = FALSE, 
                   na = c("", "NA", "#N/B"))    %>%
    select(cetr_5, cetr_5_s, ASSETS, FTSE_ESG_sr, ISIN, Country, Sector, year) %>%
    mutate(., ctrygrp = with(., case_when(
    (Country %in% c("US", "JE")) ~ "USA",
    (Country %in% c("SE", "DE", "FR", "CH", "NL", "BE", "DK", "ES", "IT", "FI")) ~ 'EUR',
    (Country %in% c("KR", "JP", "TW")) ~ 'JKT',
    (TRUE ~ "OTH")
  )))
load("performance.Rdata")
df1 <- df1 %>% left_join(.,e, by = c("ISIN" = "ISIN", "year"="Year"))
save(df1, file = "df1.Rdata")

#==================================================================================



longlist_3 <- 
  inner_join(longlist_3, e, by = c( "year" = "Year", "ISIN" = "ISIN")) %>%
  distinct(ISIN, year, .keep_all = TRUE)

longlist <- longlist_3 %>% select(-Column1) %>%
  filter(year == 2016 | year == 2020) 


#logit conversion function to be used with "SHELTER", Wilson 2009. Is een logistic,
logit2prob = function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

#create new variables. 
df2 <- longlist %>% mutate(
                      LEV = LTD/ASSETS,
                      RDS = RD/ASSETS, #R&D expenses scaled by assets
                      SIZE = log(ASSETS),
                      FOREIGN = case_when(
                      PTI_FOREIGN > 0 ~ 1, #foreign pre-tax income
                        is.na(PTI_FOREIGN)  ~ 0,
                        TRUE ~ 0),
                      FOR_PERC = case_when(
                        PTI_FOREIGN > 0 ~ PTI_FOREIGN/PTI, #foreign pre-tax income
                        is.na(PTI_FOREIGN)  ~ 0,
                        TRUE ~ 0),
                      BTD = (PTI*str*0.01-CTP)/ASSETS #book-tax difference
                    
  )



# Load the SDG scores per SDG e.o. per ISIN from NLP-test
load("C:/Users/HW van der Waal/projects/NLP-test/outdata.Rdata")


#Sum the  SDG-scores of companies with more than 1 document, so that every company has only one entry.
#create a dummy for report (==1) so that companies with report are marked, 
#before being joined to companies without report.

out_temp <- outdata %>%
  mutate(report = ifelse(is.na(doc_id), 0, 1)) %>%
  group_by(company, year) %>%
  summarize(sdg01 = sum(sdg01),
            sdg02 = sum(sdg02),
            sdg03 = sum(sdg03),
            sdg04 = sum(sdg04),
            sdg05 = sum(sdg05),
            sdg06 = sum(sdg06),
            sdg07 = sum(sdg07),
            sdg08 = sum(sdg08),
            sdg09 = sum(sdg09),
            sdg10 = sum(sdg10),
            sdg11 = sum(sdg11),
            sdg12 = sum(sdg12),
            sdg13 = sum(sdg13),
            sdg14 = sum(sdg14),
            sdg15 = sum(sdg15),
            sdg16 = sum(sdg16),
            sdg17 = sum(sgd17),
            sdg = sum(sdg),
            gc = sum(gc),
            gri = sum(gri),
            int = sum(int), 
            report = sum(report),
            sdg_total = sum(sdg01:sdg17))
#join the resulting scores data frame on to the data2.
#check if SDG is mentioned
#out_temp$SDG_count <- rowSums(out_temp[,c(3:19)]>0)

out_temp <- out_temp %>% rowwise() %>% 
  mutate(s_themes = sum(c_across(sdg01:sdg17)>0)) %>%
  coalesce(sdg_total:s_themes)
          
#check if every company per year is only mentioned once
out_temp %>% group_by(company, year) %>% summarize(n = n()) %>% 
  arrange(desc(n))



length(unique(out_temp$company))

df <- df2 %>% left_join(.,out_temp, by = c("ISIN" = "company", "year"= "year"))

#create country groups based on country,# the last line of code replaces all NAs with zero

df <- df %>% mutate(., ctrygrp = with(., case_when(
  (Country %in% c("US", "JE")) ~ "USA",
  (Country %in% c("SE", "DE", "FR", "CH", "NL", "BE", "DK", "ES", "IT", "FI")) ~ 'EUR',
  (Country %in% c("KR", "JP", "TW")) ~ 'JKT',
  (TRUE ~ "OTH")
)),
mutate(., across(sdg01:int, ~ifelse(is.na(.x),0,.x)))) %>%
  mutate(report = ifelse(is.na(report),0, 1))
# and write the final data frame disk for future reference
write.csv(df, "df_table.txt")


# ANALYSIS
# Create new proxies for Tax Aggressiveness.

#The market value of assets is the book value of assets plus the difference 
#between market value of equity minus the book value of equity 

# create a report and SDG dummy variable - if no document-id, then there was no report.
# No SDG mentioning, SDG is 0, else 1

df <- df %>% 
   mutate(
    SDG = as.factor(case_when(
      sdg > 0 ~ 1,
      is.na(sdg)  ~ 0,
      TRUE ~ 0)),
    report = as.factor(case_when(
      report > 0 ~ 1,
      is.na(report)  ~ 0,
      TRUE ~ 0)),
    GC = as.factor(ifelse(gc > 0, 1, 0)),
    GRI = as.factor(ifelse(gri > 0, 1, 0)),
    INT = as.factor(ifelse(int> 0, 1, 0))
    )

# convert the factors to descriptive level names
      
df$SDG <- recode(df$SDG, "1" = "SDG", "0" = "noSDG")
df$report <- recode(df$report, "1" = "report", "0" = "no report")
df$TAP <- recode(df$TAP, "1" = "Tax Aggressive", "0" = "Not Tax Aggressive")
# and write the final data frame disk for future reference
write.csv(df, "df_tax.txt")
save(df, file = "df_tax.Rdata" )

#check how many different companies there are. It should be 300
length(unique(df$ISIN))

# Make a summary statistics table
library(vtable)
var.labs <- data.frame(var = c("SIZE", 
                               "PTI", "CTP","ROA", "PTI_FOREIGN", "cetr_5", "cetr_5_s", 'etr_5',
                               "etr_5_s","stat_rate",
                               "LEV", 
                               "TAP",  "report", "SDG"),
                       labels = c("Size (log(Assets))", 
                                   "Pre-tax Income", 
                                  "Cash Taxes Paid", "Return on Assets %",
                                  "Foreign Pre-tax Income",
                                  "5-year CETR", "scaled 5-year CETR",
                                  "5-year ETR", "scaled 5-year ETR",
                                  "Statutory Tax Rate", 
                                  "Leverage%",  
                                  "Tax Aggressiveness Prob.",
                                  "SR/IR report dummy", "SDG mention. dummy"
                  ))


df %>%  sumtable(., vars = var.labs$var, labels = var.labs)





#GET TO KNOW THE DATA BY MAKING GRAPHS
# CETR_5_S versus reporting characteristics

#make a boxplot of cash tax paid versus ESG-rating
df1 %>% mutate(cetr_w = Winsorize(cetr, minval = 0, maxval = 1, na.rm = TRUE)) %>%
  ggplot(aes(cetr_w, FTSE_ESG_sr, col = ctrygrp)) +
  geom_point() 



#H1 tax versus SR/IR
#make a boxplot of  Cash ETR_5 >0;  truncate data 
df %>% filter(year %in% c(2016,2020) & cetr_5 >0 & cetr_5 <1) %>%
  ggplot(., aes(x=report, y=cetr_5_s, col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ year)


#winsorize cetr-data

library(DescTools)


#make a boxplot of  Cash ETR_5 >0;  winsorize data 
df %>% filter(year %in% c(2016,2020)) %>%
  mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  ggplot(., aes(x=report, y=cetr_5_w, col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ year) +
  labs(x = "report",
       y = "scaled 5-year cash effective tax rate - 5yr CETR_s",
       col = "country group",
       title = NULL)

df %>% filter(year %in% c(2016,2020)) %>%
  mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  ggplot(., aes(x=INT, y=cetr_5_w, col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ year)

df %>% filter(year %in% c(2016,2020)) %>%
  mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  ggplot(., aes(x=GC, y=cetr_5_w, col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ year)




#H2 make boxplots of CETR_5_S versus SDG mentioning
#make a boxplot of scaled Cash ETR
df %>% filter(year %in% c(2016,2020)) %>%
  mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  ggplot(., aes(x=performance, y=cetr_5_w)) +
  geom_point() +
  facet_grid(. ~ year)

df %>% filter(year %in% c(2016,2020)) %>%
  mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  ggplot(., aes(x=log(sdg+1), y=cetr_5_w, col = ctrygrp)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE,aes(group=ctrygrp))

df %>% filter(year %in% c(2016,2020)) %>%
  mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  ggplot(., aes(x=s_themes, y=cetr_5_w)) +
  geom_point() +
    facet_grid(year ~ctrygrp)

df %>% mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  ggplot(., aes(x=ctrygrp, y=cetr_5_w)) +
  geom_boxplot() +
  facet_grid(. ~ year)

df %>% mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  select(ctrygrp, year, s_themes) %>%
  ggplot(., aes(x=ctrygrp, y=s_themes)) +
  geom_boxplot() +
  facet_grid(. ~ year)

df %>% mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  pivot_longer(cols = sdg01:sdg17, names_to = "Goal", values_to = "score") %>%
  filter(year ==2020) %>%
  ggplot(., aes(x=log(score+1), y=cetr_5_s, col = ctrygrp)) +
  geom_point(alpha = 0.5) +
  facet_wrap(. ~ Goal)

df %>% 
  pivot_longer(cols = sdg01:sdg17, names_to = "Goal", values_to = "score") %>%
  ggplot(., aes(x=log(score+1), y=FTSE_ESG_sr, col = ctrygrp)) +
  geom_point(alpha = 0.5) +
  facet_wrap(. ~ Goal)



df %>% mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  pivot_longer(cols = sdg01:sdg17, names_to = "Goal", values_to = "score") %>%
  filter(year ==2020) %>%
  ggplot(., aes(x=log(score+1), y=cetr_5_w, col = ctrygrp)) +
  geom_point(alpha = 0.5) +
  facet_wrap(. ~ Goal)



#make graph of cetr versus performance and SDG

  df %>% mutate(cetr_w = Winsorize(cetr, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  ggplot(., aes(x=performance, y=cetr_w)) +
  geom_point() +
  facet_grid(ctrygrp ~ year) +
    geom_smooth(type = "glm", formula = y ~x)



#Unpaired Wilcoxon 
# Subset  data before treatment
report <- subset(df,  report == "report", cetr_5_s,
                 drop = TRUE)
# subset  data after treatment
noreport <- subset(df,  report == "no report", select = cetr_5_s,
                drop = TRUE)
wilcox.test(report, noreport, alternative = "greater")


#Unpaired Wilcoxon 
# Subset  data before treatment

report1 <- subset(df,  report == "report", CTP,
                 drop = TRUE)
# subset  data after treatment
noreport1 <- subset(df,  report == "no report", CTP,
                   drop = TRUE)
wilcox.test(report1, noreport1, alternative = "greater")

#PERFORM SOME REGRESSION ANALYSES

#some regressions

# fit some PLM models on BTD
library(plm)
#check if df is balanced
df %>% is.pbalanced(index =c("year", "ISIN"))
#pdata <- unique(df, by = c("Year", "ISIN"))

#truncate data for cetr_5 > 0 and cetr_5 <0.5 (as is customary in other)
df_t <- df %>% mutate(cetr_5_w = Winsorize(cetr_5_s, probs = c(0.01, 0.99), na.rm = TRUE),
                      cetr = (CTP/PTI-str),
                      cetr_w = Winsorize(cetr, probs = c(0.01, 0.99), na.rm = TRUE))
# check if data set is balanced
df_t %>% is.pbalanced(index =c("year", "ISIN"))
#if required, balance truncated data
df_t_b <- df_t %>% make.pbalanced(index =c("year", "ISIN"), balance.type = "shared.times")
duplicated(df_t)

#SOME REGRESSIONS

form1a <- cetr_5_w ~ SIZE + FTSE_ESG_sr + performance + ctrygrp + factor(year)
ols1a <- lm(form1a, data = df_t)
summary(ols1a)

form1b <- cetr ~ SIZE + FTSE_ESG_sr + performance + LEV+ ROA + RDS + FOR_PERC + 
  LTD + ctrygrp + factor(year)
ols1b <- lm(form1b, data = df_t)
summary(ols1b)

#do a lagged regression
library(dynlm)

f_a <- dynlm(cetr ~ SIZE + FTSE_ESG_sr + L(performance,1:5) + 
               L(cetr,1:5) + ctrygrp + factor(year),
             data = df1_t)
summary(f_a)

form1 <- cetr_5_w ~ log(sdg+1) + SIZE + FTSE_ESG_sr + performance + ctrygrp + factor(year)

form2 <- cetr_5_w ~   log(sdg01+1) + log(sdg02+1) + log(sdg03+1) + log(sdg04+1) + 
  log(sdg05+1) + log(sdg06+1) + log(sdg07+1) + log(sdg08+1) + 
  +log(sdg09+1) + log(sdg10+1) + log(sdg11+1) + log(sdg12+1) + 
  log(sdg13+1) + log(sdg14+1) + log(sdg15+1) + log(sdg16+1) + 
  log(sdg17+1) + FTSE_ESG_sr + SIZE + performance + ctrygrp + factor(year)

form3 <- cetr ~ log(sdg+1) + SIZE + FTSE_ESG_sr + ctrygrp +  factor(year)


#test within and random effect and run Hausmann test

ols <- lm(form1, data = df_t)
ols2 <- lm(form2, data = df_t)
ols3 <- lm(form3, data = df_t)
wi <- plm(form1, data = df_t, index=c("year"), model = "within")
re <- plm(form2, data = df_t, index=c("year"), model = "random",  random.method = "walhus")
#F-test to see if LM is better than OLS (pooling) https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html
pFtest(wi, ols) # 
phtest(re, wi) #  If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects.
summary(ols)
summary(ols2)
summary(ols3)
summary(wi)
summary(re)


library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
cor.mat <- df_t %>% select(cetr_5_w, cetr_5_s, cetr_5, cetr, sdg, sdg_total,  FTSE_ESG_sr, 
                  performance, SIZE) %>%
  drop_na() %>%
  cor() %>%
  round(.,2)

print(cor.mat)
library(psych)
df_t %>% select(cetr_5_s, cetr_5, cetr, sdg, SIZE, FTSE_ESG_sr, performance, ctrygrp) %>%
  drop_na() %>%
  pairs() 

matrix <- df_t %>% select(cetr_5_s, cetr_5, cetr, sdg, SIZE, FTSE_ESG_sr, LEV, 
                           FOREIGN, performance) %>%
  drop_na()
mycorrelations <- psych::corr.test(matrix)
mycorrelations
print(mycorrelations$stars,quote=FALSE)


df_t %>% ggplot(aes(cetr_5_w, FTSE_ESG_sr)) +
  geom_jitter() +
  facet_grid(year ~ ctrygrp) +
  geom_smooth(method='lm', formula= y~x)
df_USA <- df_t %>% filter(ctrygrp == "USA")
df_EUR <- df_t %>% filter(ctrygrp == "EUR")
df_JKT <- df_t %>% filter(ctrygrp == "JKT")

ols_USA <-lm(form1, data = df_USA) 
ols_EUR <-lm(form1, data = df_EUR)
ols_JKT <-lm(form1, data = df_JKT)

summary(ols_USA)
summary(ols_EUR)
summary(ols_JKT)

ftsdg <- lm(  FTSE_ESG_sr ~ log(sdg+1), data = df)
summary(ftsdg)

ftsdg_full <- glm(FTSE_ESG_sr ~ log(sdg+1) + log(sdg01+1) + log(sdg02+1) + log(sdg03+1) + log(sdg04+1) + 
                     log(sdg05+1) + log(sdg06+1) + log(sdg07+1) + log(sdg08+1) + 
                     +log(sdg09+1) + log(sdg10+1) + log(sdg11+1) + log(sdg12+1) + 
                     log(sdg13+1) + log(sdg14+1) + log(sdg15+1) + log(sdg16+1) + 
                     log(sdg17+1) + log(sdg_total+1) + log(s_themes+1) + factor(report) + factor(GRI) +
                     factor(GC) + factor(INT) + ctrygrp + 
                     SIZE + factor(year), data = df)
summary(ftsdg_full)


fixed2 <- plm(form2, data=df_t, index=c("year"), model="within")
summary(fixed2)

random2 <- plm(form2, data=df_t, index=c("year"), model="random",
               random.method = "walhus")
summary(random2)
phtest(fixed2, random2)

#create some alternative tax avoidance measures using 10-year panel
form4 <- cetr_w ~ log(ASSETS) + LTD + RD + PTI_FOREIGN + ROA + FTSE_ESG_sr + performance +   Country +  year
form5 <- TD ~ log(ASSETS) + LTD + RD + PTI_FOREIGN + ROA + FTSE_ESG_sr + Country +  year
form6 <- BTD ~ log(ASSETS) + LTD + RD + PTI_FOREIGN + ROA + FTSE_ESG_sr + Country +  year
longlist_4 <- longlist_3 %>% mutate(., across(RD:PTI_FOREIGN, ~ifelse(is.na(.x),0,.x))) %>%
  drop_na() %>% mutate(cetr = CTP/PTI, TD = cetr-str, BTD = (CTP-PTI*str)/ASSETS) %>%
  distinct()
#using longlist_4, remove financial sector as by Zeng 2019
longlist_5 <- longlist_4 %>% filter(Sector != "FINANCIAL") %>% 
  mutate(cetr_w = Winsorize(cetr, minval = 0, maxval = 1, na.rm = TRUE),
         TD_w = Winsorize(cetr, probs = c(0.01, 0.99), na.rm = TRUE),
         BTD_w = Winsorize(cetr, probs = c(0.01, 0.99), na.rm = TRUE),
         RD_a = RD/ASSETS) %>%
  drop_na()

length(longlist_3$FTSE_ESG_sr[!is.na(longlist_3$FTSE_ESG_sr)])
random4_cetr <- plm(form4, data=longlist_5, index=c("year"), 
               effect = "individual",
               model="random", 
               print.level = 3,
               random.method = "nerlove")
summary(random4_cetr)

summary(longlist_5$performance)

longlist_5 %>% filter(cetr > 0 & cetr < 1) %>%
  ggplot(aes(cetr, FTSE_ESG_sr)) + geom_point() +
  geom_smooth()

random4_TD <- plm(form5, data=longlist_5, index=c("year", "Country"), 
                    effect = "twoways",
                    model="random", 
                    print.level = 3,
                    random.method = "walhus")
summary(random4_TD)


random4_BTD <- plm(form6, data=longlist_5, index=c("year", "Country"), 
                  effect = "twoways",
                  model="random", 
                  print.level = 3,
                  random.method = "walhus")
summary(random4_BTD)







ols4 <- lm(form4, data=longlist_4)
summary(ols4)



random4tb <- plm(form2, data=df_t_b, index=c("year"), 
               effect = "twoways",
               model="random", 
               print.level = 3,
               random.method = "walhus")
summary(random4tb)

pool <- plm(form1, data=df_t, model="pooling", index=c("year"), random.method = "walhus")
summary(pool)

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
plmtest(pool, type=c("bp"))
# conclusion OLS is better

ols <- lm(form1, data = df_t_b)
summary(ols)


library(pglm)

random6 <- pglm(form1 ,
               data=df_t, index=c("year"), 
               family = gaussian, 
               effect = "time",
               model="random", 
               method = "nr",
               print.level = 3)
summary(random6)

random7 <- pglm(form1, data=df_t, index=c("year"), 
                family = gaussian, 
                model="random", 
                effect = "twoways",
                method = "nr",
                print.level = 3)
summary(random7)

glm <- glm(form1, data=df_t,  
            family = gaussian 
            )
summary(glm)


#we draaien de DV en IV om
form3 <- SDG ~ cetr_5_w + SIZE + FTSE_ESG_sr + performance + factor(year)
random8 <- pglm(form3, data=df_t, index=c("year", "ctrygrp"), 
                family = binomial('probit'), 
                model="random", 
                effect = "twoways",
                method = "nr",
                print.level = 3)
summary(random8)

# Do some other analyses

#top scorers SDG02 per Sector
df %>% filter(cetr_5_s > -0.5 & cetr_5_s < 0.5 & Year == 2020) %>%
  pivot_longer(cols = sdg01:sdg17, names_to = "Goal", values_to = "score") %>%
  group_by(Sector, Goal) %>%
  summarize(refs = sum(log(score+1))) %>%
  pivot_wider(names_from = "Goal", values_from = "refs") 

df %>% filter(cetr_5_s > -0.5 & cetr_5_s < 0.5 & Year == 2020) %>%
  pivot_longer(cols = sdg01:sdg17, names_to = "Goal", values_to = "score") %>%
  group_by(Sector, Goal) %>%
  summarize(refs = mean(log(score+1))) %>%
  ggplot(aes(Sector, Goal)) +
  geom_tile(aes(fill = refs), na.rm = TRUE) +
  scale_fill_distiller(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.text.y=element_text(size=10))

#mean scores per sector of tax aggressiveness
newdat <- df %>% filter(cetr_5_s > -0.5 & cetr_5_s < 0.5 & (Year == 2016 | Year == 2020)) %>%
  group_by(Sector, Year) %>%
  summarize(cetr_5_s_mean = mean(cetr_5_s), cetr_5_s_median = median(cetr_5_s), n = n()) 

p<- newdat %>%
  ggplot(aes(Sector, factor(Year))) +
  geom_tile(aes(fill = -cetr_5_s_mean), na.rm = TRUE) +
  scale_fill_distiller(palette = "Spectral") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.text.y=element_text(size=10))
p + geom_text(data=newdat, aes(Sector, factor(Year),  
                               label=n), col="black")

# run a t-test on the means
t.test(x, y = NULL,
       alternative = c("greater"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95)

test <- wilcox.test(x, y, alternative = "greater")
test


#make a heatmap of BTD versus Sector and ctrygrp

df_windsor <- df  %>%  dplyr::select(Year, Sector, ctrygrp, cetr_5, cetr_5_s, sdg, report) %>%
  filter(Year %in% c(2016,2020) & cetr_5_s > -0.25 & cetr_5_s < 0.25) %>% 
  group_by(Sector, ctrygrp, report) %>% summarize(mCETR_scaled = mean(CETR_scaled, na.rm=TRUE), Year = Year, report = report)

df_t %>%
  ggplot(aes(Sector, ctrygrp)) +
  geom_tile(aes(fill = cetr_5_s)) +
  scale_fill_continuous(type = "viridis") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + 
  theme(axis.text.y=element_text(size=5)) + 
  coord_flip() +
  facet_grid(report ~ Year)


#make a decision tree
library(caret)
library(rpart)
library(rpart.plot)

#create a data partition
data <- df_t %>% select(cetr_5_s, SIZE, year, report,  ROA, sdg, mPerf, 
                          LEV, FOREIGN, ctrygrp,  SDG, sdg01:sdg17) %>%
  drop_na() %>% mutate(SDG = as.factor(as.numeric(SDG)),
                       TA = as.factor(ifelse(cetr_5_s < median(df_t$cetr_5_s), 1, 0)))
set.seed(1, sample.kind = "Rounding")
trainIndex <- createDataPartition(data$TA, p = .5, 
                                  list = FALSE, 
                                  times = 1)
summary(df_t)
#  mutate(SDG = as.factor(SDG))
train <- data[trainIndex,]
test <- data[-trainIndex,]

# train a decision tree for test
library(rpart)
set.seed(2, sample.kind = "Rounding")
model1 <- train(factor(TA) ~ SIZE +  report + SDG + mPerf + ctrygrp , data = train, 
                method = "rpart",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"),
                na.action=na.exclude)

y_hat1 <- predict(model1, newdata = test)
cfm_m1 <- confusionMatrix(y_hat1, factor(test$TA), dnn = c("Prediction", "Reference"))
cfm_m1$overall[1]
importance(model1)
print(cfm_m1)
summary.r(data3$TA)

#visualize the decision tree
summary(data$TA)
library(rpart.plot)
set.seed(3, sample.kind = "Rounding")
model2 <- rpart(cetr_5_s ~  SIZE + ctrygrp + year + sdg + FOREIGN + LEV + mPerf, data = train, 
                method = "anova", 
                control = rpart.control(cp = 0.01),
                na.action=na.exclude)
y_hat2 <- predict(model2, newdata = test, type = "class")

cfm_m2 <- confusionMatrix(y_hat2, test$cetr_5_s, dnn = c("Prediction", "Reference"),
                          na.action = na.pass)
length(y_hat2)
length(test$TA)
importance <- as.data.frame(model2$variable.importance)
plot(model2)
rpart.plot(model2, type = 2)
printcp(model2)

length(train$ctrygrp[train$ctrygrp=="USA"])
length(train$ctrygrp[train$ctrygrp=="EUR"])
length(train$ctrygrp[train$ctrygrp=="JKT"])


prp(model2, main = "3")

#random forest
library(randomForest)
unique(df$TAP)
set.seed(4, sample.kind = "Rounding")
ft = randomForest(cetr_5_s ~ SIZE + report + ctrygrp + SDG + sdg16 + sdg17 + FOREIGN + RDS + Year , 
                  data = train, 
                  importance = TRUE,
                  na.action=na.exclude)
ft
importance(ft)
y_hat3 <- predict(ft, newdata = test)
cm <- confusionMatrix(y_hat3, test$cetr_5_s, dnn = c("Prediction", "Reference"))
summary(ft)


#random forest
library(randomForest)
set.seed(4, sample.kind = "Rounding")

ft2 = randomForest(TAP2 ~ SIZE +  report + ctrygrp + LEV  + ROA +  FOREIGN +sdg, 
                   data = train, importance = TRUE)
ft2
importance(ft2)
y_hat <- predict(ft2, newdata = test)
cm <- confusionMatrix(y_hat, test$TAP2, dnn = c("Prediction", "Reference"))
print(cm)


## regressies op FTSE ESG en SDG
form7 <- FTSG_ESG_sr ~ log(sdg+1) 
model_sdg <- lm(formula = ftsdg_full, data = df)
summary(model_sdg)

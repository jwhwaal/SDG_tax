library(readr)
library(tidyverse)
library(readxl)


# DATA LOADING AND WRANGLING
#read the master financial data of the sample from the Factset Excel-sheet

data <- read_excel("sample_report.xlsx", 
                                    sheet = "sample_long") %>%
  filter(Year %in% c(2016, 2020))

#import statutory tax rates from Tax Foundation.org
taxrates_81_21 <- read_excel("taxrates_81_21.xlsx")
tr <- taxrates_81_21 %>% filter(iso_2 %in% unique(data$Country) & year %in% c(2016:2020)) %>%
  mutate(stat_rate=as.numeric(rate)) %>%
  select(iso_2, country, year, stat_rate)
unique(data$Country)

# import Cetr_5_scaled file calculated separately.
longlist_3 <- read_delim("longlist_3.csv", 
                         ";", escape_double = FALSE, 
                         col_types = cols(str = col_number(), 
                         PTI = col_number(), CTP = col_number(), 
                         cetr_5 = col_number(), diff = col_number(), 
                         cetr_5_s = col_number()), trim_ws = TRUE,
                         na =c("#N/B")) %>%
                         filter(year %in% c(2016, 2020))

#join the cetr_5_scaled data and the country/year statutory tax rates on the Factset data and 
data1 <- data %>% left_join(.,longlist_3, by = c("Year" = "year", "ISIN" = "ISIN", "Country" = "Country" )) %>%
  select(ISIN, Sector, Year, LTD, RD, PTI_FOREIGN, PTI, CTP, cetr_5, cetr_5_s, ASSETS, Country, ROA, MCAP) %>%
  mutate(LTD = as.numeric(LTD), RD = as.numeric(RD), PTI_FOREIGN = as.numeric(PTI_FOREIGN)) %>%
  left_join(.,tr, by = c("Country" = "iso_2", "Year" = "year")) %>%
  mutate(across(PTI:cetr_5_s, as.numeric))


#logit conversion function to be used with "SHELTER", Wilson 2009. Is een logistic,
logit2prob = function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}


#create new variables. TAP = Tax aggressiveness probability
data2 <- data1 %>% mutate(
                      LEV = LTD/ASSETS,
                      RDS = RD/ASSETS, #R&D expenses scaled by assets
                      SIZE = log(ASSETS),
                      FOREIGN = case_when(
                      PTI_FOREIGN > 1 ~ 1, #foreign pre-tax income
                        is.na(PTI_FOREIGN)  ~ 0,
                        TRUE ~ 0),
                      BTD = (PTI*stat_rate*0.01-CTP)/ASSETS, #book-tax difference
                      SHELTER = -4.30 + 6.63*BTD -1.72*LEV + 
  0.66*SIZE + 2.26*ROA + 1.62*FOREIGN + 1.56*RDS, #Wilson's Tax Shelter Probability
  TAP = as.factor(round(logit2prob(SHELTER),0))
  )
is.na(data2$TAP)

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
            report = sum(report))
#join the resulting scores data frame on to the data2.

out_temp %>% group_by(company, year)  %>% summarize(n = n()) %>% arrange(desc(n))



length(unique(out_temp$company))
length()
df <- data2 %>% left_join(.,out_temp, by = c("ISIN" = "company", "Year"= "year"))

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

#check how many different companies there are. It should be 300
length(unique(df$ISIN))

# Make a summary statistics table
library(vtable)
var.labs <- data.frame(var = c("SIZE", "PTI_FOREIGN",
                               "PTI", "CTP","ROA", "cetr_5", "cetr_5_s", 
                                  "stat_rate",
                               "LEV", "BTD",
                               "TAP",  "report", "SDG"),
                       labels = c("Size (log(Assets))", 
                                  "Foreign Pre-tax Income", "Pre-tax Income", 
                                  "Cash Taxes Paid", "Return on Assets %",
                                  "5-year CETR", "scaled 5-year CETR",
                                  "Statutory Tax Rate", 
                                  "Leverage%",  
                                  "Book-Tax Difference/Assets",
                                  "Tax Aggressiveness Prob.",
                                  "SR/IR report dummy", "SDG mention. dummy"
                  ))


df %>%  sumtable(., vars = var.labs$var, labels = var.labs)

#GET TO KNOW THE DATA BY MAKING GRAPHS
# CETR_5_S versus reporting characteristics

#make a boxplot of cash tax paid versus report/integrated report/GC-membership
df %>% 
  ggplot(aes(report, CTP)) +
  geom_boxplot()
#H1 tax versus SR/IR
#make a boxplot of  Cash ETR_5 >0;  winsorize data 
df %>% filter(Year %in% c(2016,2020) & cetr_5 >0 & cetr_5 <1) %>%
  ggplot(., aes(x=report, y=cetr_5_s, col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ Year)

df %>% filter(Year %in% c(2016,2020) & cetr_5 >0 & cetr_5 <0.5) %>%
  ggplot(., aes(x=INT, y=cetr_5_s, col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ Year)

df %>% filter(Year %in% c(2016,2020) & cetr_5 >0 & cetr_5 <0.5) %>%
  ggplot(., aes(x=GC, y=cetr_5_s, col = ctrygrp)) +
  geom_boxplot() +
  facet_grid(. ~ Year)


#H2 make boxplots of CETR_5_S versus SDG mentioning
#make a boxplot of scaled Cash ETR
df %>% filter(cetr_5 < 0.5 & cetr_5 > 0) %>%
  ggplot(., aes(x=SDG, y=cetr_5_s)) +
  geom_boxplot() +
  facet_grid(ctrygrp ~ Year)

df %>% filter(cetr_5_s < 0.25 & cetr_5_s > -0.25) %>%
  ggplot(., aes(x=log(sdg+1), y=cetr_5_s, col = ctrygrp)) +
  geom_point() +
  geom_smooth(method=lm, se=TRUE, fullrange=TRUE,aes(group=ctrygrp))


df %>% filter(cetr_5_s < 0.25 & cetr_5_s > -0.25) %>%
  ggplot(., aes(x=ctrygrp, y=cetr_5_s)) +
  geom_boxplot() +
  facet_grid(. ~ Year)

df %>% filter(cetr_5_s < 0.25 & cetr_5_s > -0.25) %>%
  pivot_longer(cols = sdg01:sdg17, names_to = "Goal", values_to = "score") %>%
  filter(Year ==2020) %>%
  ggplot(., aes(x=log(score+1), y=cetr_5_s, col = ctrygrp)) +
  geom_point(alpha = 0.5) +
  facet_wrap(. ~ Goal)

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
df %>% is.pbalanced(index =c("Year", "ISIN"))
#pdata <- unique(df, by = c("Year", "ISIN"))

#truncate data for cetr_5 > 0 and cetr_5 <0.5 (as is customary in other)
df_t <- df %>% filter(cetr_5 > 0 & cetr_5 < 0.5)
# check if data set is balanced
df_t %>% is.pbalanced(index =c("Year", "ISIN"))
#if required, balance truncated data
df_t_b <- df_t %>% make.pbalanced(index =c("Year", "ISIN"), balance.type = "shared.times")
duplicated(df_t)

#test within and random effect and run Hausmann test
form1 <- cetr_5_s ~ log(sdg+1) + RDS + SIZE + LEV + ROA + FOREIGN + ctrygrp
ols <- lm(cetr_5_s ~ log(sdg+1) + RDS + SIZE + LEV + ROA + FOREIGN + ctrygrp + factor(Year), data = df)
wi <- plm(form1, data = df, index=c("Year"), model = "within")
re <- plm(form1, data = df_t, index=c("Year"), model = "random",  random.method = "walhus")
#F-test to see if LM is better than OLS (pooling) https://rstudio-pubs-static.s3.amazonaws.com/372492_3e05f38dd3f248e89cdedd317d603b9a.html
pFtest(wi, ols) # 
phtest(re, wi) #  If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects.


form2 <- cetr_5_s ~ log(sdg01+1) + log(sdg02+1) + log(sdg03+1) + log(sdg04+1) + 
  log(sdg05+1) + log(sdg06+1) + log(sdg07+1) + log(sdg08+1) + 
  +log(sdg09+1) + log(sdg10+1) + log(sdg11+1) + log(sdg12+1) + 
  log(sdg13+1) + log(sdg14+1) + log(sdg15+1) + log(sdg16+1) + 
  log(sdg17+1) + RDS + SIZE + LEV + 
  FOREIGN + ROA + ctrygrp + factor(Year)


fixed2 <- plm(form2, data=df_t, index=c("Year"), model="within")
summary(fixed2)

random2 <- plm(form2, data=df_t, index=c("Year"), model="random",
               random.method = "walhus")
summary(random2)
phtest(fixed2, random2)

random4t <- plm(form2, data=df_t, index=c("Year"), 
               effect = "twoways",
               model="random", 
               print.level = 3,
               random.method = "walhus")
summary(random4t)


random4tb <- plm(form2, data=df_t_b, index=c("Year"), 
               effect = "twoways",
               model="random", 
               print.level = 3,
               random.method = "walhus")
summary(random4tb)

pool <- plm(form2, data=df_t_b, model="pooling", index=c("Year"), random.method = "walhus")
summary(pool)

# Breusch-Pagan Lagrange Multiplier for random effects. Null is no panel effect (i.e. OLS better).
plmtest(pool, type=c("bp"))
# conclusion OLS is better



library(pglm)

random6 <- pglm(form2 ,
               data=df_t, index=c("Year"), 
               family = gaussian, 
               effect = "time",
               model="random", 
               method = "nr",
               print.level = 3)
summary(random6)

random7 <- pglm(form2, data=df_t, index=c("Year"), 
                family = gaussian, 
                model="random", 
                effect = "twoways",
                method = "nr",
                print.level = 3)
summary(random7)

glm <- glm(form2, data=df_t_b,  
            family = gaussian 
            )
summary(glm)


#we draaien de DV en IV om
form3 <- SDG ~ cetr_5_s + RDS + SIZE + LEV + FOREIGN + ROA + ctrygrp + factor(Year)
random8 <- pglm(form3, data=df_t, index=c("Year", "ctrygrp"), 
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

df_windsor %>%
  ggplot(aes(Sector, ctrygrp)) +
  geom_tile(aes(fill = mCETR_scaled)) +
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
data <- df_t %>% select(cetr_5_s, SIZE, Year, report,  ROA, sdg,
                          LEV, FOREIGN, ctrygrp, Sector, SDG, RDS,  sdg01:sdg17) %>%
  drop_na() %>% mutate(SDG = as.factor(as.numeric(SDG)))
set.seed(1, sample.kind = "Rounding")
trainIndex <- createDataPartition(data$TAP, p = .5, 
                                  list = FALSE, 
                                  times = 1)

#  mutate(SDG = as.factor(SDG))
train <- data[trainIndex,]
test <- data[-trainIndex,]

# train a decision tree for test
library(rpart)
set.seed(2, sample.kind = "Rounding")
model1 <- train(cetr_5_s ~ SIZE +  report + SDG + ctrygrp , data = train, 
                method = "rpart",
                preProcess = c("center", "scale"),
                tuneLength = 10,
                trControl = trainControl(method = "cv"),
                na.action=na.exclude)

y_hat1 <- predict(model1, newdata = test)
cfm_m1 <- confusionMatrix(y_hat1, test$TAP2, dnn = c("Prediction", "Reference"))
cfm_m1$overall[1]
importance(model1)
print(cfm_m1)
summary.r(data3$TAP)

#visualize the decision tree
summary(data$TAP)
library(rpart.plot)
set.seed(3, sample.kind = "Rounding")
model2 <- rpart( cetr_5_s ~  SIZE + ctrygrp + Year + sdg01 + sdg02 +sdg03 +sdg04 + sdg05 +
                   sdg06 + sdg07 + sdg08 +sdg09 +sdg10 +sdg11 +sdg12 +sdg13 +sdg14 +sdg15 +
                   sdg16 + sdg17 + FOREIGN + LEV + RDS , data = train, 
                method = "anova", 
                control = rpart.control(cp = 0.01),
                na.action=na.exclude)
y_hat2 <- predict(model2, newdata = test)
cfm_m2 <- confusionMatrix(y_hat2, test$SDG, dnn = c("Prediction", "Reference"),
                          na.action = na.pass)
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

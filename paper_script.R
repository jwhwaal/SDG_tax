library(readr)
library(tidyverse)
library(readxl)
library(DescTools) #for winsorizing)


# ===========   DATA LOADING AND WRANGLING =====================================

# import financial data
longlist_3  <- read_delim("longlist_3.csv", 
                          delim = ";", 
                          escape_double = FALSE, 
                          na = c("", "NA", "#N/B"))

# calculate average PERFs per Factset_sectorand year
a <- longlist_3 %>% filter(!(Sector == "#N/B")) %>%
  group_by(Sector, year) %>% dplyr::summarise(mean_ROA = mean(ROA, na.rm=T), n= n(),
                                                      median_ROA = median(ROA, na.rm=T))
longlist_3 <- left_join(longlist_3, a, by = c("Sector" = "Sector", "year" = "year"))

#df1 is de data file voor regressies op alle jaren met alleen CETR en FTSE_ESG
df1 <- longlist_3 %>% select(-(cetr_5:diff_c), -etr_5, -(etr_5:etr_5_s)) %>%
    mutate(., ctrygrp = with(., case_when(
    (Country %in% c("US", "JE")) ~ "USA",
    (Country %in% c("SE", "DE", "FR", "CH", "NL", "BE", "DK", "ES", "IT", "FI")) ~ 'EUR',
    (Country %in% c("KR", "JP", "TW")) ~ 'JKT',
    (TRUE ~ "OTH")
  ))) %>%
  mutate(PERF = ROA-mean_ROA) %>%
    mutate(PERF = Winsorize(PERF, probs = c(0.01, 0.99), na.rm = TRUE),
           CETR = CTP/PTI,
           CETR_s = CETR - str,
           LEV = LTD/ASSETS,
           RDS = RD/ASSETS, #R&D expenses scaled by assets
           SIZE = log(ASSETS),
           FOREIGN = case_when(
             PTI_FOREIGN > 0 ~ 1, #foreign pre-tax income
             is.na(PTI_FOREIGN)  ~ 0,
             TRUE ~ 0),
           FOR = case_when(
             PTI_FOREIGN > 0 ~ PTI_FOREIGN/PTI, #foreign pre-tax income
             is.na(PTI_FOREIGN)  ~ 0,
             TRUE ~ 0),
           BTD = (PTI*str*0.01-CTP)/ASSETS) #book-tax difference)

# # Load the SDG scores per SDG e.o. per ISIN from NLP-test
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
  mutate(sdg_themes = sum(c_across(sdg01:sdg17)>0)) 
#check if every company per year is only mentioned once
out_temp %>% group_by(company, year) %>% summarize(n = n()) %>% 
  arrange(desc(n))

# join de SDG-data op de financiële data 
df <- df1 %>% 
  select(-Column1) %>%
  filter(year == 2016 | year == 2020)  %>% 
  left_join(.,out_temp, by = c("ISIN" = "company", "year"= "year"))

#create country groups based on country,# the last line of code replaces all NAs with zero

df <- df %>% mutate(., ctrygrp = with(., case_when(
  (Country %in% c("US", "JE")) ~ "USA",
  (Country %in% c("SE", "DE", "FR", "CH", "NL", "BE", "DK", "ES", "IT", "FI")) ~ 'EUR',
  (Country %in% c("KR", "JP", "TW")) ~ 'JKT',
  (TRUE ~ "OTH")
)),
mutate(., across(sdg01:int, ~ifelse(is.na(.x),0,.x)))) %>%
  mutate(report = ifelse(is.na(report),0, 1),
      SDG = as.factor(case_when(
      sdg > 0 ~ 1,
      is.na(sdg)  ~ 0,
      TRUE ~ 0)),
    REP = as.factor(case_when(
      report > 0 ~ 1,
      is.na(report)  ~ 0,
      TRUE ~ 0)),
    GC = as.factor(ifelse(gc > 0, 1, 0)),
    GRI = as.factor(ifelse(gri > 0, 1, 0)),
    INT = as.factor(ifelse(int> 0, 1, 0)),
    CETR = CTP/PTI,
    sdg_total = coalesce(sdg_total,0), #replaces NA's by 0 in this case
    sdg_themes = coalesce(sdg_themes,0)
  )

# convert the factors to descriptive level names

df$SDG <- recode(df$SDG, "1" = "SDG", "0" = "noSDG")
df$REP <- recode(df$REP, "1" = "report", "0" = "no report")
df$GC <- recode(df$GC, "1" = "GC-member", "0" = "no GC-member")
df$GRI <- recode(df$GRI, "1" = "GRI", "0" = "no GRI")
df$INT <- recode(df$INT, "1" = "<IR>", "0" = "no <IR>")

# and write the final data frame disk for future reference
write.csv(df, "df_tax.txt")
save(df, file = "df_tax.Rdata" )

# and write the final data frame disk for future reference
write.csv(df, "df_table.txt")

#============ END  ===========================================================

#=============================== DATA EXPLORATION SECTION ============================
# Summary statistics table
library(vtable)

# Make a summary statistics table
library(vtable)
var.labs1 <- data.frame(var = c("SIZE", 
                               "PTI", 
                               "CTP",
                               "ROA",
                               "PERF",
                               "LEV",
                               "CETR",
                               "CETR_s",
                               "CETR_3",
                               "str",
                               "FOR",
                               "ESG"),
                       labels = c("Size (log(Assets))", 
                                  "Pre-tax Income (PTI)", 
                                  "Cash Taxes Paid (CTP)", 
                                  "Return on Assets % (ROA)",
                                  "Relative Performance (PERF)",
                                  "Long Term Debt / Assets (LTD)",
                                  "Cash Effective Tax Rate (CETR)",
                                  "Cash Effective Tax Rate scaled (CETR_s)",
                                  "Cash Effective Tax Rate 3_yr scaled (CETR_3_s)",
                                  "Statutory Tax Rate (str)", 
                                  "Foreign Pre-tax Income Percentage (FOR)",
                                  "FTSE Russel ESG rating (ESG)"
                       ))


df1 %>%  sumtable(., vars = var.labs1$var, labels = var.labs1, out = "csv", file = "summary1.csv")


var.labs2 <- data.frame(var = c("REP",
                                "sdg01", 
                                "sdg02", 
                                "sdg03", 
                                "sdg04",
                                "sdg05", 
                                "sdg06",
                                "sdg07", 
                                "sdg08", 
                                "sdg09", 
                                "sdg10",
                                "sdg11", 
                                "sdg12",
                                "sdg13", 
                                "sdg14", 
                                "sdg15", 
                                "sdg16",
                                "sdg17", 
                                "sdg",
                                "sdg_total",
                                "sdg_themes",
                                "SDG",
                                "GC",
                                "GRI",
                                "INT"
                                 ),
                        labels = c("Sustainability report incl. integrated dummy (REP)",
                                   "SDG  1 No Poverty (sdg01)", 
                                   "SDG  2 Zero Hunger (sdg02)", 
                                   "SDG  3 Good Health & Wellbeing (sdg03)", 
                                   "SDG  4 Quality Education (sdg04)",
                                   "SDG  5 Gender Equality (sdg05)",
                                   "SDG  6 Clean Water & Sanitation (sdg06)",
                                   "SDG  7 Affordable & Clean Energy (sdg07)",
                                   "SDG  8 Decent Work & Econ. Growth (sdg08)",
                                   "SDG  9 Industry, Innovation & Infrastructure (sdg09)",
                                   "SDG 10 Reduced Inequalities (sdg10)",
                                   "SDG 11 Sustainable Cities & Communities (sdg11)",
                                   "SDG 12 Responsible Consumption & Production (sdg12)", 
                                   "SDG 13 Climate Action (sdg13)",
                                   "SDG 14 Life Below Water (sdg14)",
                                   "SDG 15 Life on Land (sdg15)",
                                   "SDG 16 Peace, Justice & Strong Institutions (sdg16)",
                                   "SDG 17 Partnership for the Goals (sdg17)",
                                   "SDG & 2030 Agenda mentioning (sdg)",
                                   "Total SDG1 - SDG 17 mentioning (sdg_total)",
                                   "SDG Themes (number) (sdg_themes)",
                                   "SDG mentioning dummy (SDG)",
                                   "Global Compact  dummy (GC)",
                                   "Global Reporting Initiative GRI dummy (GRI)",
                                   "Integrated Reporting IIRC dummy (INT)"
                                   
                        ))


df %>%  sumtable(., vars = var.labs2$var, labels = var.labs2, out = "csv", file = "summary2.csv")

#=========================================== END ======================================

# =================================== DATA VISUALISATION ==============================
#make a scatterplot of CETR_s versus cetr_5_s
df1 %>% mutate(CETR_w = Winsorize(CETR_s, minval = -1, maxval = 1, na.rm = TRUE),
              CETR_3_s = Winsorize(CETR_3, minval = -1, maxval = 1, na.rm = TRUE)) %>%
  ggplot(aes(CETR_w, CETR_3_s)) +
  geom_point() +
  geom_density_2d() +
  geom_abline(intercept = 0, slope = 1, size = 0.5) +
  xlab( "CETR_s") +
  ylab("CETR_3_s") 

#make a boxplot of REP versus cash effective tax rate scaled
df %>% mutate(CETR_w = Winsorize(CETR_3, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
  group_by(REP, ctrygrp) %>%
  mutate(count = n()) %>%
  ggplot(aes(REP, CETR_w)) +
  geom_boxplot() +
  xlab( "SR report") +
  ylab("Cash Effective Tax Rate 3_years scaled") +
  facet_grid(ctrygrp ~ .) +
  geom_label(aes(label= count , y = 0), # <- change this to move label up and down
             size = 4, position = position_dodge(width = 0.75))


#make a boxplot of SDG versus cash effective tax rate scaled
df %>% mutate(CETR_w = Winsorize(CETR_3, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
  group_by(SDG, ctrygrp) %>%
  mutate(count = n()) %>%
  ggplot(aes(SDG, CETR_w)) +
  geom_boxplot() +
  xlab( "SDG yes/no") +
  ylab("Cash Effective Tax Rate 3_years scaled") +
   facet_grid(ctrygrp ~ .) +
  geom_label(aes(label= count , y = 0), # <- change this to move label up and down
             size = 4, position = position_dodge(width = 0.75))


#make a scatterplot of sdg_total versus ESG-rating
df %>% 
  ggplot(aes(log(sdg+1), ESG)) +
  geom_point() +
  geom_density_2d() +
  xlab( "SDG mentioning (log)") +
  ylab("ESG-rating") +
  labs(col = "Country Group") +
  facet_wrap(. ~ ctrygrp)

#make a scatterplot of sdg_total versus CETR
df %>% mutate(CETR = Winsorize(CETR_3, probs = c(0.05, 0.95), na.rm = TRUE)) %>%
  ggplot(aes(log(sdg+1), CETR, col = ctrygrp)) +
  geom_point() +
  xlab( "SDG mentioning (log)") +
  ylab("CETR_3yrs scaled") +
  labs(col = "Country Group") +
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=1)
#make a boxplot of CETR versus ESG-rating 
#make a scatterplot of sdg_total versus ESG-rating
df1 %>% mutate(CETR = Winsorize(CETR_3, minval = -1, maxval = 1, na.rm = TRUE)) %>%
  ggplot(aes(ESG, CETR)) +
  geom_point() +
  geom_density_2d_filled() +
  xlab( "ESG rating") +
  ylab("CETR 3-yrs scaled") +
  labs(col = "Country Group") +
  geom_hline(yintercept=0, linetype="dashed", 
            color = "red", size=1) +
  facet_wrap(. ~ ctrygrp)


# make a scatterplot of SDG

df %>% 
  ggplot(aes(ESG, log(sdg+1))) +
  geom_point() + 
  facet_wrap(year ~ ctrygrp) +
  xlab( "ESG-rating") +
  ylab("SDG mentioning (log scale)") +
  geom_smooth(method = lm, formula = y ~ x, se = TRUE)


df %>% 
  ggplot(aes(ESG, sdg_themes)) +
  geom_point() + 
  facet_wrap(year ~ ctrygrp) +
  xlab( "ESG-rating") +
  ylab("SDG themes") +
  geom_smooth(method = lm, formula = y ~ x, se = TRUE) +
  geom_density_2d()

 #====================================END=======================================
#==============================CORRELATION MATRIX ==============================

library(corrplot)
source("http://www.sthda.com/upload/rquery_cormat.r")
#correlation matrix DF1 
cor.mat <- df1 %>% 
  select(SIZE, PTI, CTP, ROA, PERF, LTD, CETR, BTD, str, FOR, ESG) %>%
  drop_na() %>%
  cor() %>%
  round(.,2)

print(cor.mat)
library(psych)
#correlation matrix for financial dataset
mycorrelations1 <- df1 %>% 
  mutate(CETR_s = Winsorize(CETR_s, probs = c(0.01, 0.99), na.rm = TRUE),
         CETR_3_s = Winsorize(CETR_3, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  select(SIZE, PTI, CTP, ROA, PERF, LTD, CETR_s, CETR_3_s, str, FOR, ESG) %>%
  psych::corr.test(.)

print(mycorrelations1$stars,quote=FALSE)
cormat1 <- as.data.frame(mycorrelations1$stars)
write_csv(as.data.frame(mycorrelations1$stars), file = "cormat1.csv")

#correlation matrix for full dataset
mycorrelations2 <- df %>% 
  mutate(CETR = Winsorize(CETR, probs = c(0.01, 0.99), na.rm = TRUE),
         CETR_3_s = Winsorize(CETR_3, probs = c(0.01, 0.99), na.rm = TRUE),
         REP = as.numeric(REP),
         SDG = as.numeric(SDG),
         GC = as.numeric(GC),
         GRI = as.numeric(GRI),
         INT = as.numeric(INT)) %>%
  select(SIZE, PTI, CTP, ROA, PERF, LTD, CETR, CETR_3_s, str, FOR, ESG, REP, sdg01:INT) %>%
  psych::corr.test(.)

df %>% 
  mutate(CETR = Winsorize(CETR, probs = c(0.01, 0.99), na.rm = TRUE)) %>%
  select(CETR, BTD, SIZE, ROA, LEV, LTD, PERF, FOR, ESG) %>% lowerCor 

print(mycorrelations2$stars,quote=FALSE)
cormat2 <- as.data.frame(mycorrelations2$stars)
write_csv(as.data.frame(mycorrelations2$stars), file = "cormat2.csv")
########################################################################################

## correlation matrix in paper

matrix <- df %>% 
  mutate(CETR = Winsorize(CETR, probs = c(0.01, 0.99), na.rm = TRUE),
         CETR_s = Winsorize(CETR_s, probs = c(0.01, 0.99), na.rm = TRUE),
         CETR_3_s = Winsorize(CETR_3, probs = c(0.01, 0.99), na.rm = TRUE),
         REP = as.numeric(REP),
         SDG = as.numeric(SDG),
         GC = as.numeric(GC),
         GRI = as.numeric(GRI),
         INT = as.numeric(INT),
         sdg_total = log(sdg_total+1),
         sdg = log(sdg+1)) %>%
  select(year, SIZE, ROA, PERF, LTD, CETR, CETR_s, CETR_3_s, str, FOR, ESG, REP, 
         sdg_total, sdg:INT, -int, -gri, -gc, -report, -SDG) 

cormat <- matrix %>% 
  cor(use = "complete.obs") 
corrplot(cormat)
# mat : is a matrix of data
# ... : further arguments to pass to the native R cor.test function
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}
# matrix of the p-value of the correlation
p.mat <- cor.mtest(matrix)
head(p.mat[, 1:5])

# dit is de correlatiematrix om af te drukken
corrplot(cormat, p.mat = p.mat, sig.level = 0.05, method = "color",  order = "hclust")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(cormat, method="color", col=col(200),  
         type="upper", order="AOE", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=45, #Text label color and rotation
         # Combine with significance
         p.mat = p.mat, sig.level = 0.05, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE ,
         number.cex = 0.7,
         tl.cex = 1.0
)
########################################################################################

#correlation matrix for SDG narrow  dataset
mycorrelations2 <- df %>% 
  mutate(CETR = Winsorize(CETR, minval = 0, maxval = 1, na.rm = TRUE)) %>%
  select(CETR_5_s, CETR_5, CETR, BTD, SIZE, ROA, FOR, ESG, 
         sdg, sdg_total, sdg13, sdg_themes, SDG, GC, GRI, INT, REP) %>%
  mutate(sdg = log(sdg+1), sdg_themes = log(sdg_themes+1), sdg_total=log(sdg_total+1),
         sdg13 = log(sdg13+1), SDG = as.integer(SDG), GC = as.integer(GC),
         GRI = as.integer(GRI), INT = as.integer(INT), REP = as.integer(REP)) %>%
  psych::corr.test(.)

print(mycorrelations2$stars,quote=FALSE)
cormat <- as.data.frame(mycorrelations2$stars)
write_csv(as.data.frame(mycorrelations2$stars), file = "cormat2.csv")

#=========================================== END ===============================

#=========================================== REGRESSIONS =======================
# fit some PLM models 
library(plm)
#check if data frames are balanced
df %>% is.pbalanced(index =c("year", "ISIN"))
df1 %>% is.pbalanced(index =c("year", "ISIN"))


#first stage: regression of CETR versus ESG-PERF on DF1 (3000 observations)
form1a <- CETR_w ~ SIZE + ESG + ROA + PERF + LTD + LEV + FOR + FOREIGN + ctrygrp + factor(year)
form1b <- CETR_3 ~ SIZE + ESG + ROA + PERF + LTD + LEV + FOR + FOREIGN + ctrygrp + factor(year)
form1c <- CETR_3 ~ SIZE + ESG + ROA + PERF + LTD + LEV + FOR + FOREIGN + ctrygrp +REP + GC + GRI + INT + SDG +factor(year)
#winsorize data 
df1_1 <- df1 %>% mutate(CETR_w = Winsorize(CETR_s, minval = -0.5, maxval = 0.5, na.rm = TRUE))
df1_2 <- df1 %>% mutate(CETR_3_s = Winsorize(CETR_3, probs = c(0.01, 0.99), na.rm = TRUE))
df_w <- df %>% mutate(CETR_3_s = Winsorize(CETR_3, probs = c(0.01, 0.99), na.rm = TRUE))
                      
#SOME REGRESSIONS
model1.pooling <- lm(form1a, data = df1_1)
summary(model1.pooling)

model1b.pooling <- lm(form1b, data = df1_2)
summary(model1b.pooling)

model1c.pooling <- lm(form1c, data = df_w)
summary(model1c.pooling)


model2a.pool <- plm(form1a, data=df1_w, model="pooling", index=c("year"), random.method = "walhus")
summary(model2a.pool)

model2b.pool <- plm(form1b, data=df1_w, model="pooling", index=c("year"), random.method = "walhus")
summary(model2b.pool)

#is identiek aan gewone LM

model.random <- plm(form1b, data=df1_w, model="random", effect = "individual", 
                    index=c("year"), random.method = "walhus")
summary(model.random)
# we zien geen wezenlijke verschillen, dus kunnen we gewoon een pooled OLS doen.

# Op de kleine dataset, het verband tussen SDG en ESG vaststellen.
df_w <- df %>% 
  mutate(CETR_s = Winsorize(CETR_s, probs = c(0.01,0.99), na.rm = TRUE))

# een klein model om ESG-rating met SDG-variabelen in verband te brengen
form2 <- ESG ~ log(sdg+1) + log(sdg_total+1) + log(sdg_themes+1) +  ctrygrp + 
   SIZE + factor(year)
model.sdg<- lm(form2, data = df)
summary(model.sdg)

# een groter model dat ook andere variabelen heeft, die sterker correlerenn
form3 <- ESG ~ log(sdg+1) + log(sdg_themes+1) +  ctrygrp + 
  SIZE + SDG + GC + GRI + INT + REP + factor(year)
model.sdg2<- lm(form3, data = df)
summary(model.sdg2)

model.sdg2b<- lm(log(sdg+1)  ~ ctrygrp + 
                   SIZE + CETR_s + BTD + GC + GRI + INT  + factor(year), 
                 data = df_w)
summary(model.sdg2b)

model.sdg2c<- lm(log(sdg_total+1)  ~ ctrygrp + 
                   SIZE + CETR_s + BTD + GC + GRI + INT + factor(year), 
                 data = df_w)
summary(model.sdg2c)

model.sdg2d<- lm(s_themes  ~ ctrygrp + 
                   SIZE + CETR_s + BTD + GC + GRI + INT + factor(year), 
                 data = df_w)
summary(model.sdg2d)






form4 <- BTD ~ log(sdg+1) + log(sdg_total+1) + log(sdg_themes+1) + ESG + 
  PERF +  ctrygrp +
  SIZE + ROA + LEV + LTD + SDG + GC + GRI + INT + REP + factor(year)
model.sdg4<- lm(form4, data = df_w)
summary(model.sdg4)

#is more SDG mentioning in disclosure related to better ESG-ratings?

form5 <- CETR_s ~ log(sdg+1) + log(sdg_total+1) + log(sdg_themes+1) + ESG + 
  PERF +  ctrygrp + 
  SIZE + ROA + LEV + LTD + SDG + GC + GRI + INT + REP + factor(year)
model.sdg5<- lm(form5, data = df_w)
summary(model.sdg5)


form6 <- ESG ~  REP + log(sdg01+1) + log(sdg02+1) + log(sdg03+1) + log(sdg04+1) + 
  log(sdg05+1) + log(sdg06+1) + log(sdg07+1) + log(sdg08+1) + 
  +log(sdg09+1) + log(sdg10+1) + log(sdg11+1) + log(sdg12+1) + 
  log(sdg13+1) + log(sdg14+1) + log(sdg15+1) + log(sdg16+1) + 
  log(sdg17+1) + log(sdg+1) +log(sdg_total+1) + SDG + sdg_themes + GC + GRI + INT

model.sdg6<- lm(form6, data = df_w)
summary(model.sdg6)

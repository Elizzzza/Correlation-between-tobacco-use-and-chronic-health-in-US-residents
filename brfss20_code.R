#install.packages("rio")
#install_formats()
#install.packages("survey")
#install.packages("srvyr")
library(rio)
library(tidyverse)
library(survey)
library(srvyr)

## Import the data file
llcp2020 <- import("LLCP2020.XPT")

## Inspect your file 
head(llcp2020)
str(llcp2020)
# or glimpse(llcp2020)

## View column names 
colnames(llcp2020)
# or names(llcp2017)

## Export the file as a csv 
export(llcp2020, "llcp2020.csv")

## Re-import data file in csv
llcp2020 <- read.csv("llcp2020.csv", header = TRUE)

## Return back only columns with 100% missing data 
drop_v <- which(colSums(is.na(llcp2020)) == nrow(llcp2020))

## Drop the drop_v columns from data file and rename the data file
llcp2020drop_v <- llcp2020[, -drop_v]

## Subset data file with variables of interest
subset <- c("X_PSU", "X_LLCPWT", "X_STSTR", "LASTDEN4", "RMVTETH4", 
                   "SMOKE100", "CVDINFR4", "CVDCRHD4", "CVDSTRK3", "ASTHMA3", 
                   "CHCSCNCR", "CHCOCNCR", "CHCCOPD2", "HAVARTH4", "CHCKDNY2", 
                   "DIABETE4", "X_AGEG5YR", "SEXVAR", "X_IMPRACE", "INCOME2", 
                   "HLTHPLN1", "X_RFBMI5", "EDUCA")
llcp20_df <- llcp2020[, subset]
str(llcp20_df)

llcp20_df$LASTDEN4 <- as.factor(recode(llcp20_df$LASTDEN4, '1' = 'ref', 
                                        .default = 'com', '9' = 'NA', .missing = 'NA'))
llcp20_df$RMVTETH4 <- as.factor(recode(llcp20_df$RMVTETH4, '1' = 'ref', '8' = 'ref',
                                        .default = 'com', '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$SMOKE100 <- as.factor(recode(llcp20_df$SMOKE100, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$CVDINFR4 <- as.factor(recode(llcp20_df$CVDINFR4, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$CVDCRHD4 <- as.factor(recode(llcp20_df$CVDCRHD4, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$CVDSTRK3 <- as.factor(recode(llcp20_df$CVDSTRK3, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$ASTHMA3 <- as.factor(recode(llcp20_df$ASTHMA3, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$CHCSCNCR <- as.factor(recode(llcp20_df$CHCSCNCR, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$CHCOCNCR <- as.factor(recode(llcp20_df$CHCOCNCR, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$CHCCOPD2 <- as.factor(recode(llcp20_df$CHCCOPD2, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$HAVARTH4 <- as.factor(recode(llcp20_df$HAVARTH4, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$CHCKDNY2 <- as.factor(recode(llcp20_df$CHCKDNY2, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$DIABETE4 <- as.factor(recode(llcp20_df$DIABETE4, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$SEXVAR <- as.factor(recode(llcp20_df$SEXVAR, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$INCOME2 <- as.factor(recode(llcp20_df$INCOME2, '7' = 'ref','8' = 'ref',
                                       .default = 'com','77' = 'NA', '99' = 'NA', .missing = 'NA'))
llcp20_df$HLTHPLN1 <- as.factor(recode(llcp20_df$HLTHPLN1, '1' = 'ref',.default = 'com', 
                                        '7' = 'NA', '9' = 'NA', .missing = 'NA'))
llcp20_df$X_RFBMI5 <- as.factor(recode(llcp20_df$X_RFBMI5, '1' = 'ref',.default = 'com', 
                                        '9' = 'NA', .missing = 'NA'))
llcp20_df$X_IMPRACE <- as.factor(recode(llcp20_df$X_IMPRACE, '1' = 'White, non-Hispanic', 
                                        '2' = 'Black, non-Hispanic', '3' = 'Asian, non-Hispanic',
                                        '4' = 'American Indian/ Alaskan native, non-Hispanic', 
                                        '5' = 'Hispanic', '6' = 'other race, non-Hispanic',
                                        .missing = 'NA'))
llcp20_df$EDUCA <- as.factor(recode(llcp20_df$EDUCA, '1' = 'little/ none education', 
                                         '2' = 'little/ none education', '3' = 'some education',
                                         '4' = 'some education', '5' = 'advanced education', 
                                         '6' = 'advanced education', '9' = 'NA',
                                         .missing = 'NA'))
llcp20_df$X_AGEG5YR <- as.factor(recode(llcp20_df$X_AGEG5YR, '1' = 'com', '2' = 'com', 
                                         '3' = 'com', '4' = 'com', '5' = 'com', 
                                         '6' = 'com', '7' = 'com', '8' = 'com', 
                                         '9' = 'com', '10' = 'ref', '11' = 'ref',
                                         '12' = 'ref', '13'= 'ref', '14' = 'NA'))
llcp20_ad <- llcp20_df %>% 
        mutate(missing = ifelse(LASTDEN4 == "NA" | RMVTETH4 == "NA" | CVDINFR4 == "NA" | CVDCRHD4 == "NA" | CVDSTRK3 == "NA" 
                                | ASTHMA3 == "NA" | CHCSCNCR == "NA" | CHCOCNCR == "NA"| CHCCOPD2 == "NA" | HAVARTH4 == "NA"  
                                | CHCKDNY2 == "NA" | DIABETE4 == "NA" | SMOKE100 == "NA" | X_AGEG5YR == "NA"| SEXVAR == "NA" 
                                | X_IMPRACE == "NA"| INCOME2 == "NA"| HLTHPLN1 == "NA"| X_RFBMI5 == "NA"| EDUCA == "NA", 1 , 0)) %>% 
        filter(missing == 0) %>% 
        select(-missing) %>%                                   
        mutate(oral_health = ifelse(LASTDEN4 == "ref" & RMVTETH4 == "ref", 1, 0)) %>% 
        mutate(chronic_health = ifelse(CVDCRHD4 == 1 & CVDSTRK3 == 0
                                       & ASTHMA3 == 0 & CHCSCNCR == 0 & CHCOCNCR == 0
                                       & CHCCOPD2 == 0 & HAVARTH4 == 0 & CHCKDNY2 == 0 
                                       & DIABETE4 == 0, 1 ,0)) %>% 
        mutate(oral_health = as.factor(ifelse(LASTDEN4 == "ref" & RMVTETH4 == "ref", 1 ,0))) %>% 
        mutate(chronic_health = as.factor(ifelse(CVDCRHD4 == "com" & CVDSTRK3 == "com" & ASTHMA3 == "com"
                                       & CHCOCNCR == "com" & CHCCOPD2 == "com" & CHCKDNY2 == "com" 
                                       & DIABETE4 == "com", 1 ,0))) %>% 
        mutate(oral.health.indicator = ifelse(oral_health == 1, "good", "poor")) %>%  
        mutate(chronic.health.indicator = ifelse(chronic_health == 1, "good", "poor")) %>%  
        mutate(sex = ifelse(SEXVAR == "ref", "M", "F"))

## EDA for missing data
llcp20_miss <- llcp2020[, subset]
llcp20_miss <- llcp20_miss %>% 
        mutate(LASTDEN4 = as.numeric(recode(LASTDEN4, '1' = '1', .default = '0', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(RMVTETH4 = as.numeric(recode(RMVTETH4, '1' = '1', '8' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(SMOKE100 = as.numeric(recode(SMOKE100, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(CVDINFR4 = as.numeric(recode(CVDINFR4, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(CVDCRHD4 = as.numeric(recode(CVDCRHD4, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(CVDSTRK3 = as.numeric(recode(CVDSTRK3, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(ASTHMA3 = as.numeric(recode(ASTHMA3, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(CHCSCNCR = as.numeric(recode(CHCSCNCR, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(CHCOCNCR = as.numeric(recode(CHCOCNCR, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(CHCCOPD2 = as.numeric(recode(CHCCOPD2, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(HAVARTH4 = as.numeric(recode(HAVARTH4, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(CHCKDNY2 = as.numeric(recode(CHCKDNY2, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(DIABETE4 = as.numeric(recode(DIABETE4, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(SEXVAR = as.numeric(recode(SEXVAR, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(INCOME2 = as.numeric(recode(INCOME2, '7' = '1','8' = '1', .default = '0','77' = 'NA', '99' = 'NA', .missing = 'NA'))) %>% 
        mutate(HLTHPLN1 = as.numeric(recode(HLTHPLN1, '1' = '1', .default = '0', '7' = 'NA', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(X_RFBMI5 = as.numeric(recode(X_RFBMI5, '1' = '1', .default = '0', '9' = 'NA', .missing = 'NA'))) %>% 
        mutate(X_IMPRACE = recode(X_IMPRACE, '1' = 'White, non-Hispanic', '2' = 'Black, non-Hispanic', '3' = 'Asian, non-Hispanic',
                                        '4' = 'American Indian/ Alaskan native, non-Hispanic', 
                                        '5' = 'Hispanic', '6' = 'other race, non-Hispanic',
                                        .missing = 'NA')) %>% 
        mutate(EDUCA = recode(EDUCA, '1' = 'little/ none education', '2' = 'little/ none education', '3' = 'some education',
                              '4' = 'some education', '5' = 'advanced education', '6' = 'advanced education', 
                              '9' = 'NA', .missing = 'NA')) %>% 
        mutate(X_AGEG5YR = as.numeric(recode(X_AGEG5YR, '1' = '0', '2' = '0','3' = '0', '4' = '0', '5' = '0','6' = '0', '7' = '0', 
                                  '8' = '0', '9' = '0', '10' = '1', '11' = '1', '12' = '1', '13'= '1', '14' = 'NA')))
## Number of missing data by variable
missing.values <- llcp20_miss %>%
        gather(key = "key", value = "val") %>%
        mutate(is.missing = is.na(val)) %>%
        group_by(key, is.missing) %>%
        summarise(num.missing = n()) %>%
        filter(is.missing==T) %>%
        select(-is.missing) %>%
        arrange(desc(num.missing)) 

## Histogram of # of missing data
missing.values %>%
        ggplot() +
        geom_bar(aes(x=key, y=num.missing), stat = 'identity') +
        labs(x='variable', y="number of missing values", title='Number of missing values') +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Percentage of missing data by variable
missing.values <- llcp20_miss %>%
        gather(key = "key", value = "val") %>%
        mutate(isna = is.na(val)) %>%
        group_by(key) %>%
        mutate(total = n()) %>%
        group_by(key, total, isna) %>%
        summarise(num.isna = n()) %>%
        mutate(pct = num.isna / total * 100)

levels <- (missing.values  %>% filter(isna == T) %>% arrange(desc(pct)))$key

## Histogram of percentage of missing data
percentage.plot <- missing.values %>%
        ggplot() +
        geom_bar(aes(x = reorder(key, desc(pct)), 
                     y = pct, fill=isna), 
                 stat = 'identity', alpha=0.8) +
        scale_x_discrete(limits = levels) +
        scale_fill_manual(name = "", 
                          values = c('steelblue', 'tomato3'), labels = c("Present", "Missing")) +
        coord_flip() +
        labs(title = "Percentage of missing values", x =
                     'Variable', y = "% of missing values")

percentage.plot

## Histogram of smoking status by sex
llcp20_ad %>% mutate(smoking = ifelse(SMOKE100 == "ref", "smoker", "non-smoker")) %>% 
        ggplot(aes(x = smoking, fill = sex)) +
        geom_bar(stat = "count") +
        xlab("smoking status") +
        ylab("# of observations")
        theme_bw()
        
## Histogram of smoking status by race
llcp20_ad %>% mutate(smoking = ifelse(SMOKE100 == "ref", "smoker", "non-smoker")) %>% 
        ggplot(aes(x = smoking, fill = X_IMPRACE)) +
        geom_bar(stat = "count") +
        xlab("smoking status") +
        ylab("# of observations") +
        theme_bw()

## Histogram of oral health by sex
llcp20_ad %>% ggplot(aes(x = oral.health.indicator, fill = sex)) +
        geom_bar(stat = "count") +
        xlab("oral health status") +
        ylab("# of observations") +
        theme_bw()

## Histogram of oral health by race
llcp20_ad %>% ggplot(aes(x = oral.health.indicator, fill = X_IMPRACE)) +
        geom_bar(stat = "count") +
        xlab("oral health status") +
        ylab("# of observations") +
        theme_bw()

## Histogram of chronic health by sex
llcp20_ad %>% ggplot(aes(x = chronic.health.indicator, fill = sex)) +
        geom_bar(stat = "count") +
        xlab("chronic health status") +
        ylab("# of observations") +
        theme_bw()

## Histogram of chronic health by race
llcp20_ad %>% ggplot(aes(x = chronic.health.indicator, fill = X_IMPRACE)) +
        geom_bar(stat = "count") +
        xlab("Chronic health status") +
        ylab("# of observations") +
        theme_bw()

## Set the survey design
options(survey.adjust.domain.lonely=TRUE)
options(survey.lonely.psu="adjust")

llcp20_dsgn <- svydesign(id = ~1, strata = ~X_STSTR, weights = ~X_LLCPWT, 
                         data = llcp20_ad)

## caluate weighted prevelance and CI of sex 
svymean(~sex, llcp20_dsgn)
confint(svymean(~sex, llcp20_dsgn))

## calculate weighted prevelance and CI of oral health by sex
prev_oh_sex <- svyby(~oral_health, ~sex, llcp20_dsgn, svymean) 
prev_oh_sex <- as.data.frame(prev_oh_sex)        

## calculate weighted prevelance and CI of chronic health by sex
prev_ch_sex <- svyby(~chronic_health, ~sex, llcp20_dsgn, svymean)
prev_ch_sex <- as.data.frame(prev_ch_sex)  

## calculate weighted prevelance and CI of smoking by sex
prev_sm_sex <- svyby(~SMOKE100, ~sex, llcp20_dsgn, svymean)
prev_sm_sex <- as.data.frame(prev_sm_sex) 

prev_table <- cbind(prev_sm_sex, prev_oh_sex, prev_ch_sex) 
prev_table <- prev_table[,c(1,2,4,9,10,14,15)] %>% 
              rename(SMOKE100_1 = SMOKE100ref) %>% 
              rename(SMOKE100_0 = SMOKE100com) 


## calculate weighted prevalence and CI for all variables
prev <- as.data.frame((svymean(reformulate(names(llcp20_ad)), llcp20_dsgn)))
conf <- as.data.frame(confint(svymean(reformulate(names(llcp20_ad)), llcp20_dsgn)))
table3 <- bind_cols(prev, conf) %>% 
        mutate_if(is.numeric, round, digits = 3) %>% 
        filter(mean != 0 ) 
table3 <- table3[4:52, ] 
hv <- c("Dental Visit > 1 yr", "Dental Visit < 1 yr"," Teeth removed > 5" , " Teeth removed < 5", "Smoker0", "Smoker1", "Heart_attack0", "Heart_attack1", 
        "Heart_disease0", "Heart_disease1", "Stroke0", "Stoke1", "Asthma0", "Asthma1", "Skin_cancer0", "Skin_cancer1", "Cancer0", "Cancer1", "COPD0",
        "COPD1", "Arthritis0", "Arthritis1", "Kidney_disease0", "Kidney_disease1", "Diabetes0", "Diabetes1", "Age < 65", "Age > 65", "Sex: Female", "Sex: Male", 
        "Race: American Indian/ Alaskan native, non-Hispanic", "Race: Asian, non-Hispanic", "Race: Black, non-Hispanic", "Race: Hispanic", "Race: other race, non-Hispanic", 
        "Race: White, non-Hispanic", "Income < $50000 yr", "Income > $50000 yr", "Healthcare_coverage0", "Healthcare_coverage1", "BMI (underweight/ normal)", 
        "BMI (overweight/ obsess)", "Advanced education", "Little/ none education", "Some education", "oral_health0", "oral_health1", 
        "chronic_health0", "chronic_health1")  
table3 <- bind_cols(hv, table3) %>% 
        rename(hv = '...1') %>% 
        rename(proportion = "mean") %>% 
        rename(proportion_low = "2.5 %") %>% 
        rename(proportion_high = "97.5 %") 

table3.1 <- table3 %>% select(-hv)

knitr::kable(table3.1, bookstabs = TRUE, format = "markdown",
             caption = "Weighted Prevelance for health variables")

## histogram of weighted prevelance
ggplot(data=table3, aes(x=hv, y=proportion)) + 
        geom_bar(stat="identity", position=position_dodge()) + 
        coord_flip()

## Fit adjusted logistic regression model for oral health
alr <- svyglm(oral_health ~ SMOKE100 *(SEXVAR + X_IMPRACE) + X_AGEG5YR + INCOME2 + 
                      HLTHPLN1 + X_RFBMI5 + EDUCA + CVDCRHD4 + CVDSTRK3 + ASTHMA3 + 
                      CHCOCNCR + CHCCOPD2 + HAVARTH4 + CHCKDNY2 + DIABETE4, 
                      family = quasibinomial, design = llcp20_dsgn)
summary(alr)
confint(alr)

fit <- as.data.frame(coef(summary(alr))) 
table4 <- cbind(fit, confint(alr))
        
exp(coef(alr))
exp(cbind(AOR = coef(alr), confint(alr)))
exp(cbind(AOR = coef(alr), confint(alr)))[-1, ]
adj_or <- exp(cbind(AOR = coef(alr), confint(alr)))[-1, ]
adj_or <- as.data.frame(adj_or)

ohv <- c("Smoker", "Age > 65", "Race: Asian, non-Hispanic", "Race: Black, non-Hispanic", "Race: Hispanic", 
             "Race: other race, non-Hispanic", "Race: White, non-Hispanic", "Male", "Income > $50000 yr", "Healthcare coverage",
             "BMI (overweight/ obsess)", "Little/ none education", "some eudcation", "Heart disease", "Stroke", "Asthma", "Cancer",
             "COPD", "Arthritis","Kidney disease", "Diabetes", "Smoker: Male", "Smoker: Race: Asian, non-Hispanic", 
             "Smoker: Race: Black, non-Hispanic", "Smoker: Race: Hispanic", "Smoker: Race: other race, non-Hispanic", "Smoker: Race: White, non-Hispanic")          
table1 <- cbind(ohv, adj_or[,c("AOR","2.5 %", "97.5 %")])
table1$aorll <- table1$"2.5 %"
table1$aorul <- table1$"97.5 %"

## Fit adjusted logistic regression model for chronic health
alr_2 <- svyglm(chronic_health ~ SMOKE100 *(SEXVAR + X_IMPRACE) + X_AGEG5YR + INCOME2 + 
                      HLTHPLN1 + X_RFBMI5 + EDUCA + CVDINFR4 + HAVARTH4, 
              family = quasibinomial, design = llcp20_dsgn)
summary(alr_2)
confint(alr_2)

fit_2 <- as.data.frame(coef(summary(alr_2))) 
table5 <- cbind(fit_2, confint(alr_2))

exp(coef(alr_2))
exp(cbind(AOR = coef(alr_2), confint(alr_2)))
exp(cbind(AOR = coef(alr_2), confint(alr_2)))[-1, ]
adj_or_2 <- exp(cbind(AOR = coef(alr_2), confint(alr_2)))[-1, ]
adj_or_2 <- as.data.frame(adj_or_2)
exp(coef(alr_2))
exp(cbind(AOR = coef(alr_2), confint(alr_2)))
exp(cbind(AOR = coef(alr_2), confint(alr_2)))[-1, ]
adj_or_2 <- exp(cbind(AOR = coef(alr_2), confint(alr_2)))[-1, ]
adj_or_2 <- as.data.frame(adj_or_2)

chv <- c("Smoker", "Age > 65", "Race: Asian, non-Hispanic", "Race: Black, non-Hispanic", "Race: Hispanic", 
         "Race: other race, non-Hispanic", "Race: White, non-Hispanic", "Male", "Income > $50000 yr", "Healthcare coverage",
         "BMI (overweight/ obsess)", "Little/ none education", "some eudcation", "Heart attack", "Arthritis",
         "Smoker: Male", "Smoker: Race: Asian, non-Hispanic", "Smoker: Race: Black, non-Hispanic", "Smoker: Race: Hispanic", 
         "Smoker: Race: other race, non-Hispanic", "Smoker: Race: White, non-Hispanic")          
table2 <- cbind(chv, adj_or_2[,c("AOR","2.5 %", "97.5 %")])
table2$aorll <- table2$"2.5 %"
table2$aorul <- table2$"97.5 %"

## Histograms of AOR
table1 %>% ggplot(aes(x=ohv, y = AOR)) + 
        geom_bar(stat = "identity") + 
        geom_abline(slope = 0, intercept = 1, color = "red", size = 2) + 
        geom_errorbar(aes(ymin=aorll, ymax=aorul), width = .2) +
        xlab("Variables") +
        coord_flip() 

table2 %>% ggplot(aes(x=chv, y = AOR)) + 
        geom_bar(stat = "identity") + 
        geom_abline(slope = 0, intercept = 1, color = "red", size = 2) + 
        geom_errorbar(aes(ymin=aorll, ymax=aorul), width = .2) +
        xlab("Variables") +
        coord_flip()

## Run Wald test with oh and obtain p-values 
list <- c(names(alr$model)[2:16], "SMOKE100:SEXVAR", "SMOKE100:X_IMPRACE")
oh_p <- sapply(list, function(x) signif(regTermTest(alr, x, method="Wald")$p, 3))
oh_p <- as.data.frame(oh_p)
list_2 <- c(names(alr_2$model)[2:11], "SMOKE100:SEXVAR", "SMOKE100:X_IMPRACE")
ch_p <- sapply(list_2, function(x) signif(regTermTest(alr_2, x, method="Wald")$p, 3))
ch_p <- as.data.frame(ch_p)



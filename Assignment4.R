library(tidyr)
library(tidyverse)
library(dplyr)
setwd("/Users/juliansauvage/Desktop/Econ613/Assignment 4/Data 3")

###############
## Exercise 1 #
###############
NLSY97 <- read.csv("dat_A4.csv")

# To find work experience in years, I sum weeks of work experience across all potential job columns (1-11),
# then I divide by 52 to translate this figure to years of work experience.
# Note that years of work experience exceeds age for some respondents, likely because they worked multiple
# jobs simultaneously.

NLSY97 <- NLSY97 %>%
  mutate(age = case_when(
    KEY_BDATE_M_1997 < 4 ~ 2019 -KEY_BDATE_Y_1997,
    TRUE ~ 2019 -KEY_BDATE_Y_1997-1
  )) %>% 
  mutate(sex = KEY_SEX_1997) %>%
  mutate(total_work_week = select(., CV_WKSWK_JOB_DLI.01_2019:CV_WKSWK_JOB_DLI.11_2019) %>% rowSums(na.rm = TRUE)) %>%
  mutate(work_exp = total_work_week/52)

# Replace mother/father years of education with 0 if they are denoted "Ungraded"
NLSY97[ , 8:11 ][ NLSY97[ , 8:11 ] == 95 ] <- 0

# For an individual's education recode, I coded years of education as 0 where data was missing or no degree was 
# recieved. I also assume that a GED and High school diploma represent equivalent years of schooling since they
# are essentially substitutes. Associate/JC is assumed to be 2 years, Bachelor's is 4 years, Master's is an 
# additional 2 years, and then the variable is top-coded at 20 years for PhD/Professional degrees
NLSY97 <- NLSY97 %>%
  mutate(educ_years = case_when(
   YSCH.3113_2019 == 1 ~ 0,
   YSCH.3113_2019 == 2 ~ 12,
   YSCH.3113_2019 == 3 ~ 12,
   YSCH.3113_2019 == 4 ~ 14,
   YSCH.3113_2019 == 5 ~ 16,
   YSCH.3113_2019 == 6 ~ 18,
   YSCH.3113_2019 == 7 ~ 20,
   YSCH.3113_2019 == 8 ~ 20,
   TRUE ~ 0
  ))

age_mean <- NLSY97 %>%
  group_by(age) %>%
  drop_na(YINC_1700_2019) %>%
  dplyr::summarize(mean = mean(YINC_1700_2019))

ggplot(NLSY97[NLSY97$YINC_1700_2019 > 0, ], aes(x=YINC_1700_2019, color= factor(age))) + 
  geom_density() +
  labs(title = "Income Distribution by Age", x = "Income", color = "Age Group") +
  geom_vline(data = age_mean, aes(xintercept = mean, color = factor(age)))

sex_mean <- NLSY97 %>%
  group_by(sex) %>%
  drop_na(YINC_1700_2019) %>%
  dplyr::summarize(mean = mean(YINC_1700_2019))

ggplot(NLSY97[NLSY97$YINC_1700_2019 > 0, ], aes(x=YINC_1700_2019, color= factor(sex))) + 
  geom_density() +
  labs(title = "Income Distribution by Gender", x = "Income", color = "Gender") +
  geom_vline(data = sex_mean, aes(xintercept = mean, color = factor(sex)))

child_mean <- NLSY97 %>%
  group_by(CV_BIO_CHILD_HH_U18_2019) %>%
  drop_na(YINC_1700_2019) %>%
  dplyr::summarize(mean = mean(YINC_1700_2019))

ggplot(NLSY97[NLSY97$YINC_1700_2019 > 0 & !is.na(NLSY97$CV_BIO_CHILD_HH_U18_2019), ], aes(x=YINC_1700_2019, color= factor(CV_BIO_CHILD_HH_U18_2019))) + 
  geom_density()  +
  labs(title = "Income Distribution by # of Children", x = "Income", color = "# of Children") +
  geom_vline(data = child_mean, aes(xintercept = mean, color = factor(CV_BIO_CHILD_HH_U18_2019)))

ggplot(NLSY97[NLSY97$YINC_1700_2019 > 0, ], aes(x=age, y=YINC_1700_2019)) +
  geom_bar(stat='identity')


NLSY97$sex <-factor(NLSY97$sex,
         levels = c(1,2),
         labels = c("Male", "Female"))

NLSY97$CV_MARSTAT_COLLAPSED_2019 <- factor(NLSY97$CV_MARSTAT_COLLAPSED_2019,
                                           levels = c(0,1,2,3,4),
                                           labels = c("Never-married", "Married", "Separated", "Divorced", "Widowed"))

nlsy <- NLSY97 %>%
  dplyr::rename(income = YINC_1700_2019,
                kids = CV_BIO_CHILD_HH_U18_2019,
                sat_math = TRANS_SAT_MATH_HSTR,
                marstat = CV_MARSTAT_COLLAPSED_2019,
                ethnicity = KEY_RACE_ETHNICITY_1997,
                dad_edu = CV_HGC_RES_DAD_1997,
                mom_edu = CV_HGC_RES_MOM_1997) 


income_age <- nlsy %>%
  mutate(income_pos = (income > 0)) %>%
  group_by(age, income_pos) %>%
  dplyr::summarize(Count0 = n()) %>%
  drop_na() %>%
  group_by(age) %>%
  mutate(Total = sum(Count0)) %>%
  mutate(Share0 = Count0/Total) %>%
  filter(income_pos == FALSE) %>%
  subset(., select = c("age", "Count0", "Total", "Share0") )


income_sex <- nlsy %>%
  mutate(income_pos = (income > 0)) %>%
  group_by(sex, income_pos) %>%
  dplyr::summarize(Count0 = n()) %>%
  drop_na() %>%
  group_by(sex) %>%
  mutate(Total = sum(Count0)) %>%
  mutate(Share0 = Count0/Total) %>%
  filter(income_pos == FALSE) %>%
  subset(., select = c("sex", "Count0", "Total", "Share0") )

income_kids <- nlsy %>%
  mutate(income_pos = (income > 0)) %>%
  group_by(kids, income_pos) %>%
  dplyr::summarize(Count0 = n()) %>%
  drop_na() %>%
  group_by(kids) %>%
  mutate(Total = sum(Count0)) %>%
  mutate(Share0 = Count0/Total) %>%
  filter(income_pos == FALSE) %>%
  subset(., select = c("kids", "Count0", "Total", "Share0") )

income_marstat <- nlsy %>%
  mutate(income_pos = (income > 0)) %>%
  group_by(marstat, income_pos) %>%
  dplyr::summarize(Count0 = n()) %>%
  drop_na() %>%
  group_by(marstat) %>%
  mutate(Total = sum(Count0)) %>%
  mutate(Share0 = Count0/Total) %>%
  filter(income_pos == FALSE) %>%
  subset(., select = c("marstat", "Count0", "Total", "Share0") )



################
## Exercise 2 ##
################


nlsy <- nlsy %>%
  mutate(truncate = (income > 0)) %>%
  mutate(age2 = age*age) %>%
  drop_na(truncate, educ_years, work_exp, age, marstat, kids, income, sex)

model <- lm(formula = income ~  educ_years + work_exp + age + sex + marstat + kids, data = nlsy)
summary(model)


probit = glm(formula = truncate ~  work_exp  + age + marstat + kids + sex,
             data   = nlsy,
             family = binomial(link = 'probit'))

probit_lp = predict(probit)

nlsy$imr = dnorm(probit$linear.predictors)/pnorm(probit$linear.predictors)

lm_select = lm(income ~  educ_years + work_exp + age + sex + marstat + kids + imr, 
               data = nlsy)
summary(lm_select)

# Use sampleSelection package to check answers
selection_2step = selection(truncate ~  work_exp  + age + marstat + kids, 
                            income ~  educ_years + work_exp + age + sex + marstat + kids, 
                            method = '2step', data = nlsy)
summary(selection_2step)



##################
### Exercise 3 ###
##################


ggplot(nlsy, aes(x=income)) + 
  geom_histogram(bins = 20) 


nlsy <- nlsy %>%
  mutate(censor = (income < 100000)) %>%
  drop_na(censor, educ_years, work_exp, age, marstat)

probit = glm(formula = censor ~ educ_years + work_exp  + age + kids + marstat,
             data   = nlsy,
             family = binomial(link = 'probit'))

probit_lp = predict(probit)

nlsy$imr = dnorm(probit$linear.predictors)/pnorm(probit$linear.predictors)

lm_select = lm(income ~  educ_years + work_exp + age +sex + marstat + imr, 
               data = nlsy)
summary(lm_select)

lm_comp = lm(income ~   educ_years + work_exp + age +sex + marstat, 
               data = nlsy)
summary(lm_comp)

# Use sampleSelection package to check answers
selection_2step = selection(censor ~ educ_years + work_exp  + age + kids + marstat, 
                            income ~  educ_years + work_exp + age +sex + marstat, 
                            method = '2step', data = nlsy)
summary(selection_2step)


##################
### Exercise 4 ###
##################


# Ability bias: Individuals with higher innate ability are also more likely to pursue
# greater education. Even if they had average education they would still be earning greater than average wages.
# Most productive individuals have incentives to study for as long as possible.

library("panelr")
library("plm")

nlsy_panel <- read.csv("dat_A4_panel.csv", header = TRUE)

tmp <- long_panel(nlsy_panel, prefix = "_", begin = 1997, end = 2019, label_location = "end") %>%
  mutate(age = wave - KEY_BDATE_Y) %>% 
  dplyr::rename(sex = KEY_SEX,
                income = YINC.1700)
  
  
  
tmp <- tmp %>%
  rowwise() %>%
  dplyr::mutate(total_work_week = sum(across(starts_with("CV_WKSWK_JOB")), na.rm = T)) %>%
  mutate(work_exp = total_work_week/52)

panel_data <- tmp %>%
  dplyr::rename(educ = CV_HIGHEST_DEGREE_EVER_EDT, 
                marstat = CV_MARSTAT_COLLAPSED)

panel_data <- pdata.frame(panel_data, index=c("id", "wave"))

formula = income ~ work_exp + educ + age + marstat + sex + factor(KEY_RACE_ETHNICITY)

model_within = plm(formula, data=panel_data, model="within")
model_bw = plm(formula, data=panel_data, model="between")
model_fd = plm(formula, data=panel_data, model="fd")

texreg::texreg(list(model_within,model_bw,model_fd))





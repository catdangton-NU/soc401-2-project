library(pacman)
p_load (tidyverse, haven, modelsummary, marginaleffects, estimatr, psych, MASS)
rm(list = ls()) # refresh environment
getwd()
df_raw <- read.csv("03_Clean/Input/HRS_widows_demog_tracker_merged.csv")


# Select relevant variables; recode categorical variables
df <- df_raw %>%
    filter(USBORN %in% c(1, 5), !is.na(SS048M1)) %>%
    dplyr::select(respondent_id, USBORN, DEGREE, SS048M1, SS044, SS045, SS046, SS047, SS003_1, SS004_1, SS008_1, SS005_1, SS006_1, SS007_1) %>%
    mutate(deathexpense_special = case_when(SS048M1 %in% c(1,2,3,4,7) ~ 1, SS048M1 == 5 ~ 0, TRUE ~ NA)) %>%
    mutate(usborn = case_when(USBORN == 1 ~ 1, USBORN == 5 ~ 0, TRUE ~ NA))

##### CONVERT variables to factor format to present descriptive matrix #### 
# I should not factor the dependent variable
df$usborn <- factor(df$usborn, levels = c(0, 1), labels = c("foreign", "us-born"))

# Recode Degree into a factor variable
df$degree <- factor(df$DEGREE,
                    levels = c(0, 1, 2, 3, 4, 5, 6, 9),
                    labels = c("No degree",
                               "GED",
                               "High school diploma",
                               "Two year college degree",
                               "Four year college degree",
                               "Master degree",
                               "Professional degree (Ph.D., M.D., J.D.)",
                               "Degree unknown/Some College"))
# Recode Degree into fewer categories //TODO
df <- df %>%
  mutate(degree = factor(case_when(
    degree %in% c("Two year college degree", "Four year college degree") ~ "Associate's and Bachelor's",
    degree %in% c("Master degree", "Professional degree (Ph.D., M.D., J.D.)") ~ "Postgraduate degree",
    TRUE ~ as.character(degree)
  )))
# make descriptive crosstabs
table1 <- table(df$deathexpense_special, df$usborn)
table2 <- table(df$deathexpense_special, df$degree)
print(table1)
print(table2)

##### Recode continuous variables ######

## 1. Death expenses ##
library(purrr)

## THE STEPS: 
# 1. Remove refused, don't know, not ascertained, no breakpoint values given in SS047
# Basically, any non-NA values in SS047.
# 2. Recode SS044 (death expenses reported in exact dollar amounts) into 'deathexpense_usd'
# Cases coded as 9999998 in SS044 included both "dont know" and "not ascertained", 
# and so the surveyor moves onto questions SS045-046 to obtain an approximate range.
# 3. Calculate midpoint values between what's in SS045 (expense estimates, min) and SS046 (expense extimates, max)
# Then map those values onto the variable.
# //TODO create a regex function that applies these steps to income start/stop looped vars (S004-S008)

# NOTE: # SS046 = 99999996 stands for expenses totaling above $10000. 
# I chose a value of 1 SD above the mean of SS044 to substitute for 99999996. 
# calculate the mean and sd below: 
mean <- mean(df$SS044[ df$SS044 != 99998 & df$SS044 != 99999 & df$SS044 != 9999998 & df$SS044 != 9999999], na.rm = TRUE)
sd <- sd(df$SS044[ df$SS044 != 99998 & df$SS044 != 99999 & df$SS044 != 9999998 & df$SS044 != 9999999], na.rm = TRUE)
stand_in <- mean + sd
# SS044's standard deviation is $10488 and mean is $7653 according to H22S_R codebook. 
df <- df %>%
  filter(is.na(SS047)) %>%
  mutate(deathexpense_usd = SS044)  %>% 
  mutate(deathexpense_usd = case_when(
   SS045 == 0 & SS046 == 99999996 ~ stand_in, 
   SS045 >= 0 & SS046 != 99999996 ~ (SS045 + SS046) / 2,
   SS045 != 0 & SS046 == 99999996 ~ (SS045 + stand_in) / 2,
    TRUE ~ deathexpense_usd
  ))

# Recode SS044 (death expenses reported in exact dollar amounts) into 'deathexpense_usd'
df <- df %>%
  mutate(deathexpense_usd = SS044)

# Cases coded as 9999998 in SS044 included both "dont know" and "not ascertained", 
# and so the surveyor moves onto questions SS045-046 to obtain an approximate range (less than 2000, 2000-10000, more then 10000).
# 
# Recode SS045 (expense estimates, min) and SS046 (expense extimates, max) to get calculable midpoint values
# Then map those values onto the variable.
df <- df %>%
  mutate(deathexpense_usd = case_when(
   SS045 == 0 & SS046 == 99999996 ~ 18141.51, # SS046 = 99999996 stands for expenses totaling above $10000. I chose a value of 1 SD above the mean to substitute for 99999996. standard deviation for SS044 is $10488 and mean is $7653 according to codebook. 
   SS045 >= 0 & SS046 != 99999996 ~ (SS045 + SS046) / 2,
   SS045 != 0 & SS046 == 99999996 ~ (SS045 + 18141.51) / 2,
    TRUE ~ deathexpense_usd
  ))

## Recoding has errors due to not incorporating SS047 (refusal to answer) and non-response values in SS044 
# but I've been at this for hours, recoded values keep overriding one another, and I am too tired 
### UPDATE 04/13/24: resolved!

# Summary of the new variable
summary(df$deathexpense_usd)

#### EXPORT OUTPUT ####

write.csv(df, file = "03_Clean/Output/HRS_widows_demog_tracker_cleaned.csv")
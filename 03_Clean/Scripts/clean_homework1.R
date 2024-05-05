library(pacman)
p_load (tidyverse, haven, modelsummary, marginaleffects, estimatr, psych, MASS)
rm(list = ls()) # refresh environment
getwd()
df_raw <- read.csv("03_Clean/Input/HRS_widows_employ_tracker_merged.csv")


# Select relevant variables
df <- df_raw %>%
    filter(USBORN %in% c(1, 5), !is.na(SS048M1)) %>%
    dplyr::select(respondent_id, Year, USBORN, DEGREE, BIRTHYR, GENDER, RACE, HISPANIC, # tracker variables # nolint
                  SS048M1, SS048M2, SS048M3, # death expense resolution (dependent variable)
                  SS044, SS045, SS046, SS047, # death expenses beyond insurance and estate coverage
                  SJ005M1, SJ005M2, SJ005M3, # Current job status (up to 3 electives)
                  SS003_1, SS004_1, SS008_1, SS005_1, SS006_1, SS007_1, # social security income
                  SS003_2, SS004_2, SS008_2, SS005_2, SS006_2, SS007_2, # supplemental security income
                  SS003_3, SS004_3, SS008_3, SS005_3, SS006_3, SS007_3, # Veterans benefits
                  SS003_4, SS004_4, SS008_4, SS005_4, SS006_4, SS007_4) # Other pensions or annuities

df <- df %>% # Recode dependent variable and some demographic variables
    mutate(deathexpense_special = case_when(SS048M1 %in% c(1, 2, 3, 4, 6, 7) ~ 1, SS048M1 == 5 ~ 0, TRUE ~ NA)) %>%
    mutate(foreign = case_when(USBORN == 5 ~ 1, USBORN == 1 ~ 0, TRUE ~ NA)) %>% # Respondent's US-born vs. foreign born status
    mutate(female = ifelse(GENDER == 2, 1, 0)) %>% 
    mutate(age = as.numeric(Year) - as.numeric(BIRTHYR)) %>%
    mutate(disabled = case_when(SJ005M1 == 4 | SJ005M2 == 4 | SJ005M3 == 4 ~ 1,
                                SJ005M1 %in% c(1, 2, 3, 5, 6, 7, 8) | SJ005M2 %in% c(1, 2, 3, 5, 6, 7, 8) | SJ005M3 %in% c(1, 2, 3, 5, 6, 7, 8) ~ 0,
                                TRUE ~ NA))

df <- df %>% # Code race & ethnicity based on available data
    mutate(black_nh = ifelse(RACE == 2 & HISPANIC == 5, 1, 0))  %>% 
    mutate(white_nh = ifelse(RACE == 1 & HISPANIC == 5, 1, 0))  %>% 
    mutate(hisp_allraces = ifelse(HISPANIC %in% c(1, 2, 3), 1, 0))  %>% 
    mutate(black_hi = ifelse(RACE == 2 & hisp_allraces == 1, 1, 0)) %>%
    mutate(white_hi = ifelse(RACE == 1 & hisp_allraces == 1, 1, 0)) %>%
    mutate(hisp_other = ifelse(hisp_allraces == 1 & RACE == 7, 1, 0)) %>%
    # mutate(raceEth_unk = ifelse(HISPANIC == 0 & RACE == 0, 1, 0)) %>% there are no cases with unknown race ethnicity
    mutate(non_bwh = ifelse(black_nh == 0 & white_nh == 0 & hisp_allraces == 0, 1, 0))

df <- df %>% # Code current job status
    # working now (1), temporarily laid off (3), on sick or other leave (8)
    mutate(employed = case_when(SJ005M1 %in% c(1, 3, 8) | SJ005M2 %in% c(1, 3, 8) | SJ005M3 %in% c(1, 3, 8) ~ 1, 
                                SJ005M1 %in% c(2, 4, 5, 6, 7) | SJ005M2 %in% c(2, 4, 5, 6, 7) | SJ005M3 %in% c(2, 4, 5, 6, 7) ~ 0,
                                TRUE ~ NA)) %>%
    mutate(retired = case_when(SJ005M1 %in% c(5) | SJ005M2 %in% c(5) | SJ005M3 %in% c(5) ~ 1, 
                                SJ005M1 %in% c(2, 4, 1, 3, 8, 6, 7) | SJ005M2 %in% c(2, 4, 1, 3, 8, 6, 7) | SJ005M3 %in% c(2, 4, 1, 3, 8, 6, 7) ~ 0,
                                TRUE ~ NA)) %>%
    mutate(homemaker = case_when(SJ005M1 %in% c(6) | SJ005M2 %in% c(6) | SJ005M3 %in% c(6) ~ 1, 
                                SJ005M1 %in% c(2, 4, 1, 3, 8, 5, 7) | SJ005M2 %in% c(2, 4, 1, 3, 8, 5, 7) | SJ005M3 %in% c(2, 4, 1, 3, 8, 5, 7) ~ 0,
                                TRUE ~ NA))

df <- df %>% # code more detailed outcomes
  mutate(sell_assets = case_when(SS048M1 == 1 | SS048M2 == 1 | SS048M3 == 1 ~ 1, TRUE ~ 0),
        withdraw_money = case_when(SS048M1 == 2 | SS048M2 == 2 | SS048M3 == 2 ~ 1, TRUE ~ 0),
        fam_help = case_when(SS048M1 == 3 | SS048M2 == 3 | SS048M3 == 3 ~ 1, TRUE ~ 0),
        charity_help = case_when(SS048M1 == 4 | SS048M2 == 4 | SS048M3 == 4 ~ 1, TRUE ~ 0),
        did_nothing_special = case_when(SS048M1 == 5 | SS048M2 == 5 | SS048M3 == 5 ~ 1, TRUE ~ 0),
        borrow_money = case_when(SS048M1 == 6 | SS048M2 == 6 | SS048M3 == 6 ~ 1, TRUE ~ 0),
        other = case_when(SS048M1 == 7 | SS048M2 == 7 | SS048M3 == 7 ~ 1, TRUE ~ 0),
        dk_na = case_when(SS048M1 == 8 | SS048M2 == 8 | SS048M3 == 8 ~ 1, TRUE ~ 0),
        rf = case_when(SS048M1 == 9 | SS048M2 == 9 | SS048M3 == 9 ~ 1, TRUE ~ 0),
        inap = case_when(is.na(SS048M1) | is.na(SS048M2) | is.na(SS048M3) ~ 1, TRUE ~ 0))

df <- df %>%
  mutate(deathexpense_sources = case_when(sell_assets == 1 | withdraw_money == 1 ~ "assets_savings", 
                                          fam_help == 1 ~ "relatives_friends",
                                          charity_help == 1 ~ "charities", 
                                          borrow_money == 1 ~ "loans",
                                          other == 1 ~ "other",
                                          # full coverage is where the costs exceeding insurance/estate coverage is less than $300.
                                          SS044 <= 300 ~ "insurance_estate_full", 
                                          TRUE ~ NA))
df$deathexpense_sources <- as.factor(df$deathexpense_sources)
df$deathexpense_sources <- relevel(df$deathexpense_sources, ref = "insurance_estate_full")
table(df$deathexpense_sources)

# //TODO ask about weird cases (temporarily laid off AND retired)
table(df$SJ005M1, df$SJ005M2) 

##### CONVERT variables to factor format to present descriptive matrix #### 
# I should not factor dummy variables

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
    degree %in% c("Two year college degree", "Four year college degree") ~ "College (2-year or 4-year)",
    degree %in% c("Master degree", "Professional degree (Ph.D., M.D., J.D.)") ~ "Postgraduate degree",
    TRUE ~ as.character(degree)
  )))
# make descriptive crosstabs
table1 <- table(df$deathexpense_special, df$foreign)
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
# //TODO figure out why 100 cases were dropped in this step.

# NOTE: # SS046 = 99999996 stands for expenses totaling above $10000. 
# I chose a value of 1 SD above the mean of SS044 to substitute for 99999996 (stand_in). 
# calculate the mean, sd, and the stand-in: 
mean <- mean(df$SS044[df$SS044 != 99998 & df$SS044 != 99999 & df$SS044 != 9999998 & df$SS044 != 9999999], na.rm = TRUE)
sd <- sd(df$SS044[df$SS044 != 99998 & df$SS044 != 99999 & df$SS044 != 9999998 & df$SS044 != 9999999], na.rm = TRUE)
stand_in <- mean + sd
# SS044's standard deviation is $10488 and mean is $7653 according to H22S_R codebook. 
df <- df %>%
  filter(is.na(SS047)) %>% # remove NAs
  mutate(deathexpense_usd = SS044)  %>% 
  mutate(deathexpense_usd = case_when(
   SS045 == 0 & SS046 == 99999996 ~ stand_in, 
   SS045 >= 0 & SS046 != 99999996 ~ (SS045 + SS046) / 2,
   SS045 != 0 & SS046 == 99999996 ~ (SS045 + stand_in) / 2,
    TRUE ~ deathexpense_usd
  ))
# Summary of the new variable
summary(df$deathexpense_usd)

#### EXPORT OUTPUT ####

write.csv(df, file = "03_Clean/Output/HRS_widows_employ_tracker_cleaned.csv")
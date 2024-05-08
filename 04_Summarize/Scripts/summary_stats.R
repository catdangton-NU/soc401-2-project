library(pacman)
p_load (tidyverse, haven, modelsummary, marginaleffects, estimatr, psych, MASS)
rm(list = ls()) # refresh environment
getwd()

df <- read.csv("04_Summarize/Input/HRS_widows_employ_tracker_cleaned.csv")

table(df$deathexpense_special)
table(df$deathexpense_sources)
table(df$SS040, df$deathexpense_special) # whether they got insurance payout vs. whether they had to do sth special to supplement
# those 431 cases who didnt get insurance and didnt do anything special are weird. wonder what they're up to.

table(df$SS040, df$deathexpense_special)

summary(df$age)
# Create a histogram of death expenses in dollars
hist(df$deathexpense_usd, breaks = 100) # before removing outliers

# Identify the outliers
outliers <- boxplot.stats(df$deathexpense_usd)$out

# Filter out the outliers
df_no_outliers <- df %>% filter(!(deathexpense_usd %in% outliers))

# Create a histogram of the data without outliers
hist(df_no_outliers$deathexpense_usd, breaks = 20)

table(df$deathexpense_special)

write.csv(df, file = "04_Summarize/Output/HRS_widows_employ_tracker_cleaned.csv")
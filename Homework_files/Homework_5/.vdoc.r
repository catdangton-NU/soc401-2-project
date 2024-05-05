#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#

library(pacman)
p_load(tidyverse, broom, haven, skimr, janitor, marginaleffects, lmtest, modelsummary, flextable)

# Turn off scientific notation
options(scipen = 100)
# refresh environment
rm(list = ls()) 

# set working directory to project directory
# setwd(here::here("FINAL_PROJECT_HRS_SPOUSAL_DEATH"))
# read data in relation to working directory
df <- read.csv("../Input/HRS_widows_employ_tracker_cleaned.csv")

df$deathexpense_sources <- relevel(df$deathexpense_sources, ref = "assets_savings")
```
#
#
#
#
#
#
#
#
#

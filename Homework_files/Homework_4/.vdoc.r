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

#
#
#
#
#

# recode death expenses for easier interpretation
df$deathexpense_1k <- df$deathexpense_usd/1000 

# Estimate a logistic regression model
model_logit <- glm(deathexpense_special ~ foreign + female + disabled + employed + homemaker + black_nh + hisp_allraces + non_bwh + deathexpense_1k, 
  data = df, 
  family = binomial(link = "logit")) #specify that we want logistic regression(link = "logit")) 
#
#
#

# use coef_map to rename and reorder variables ("default name" = "new name")
  # any variables not listed in coef_map will be omitted from results
# assigning values to be included in coef_map to R object to avoid repetition
coef <- c("foreign" = "Foreign-born", 
          "female" = "Female",
          "disabled" = "Disabled",
          "employed" = "Currently employed",
          "retired" = "Retired",
          "homemaker" = "Homemaker",
          "black_nh" = "NH Black",
          "white_nh" = "NH White", # can I set White as reference category by excluding it from the coef_map?
          "hisp_allraces" = "Hispanic (of all races)",
          "non_bwh" = "Other nonwhite",
          "deathexpense_1k" = "Death expenses ($1000s)",
          "female:black_nh" = "Female x NH Black",
          "female × hisp_allraces" = "Female x Hispanic",
          "(Intercept)" = "Constant" # shift order of intercept to last
          )

# use gof_map to adjust goodness-of-fit statistics shown in table
  # like coef_map, statistics not listed in gof_map will be omitted from results
# assigning values to be included in gof_map to R object to avoid repetition
gof <- c("nobs", "aic", "bic", "logLik", "r2.tjur")

#
#
#
#
modelsummary(model_logit,
            coef_map = coef,
            gof_map = gof,
            fmt = 4, # 4 decimal digits (to avoid rounding to 0)
            stars = T, # significance stars
            exponentiate = T,
            title = "Logistic Regression Results for Resorting to Special Means to Cover Death Expenses (odds ratios)",
            notes = c("SE in parentheses",
                        "Ref: US-born, male, non-disabled, retired, white")) 
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
# Calculate the average marginal effects
ame <- avg_slopes(model_logit)

print(ame)
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
model_interact <- glm(deathexpense_special ~ foreign + female + female*black_nh + female*hisp_allraces + disabled + employed + homemaker + black_nh + hisp_allraces + non_bwh + deathexpense_1k, 
  data = df, 
  family = binomial(link = "logit")) #specify that we want logistic regression(link = "logit")) 

modelsummary(model_interact,
            # coef_map = coef,
            gof_map = gof,
            fmt = 4, # 4 decimal digits (to avoid rounding to 0)
            stars = T, # significance stars
            exponentiate = T,
            title = "Logistic Regression Results for Resorting to Special Means to Cover Death Expenses (odds ratios)",
            notes = c("SE in parentheses",
                        "Ref: US-born, male, non-disabled, retired, white")) 
```
#
#
#
#
#
# First off, plotting predicted probability with no interaction
plot_predictions(model_interact,
                 condition = "female") 

# Plot predicted probability with interaction effect
plot_predictions(model_interact,
                 condition = c("female", "black_nh")) 

# Plot predicted probability with interaction effect
plot_predictions(model_interact,
                 condition = c("female", "hisp_allraces")) 
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

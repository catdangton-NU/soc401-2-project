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
# Import inputs
library(pacman)
p_load (tidyverse, haven, modelsummary, marginaleffects, estimatr, psych, MASS,ggplot2)
rm(list = ls()) # refresh environment
# set working directory to project directory
setwd(here::here("FINAL_PROJECT_HRS_SPOUSAL_DEATH"))
# read data in relation to working directory
df <- read.csv("Homework_files/Input/HRS_widows_demog_tracker_cleaned.csv")

```
#
#
#
# Convert dependent variable and continuous variable to numeric so the model takes it
df$deathexpense_special <- as.numeric(df$deathexpense_special)
df$deathexpense_usd <- as.numeric(df$deathexpense_usd)
# convert unit of analysis to thousands of dollars
df$deathexpense_1k <- df$deathexpense_usd/1000 
# run model
model <- lm_robust(deathexpense_special ~ usborn + deathexpense_1k + degree, data = df)

modelsummary(model)
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

# Generate predicted values
df$predicted_prob <- predict(model, newdata = df)

# Create a scatter plot of the predicted probabilities
ggplot(df, aes(x = usborn, y = predicted_prob)) +
  geom_point() +
  labs(x = "US Born", y = "Predicted Probability of Special Death Expenses") +
  theme_minimal()
#
#
#
#
#
# Estimate a logistic regression model
model_logit <- glm(deathexpense_special ~ usborn + degree + deathexpense_1k, 
  data = df, 
  family = binomial(link = "logit")) #specify that we want logistic regression(link = "logit")) 
modelsummary(model_logit) # print logit regression estimates
#
#
#
# Calculate and print odds ratios
rbind(coef(model_logit), # get coefficients
  exp(coef(model_logit))) # get odds ratios
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
# Average Marginal Effects is:
# The average of all observation-specific marginal effects
avg_slopes(model_logit)
#
#
#
#
#
#
#
# 1. Graph the probability of the outcome for values of one of the independent variables, 
# holding the other variables at their mean or mode.
# Also known as the default setting of plot_predictions.
# condition = independent variable of interest.
# if condition var = continuous, plot_predictions plots the mean.
plot_predictions(model_logit, condition = "deathexpense_1k") # continuous indep. var.

# 2. Plot average predictions 
# graph average predictions based on letting controls retain original values,
# but changing X rather than holding other variables at mean/mode.
# this is known as a "counterfactual" grid type in plot_predictions()
plot_predictions(model_logit, by = "deathexpense_1k", newdata = datagrid(deathexpense_1k = 0:700, grid_type = "counterfactual"), type = "response")

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
#
#
#
#
#
#
#
#

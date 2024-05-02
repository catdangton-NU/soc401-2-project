####### TEST SCRIPT TO IMPORT HRS SURVEY DATA ########
# //TODO make function for script to avoid copy-pasting steps for each variable
library(pacman)
p_load(tidyverse, dplyr, yaml, here, haven) # nolint
rm(list = ls()) # refresh environment

# set the working directory to Final_Project_HRS_Spousal_Death/Import_HRS
# I currently have Final_Project_HRS_Spousal_Death open as the main dir in VSCode. #nolint
setwd(here::here("FINAL_PROJECT_HRS_SPOUSAL_DEATH"))

# read yaml file
config <- yaml::read_yaml("config.yaml")

# Access variables from yaml file
widowhood_22 <- config$widowhood_22
employment_22 <- config$employment_22
tracker_22 <- config$tracker_22
widowhood_20 <- config$widowhood_20
employment_20 <- config$employment_20
tracker_20 <- config$tracker_20

# Read the data files into data frames
df_widowhood_22 <- read_dta(widowhood_22)
df_employment_22 <- read_dta(employment_22)
df_tracker_22 <- read_dta(tracker_22)
df_widowhood_20 <- read_dta(widowhood_20)
df_employment_20 <- read_dta(employment_20)
df_tracker_20 <- read_dta(tracker_20)

# Create the output directory if it doesn't exist
if (!dir.exists("01_Import_HRS/Output")) {
  dir.create("01_Import_HRS/Output")
}
# Delete existing files in the Output folder
file.remove(list.files("01_Import_HRS/Output", full.names = TRUE))

# Export the data frames as CSV files
write.csv(df_widowhood_22, file = "01_Import_HRS/Output/widowhood_22.csv")
write.csv(df_employment_22, file = "01_Import_HRS/Output/employment_22.csv")
write.csv(df_tracker_22, file = "01_Import_HRS/Output/tracker_22.csv")
write.csv(df_widowhood_20, file = "01_Import_HRS/Output/widowhood_20.csv")
write.csv(df_employment_20, file = "01_Import_HRS/Output/employment_20.csv")
write.csv(df_tracker_20, file = "01_Import_HRS/Output/tracker_20.csv")
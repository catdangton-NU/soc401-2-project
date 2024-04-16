####### TEST: IMPORT HRS SURVEY DATA, CONNECT TO A MAKEFILE ########

library(pacman)
p_load(tidyverse, dplyr, yaml, here, haven) # nolint
rm(list = ls()) # refresh environment

# Define input data paths using environment variables from Makefile
widowhood_survey_path <- Sys.getenv("WIDOWHOOD_SURVEY")
demographics_survey_path <- Sys.getenv("DEMOGRAPHICS_SURVEY")
respondent_tracker_path <- Sys.getenv("RESPONDENT_TRACKER")
# Read data
widowhood_survey <- haven::read_dta(widowhood_survey_path)
demographics_survey <- haven::read_dta(demographics_survey_path)
respondent_tracker <- haven::read_dta(respondent_tracker_path)
# Create the output directory if it doesn't exist
if (!dir.exists("Output2")) {
  dir.create("Output2")
}

# Export the data into the output folder
write.csv(widowhood_survey, file = "Output2/widowhood_survey.csv")
write.csv(demographics_survey, file = "Output2/demographics_survey.csv")
write.csv(respondent_tracker, file = "Output2/respondent_tracker.csv")
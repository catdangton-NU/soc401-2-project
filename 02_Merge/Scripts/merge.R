library(pacman)
p_load(tidyverse, dplyr, yaml, here, haven) # nolint
rm(list = ls()) # refresh environment

# The working directory is Final_Project_HRS_Spousal_Death.

### Check if there's a difference in observations in tracker_20 and tracker_22 ###
# I suspect it is the same file.
# step 1: Import dataframes
tracker_20 <- read.csv("02_Merge/Input/tracker_20.csv")
tracker_22 <- read.csv("02_Merge/Input/tracker_22.csv")
# step 2: print the number of rows that are different
print(nrow(setdiff(tracker_22, tracker_20)))
# No difference. Using tracker_22 from now on.
df_tracker <- tracker_22

#### Combine survey dataframes from 2020 and 2022 ####

# Read the dataframes
df_widowdiv_20 <- read.csv("02_Merge/Input/widowhood_20.csv")
df_widowdiv_22 <- read.csv("02_Merge/Input/widowhood_22.csv")
df_demog_20 <- read.csv("02_Merge/Input/demographics_20.csv")
df_demog_22 <- read.csv("02_Merge/Input/demographics_22.csv")

# Add a new column to specify the year
df_widowdiv_20$Year <- 2020
df_widowdiv_22$Year <- 2022
df_demog_20$Year <- 2020
df_demog_22$Year <- 2020

# Create respondent IDs
df_tracker$respondent_id <- paste(df_tracker$HHID, df_tracker$PN, sep = "_")
df_widowdiv_20$respondent_id <- paste(df_widowdiv_20$HHID, df_widowdiv_20$PN, sep = "_")
df_widowdiv_22$respondent_id <- paste(df_widowdiv_22$HHID, df_widowdiv_22$PN, sep = "_")
df_demog_20$respondent_id <- paste(df_demog_20$HHID, df_demog_20$PN, sep = "_")
df_demog_22$respondent_id <- paste(df_demog_22$HHID, df_demog_22$PN, sep = "_")



## Filter respondents who had a deceased spouse
# I viewed the codebook (raw_data/codebook/H22S_R.txt)
# and found that SS052M and RS052M ("in which state or country did your partner die?"") captured all cases.
df_widow_20 <- df_widowdiv_20 %>%
    filter(!is.na(RS052M))
df_widow_22 <- df_widowdiv_22 %>%
    filter(!is.na(SS052M))
# check number of variables and cases
print(ncol(df_widow_20))
print(nrow(df_widow_20))

### MERGE 2020 into 2022 survey results ###
# Rename the columns in 2020 data to enable dataframe merging to latest wave
df_widow_renamed20 <- df_widow_20 %>%
  rename_with(~ gsub("^R", "S", .), starts_with("R"))
df_demog_renamed20 <- df_demog_20 %>%
  rename_with(~ gsub("^R", "S", .), starts_with("R"))

# Find the columns that are in df_widow_renamed20 but not in df_widow_22
diff_columns <- setdiff(names(df_widow_renamed20), names(df_widow_22))
print(diff_columns)

## Find respondent_ids that overlap between df_widow_renamed20 and df_widow_22
view(intersect(df_widow_renamed20$respondent_id, df_widow_22$respondent_id))
# There are 21 overlaps. 
# Assuming each respondent_ID corresponds to a unique person,
# This means 21 people here were interviewed in both waves. 

#### View overlapped cases' survey responses ###

# Find respondent_ids that overlap between df_widow_renamed20 and df_widow_22
common_ids <- intersect(df_widow_renamed20$respondent_id, df_widow_22$respondent_id)

# Filter df_widow_renamed20 and df_widow_22 for the common respondent_ids
df_widow_renamed20_common <- df_widow_renamed20[df_widow_renamed20$respondent_id %in% common_ids, ]
df_widow_22_common <- df_widow_22[df_widow_22$respondent_id %in% common_ids, ]
View(df_widow_renamed20_common)
View(df_widow_22_common)

# These respondents' survey answers are different between the two years. 
# I don't know how to deal with this. 
# Solution: remove these cases.
df_widow_renamed20 <- df_widow_renamed20  %>% 
    filter(!(respondent_id %in% common_ids))
df_widow_22 <- df_widow_22  %>% 
    filter(!(respondent_id %in% common_ids))

##### REPEAT the previous section FOR df_demog #### 
## DID NOT WORK //TODO fix this
# Find respondent_ids that overlap between df_widow_renamed20 and df_widow_22
common_ids <- intersect(df_demog_renamed20$respondent_id, df_demog_22$respondent_id)

# Filter df_widow_renamed20 and df_widow_22 for the common respondent_ids
df_demog_renamed20_common <- df_demog_renamed20[df_demog_renamed20$respondent_id %in% common_ids, ]
df_demog_22_common <- df_demog_22[df_demog_22$respondent_id %in% common_ids, ]
View(df_demog_renamed20_common)
View(df_demog_22_common)

# These respondents' survey answers are different between the two years. 
# I don't know how to deal with this. 
# I cannot remove these cases because there are 12159 of them.

# //TODO 

#### Merge 2020 and 2022 dataframe by respondent_id, ####
# bind_rows() contain all rows from both df_widow_22 and df_widow_renamed20,
# without splitting the common columns into .x and .y. 
## It also fills in NA values for columns that are not found in the other dataframe 
# Merge df_widow_22 and df_widow_renamed20
df_widow <- bind_rows(df_widow_22, df_widow_renamed20)

# Merge df_demog_22 and df_demog_renamed20
df_demog <- bind_rows(df_demog_22, df_demog_renamed20)

print(ncol(df_widow_22))
print(ncol(df_widow))
print(nrow(df_widow_22))
print(nrow(df_widow))

print(ncol(df_demog_22))
print(ncol(df_demog))
print(nrow(df_demog_22))
print(nrow(df_demog))


# Merge df_widow and df_demog based on respondent ID
# df_widow_d <- df_widow %>%
#    left_join(df_demog, by = "respondent_id")
# // TODO this created duplicate respondent_ids??? what is going on?
# check number of variables after the merge
# print(ncol(df_widow_d))

## Note: there may not be enough overlaps to justify merging demographics and widowhood surveys.
# But let's keep it merged for now.

# Merge resulting dataframe with tracker
df_widow_tr_d <- df_widow %>%
    left_join(df_tracker, by = "respondent_id")
# check number of variables after the merge
print(ncol(df_widow_tr_d))

# Write output: HRS data for widows, merged with demographics survey and tracker.
# HRS survey years merged: 2020 and 2022
write.csv(df_widow_tr_d, file = "02_Merge/Output/HRS_widows_demog_tracker_merged.csv")
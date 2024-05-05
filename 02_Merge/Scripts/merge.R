library(pacman)
p_load(tidyverse, dplyr, yaml, here, haven) # nolint
rm(list = ls()) # refresh environment

# The working directory is Final_Project_HRS_Spousal_Death.

### Check if there's a difference in observations in tracker_20 and tracker_22 ###
# I suspect it is the same file.
# step 1: Import dataframes
# tracker_20 <- read.csv("02_Merge/Input/tracker_20.csv")
tracker_22 <- read.csv("02_Merge/Input/tracker_22.csv")
# step 2: print the number of rows that are different
# print(nrow(setdiff(tracker_22, tracker_20)))
# No difference. Using tracker_22 from now on.
df_tracker <- tracker_22

#### Combine survey dataframes from 2020 and 2022 ####

# create function to read, add year and respondent id
read_and_process <- function(file_path, year) {
    df <- read.csv(file_path)
    df$Year <- year
    df$respondent_id <- paste(df$HHID, df$PN, sep = "_")
    return(df)
}

df_widowdiv_20 <- read_and_process("02_Merge/Input/widowhood_20.csv", 2020)
df_widowdiv_22 <- read_and_process("02_Merge/Input/widowhood_22.csv", 2022)
df_widowdiv_18 <- read_and_process("02_Merge/Input/widowhood_18.csv", 2018)
df_employ_20 <- read_and_process("02_Merge/Input/employment_20.csv", 2020)
df_employ_22 <- read_and_process("02_Merge/Input/employment_22.csv", 2022)
df_employ_18 <- read_and_process("02_Merge/Input/employment_18.csv", 2018)
df_tracker <- read_and_process("02_Merge/Input/tracker_22.csv", 2022)



## Filter respondents who had a deceased spouse
# I viewed the codebook (raw_data/codebook/H22S_R.txt)
# and found that SS052M and RS052M ("in which state or country did your partner die?"") captured all cases.
df_widow_22 <- df_widowdiv_22 %>%
    filter(!is.na(SS052M))
df_widow_20 <- df_widowdiv_20 %>%
    filter(!is.na(RS052M))
df_widow_18 <- df_widowdiv_18 %>%
    filter(!is.na(QS052M))
# check number of variables and cases
print(ncol(df_widow_22))
print(nrow(df_widow_22))

### MERGE 2020 into 2022 survey results ###
# Rename the columns in 2020 data to enable dataframe merging to latest wave
df_widow_renamed20 <- df_widow_20 %>%
    rename_with(~ gsub("^R", "S", .), starts_with("R"))
df_employ_renamed20 <- df_employ_20 %>%
    rename_with(~ gsub("^R", "S", .), starts_with("R"))

df_widow_renamed18 <- df_widow_18 %>%
    rename_with(~ gsub("^Q", "S", .), starts_with("Q"))
df_employ_renamed18 <- df_employ_18 %>%
    rename_with(~ gsub("^Q", "S", .), starts_with("Q"))

# Find the columns that are in df_widow_renamed20 but not in df_widow_22
diff_columns <- setdiff(names(df_widow_renamed20), names(df_widow_22))
print(diff_columns)

## Find respondent_ids that overlap between df_widow_renamed20 and df_widow_22
View(intersect(df_widow_renamed20$respondent_id, df_widow_22$respondent_id))
View(intersect(df_widow_renamed18$respondent_id, df_widow_22$respondent_id))
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

##### REPEAT the previous section FOR 2018 cases in df_widow #### 
common_ids <- intersect(df_widow_renamed18$respondent_id, df_widow_22$respondent_id)
df_widow_renamed18 <- df_widow_renamed20  %>% 
    filter(!(respondent_id %in% common_ids))
df_widow_22 <- df_widow_22  %>% 
    filter(!(respondent_id %in% common_ids))

# Merge widows for all three years
df_widow <- bind_rows(df_widow_22, df_widow_renamed20)
df_widow <- bind_rows(df_widow, df_widow_renamed18)
# bind_rows() contain all rows from both df_widow_22 and df_widow_renamed20,
# without splitting the common columns into .x and .y. 
## It also fills in NA values for columns that are not found in the other dataframe 

##### Left join df_widow with employment information
# in a way that privileges the latest employment info

# Find respondent_ids that overlap between df_employ of 3 years and the merged df_widow
common_ids_1 <- intersect(df_employ_22$respondent_id, df_widow$respondent_id) 
common_ids_2 <- intersect(df_employ_renamed20$respondent_id, df_widow$respondent_id) 
common_ids_2 <- intersect(df_employ_renamed18$respondent_id, df_widow$respondent_id) 

common_ids <- unique(c(common_ids_1, common_ids_2, common_ids_3))
length(common_ids)
# there's 828 cases in which employment data overlaps with widowhood data. 

# Merge df_widow and df_employ based on respondent ID
df_widow_em <- df_widow %>%
    left_join(df_employ_renamed18, by = "respondent_id") %>%
    left_join(df_employ_renamed20, by = "respondent_id") %>%
    left_join(df_employ_22, by = "respondent_id") %>%
    rename(Year = Year.x) %>% # Keep Year values from df_widow
    select(-Year.y) # Discard Year values from df_employ
# check number of variables after the merge
print(ncol(df_widow_em))
print(nrow(df_widow_em))

## Note: there may not be enough overlaps to justify merging employment and widowhood surveys.
# But let's keep it merged for now.

# Merge resulting dataframe with tracker
df_widow_tr_em <- df_widow_em %>%
    left_join(df_tracker, by = "respondent_id") %>%
    rename(Year = Year.x) %>% # Keep Year values from df_widow
    select(-Year.y)
# check number of variables after the merge
print(ncol(df_widow_tr_em))
print(nrow(df_widow_em))

# Write output: HRS data for widows, merged with employment survey and tracker.
# HRS survey years merged: 2020 and 2022
write.csv(df_widow_tr_em, file = "02_Merge/Output/HRS_widows_employ_tracker_merged.csv")
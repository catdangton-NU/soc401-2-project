library(yaml)
library(purrr) 
library(readr)

## This import code is for the 2016 .da files. 
## DID NOT WORK 
# //TODO 

# Set path to the data file "*.DA"
data.file <- "./raw_data/h16core/stata/H16S_R.da"

# Set path to the dictionary file "*.DCT"
dict.file <- "./raw_data/h16core/stata/H16S_R.dct"

# Read the dictionary file
df.dict <- read.table(dict.file, skip = 1, fill = TRUE, stringsAsFactors = FALSE)

# Set column names for dictionary dataframe
colnames(df.dict) <- c("col.num","col.type","col.name","col.width","col.lbl")

# Remove last row which only contains a closing }
df.dict <- df.dict[-nrow(df.dict),]

# Extract numeric value from column width field
df.dict$col.width <- as.integer(sapply(df.dict$col.width, gsub, pattern = "[^0-9\\.]", replacement = ""))

# Convert column types to format to be used with read_fwf function
df.dict$col.type <- sapply(df.dict$col.type, function(x) ifelse(x %in% c("int","byte","long"), "i", ifelse(x == "float", "n", ifelse(x == "double", "d", "c"))))

# Read the data file into a dataframe
df <- read_fwf(file = data.file, fwf_widths(widths = df.dict$col.width, col_names = df.dict$col.name), col_types = paste(df.dict$col.type, collapse = ""))

# Add column labels to headers
attributes(df)$variable.labels <- df.dict$col.lbl

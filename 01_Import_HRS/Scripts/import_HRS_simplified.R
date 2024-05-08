library(yaml)
library(haven) # for reading .dta files
library(purrr)

# Create a function that takes .dta files specified by config.yaml
process_data_files <- function(yaml_file, output_dir) {
  # Access variables from yaml file
  config <- yaml::yaml.load_file(yaml_file)
  
  # Read the data files into data frames
  data_frames <- map(config, haven::read_dta)
  
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # Delete existing files in the Output folder
  old_files <- list.files(output_dir, full.names = TRUE)
  file.remove(old_files)
  
  # Export the data frames as CSV files
  map2(data_frames, names(data_frames), function(df, name) {
    write.csv(df, file.path(output_dir, paste0(name, ".csv")), row.names = FALSE)
  })
}
## The yaml file should have a KEY that stands for the name of the csv, and a PATH to the original .dta file.
# example. widowhood_22: raw_data/hrs2022/stata/H22S_R.dta 

# Call function:
process_data_files("config.yaml", "01_Import_HRS/Output")

# The function should run on all keys specified in config.yaml and put all outputs into an Output folder.
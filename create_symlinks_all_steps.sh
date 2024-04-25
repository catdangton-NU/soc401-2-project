#!/bin/bash

# Define the root directory (project directory)
root_directory="/Users/catdangton/Library/CloudStorage/OneDrive-NorthwesternUniversity/Northwestern/Coursework/SPRING_2024/SOC_401_Categorical_Regression/Final_project_HRS_Spousal_death"

# Create an array of subdirectories
subdirs=("$root_directory"/*/)

# Get the length of the array
length=${#subdirs[@]}

# Loop through each subdirectory within the project directory
for ((i=0; i<$length-1; i++)); do
    # If the current subdirectory is raw_data, skip to the next iteration.
    # AKA exclude raw_data from the loop
    if [[ ${subdirs[$i]} == *"raw_data"* ]]; then
        continue
    fi

    # Check if the Output subfolder exists within the current subdirectory
    if [ -d "${subdirs[$i]}/Output" ]; then
        # Create symbolic links for all .csv files within the Output subfolder to the Input subfolder in the next directory
        mkdir -p "${subdirs[$i+1]}/Input"
        # Remove existing .csv files in the Input subfolder
        rm "${subdirs[$i+1]}/Input/"*.csv     
        # Create symbolic links
        ln -s "${subdirs[$i]}/Output/"*.csv "${subdirs[$i+1]}/Input"
    fi
done

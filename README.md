This repository contains all the statistical code I used to turn raw data from the Health and Retirement Survey into final paper outputs.

## How to navigate

### 1. Task folders

Each numbered folder (e.g. 01_Import_HRS, 02_Merge, 03_Clean) contains the Input, Scripts and Outputs for a single data analysis task.

To see my code, go to Scripts.

### 2. Homework files

This folder contains all Quarto documents and rendered paper outputs I created to complete course assignments. 

Each assignment is a subfolder. The project proposal and final project paper will be stored as subfolders here as well.

The input data for these documents are linked from the latest numbered task folder (e.g. 03_Clean). 

### 3. Other relevant scripts

config.yaml: Specifies the path to HRS survey sections that are imported to my project. The HRS breaks up surveys into many sections, each with its own .dta file. This script helps me quickly add or change the survey sections included in the data analysis pipeline.

renv and renv.lock: Specifies the R virtual environment for this project, including the R version and the packages used to run the code. This will come in handy in the future, when version/package updates can cause compatibility issues.

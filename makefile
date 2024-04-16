# Define paths relative to the location of the Makefile
WIDOWHOOD_SURVEY = raw_data/hrs2022/stata/H22S_R.dta
DEMOGRAPHICS_SURVEY = raw_data/hrs2022/stata/H22B_R.dta
RESPONDENT_TRACKER = raw_data/hrs2022/CW_Tracker/trk2020tr_r.dta

IMPORT_SCRIPT = 01_Import_HRS/Scripts/makefile_test_import_HRS.R

# Define the target to execute the R script and specify its dependencies
import_HRS:
	WIDOWHOOD_SURVEY="$(WIDOWHOOD_SURVEY)" \
	DEMOGRAPHICS_SURVEY="$(DEMOGRAPHICS_SURVEY)" \
	RESPONDENT_TRACKER="$(RESPONDENT_TRACKER)" \
	Rscript $(IMPORT_SCRIPT)

# Define target to clean up intermediate files
clean:
	rm -f intermediate_file.txt

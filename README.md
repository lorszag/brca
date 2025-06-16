Includes all of the analytics done on BRCA data from GHS & UKBB.

The first set of scripts create our case and control groups:
1. UKBB_BRCA1.Rmd
2. UKBB_BRCA2.Rmd
3. GHS_BRCA1.Rmd
4. GHS_BRCA2.Rmd
5. avengers.R: Pulls out avengers mutations
6. avengers_run.R: collect avengers mutations and add to dataframe
7. exclusion_ids.Rmd: create list of individuals to exclude from control group
8. dualmutations.Rmd: create list of individuals with >1 mutation in BRCA1 or BRCA2
9. filter_dataset.R: script to filter genome data for quality

Forest plots:
1. brca1_forestplots.Rmd
2. brca2_forestplots.Rmd
3. forest_plot.R: functions used in above files
4. forestplots_ukbb.Rmd: forest plots for fellows' symposium poster

Survival plots:
1. operations.Rmd: mutating operation data for use in censoring for survival plots
2. survival_analysis_brca1.Rmd
3. survival_analysis_brca2.Rmd
4. survival_plot.R: functions used in above files

Other:
1. case_data.Rmd: link case id #s to actual data
2. ghs_censoring_script.R: censoring example for Geisinger analysts


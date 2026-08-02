# Associations between per- and polyfluoroalkyl substance mixtures and all-cause mortality among U.S. adults: NHANES 1999–2018

This repository contains all the code necessary to reproduce the analysis in the above-named manuscript (Huerne, Rincón and Bird, 2026)

The very first step is downloading mortality data from the CDC website for the nine survey cycles relevant to this project, 1999-00 to 2017-18 (https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/). Note that for some reason, NHANES does not include PFAS data for 2001-02 cycle, so this cycle is excluded from the analysis. These data are not called directly to ensure this analysis will run if the URL above is altered or the mortality files are moved to a different location (i.e.future-proofing). ```Navigation.R``` specifies the survey cycles from which our exposure data are drawn.

If you wish to reproduce the analysis step by step, clone the repository to your local machine, then open ```.Rproj``` and go into the ```Code``` subfolder and run the scripts in this order:

## Data preparation and descriptive analysis
  - ```Functions.R``` defines the functions which are used to extract and format data presented throughout the document. Note that certain analysis-specific functions can be found in their respective analytical scripts.
  - In ```Initial_readin.R```, these user-defined functions are deployed to read data in across the nine survey cycles.
  - ```MASTER_Merge.R``` does what it says on the tin: namely it concatenates all the yearly data into a single dataframe and then wrangles/reformats certain variables as appropriate.
  - Next, the survey design object which accounts for NHANES weights, strata and clusters is completed in ```survey_design.R```.
  - Following which Table 1 (stratified by outcome) is produced and outputted as both counts and percentages to your local machine in ```Table1.R```.

## Statistical analysis 
NOTE: each script contains a defined function as appropriate
  - First, the unadjusted (i.e. crude) exposure-outcome analysis for each PFAS compound is conducted and then exported in ```Crude_analysis.R```.
  - Then, the initial multivariable analysis (complete case) for standard adjusted and interaction-adjusted models is conducted and exported in ```Completecase.R```.
  - To handle missingness, multiple imputation by chained equations (MICE) is performed. After exploring the data to confirm MAR, the missing data is imputed in ```MICE_setup.R```, and a new survey design is also created here to account for the newly-imputed data.
  - The actual MICE analysis for both model sets is performed in ```MICE_analysis.R```.
  - The script for producing the forest plot in Figure 2 is in the aptly-named ```Figure2.R```.

## Additional analysis
  - A pre-specified subgroup analysis was performed by gender to account for differences in PFAS elimination between men and women (and also pre-menopausal and post-menopausal) women in ```subgroup_gender.R```
  - Although we selected confounders based on the disjunctive cause criterion, there is some debate over whether certain diseases were in fact mediators. We therefore re-ran the complete case and MICE analysis for both model sets for all PFAS compounds with these potential mediators removed in ```Mediationanalysis.R```
  - A post-hoc analysis was carried out to assess the potential for unmeasured confounding via E-values, as well as the validity of incorporating interaction terms compared to a standard adjusted model via a multivariable Wald test in ```confounding.R```


Contact: Joshua Bird (joshua.bird@bccsu.ubc.ca) 

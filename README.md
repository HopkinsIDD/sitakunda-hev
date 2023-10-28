# Code and data for Sitakunda HEV serological cohort study

This repository contains the code and data associated with the manuscript _“Annual risk of hepatitis E virus infection and seroreversion: insights from a serological cohort in Sitakunda, Bangladesh”_. 

The data and code should be sufficient to reproduce analyses detailed in the manuscript and Supplementary Materials with the following exceptions:

* We have not included household GPS coordinates in the publicly available data, therefore the maps generated by `generate_figS1.R` and `generate_fig1map.R` and the semivariogram generated by “generate_figS2.R” will not be immediately reproducible. Please contact azman@jhu.edu and/or adighe1@jhmi.edu if you wish to access the location data.
* We have rounded age to the nearest year to protect date of birth information, so, if re-generated, results of age-binned analyses could vary slightly from those in the manuscript. 
* We omitted the date of self-reported jaundice and interview dates meaning that the number of seroconverters with self-reported jaundice during the study cannot be reproduced using this repository.
* Since some of the stanfit files from the catalytic model fitting are very large, we have included summaries of the fits which are sufficient to produce the parameter estimates presented in the manuscript. You will need to run the models on your own computer to generate the full posterior distributions for use in model fit comparison in `6_compare_fits.R` and to reproduce Figure 2 and Figure S8 using `generate_fig2.R` and `generate_figS8.R`.

## Structure of the repository:

You will need to load the library “here” to create the relative file paths needed to source this code on your own computer.
Packages used are within the `dependencies.R` file

### Data:
* The file `merged_clean.rds` contains the data collected through the serosurvey
* The files `reshaped_stanR1.rds` and `reshaped_stanR3.rds` contains the age-stratified seroprevalence data in the format required for catalytic model fitting in rstan
* The shapefiles folder contains the geographical shapefiles for spatial boundaries of Bangladesh at admin level 0-4 sourced from https://data.humdata.org/ and used to visualise the spatial data.

### Code:
* The folder rstan contains the catalytic model files written in stan programming language.
* All remaining code was written in R and is available in the R folder. This includes in-house functions, scripts used to analyse the data, and scripts used to generate the figures and tables in the manuscript.

### Generated data:
* This folder contains estimated seroprevalence, seroconversion and seroreversion generated from the empirical data on serostatus changes plus all data generated through the logistic regression catalytic model fitting.

### Tables and Figures:
* These folders contain the tables and figures produced for the manuscript and Supplementary Materials

If you have any questions please contact: adighe1@jhmi.edu and/or azman@jhu.edu 

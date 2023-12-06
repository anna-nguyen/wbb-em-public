# WASH Benefits Temperature and Precipitation Effect Modification

This repository contains replication scripts for the manuscript "Influence of temperature and precipitation on the effectiveness of water, sanitation, and handwashing interventions against childhood diarrheal disease in rural Bangladesh: a reanalysis of the WASH Benefits Bangladesh trial", currently available as a prepreint at https://www.medrxiv.org/content/10.1101/2022.09.25.22280229v2

## Directory Structure

**`00-run_analysis.sh`**: a bash script, when executed all analysis script is run to replicate study results

**`0-config.R` :** configuration file that sets data directories, sources base functions, and loads required libraries.

**`0-Utils` :** folder containing scripts for generating estimates and confidence intervals, functions to process results, and other general functions. 

**`1-data-processing` :** folder containing scripts to clean, process, and merge data from the WASH Benefits Bangaldesh trial and publically available weather datasets

**`2-fit-models` :** folder containing scripts for fitting models to continuous and categorical risk factors. 

**`3-analysis` :** folder containing scripts to clean raw WASH Bangaldesh data, save data objects used in analysis, and estimate intervention effects under different climate scenarios.

**`4-fig-tab` :** folder containing scripts to produce tables and plots.

**`figures` :** folder containing figure files. 
**`tables` :** folder containing table files.
**`results` :** folder containing various analysis result objects.
**`renv` :** folder containing materials for renv, a package version control tool

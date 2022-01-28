# Prediction of Early Periprosthetic Joint Infection after Total Hip Arthroplasty
Prediction models for prosthesis joint infection (PJI) within 90 days of total hip arthsoplasty (THA).

The file structure follows the "ProjectTemplate" structure described here: http://projecttemplate.net/index.html
 
`data/` defines what data to use for the project (no actual patient data included due to GDPR, as well as Swedish and Danish laws and regulations).  
`../linkage.R` defines the relevant variables from  the Swedish registers (requires an active connection to an internal SQL-database which is not shared). Note that the differenc co-morbidity measures (Charlson, Elixhauser and Rx Risk V have been pre-calculated and are already available from the SQL database.)  
`../categorization.xlsx` defines our grouping of co-morbidities based on individual conditiond from the Charlson, Elixhauser and Rx Risk V clasifications.   
`munge/` Data munging steps performed on the "raw data sets"  
`../00-filter.R` Applies inclusion criteria (and produce a flowchart "on the fly")  
`../01-outcome.R` Identify patients with PJI within 90 days. This is based on relevant ICD-10 and NOMESCO codes recorded at hospital visits during the the year before surgery, or if reoperation was recorded to SHAR due to PJI.  
`../02-survdata.R` The filename is missleading but here is the place for additional variable transformation. BMI is categorized according to the WHO classification, individual diagnoses are grouped and factor levels are translated from Swedish to English etc.  
`../03-compositvariabler.R` We here use the data/categorization.xlsx file mentioned above to construct the variables for co-morbidity (as well as a table to present those groups).  
`../04-remove_empty_variables.R` Here we identify variables (including dummy) with less than 10 observations for each positive or negative outcome (PJI or not within 90 days). Too rare conditions are droped and not included as potential predictors.  
`../05-BRLasso.R` is for variable selection, which requires some additional help functions etc from  the lib-folder.  
`../06-variables_differnet_BRLasso_models.R` extract the selected variables to be used in the "main" and "reduced" models.  
`../07-compare_models.R` estimate AUC-values and data for ROC-curves etc, botth for the derived models, as well as for simpler comparisons.   
`src/` contains scripts to make either figures (exported to the graphs-folder) or tables (not included) for later use in the manuscript.  
`/reports` contains files for the submitted manuscript.  

The `config/`-folder, `.gitignore`-file and the `.Rproj`-file contains process configurations for ProjectTemplate, Git and RStudio.

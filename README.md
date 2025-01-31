# Modelling farmer resilience to rainfall shocks in NSW
 Repository for the simulation model and ABS datalab econoemtric analysis for the project titled *Modelling farmer resilience to rainfall shocks in NSW*. This project is an output of the ARC linkage grant titled Innovations in Agricultural Greenhouse Gas Management and Policy. 

Access to the simulation model ShinyApp can be found through this link: https://daniel-jw-hill.shinyapps.io/FarmPerformanceSimulations/

Contained in this directory:
 - app.R - the head script for the simulation model shinyapp. Accesses functions from the utilities folder. 
 - SA4_boundaries.Rdata - multipolygon boundaries for study regions, derived from Australian Bureau of Statistics Statistical Area Levels Three and Four.
 - utilities - Contains all functions required to run the simulation shinyapp.
 - Regression_parameters - Results from the Econometrics_scripts, and contains all marginal elasticities for the input demand functions and revenue functions simulated in the simulation model.
 - GAMs_Data - Contains simulated fitted values from the GAMs models estimated in the Econometrics_scripts. The predicted values represent the unexplained variation in mean expenditure per ha, stock value per ha, or revenue per ha given realisations of precicipitation over a four year period. 
 - Econometrics_scripts - Contains all replication scripts for the econometric analysis, with the exported results saved in the Regression_parameters and GAMs_Data folders. These scripts can only be executed within the ABS Datalab environment. More details on how to access this environment can be found here https://www.abs.gov.au/statistics/microdata-tablebuilder/datalab/. * Note - scripts are currently under review by the ABS Datalab team for export.*. 

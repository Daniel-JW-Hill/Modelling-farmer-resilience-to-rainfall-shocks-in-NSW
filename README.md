# Modelling farmer resilience to rainfall shocks in NSW

Repository for the simulation model and ABS datalab econometric analysis for the project titled *Modelling farmer resilience to rainfall shocks in NSW*. This project is an output of the ARC linkage grant titled Innovations in Agricultural Greenhouse Gas Management and Policy.



Access to the simulation model ShinyApp can be found through this link: https://daniel-jw-hill.shinyapps.io/FarmPerformanceSimulations/

Contained in this directory:

* app.R - the head script for the simulation model shinyapp. Accesses functions from the utilities folder.
* SA4\_boundaries.Rdata - multipolygon boundaries for study regions, derived from Australian Bureau of Statistics Statistical Area Levels Three and Four.
* Utilities - Contains all functions required to run the simulation shinyapp.
* Regression\_parameters - Results from the Econometrics\_scripts, and contains all marginal elasticities for the input demand functions and revenue functions simulated in the simulation model.
* GAMs\_Data - Contains simulated fitted values from the GAMs models estimated in the Econometrics\_scripts. The predicted values represent the unexplained variation in mean expenditure per ha, stock value per ha, or revenue per ha given realisations of precipitation over a four year period.
* Econometrics\_scripts - Contains all replication scripts for the econometric analysis, with the exported results saved in the Regression\_parameters and GAMs\_Data folders. These scripts can only be executed within the ABS Datalab environment. More details on how to access this environment can be found here https://www.abs.gov.au/statistics/microdata-tablebuilder/datalab/.



The simulation model takes rainfall outcomes for 6 years (4 lags, current, and one expectation), and sequentially models changes in log stock value, expenditure, and revenue (gross margins) per ha over a defined time period. The model uses coefficients/elasticities from the Arellano Bond Estimators from the econometric analysis to recursively estimate outcomes. The econometric analysis assumed the following decision pathway from the farmer: a) opening stock value decisions are made at the start of the (financial) year, observing revenue, expenditure, stock, rainfall, and terms of trade values in the previous year, while the choice is conditional on asset levels; b) expenditure decisions are then made observing these same past outcomes, and conditional on the stocking decision made earlier; and c) revenue outcomes are conditional on these input decisions, and is also influenced by prevailing rainfall in that year.



The simulation model follows this same logic for each year, with the first calibration year (t-4) defining just a revenue shock, with stock and expenditure decisions reacting in the following year. This carries through recursively over all years.



Confidence intervals are also modelled for the outcomes. Confidence intervals were estimated by taking independent samples of each model coefficient across the three Arellano-Bond estimator results (500 for each coefficient), and re-simulating outcomes. These intervals assume independence between coefficients, and do not capture uncertainty from the rainfall index model (estimated via GAMs). This was due to limitations in what could be exported from the ABS datalab.



Changes, in percentage terms, are presented relative to 'normal' conditions, where the SPI index is equal to 0 (the long run mean rainfall) for all periods. Changes are also presented in AUD, obtained by translating the Arellano–Bond log-change estimates into level effects using the median revenues, expenditure, assets, and stock value per hectare (which are assumed to be representative of long run equilibrium values for representative farms in each region). 


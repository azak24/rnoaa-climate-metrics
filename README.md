# rnoaa-climate-metrics
Pulling weather station data from rnoaa to calculate metrics of winter climate in Pennsylvania for 1994-2004 and 2013-2023.

Stations with relevant climate data from 1994-2004 and 2013-2023 were identified, then climate data for these years and stations were pulled. Weather station data were used to calculate four climate metrics: Average annual snowfall, degree days below freezing, number of days with snow cover, and number of days with maximum temperature below freezing. Point data are exported for later interpolation in ESRI ArcGIS Pro. Final climate surfaces were used as covariates in snowshoe hare occupancy model.

Script 1 (rnoaa-datapull): This script pulls the weather station data and saves it in .RData format for use in script 2. This code is provided as documentation but does not need to be run; it takes a very long time to run, the rnoaa package may not function correctly as of 2022, and necessary output is provided in the "data" folder.

Script 2 (rnoaa-calculate-metrics): This script uses the .RData weather station data output from script 1 to calculate desired climate metrics.

Note: rnoaa package is to be archived; replacement tba (https://github.com/ropensci/rnoaa). It is still functional, but data stops at 2022, and future usability is not assured.

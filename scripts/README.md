# About APP/scripts Folder
This folder contains scripts that we used to clean and analyze all our data. Outputs are stored in APP/cleaned.

## About OD_by_NCs_script.R
This script cleans the LADOT Data in https://github.com/kaye2929/APP/tree/main/data/LADOT/CPRA%20%2322-10589%20Data.
The script completes the following items:
- Pivoted the data from wide to long form
- Append the 4 workbooks and every sheet inside 
- Added `month` column to indicate specific month-year
- Added geometries for each possible origin-destination pairing of Neighborhood Councils 

Output is located APP/output/files.

## About Create_NCgeo_Reference.R
There are two Neighborhood Council shapefiles. Both of them contain 99 observations, which is the number of NCs currently in LA. The old files were downloaded from LA GeoHub on November 7, 2022 from https://geohub.lacity.org/datasets/lahub::neighborhood-councils-certified/about. However, some of the NC names were updated since then, so we downloaded the .shp files again on 1/4/2023. 
We created a reference file (.shp and .csv) to connect the NC names from both .shp files and the names given in the data provided from LADOT. `nc_id` is the primary key between the different names. The script `Create_NCgeo_Reference.R` includes the code and documentation for creating the reference file.

## About Get_ACS2021_Demos.R
Get_ACS2021_Demos.R is a script that pulls Census Bureau's American Community Survey data for the 2021 5-Year Estimates. 
We obtained ACS data via the Census Bureau API for the following demographics:
- Total Population (B02001)
- Population by Race (B02001)
- Poverty Levels (S1701)

The resulting .geojson file contains census tract level data for all the tables above for the County of Los Angeles. The mapping projects files aggregating the tract level data to neighborhood council level demographics.

## About penalty_data_cleaning.R
This script is used to reorganize the LA311 requests received from LADOT. The data can be found at the following link:
https://github.com/kaye2929/APP/blob/main/data/UCLA%20Data%20Request%20Dockless%20Violations%20MyLA311.xlsx
The script completes the following items:
- Matched the name of each Neighborhood Councils with the NC name in output/files/NCZone_GeoRef_noSOZ.geojson 
- Append the 4 workbooks and every sheet inside 
- Plotted the figures for the APP report

## About api_cleaning.ipynb
We developed this script to examine the pattern of scooter deployment over the API collection period. We analyzed the deployment trends of two operators, namely Bird and Wheels. The data for these two operators can be found at the following link:
https://github.com/kaye2929/APP/tree/main/data/API_scooter_locations

## About vehicle_deployment_cleaning.R
We created this script for combining [Vehicle Deployment Neighborhood Council Districts Data](https://github.com/kaye2929/APP/blob/main/data/CPRA%20%2322-10589%20Data/Vehicle%20Deployment.%20Neighborhood%20Council%20Districts.xlsx) to one sheet (both long and wide formats)

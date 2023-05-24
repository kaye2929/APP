# About this Folder
This folder contains scripts that we used to clean and analyze all our data. Outputs are stored in [APP/output/files](../output/files).

## About API_TotDepl.R
This script aggregates and summarizes the point-level [Bird and Wheels data](../data/API_scooter_locations).
The script completes these items:
- Count the number of vehicles (points) in each Neighborhood Council (polygon).
- Summarize daily total deployment
- Calculate the daily percent of the total fleet in EFMDDs
- Plot data

## About Corr_Check_res.Rmd
This script calculates the Spearman's Rho and Kendall's Tau to evaluate the correlation between deployment, trips, and penalties.


## About Get_ACS2021_Demos.R
This script calls the ACS 2021 5-Year Estimates (2017-2021) for the County of Los Angeles. We obtained total population and total population over 18 years old from table B01001. 
The script completes the following items:
- Call Table B01001 at the census block group level for LA County
- Interpolate the population into the 99 NCs and the 3 SOZs.


## About Make_Depl_Figures.R
This scripts makes the charts and plots for deployment data. PNG outputs are located [here](../output/plots). SVG outputs are located [here](../output/plots_svg).
Example plots include:
- Average Deployment by SFV vs non-SFV NCs
- Average Deployment by the 4 Program Geos (SOZ, EFMDD, MDD, SPD)
- Subset the plot for a specific time period: Pilot Program, Current Program, around April 2021.
- Top 3 NCs with highest deployment by SFV/non-SFV and 4 Program Geos


## About Make_Mapping_Data.R
This script summarizes deployment during the Pilot and Current Program. The output was used to make deployment and trip maps. Joined the NC geometries for mapping.


## About Make_NCZone_GeoRef.R
This script creates the cross reference file between NCs and the 4 program geographies. We download [the Neighborhood Council shapefiles from LA GeoHub](https://geohub.lacity.org/datasets/lahub::neighborhood-councils-certified/about). 
This script completes the following items:
- Match the NC names between the GeoHub and CPRA data
- Append three observations for the 3 SOZs


## About Make_Trip_Figures.R
This scripts makes the charts and plots for trip data. PNG outputs are located [here](../output/plots). SVG outputs are located [here](../output/plots_svg).
Example plots include:
- Average Trips by SFV vs non-SFV NCs
- Average Trips by the 4 Program Geos (SOZ, EFMDD, MDD, SPD)
- Total Trips by SFV vs non-SFV NCs
- Total Trips by the 4 Program Geos (SOZ, EFMDD, MDD, SPD)
- Subset the plot for a specific time period: Pilot Program, Current Program, around April 2021.
- Top 3 NCs with highest trips by SFV/non-SFV and 4 Program Geos


## About OD_by_NCs_script.R
This script cleans [the LADOT CPRA Data](../data/CPRA #22-10589 Data).
The script completes the following items:
- Pivoted the data from wide to long form
- Append the 4 workbooks (2019, 2020, 2021, 2022 Neighborhood Council Trip Matrix) and every sheet inside 
- Added `month` column to indicate specific month-year
- Added geometries for each possible origin-destination pairing of Neighborhood Councils 


## About Plot_Operator_TripCosts.R
This scripts makes two charts for two datasets. PNG outputs are located [here](../output/plots). SVG outputs are located [here](../output/plots_svg).
The plots include:
- Trip Cost by Number of Operators
- Scooter Price Elasticity Estimates


## About penalty_data_cleaning.R
This script is used to reorganize the LA311 requests received from LADOT. The data can be found at the following link:
https://github.com/kaye2929/APP/blob/main/data/UCLA%20Data%20Request%20Dockless%20Violations%20MyLA311.xlsx
The script completes the following items:
- Matched the name of each Neighborhood Councils with the NC name in output/files/NCZone_GeoRef_noSOZ.geojson 
- Append the 4 workbooks and every sheet inside 
- Plotted the figures for the APP report


## About api_cleaning.ipynb
We developed this script to examine the pattern of scooter deployment over the API collection period. We analyzed the deployment trends of two operators, namely Bird and Wheels. The data is located [here](../data/API_scooter_locations)


## About vehicle_deployment_cleaning.R
We created this script for combining [Vehicle Deployment Neighborhood Council Districts Data](https://github.com/kaye2929/APP/blob/main/data/CPRA%20%2322-10589%20Data/Vehicle%20Deployment.%20Neighborhood%20Council%20Districts.xlsx) to one sheet (both long and wide formats)

# About Geos
## Neighborhood Councils
There are two Neighborhood Council shapefiles. Both of them contain 99 observations, which is the number of NCs currently in LA. The old files were downloaded from LA GeoHub on November 7, 2022 from https://geohub.lacity.org/datasets/lahub::neighborhood-councils-certified/about. However, some of the NC names were updated since then, so we downloaded the .shp files again on 1/4/2023. 
We created a reference file (.shp and .csv) to connect the NC names from both .shp files and the names given in the data provided from LADOT. `nc_id` is the primary key between the different names. The script `Create_NCgeo_Reference.R` includes the code and documentation for creating the reference file.

## Streets
This .shp file has the street network of the City of Los Angeles. The file was downloaded from (link) on DATE.

## ACS Data
...

## tl_2021_06_tract
...

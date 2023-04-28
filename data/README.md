# About APP/data Folder
This folder contains original data from our client LADOT, from ACS, from LA GeoHub, and others. All datasets include information about the data source, date accessed, and uses of the data.

## About API_scooter_locations
This folder contains vehicle point datasets provided openly by dockless mobility firms. The data adheres to the General Bikeshare Feed Specification ([GBFS](https://github.com/MobilityData/gbfs)), a format maintained by the North American Bikeshare Association ([NABSA](https://nabsa.net)). We scraped the APIs in Python from January 18, 2023, to February 16, 2023. Only data from Bird and Wheels were available through the application. The information was updated continuously, and we collected the scooter locations hourly. For the Wheels data, we began the scrape on January 23, 2023, but ended at the same time as the Bird scrape.
- Bird real-time vehicle status URL: https://mds.bird.co/gbfs/v2/public/los-angeles/free_bike_status.json
- Wheels real-time vehicle status URL: https://la-gbfs.getwheelsapp.com/free_bike_status.json

## About CPRA #22-10589 Data
LADOT data was shared with our team via CPRA in November 10, 2022 by Elizabeth Winter from the LADOT For-Hire Policy and Enforcement Division. Data contains the following information:
- Origin-Destination Trip count by LA Neighborhood Councils for 2019-2022
- .geojson files for various transporation planning areas
- Vehicle Deployment Average count by LA Neighborhood Councils

Modifications to data since it was first shared:
From EW's email to us on 12/28/2022,
Cell O28 on "2019 Neighborhood Council Trip Matrix" workbook was missing a value. EW informed us that the trip count for cell O28 is 11.
The old version of the workbook was retitled to "2019 Neighborhood Council Trip Matrix - missing data.xlsx"
The new version reflecting EW's 12/28 edit is titled "2019 Neighborhood Council Trip Matrix.xlsx"

We also generated summary stats located in `/output/files` based on LADOT data before we cleaned the workbooks. These summary stats match with the cleaned data.

## Neighborhood Councils
There are two Neighborhood Council shapefiles. Both of them contain 99 observations, which is the number of NCs currently in LA. The old files were downloaded from LA GeoHub on November 7, 2022 from https://geohub.lacity.org/datasets/lahub::neighborhood-councils-certified/about. However, some of the NC names were updated since then, so we downloaded the .shp files again on 1/4/2023. 
We created a reference file (.shp and .csv) to connect the NC names from both .shp files and the names given in the data provided from LADOT. `nc_id` is the primary key between the different names. The script `Create_NCgeo_Reference.R` includes the code and documentation for creating the reference file.

## LA Street Network
This .shp file has the street network of the City of Los Angeles. The file was downloaded from (link) on DATE.

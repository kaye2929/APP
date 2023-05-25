# About this Folder
This folder contains original data from our client LADOT, from ACS, from LA GeoHub, and others. All datasets include information about the data source, date accessed, and uses of the data.

## About API_scooter_locations
This folder contains vehicle point datasets provided openly by dockless mobility firms. The data adheres to the General Bikeshare Feed Specification ([GBFS](https://github.com/MobilityData/gbfs)), a format maintained by the North American Bikeshare Association ([NABSA](https://nabsa.net)). We scraped the APIs in Python from January 18, 2023, to February 16, 2023. Only data from Bird and Wheels were available through the application. The information was updated continuously, and we collected the scooter locations hourly. For the Wheels data, we began the scrape on January 23, 2023, but ended at the same time as the Bird scrape.
- Bird real-time vehicle status URL: https://mds.bird.co/gbfs/v2/public/los-angeles/free_bike_status.json
- Wheels real-time vehicle status URL: https://la-gbfs.getwheelsapp.com/free_bike_status.json


## About CPRA #22-10589 Data
LADOT data was shared with our team via CPRA in November 10, 2022 by Elizabeth Winter from the LADOT For-Hire Policy and Enforcement Division. Data contains the following information:
- Origin-Destination Trip count by LA Neighborhood Councils for 2019-2022
- GeoJSON files for the four program geographies: EFMDD, MDD, SOZ, SPD
- Vehicle Deployment Average count by LA Neighborhood Councils


Modifications to data since it was first shared:
From EW's email to us on 12/28/2022,
Cell O28 on "2019 Neighborhood Council Trip Matrix" workbook was missing a value. EW informed us that the trip count for cell O28 is 11.
The old version of the workbook was retitled to "2019 Neighborhood Council Trip Matrix - missing data.xlsx"
The new version reflecting EW's 12/28 edit is titled "2019 Neighborhood Council Trip Matrix.xlsx"

We also generated summary stats located in `/output/files` based on LADOT data before we cleaned the workbooks. These summary stats match the cleaned data.


## About Neighborhood Councils (Certified)
These shapefiles were downloaded on 01/04/2023 from LA GeoHub at [this link](https://geohub.lacity.org/datasets/lahub::neighborhood-councils-certified/about). There are currently 99 councils. These files were used to make map figures.


## About Operator Size, Trip, and Price
This Excel sheet contains trip price, average trip speed, and number of operators for a group of cities in the US. We collected the trip prices by viewing the per minute fee on each micromobility operator's app. We collected the average trip speed from the [Global Micromobility Dashboard](https://public.ridereport.com/). We collected the number of operators by searching on city's websites and public documents.


## About UCLA Data Request Dockless Violations MyLA311
This Excel sheet contains MyLA311 dockless vehicle requests and violations. This data was obtained via CPRA from LADOT, which they shared to us on 03/07/2023. Each observation is a violation in a Neighborhood Council with the violation type.

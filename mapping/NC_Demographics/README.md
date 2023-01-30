# Map Making Steps for Demographics by NCs

Masa
As of 2023.01.04
## Preliminary Steps
1.	Obtain Neighborhood Council (NC) data from Neighborhood Councils (Certified) | City of Los Angeles Hub (lacity.org)
2.	Obtain Census Tract Shapefiles from TIGER/Line® Shapefiles (census.gov) Year from 2021.
3.	Make a new field in NC and Census Tract that shows the area of each NC and Census Tract in square miles.

## Race per NC
1.	Obtain CSV data from B02001: RACE - Census Bureau Table. For the year 2021. (Obtained as of Jan 4th, 2023)
2.	Clean the CSV data
(1)	Delete all columns except B02001_001E (Estimate!!Total:), B02001_002E (Estimate!!Total:!!White alone), B02001_003E (Estimate!!Total:!!Black or African American alone), B02001_004E (Estimate!!Total:!!American Indian and Alaska Native alone), B02001_005E (Estimate!!Total:!!Asian alone), B02001_006E (Estimate!!Total:!!Native Hawaiian and Other Pacific Islander alone), B02001_007E (Estimate!!Total:!!Some other race alone), B02001_008E (Estimate!!Total:!!Two or more races:)
(2)	Rename variables, delete second row
(3)	By using GEO_ID variable, generate 11-digit census tract (TractID) variable
(4)	Save into xls file
3.	Install NC data and Census Tract Shapefile into Contents pane on ArcGIS
4.	Tabular join the XLS file to Census Tract
5.	Spatial join the Census Tract to NC. The Merge Rule for 
(1)	TractSqMile is SUM
(2)	Total Population, White Population, Black Population, Native American Population, Asian Population, Hawaiian Population, Other Race Population, Multiple Race Population are SUM
6.	Estimate the Total, White, Black, Native American, Asian, Hawaiian, Other Race, Multiple Race Population by prorating according to area (for details contact Masa)
(1)	Example image
 ![image](https://user-images.githubusercontent.com/110483064/211176507-ede16965-6984-448d-ba67-6d073430146c.png)

7.	Derive the population proportion for each race in each NC (for details contact Masa)
8.	Turn symbology for NC to graduated colors. Set the Field to whatever race population proportion.


## Median Household Income per NC
1.	Obtain CSV data from S1903: MEDIAN INCOME IN THE PAST 12 ... - Census Bureau Table. For the year 2021. (Obtained as of Jan 4th, 2023)
2.	Clean the CSV data
(1)	Delete all columns except S1903_C01_001E (Estimate!!Number!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households) and S1903_C03_001E (Estimate!!Median income (dollars)!!HOUSEHOLD INCOME BY RACE AND HISPANIC OR LATINO ORIGIN OF HOUSEHOLDER!!Households)
(2)	Rename variables, delete second row
(3)	By using GEO_ID variable, generate 11-digit census tract (TractID) variable
(4)	Save into xls file
3.	Install NC data and Census Tract Shapefile into Contents pane on ArcGIS
4.	Tabular join the XLS file to Census Tract
5.	Spatial join the Census Tract to NC. The Merge Rule for 
(1)	TractSqMile is SUM
(2)	Total Households is SUM
(3)	Median Income is MEAN (TBD)
6.	Turn symbology for NC to graduated colors. Set the Field to Median Income.

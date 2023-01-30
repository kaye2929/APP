# Obtain ACS Data for County of Los Angeles via Census Bureau API
# Background ########
## This script calls the ACS 2021 5-Year Estimates (2017-2021) for the County of Los Angeles. We obtained demographic data to better understand the population where scooters could be deployed. 

## The ACS 2021 5-Year Estimates covers the majority of the date range of our data which spans from 2019 to 2022. We assume that the population is not much different in 2022 compared to the 2021 5-Year Estimates. 

# Set Up ########
library(tidycensus)
library(tidyverse)
library(sf)

fileName = "Learning/myAPIkey.txt" # change to your own API key
APIkey = readChar(fileName, file.info(fileName)$size)
census_api_key(paste(APIkey)) 


# Get Variable Names ########

## Obtain all the variable names from detailed and subject tables
var_names_dtld = tidycensus::load_variables(year = 2021, dataset = "acs5") %>% 
  select(name, label)
var_names_subj = tidycensus::load_variables(year = 2021, dataset = "acs5/subject") %>% 
  select(name, label)

var_names = bind_rows(var_names_dtld, var_names_subj)

rm(var_names_dtld, var_names_subj)


## Keep the names of the variables of interest
var_names_keep = var_names %>% 
  filter(name %in% c(
    ## Identify variables of interest
    
    ### from Detailed Table B02001
    "B02001_001",
    "B02001_002",
    "B02001_003",
    "B02001_004",
    "B02001_005",
    "B02001_006",
    "B02001_007",
    "B02001_008",
    
    ### from Subject Table S1701
    "S1701_C01_001",
    "S1701_C02_001",
    "S1701_C01_038"
    
    ### add variables here
    
  )) %>% 
  
  ## Recode variable names
  mutate(var_name = recode(name,
           # from Detailed Table B02001
           "B02001_001" = "Total Population",
           "B02001_002" = "Total White alone",
           "B02001_003" = "Total Black alone",
           "B02001_004" = "Total AIAN alone",
           "B02001_005" = "Total Asian alone",
           "B02001_006" = "Total NHPI alone",
           "B02001_007" = "Total Other Race alone",
           "B02001_008" = "Total Two or More Races",
           
           # from Subject Table S1701
           "S1701_C01_001" = "Total Population for Poverty",
           "S1701_C02_001" = "Total Below 100% Poverty",
           "S1701_C01_038" = "Total Below 50% Poverty"
           
           # add renames here
           
         ))
rm(var_names)

# Get Tables ########
tables = c("B02001","S1701")
acs_data = NULL
for (tbl in tables) {
  output = tidycensus::get_acs(geography = "tract", 
                                  table = tbl,
                                  state = "CA",
                                  county = "Los Angeles",
                                  geometry = FALSE,
                                  year = 2021,
                                  survey = "acs5") %>% 
    mutate(geo_name = gsub(",.*","", NAME)) %>% 
    select(GEOID, geo_name, variable:moe)
  
  acs_data = acs_data %>% bind_rows(output)
}

rm(output, tbl, tables)

# Clean Tables ########
## Filter for the variables of interest
acs_data_keep = acs_data %>% 
  filter(variable %in% var_names_keep$name) %>% 
  
  ## Join variable renames
  left_join(var_names_keep, by = c("variable" = "name")) %>% 
  select(-variable, -label) %>% 
  
  ## Pivot wider. Each row is a Census Tract
  rename(est = estimate) %>% 
  pivot_wider(
    id_cols = c("GEOID", "geo_name"),
    names_from = c("var_name"),
    values_from = c("est", "moe"),
    names_glue = "{var_name} {.value}"
  )

rm(acs_data)


# Get & Join Geos ########
## Use any table to obtain the geos
geos = tidycensus::get_acs(geography = "tract", 
                             table = "B02001",
                             state = "CA",
                             county = "Los Angeles",
                             geometry = TRUE,
                             year = 2021,
                             survey = "acs5") %>% 
  
  ## Keep only the distinct Census Tracts
  select(GEOID, geometry) %>% 
  distinct() 

## Join geos
acs_data_keep = acs_data_keep %>% 
  left_join(geos, by = "GEOID")

rm(geos)


# Export Shapefiles ########
sf::st_write(acs_data_keep, "APP/APP/data/demos/2021_Demos_LACounty.geojson")
# race poverty income homeownership vehicles edu age empl
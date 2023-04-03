# Author: Abraham Cheung

# Obtain Population Data for County of Los Angeles via Census Bureau API

# Background ########
## This script calls the ACS 2021 5-Year Estimates (2017-2021) for the County of Los Angeles. We obtained total population and total population over 18yo. This population data helps us to know the average scooters deployed and ridden per capita. 

## The ACS 2021 5-Year Estimates covers the majority of the date range of our data which spans from 2019 to 2022. We assume that the population is not much different in 2022 compared to the 2021 5-Year Estimates. 

# Set Up ########
## Packages
pacman::p_load("tidyverse","sf","censusapi","tigris","areal")

## directory paths
output_path <- file.path('.','output','files')

## Census API Key
fileName = "../../Keys/myAPIkey.txt" # change to your own API key
APIkey = readChar(fileName, file.info(fileName)$size)
# Add key to .Renviron
Sys.setenv(CENSUS_KEY=paste(APIkey))
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")


# Get ACS variable list ####
## Get list of APIs that we can call
# apis <- listCensusApis()
# apis %>% 
#   filter(grepl("acs5", name),vintage%in%2021) %>% 
#   select(name,url)

## Find the variables that we want from the Detailed Tables in ACS 5-Year Estimates
group_B01001 <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2021,
  type = "variables",
  group = "B01001")

## Transform variable names
group_B01001 = group_B01001 %>% 
  filter(grepl("^Estimate!!",label)) %>% # search for only Estimates. we do not need MOEs.
  mutate(gender = str_split(label,"!!",simplify = TRUE)[,3],
         age_range = str_split(label,"!!",simplify = TRUE)[,4], # to ID which age is above and beloww 18, extract the digits from the ACS label
         age_low = str_extract(age_range,"[:digit:]{2}"),
         age_over18 = case_when(
           as.double(age_low)>=18 ~ TRUE,
           is.na(age_low) ~ FALSE, # NA as FALSE
           TRUE ~ FALSE) # ages below 18 coded as FALSE
  ) %>% 
  glimpse()

## Subset variable names of interest
group_B01001_subset <-
  group_B01001 %>% 
  filter(gender=="" | age_over18==TRUE) %>% # first argument selects the total population (no gender name in label). second argument selects the variables for over 18yo
  arrange(name) %>% 
  glimpse()

# Get data via API ####
acs_pop <- getCensus(
  name = "acs/acs5",
  vintage = 2021, 
  vars = c("NAME",unlist(group_B01001_subset$name)), 
  region = "block group:*", # calling block group to have more precise aggregation to NC level for population
  regionin = "county:037&state:06",
  show_call = TRUE)

## Rename the population estimate variable
acs_pop =
  acs_pop %>% 
  rename(pop_est = B01001_001E)

## Sum to find Total Population over 18
acs_pop = acs_pop %>% 
  mutate(pop_over18_est = rowSums(acs_pop %>% 
                                    select(contains("B01001"))),
         GEOID = str_c(state,county,tract,block_group)) %>% # construct GEOID
  select(NAME,pop_est,pop_over18_est,GEOID) %>% 
  glimpse()


# Read in TIGRIS files ####
LAblocks <- block_groups(state = 06, county = 037, year = 2021) %>% 
  st_transform(26945)
# Set Coordinate Reference System to 26945, based on `https://www.conservation.ca.gov/cgs/rgm/state-plane-coordinate-system` and `https://spatialreference.org/ref/epsg/26945/`


# Join tigris and ACS data ####
LApop_bg <-
  LAblocks %>% 
  select(GEOID,geometry) %>% 
  inner_join(acs_pop, by="GEOID") %>%  
  glimpse()


# Bring in NC geos ####
load(file.path(output_path,"NCZone_GeoRef.RData"))
## Set CRS
nc_georef_noSOZ <- nc_georef_noSOZ %>% 
  st_transform(26945)

nc_georef_wSOZ <- nc_georef_wSOZ %>% 
  st_transform(26945)

## Exploratory Analysis
## Filtering for BG in LA
# la_bg <- st_join(LApop_bg, nc_georef_noSOZ,
#                  join = st_intersects, left=FALSE)

## These are all the BG that overlap with a NC in the City of LA
# ggplot() + 
#   geom_sf(data = la_bg, fill = "white", color = "grey") + 
#   geom_sf(data = nc_georef_noSOZ, fill = NA, color = "red") + 
#   theme_void()


# Area-weighted interpolation ####
## for NC without SOZs
NC_noSOZ_pop <- areal::aw_interpolate(nc_georef_noSOZ, tid = NC_ID, 
                           source = LApop_bg, sid = GEOID,
                           weight = "total", output = "sf", 
                           extensive = c("pop_est", "pop_over18_est"))

## for NC with SOZs
## to calculate the population living within the SOZ, we need to assign NC_ID, which we will remove later on since SOZs are not NCs
nc_georef_wSOZ$NC_ID[nc_georef_wSOZ$data_nc_name %in% "Venice Ocean Front Walk"] <- 1001
nc_georef_wSOZ$NC_ID[nc_georef_wSOZ$data_nc_name %in% "Hollywood Walk of Fame"] <- 1002
nc_georef_wSOZ$NC_ID[nc_georef_wSOZ$data_nc_name %in% "Downtown LA"] <- 1003

## interpolate the population
NC_wSOZ_pop <- areal::aw_interpolate(nc_georef_wSOZ, tid = NC_ID, 
                                      source = LApop_bg, sid = GEOID,
                                      weight = "total", output = "sf", 
                                      extensive = c("pop_est", "pop_over18_est"))


## remove the fake NC_IDs
NC_wSOZ_pop$NC_ID[NC_wSOZ_pop$NC_ID < 0] <- NA


# Export GeoJSON ####
# save(NC_noSOZ_pop,NC_wSOZ_pop,file=file.path(output_path,"NCZone_GeoRef.RData"))
# 
# st_write(NC_noSOZ_pop,
#          file.path(output_path, 'NCZone_GeoRef_noSOZ.geojson'),
#          delete_dsn = TRUE)
# 
# st_write(NC_wSOZ_pop,
#          file.path(output_path, 'NCZone_GeoRef_wSOZ.geojson'),
#          delete_dsn = TRUE)

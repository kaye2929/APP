################ Clean NC Geos ###
## Set Up ########
## Packages
pacman::p_load("tidyverse","readxl","sf","ggsflabel","units","gghighlight")

## File Directories
# Change to whatever file directory you have
getwd() # "C:/Project/APP/APP"
data_path <- file.path('.','data')
output_path <- file.path('.','output','files')

## Read in `Neighborhood Councils (Certified)` via LA GeoHub Open API.
nc_certified <- sf::st_read("https://maps.lacity.org/lahub/rest/services/Boundaries/MapServer/18/query?outFields=*&where=1%3D1&f=geojson")

## Obtain NC names from LADOT Data
### May 2019 sheet has all the NC names
data_nc_names = readxl::read_excel(file.path(data_path,'CPRA #22-10589 Data','2019 Neighborhood Council Trip Matrix.xlsx'), sheet = "May") 


## Check for differences in names ####
setdiff(nc_certified$NAME,data_nc_names$`Origin\\Destination`) # 15 differences

## Note: "HISTORIC CULTURAL NC" from `data_nc_names` matches with "ARTS DISTRICT LITTLE TOKYO NC" in `nc_certified`. This is the only name that is very different. We checked by mapping the old geometries (from previous downloads of `Neighborhood Councils (Certified)` and the new geometeries that these two match together.


## Create GeoRef ####
nc_georef =
  data_nc_names %>% 
  select(`Origin\\Destination`) %>% 
  rename(data_nc_name = `Origin\\Destination`) %>% 
  
  # Adjust the different NC names by making new join_name column
  mutate(join_name = case_when(
    data_nc_name %in% "EMPOWERMENT CONGRESS CENTRAL AREA NDC" ~ "EMPOWERMENT CONGRESS CENTRAL AREA NC",
    data_nc_name %in% "EMPOWERMENT CONGRESS NORTH AREA NDC" ~ "EMPOWERMENT CONGRESS NORTH AREA NC",
    data_nc_name %in% "GREATER VALLEY GLEN COUNCIL" ~ "GREATER VALLEY GLEN",
    data_nc_name %in% "HISTORIC CULTURAL NC" ~ "ARTS DISTRICT LITTLE TOKYO NC",
    data_nc_name %in% "MAR VISTA CC" ~ "MAR VISTA NC",
    data_nc_name %in% "NC WESTCHESTER/PLAYA" ~ "WESTCHESTER/PLAYA NC", 
    data_nc_name %in% "NORTH HILLS EAST" ~ "NORTH HILLS EAST NC",
    data_nc_name %in% "NORTH HOLLYWOOD NORTHEAST NC" ~ "NORTH HOLLYWOOD NORTH EAST NC",
    data_nc_name %in% "NORTHRIDGE EAST" ~ "NORTHRIDGE EAST NC",
    data_nc_name %in% "NORTHRIDGE WEST" ~ "NORTHRIDGE WEST NC",
    data_nc_name %in% "PARK MESA HEIGHTS CC" ~ "PARK MESA HEIGHTS NC",
    data_nc_name %in% "UNITED NEIGHBORHOODS OF THE HISTORIC ARLINGTON HEIGHTS" ~ "UNITED NEIGHBORHOODS NC",
    data_nc_name %in% "VOICES OF 90037" ~ "VOICES NC",
    data_nc_name %in% "WEST LOS ANGELES NC" ~ "WEST LOS ANGELES SAWTELLE NC",
    data_nc_name %in% "ZAPATA KING NC" ~ "ZAPATA-KING NC",
    TRUE ~ data_nc_name
  )) %>%
  
  # join all other variables from nc_certified
  left_join(nc_certified, by=c("join_name"="NAME"),keep=TRUE) %>% 
  
  # calculate area in square miles
  mutate(
    geo_validated = st_make_valid(geometry),
    area_mi2 = units::set_units(st_area(geo_validated),mi^2),
    
    # identify which NC are part of SFV. See note below
    SFV = ifelse(grepl(".*VALLEY",SERVICE_RE),TRUE,FALSE)
  ) %>% 
  rename(cert_name = NAME) %>% 
  select(NC_ID,data_nc_name,cert_name,SERVICE_RE,SFV,area_mi2,geometry) %>% 
  glimpse()

rm(nc_certified,data_nc_names)

## Note about SFV NCs. These NCs belong to the SFV: "REGION 1 - NORTH EAST VALLEY", "REGION 2 - NORTH WEST VALLEY","REGION 3 - SOUTH WEST VALLEY","REGION 4 - SOUTH EAST VALLEY".


## Read in EFMDDs ####
efmdd <- st_read(file.path(data_path, "CPRA #22-10589 Data", "Equity_Focus_Mobility_Development_District_GeoJSON (1) (1).geojson"))

mdd <- st_read(file.path(data_path, "CPRA #22-10589 Data", "Mobility_Development_District_GeoJSON (1).geojson"))

spd <- st_read(file.path(data_path, "CPRA #22-10589 Data", "Standard_Permitted_District_GeoJSON (1).geojson")) %>% 
  mutate(Geo_Type = "Standard Permitted District")

soz <- st_read(file.path(data_path, "CPRA #22-10589 Data", "Special_Operation_Zones_(SOZ)__GeoJSON (2) (1).geojson"))

## Append 4 Zones ####
prog_geos <-
  efmdd %>% 
  select(name,Geo_Type) %>% 
  bind_rows(mdd %>% 
              select(name,Geo_Type)) %>% 
  bind_rows(spd %>% 
              select(name,Geo_Type)) %>% 
  bind_rows(soz %>% 
              rename(name = Place_Name) %>% 
              select(name,Geo_Type)) %>% 
  
  # create join_name column, see next section for reason
  mutate(join_name = str_to_title(name))
rm(efmdd,mdd,soz,spd)
## Note about Neighborhood Council Gaps in the 4 zone types. The missing gaps are areas between NC that are included as one of the 4 zone types. We ignore the gaps in our analysis since the data that we received from LADOT came at the NC level, which does not report the trips or deployment in the gap.


## Check for different NC names ####
## Note: names from prog_geos and nc_certified are not the same. `data_nc_name` from nc_georef has more matches to prog_geo when turning the words into camel case.
nc_georef =
  nc_georef %>% 
  mutate(join_name = str_to_title(data_nc_name))

setdiff(nc_georef$join_name,prog_geos$join_name) # 8 different


## Create GeoRef w/o SOZ ####
nc_georef_noSOZ =
  nc_georef %>% 
  
  # fix names
  mutate(
    join_name = case_when(
      join_name %in% "Downtown Los Angeles" ~ "Downtown Los Angeles Nc",       
      join_name %in% "United Neighborhoods Of The Historic Arlington Heights" ~ "United Neighborhoods Of The Historic Arlington Heights, West Adams, And Jefferson Park Community",
      join_name %in% "Nc Westchester/Playa" ~ "Westchester/Playa Nc",  
      join_name %in% "Nc Valley Village" ~ "Valley Village Nc",
      join_name %in% "Empowerment Congress Central Area Ndc" ~ "Empowerment Congress Central Area Dnc",
      join_name %in% "Northridge East" ~ "Northridge East Nc",       
      join_name %in% "North Hills East" ~ "North Hills East Nc",
      join_name %in% "Northridge West" ~ "Northridge West Nc",
      TRUE ~ join_name)) %>% 
  inner_join(prog_geos %>% 
               as.data.frame() %>% 
               select(Geo_Type,join_name),
             by='join_name') %>% 
  select(-join_name) %>% 
  glimpse()


## Create GeoRef w/SOZ ####
## First make a dataframe that has all the info we need to bind rows
SOZ_df =
  data.frame(
    NC_ID = c(NA,NA,NA),
    data_nc_name = c("Venice Ocean Front Walk","Hollywood Walk of Fame","Downtown LA"),
    cert_name = c("Venice Ocean Front Walk","Hollywood Walk of Fame","Downtown LA"),
    SERVICE_RE = c("REGION 11 - WEST LA","REGION 5 - CENTRAL 1","REGION 6 - CENTRAL 2"),
    SFV = c(F,F,F)) %>% 
  inner_join(prog_geos,by=c("data_nc_name"="name")) %>% 
  mutate(area_mi2 = set_units(st_area(geometry),mi^2)) %>% 
  select(-join_name) %>% 
  glimpse()

## Bind rows
nc_georef_wSOZ <- bind_rows(nc_georef_noSOZ,SOZ_df)
rm(SOZ_df)

## Export GeoJSON ####
st_write(nc_georef_noSOZ,
         file.path(output_path, 'NCZone_GeoRef_noSOZ.geojson'),
         delete_dsn = TRUE)

st_write(nc_georef_wSOZ,
         file.path(output_path, 'NCZone_GeoRef_wSOZ.geojson'),
         delete_dsn = TRUE)

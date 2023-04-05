# Make Mapping Data ##############################
# Author: Abraham Cheung
# Purpose: create .geojson files that show the aggregated averages for deployment and trips across Pilot Program years and current program years. The final output should have NCs as rows, and as columns: average trips, average deployment, geometries, and other variables

# Set Up ####
# libraries
pacman::p_load("tidyverse","sf","scales","lubridate")

# directories
data_files_dir <- file.path('.','output','files')
plots_dir <- file.path(".","output","plots")

# load trip data
trip_df <- read.csv(file.path(data_files_dir,"Trip_OD_by_NC.csv"))
deploy_df <- read_csv(file.path(data_files_dir,"veh_depl_long.csv"))

# load georeference files
nc_georef_noSOZ <- sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_noSOZ.geojson"))


# Aggregate average deployment ################
## assign program name by months of interest
deploy_df1 =
  deploy_df %>% 
  mutate(days_mo = lubridate::days_in_month(month_yr),
         program = case_when(
           grepl("2019-0[4-9]|2019-1[0-2]|2020-0[1-3]",month_yr) ~ "Pilot",
           grepl("2020-0[4-9]|2020-1[0-2]|202[1-2]",month_yr) ~ "Current",
           TRUE ~ NA_character_
         ))

## summarize deployment
depl_sum =
  deploy_df1 %>% 
  select(-data_nc_name) %>% 
  na.omit() %>% # remove Jan to Mar 2019, which are outside of Pilot Program
  group_by(NC_ID,cert_name,program) %>% 
  summarise(AvgDepl = weighted.mean(avg_depl, days_mo, na.rm=TRUE)) %>% # weighted average by days in month
  pivot_wider(id_cols = c(NC_ID,cert_name),
              names_from = program,
              names_glue = paste("{program}_AvgDepl"),
              values_from = AvgDepl) %>% 
  glimpse()


# Aggregate average trips ##########
## we will only look at trip origin counts
trip_df1 =
  trip_df %>% 
  group_by(origin_nc_id,origin_new_nc_name,month) %>% 
  summarise(trips = sum(trips)) %>% # sum total trips by origin for each NC
  mutate(days_mo = lubridate::days_in_month(month), # need days in month for weighted average across years
         program = case_when( # indicate which program the month is part of
           grepl("2019-0[4-9]|2019-1[0-2]|2020-0[1-3]",month) ~ "Pilot",
           grepl("2020-0[4-9]|2020-1[0-2]|202[1-2]",month) ~ "Current",
           TRUE ~ NA_character_
         )) %>% 
    glimpse()


## summarize averages
tr_sum =
  trip_df1 %>% 
  na.omit() %>% # remove Jan to Mar 2019 which are NA in `program`
  group_by(origin_nc_id,origin_new_nc_name,program) %>% 
  summarise(AvgTrips = weighted.mean(trips, days_mo, na.rm=TRUE)) %>% # weighted average of monthly trips, weighted by days in month
  pivot_wider(id_cols = c(origin_nc_id,origin_new_nc_name),
              names_from = program,
              names_glue = paste("{program}_AvgTrips"),
              values_from = AvgTrips) %>% 
  glimpse()


## redo "Current" average trips removing Dec 2021 since the numbers are extremely low.
tr_sum_noDec21 =
  trip_df1 %>% 
  na.omit() %>% # remove Jan to Mar 2019 which are NA in `program`
  filter(!grepl("2021-12-01",month),grepl("Current",program)) %>% # remove Dec 2021 from observations, but keep only "Current" program months
  group_by(origin_nc_id,origin_new_nc_name,program) %>% 
  summarise(AvgTrips = weighted.mean(trips, days_mo, na.rm=TRUE)) %>% # weighted average of monthly trips, weighted by days in month
  pivot_wider(id_cols = c(origin_nc_id,origin_new_nc_name),
              names_from = program,
              names_glue = paste("{program}_AvgTrips_noDec21"),
              values_from = AvgTrips) %>% 
  glimpse()


# Join the two dataframes to the georef file #############
df_out =
  depl_sum %>% 
  left_join(tr_sum_noDec21, by=c("NC_ID"="origin_nc_id","cert_name"="origin_new_nc_name")) %>% 
  left_join(tr_sum, by=c("NC_ID"="origin_nc_id","cert_name"="origin_new_nc_name")) %>% 
  left_join(nc_georef_noSOZ %>% 
              select(NC_ID,SFV,Geo_Type,Geo_Type_wSOZ,geometry),
            by="NC_ID") %>%
  rename(nc_name = cert_name) %>% 
  st_as_sf() %>% 
  glimpse()

st_geometry(df_out) # should still be "Projected CRS: NAD83 / California zone 5"

# Export geojson #########
# st_write(df_out,file.path(data_files_dir,"AvgTrip_Depl_byNC.geojson"),delete_dsn = TRUE)

# Questions
# why is trip avg so low in current, even for SOZ
df_out %>% 
  filter(Geo_Type_wSOZ %in% "Special Operations Zone") %>% 
  select(nc_name:Pilot_AvgTrips)


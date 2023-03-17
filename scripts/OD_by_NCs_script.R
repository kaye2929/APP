############### Script for Cleaning LADOT Data - OD by NCs ###
# Author: Abraham Cheung

# Set Up ########
rm(list = ls())
## load packages
pacman::p_load("tidyverse", "readxl", "lubridate", "sf")


## directory paths
cpra_dir <- file.path('.','data','CPRA #22-10589 Data')
data_files_dir <- file.path('.','output','files')


# Prep Geos #######
## load NC geos
nc_georef_noSOZ = sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_noSOZ.geojson")) %>% 
  rename(new_nc_name = cert_name)

## create df with all OD pairs of NCs using `nc_id`
### resulting dataframe contains every possible pairwise combination of NC IDs 
### purpose: we want to be able to make a map for each NC to show the number of trips started or ended in that specific NC.

nc_geos_full = NULL
for (i in nc_georef_noSOZ$NC_ID) {
  for (j in nc_georef_noSOZ$NC_ID) {
    nc_geos_full = data.frame(
      origin_nc_id = i,
      dest_nc_id = j) %>% 
      
      bind_rows(nc_geos_full)
  }
}
rm(i,j)


## join the names to both the origin and destination ID
nc_geos_full = nc_geos_full %>% 
  
  ## join NC names for origin NCs
  left_join(nc_georef_noSOZ %>%
              data.frame() %>% # turn into df otherwise the geometry will be automatically added
              select(data_nc_name, new_nc_name, NC_ID),
            by = c("origin_nc_id" = "NC_ID")) %>% 
  rename_at(vars(data_nc_name, new_nc_name),
            function(x) {paste("origin_",x,sep = "")}) %>% # rename the two columns with starting with "origin"
  
  ## join NC names for dest NCs
  left_join(nc_georef_noSOZ %>% 
              data.frame() %>% 
              select(data_nc_name, new_nc_name, NC_ID),
            by = c("dest_nc_id" = "NC_ID")) %>% 
  rename_at(vars(data_nc_name, new_nc_name),
            function(x) {paste("dest_",x,sep = "")}) # rename the two columns with starting with "dest"


# Cleaning ########
## Loop through each of the sheets in the 4 excel workbooks. Obtain and transform data

## Set Up
years <- c("2019","2020","2021","2022") # the four years of the different workbooks
data_output <- NULL

## Begin Loop
for (yr in years) {

  # obtain all the sheet names within the specific workbook
  sheet_names <- readxl::excel_sheets(file.path(cpra_dir, paste(yr,'Neighborhood Council Trip Matrix.xlsx',sep = " "))) # `paste()` function allows us to call each year's workbook in the loop
  
  for (one_month in sheet_names) {
    
    # read workbook, rename first column, pivot wider
    temp_output <- readxl::read_excel(file.path(cpra_dir, paste(yr,'Neighborhood Council Trip Matrix.xlsx',sep = " ")), sheet = one_month) %>% 
      rename("origin_nc" = "Origin\\Destination") %>% 
      pivot_longer(cols = -c("origin_nc"), names_to = "dest_nc", values_to = "trips")  
    
    # print message to show status in console
    print(paste("Loaded trip data for", one_month, yr, sep = " "))
    
    # right join the workbook data to the df with all OD pair combos
    data_output = temp_output %>% 
      right_join(nc_geos_full, by = c("origin_nc" = "origin_data_nc_name",
                                      "dest_nc" = "dest_data_nc_name")) %>% 
      
      # replace NAs with 0
      # add month for each observation based on the sheet it came from
      mutate(trips = ifelse(is.na(trips),0,trips),
             month = lubridate::mdy(paste(one_month,"1",yr))) %>% 
      
      # append data
      bind_rows(data_output)
    
    # print message to show status
    print(paste("Transformed and appended trip data for", one_month, yr, sep = " "))
  }
}
rm(yr,years,one_month,sheet_names,temp_output)


## Select columns and rearrange the order
data_output = data_output %>% 
  select(month, origin_new_nc_name, origin_nc_id, dest_new_nc_name, dest_nc_id, trips) %>% 
  arrange(month, origin_nc_id, dest_nc_id)


###################################################
# Check Work ########
nc_geos_full %>% count(origin_data_nc_name) %>% count(n) # n == nn
data_output %>% count(month) %>% count(n) # 45 unique months
data_output %>% count(origin_nc_id) %>% count(n) # n = 4455; nn = 99


# Check Summary Stats ########
# all the tibbles below match the numbers in "output/files/Summary Stats of LADOT Data.xlsx"
data_output %>% 
  filter(origin_new_nc_name == 'VENICE NC', 
         month == '2019-01-01',
         trips > 0) %>% 
  summarise(mean = mean(trips), sum = sum(trips), n_gt0 = n()) # Venice Jan 2019

data_output %>% 
  filter(dest_new_nc_name == 'EAST HOLLYWOOD NC', 
         month == '2019-06-01',
         trips > 0) %>% 
  summarise(mean = mean(trips), sum = sum(trips), n_gt0 = n()) # East Hollywood June 2019

data_output %>% 
  filter(origin_new_nc_name == "UNITED NEIGHBORHOODS NC",
         month == '2020-02-01',
         trips > 0) %>% 
  summarise(mean = mean(trips), sum = sum(trips), n_gt0 = n()) # United neighborhoods of the Historic Arlington Heights Feb 2020

data_output %>% 
  filter(dest_new_nc_name == 'MID CITY WEST CC', 
         month == '2020-07-01',
         trips > 0) %>% 
  summarise(mean = mean(trips), sum = sum(trips), n_gt0 = n()) # Mid City West CC July 2020

data_output %>% 
  filter(origin_new_nc_name == 'EAST HOLLYWOOD NC', 
         month == '2021-03-01',
         trips > 0) %>% 
  summarise(mean = mean(trips), sum = sum(trips), n_gt0 = n()) # East Hollywood Mar 2021

data_output %>% 
  filter(dest_new_nc_name == 'ELYSIAN VALLEY RIVERSIDE NC', 
         month == '2021-08-01',
         trips > 0) %>% 
  summarise(mean = mean(trips), sum = sum(trips), n_gt0 = n()) # Elysian Valley Riverside Aug 2021

data_output %>% 
  filter(origin_new_nc_name == 'SHERMAN OAKS NC', 
         month == '2022-04-01',
         trips > 0) %>% 
  summarise(mean = mean(trips), sum = sum(trips), n_gt0 = n()) # Sherman Oaks Apr 2022

data_output %>% 
  filter(dest_new_nc_name == 'EAGLE ROCK NC', 
         month == '2022-09-01',
         trips > 0) %>% 
  summarise(mean = mean(trips), sum = sum(trips), n_gt0 = n()) # Eagle Rock Sep 2022

# Export ########
# As .csv
# write.csv(data_output, file = file.path(data_files_dir,"Trip_OD_by_NC.csv"),row.names = FALSE)

# Joining the geos to every observation makes the file extremely large. As a result, I was not able to export these .geojson. Once further analysis is done, such as limiting the time period for analysis or geographic granularity, then we can join the geographies again for mapping. 

# I am leaving this code here for future reference

# Origin As .geojson
# data_out_origin =
#   data_output %>% 
#   left_join(nc_georef_noSOZ %>% 
#               select(NC_ID,geometry),
#             by=c("origin_nc_id"="NC_ID")) %>% 
#   rename(geometry_origin = geometry) %>% 
#   st_as_sf(crs=26945) %>% 
#   glimpse()
# 
# sf::st_write(data_out_origin,
#              file.path(data_files_dir,"Trip_Origin_byNC.geojson"),
#              delete_dsn=TRUE)

# Destination as .geojson
# data_out_dest =
#   data_output %>% 
#   left_join(nc_georef_noSOZ %>% 
#               select(NC_ID,geometry),
#             by=c("dest_nc_id"="NC_ID")) %>% 
#   rename(geometry_dest = geometry) %>% 
#   st_as_sf(crs=26945) %>% 
#   glimpse()
# 
# sf::st_write(data_out_origin,
#              file.path(data_files_dir,"Trip_Dest_byNC.geojson"))

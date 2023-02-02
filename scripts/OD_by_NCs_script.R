############### Script for Cleaning LADOT Data - OD by NCs ###

# Set Up ########
rm(list = ls())
## load packages
packages <- c("tidyverse", "readxl", "lubridate", "sf")
lapply(packages, library, character.only = TRUE)
rm(packages)


## directory paths
data_path <- file.path('..','data')
output_path <- file.path('..','output','files')


# Prep Geos #######
## load NC geos
nc_geos = sf::st_read(file.path(output_path,'NC_OldtoNew_Ref.geojson'))


## create df with all OD pairs of NCs using `nc_id`
### resulting dataframe contains every possible pairwise combination of NC IDs 
### purpose: we want to be able to make a map for each NC to show the number of trips started or ended in that specific NC.
nc_geos_full = NULL
for (i in nc_geos$nc_id) {
  for (j in nc_geos$nc_id) {
    nc_geos_full = data.frame(
      origin_nc_id = i,
      dest_nc_id = j) %>% 
      
      bind_rows(nc_geos_full)
  }
}
rm(i,j)

## DONT JOIN GEOS####
# FIND SOMEWHERE AT END TO INSERT GEOS. MAYBE MAKE 2 GEOJSON FILES, ONE FOR START, ONE FOR END.

## join the names to both the origin and destination ID
nc_geos_full = nc_geos_full %>% 
  
  ## join NC names for origin NCs
  left_join(nc_geos %>%
              data.frame() %>% # turn into df otherwise the geometry will be automatically added
              select(data_nc_name, new_nc_name, nc_id),
            by = c("origin_nc_id" = "nc_id")) %>% 
  rename_at(vars(data_nc_name, new_nc_name),
            function(x) {paste("origin_",x,sep = "")}) %>% # rename the two columns with starting with "origin"
  
  ## join NC names for dest NCs
  left_join(nc_geos %>% 
              data.frame() %>% 
              select(data_nc_name, new_nc_name, nc_id),
            by = c("dest_nc_id" = "nc_id")) %>% 
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
  sheet_names <- readxl::excel_sheets(file.path(data_path,'CPRA #22-10589 Data', paste(yr,'Neighborhood Council Trip Matrix.xlsx',sep = " "))) # `paste()` function allows us to call each year's workbook in the loop
  
  for (one_month in sheet_names) {
    
    # read workbook, rename first column, pivot wider
    temp_output <- readxl::read_excel(file.path(data_path,'CPRA #22-10589 Data', paste(yr,'Neighborhood Council Trip Matrix.xlsx',sep = " ")), sheet = one_month) %>% 
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
rm(yr,years,one_month,path_name,sheet_names,temp_output)


## Select columns and rearrange the order
data_output = data_output %>% 
  select(month, origin_new_nc_name, origin_nc_id, dest_new_nc_name, dest_nc_id, trips) %>% 
  arrange(month, origin_nc_id, dest_nc_id)


# Check Work ########
nc_geos_full %>% count(origin_data_nc_name) %>% count(n) # n == nn
data_output %>% count(month) %>% count(n) # 45 unique months
data_output %>% count(origin_nc_id) %>% count(n) # n = 4455; nn = 99


# Check Summary Stats ########
data_output %>% 
  filter(origin_new_nc_name == 'VENICE NC', month == '2019-01-01') %>% 
  summarise(sum = sum(trips), mean = mean(trips), sd = sd(trips)) # Venice Jan 2019

data_output %>% 
  filter(dest_new_nc_name == 'EAST HOLLYWOOD NC', month == '2019-06-01') %>% 
  summarise(sum = sum(trips), mean = mean(trips), sd = sd(trips)) # East Hollywood June 2019
## different from the excel workbook

data_output %>% 
  filter(origin_new_nc_name == 'UNITED NEIGHBORHOODS OF THE HISTORIC ARLINGTON HEIGHTS', month == '2020-02-01') %>% 
  summarise(sum = sum(trips), mean = mean(trips), sd = sd(trips)) # United neighborhoods of the Historic Arlington Heights Feb 2020
## cannot find in the origin_new_nc_name column

data_output %>% 
  filter(origin_new_nc_name == 'MID CITY WEST CC', month == '2020-07-01') %>% 
  summarise(sum = sum(trips), mean = mean(trips), sd = sd(trips)) # Mid City West CC July 2020
## different from the excel workbook

data_output %>% 
  filter(origin_new_nc_name == 'EAST HOLLYWOOD NC', month == '2021-03-01') %>% 
  summarise(sum = sum(trips), mean = mean(trips), sd = sd(trips)) # East Hollywood Mar 2021

data_output %>% 
  filter(origin_new_nc_name == 'ELYSIAN VALLEY RIVERSIDE NC', month == '2021-08-01') %>% 
  summarise(sum = sum(trips), mean = mean(trips), sd = sd(trips)) # Elysian Valley Riverside Aug 2021
## different from the excel workbook

data_output %>% 
  filter(origin_new_nc_name == 'SHERMAN OAKS NC', month == '2022-04-01') %>% 
  summarise(sum = sum(trips), mean = mean(trips), sd = sd(trips)) # Sherman Oaks Apr 2022

data_output %>% 
  filter(origin_new_nc_name == 'EAGLE ROCK NC', month == '2022-09-01') %>% 
  summarise(sum = sum(trips), mean = mean(trips), sd = sd(trips)) # Eagle Rock Sep 2022

# Export ########
# write.csv(nc_geos_full, file = "OD_by_NCs_Trips.csv")

# Scratch ########

# TO DO ########
## remove geos from export since .shp and .geojson cannot have more than 1 geometry column
## make sure mapping team has everything they need to map all possible OD combos based on this .csv and the reference .geojson file.

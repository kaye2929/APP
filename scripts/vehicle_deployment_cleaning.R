#Combine Vehicle Deployment Neighborhood Council Destricts Data to one sheet (both long and wide formats)
# Reorganize version

## load packages
pacman::p_load(tidyverse, readxl, lubridate, sf)

## directory paths
data_path <- file.path('.','data')
file_path <- file.path('.', 'output/files')
sheet_path <- file.path(data_path, 'CPRA #22-10589 Data', 'Vehicle Deployment. Neighborhood Council Districts.xlsx')

# read ref file
ref <- st_read(file.path(file_path, 'NC_OldtoNew_Ref.geojson'))

## loop 
years <- c("2019","2020","2021","2022")
ref_sub <- ref %>% select(data_nc_name, new_nc_name, nc_id)
data_output <- NULL
data_output2 <- NULL

for (yr in years) {
  
  # read sheets(pivot long)
  vd <- read_excel(path = sheet_path, sheet = paste('Deployment', yr, sep = " "), skip = 1) %>% 
    pivot_longer(cols = -c('Neighborhood Council District'), names_to = 'month', values_to = 'Avg_deployment') 

  # output of pivot long format
  data_output <- vd %>% 
    mutate(Month = mdy(paste(month,"1",yr))) %>% 
    
    # append data
    bind_rows(data_output) %>% 
    
    # remove NAs
    filter(!is.na(Avg_deployment))
  
  # output of pivot wide format
  data_output2 <- data_output %>% 
    mutate(year = year(Month),
           month_year = paste(month, year, sep = '_')) %>% 
    select('Neighborhood Council District', 'Avg_deployment', 'month_year') %>% 
    pivot_wider(names_from = month_year, 
                values_from = 'Avg_deployment')
    
  }


## arrange data
data_output <- data_output %>% 
  select('Neighborhood Council District', 'Month', 'Avg_deployment') 

# add new nc name and nc id
data_output <- ref_sub %>% left_join(data_output, by = c('data_nc_name' = 'Neighborhood Council District'))
data_output2 <- ref_sub %>% left_join(data_output2, by = c('data_nc_name' = 'Neighborhood Council District'))

## Export data (csv and geojson)
# geojson
st_write(data_output,
         file.path(file_path, 'vehicle_deployment_long.geojson'),
         delete_dsn = TRUE)
st_write(data_output2,
         file.path(file_path, 'vehicle_deployment_wide.geojson'),
         delete_dsn = TRUE)

# csv
write.csv(data_output, file = file.path(file_path, 'vehicle_deployment_long.csv'), row.names = F)
write.csv(data_output2, file = file.path(file_path, 'vehicle_deployment_wide.csv'), row.names = F)

# Combine Vehicle Deployment Neighborhood Council Districts Data to one sheet (both long and wide formats)

# Set up ##############################
## load packages
pacman::p_load(tidyverse, readxl, lubridate, sf)

## directory paths
data_raw_dir <- file.path('.','data')
data_files_dir <- file.path('.', 'output/files')
sheet_path <- file.path(data_raw_dir, 'CPRA #22-10589 Data', 'Vehicle Deployment. Neighborhood Council Districts.xlsx')

# read ref file
ref <- st_read(file.path(data_files_dir, 'NCZone_GeoRef_noSOZ.geojson'))
ref_sub <- ref %>% as.data.frame() %>% select(data_nc_name, cert_name, NC_ID)


## Loop through years to obtain and transform deployment data ############### 
years <- 2019:2022
depl_long <- NULL


for (yr in years) {
  
  # read sheets (pivot long)
  vd <- read_excel(path = sheet_path, sheet = paste('Deployment', yr, sep = " "), skip = 1) %>% 
    pivot_longer(cols = -c('Neighborhood Council District'), names_to = 'month', values_to = 'avg_depl') 

  # output of pivot long format
  depl_long <- vd %>% 
    rename(nc_name = 'Neighborhood Council District') %>% 
    mutate(month_yr = mdy(paste(month,1,yr))) %>% 
    
    # append data
    bind_rows(depl_long) %>% 
    
    # remove NAs
    filter(!is.na(avg_depl)) %>% 
    select(-month)
  
  # output of pivot wide format
  depl_wide <- depl_long %>% 
    pivot_wider(names_from = month_yr, 
                values_from = avg_depl) %>% 
    select(nc_name,sort(names(.)))
  }


# Add certified NC name and NC_ID #########################
setdiff(unique(depl_long$nc_name),unique(ref_sub$data_nc_name)) 
setdiff(unique(depl_wide$nc_name),unique(ref_sub$data_nc_name))
# there are no differences between the NC names from the reference and data file

depl_long =
  depl_long %>% 
  left_join(ref_sub, by=c("nc_name"="data_nc_name"), keep=TRUE) %>% 
  select(NC_ID,data_nc_name,cert_name,month_yr,avg_depl) %>% 
  arrange(data_nc_name,month_yr) %>% 
  glimpse()
  

depl_wide =
  depl_wide %>% 
  left_join(ref_sub, by=c("nc_name"="data_nc_name"), keep=TRUE) %>% 
  select(-nc_name) %>% 
  select(NC_ID,data_nc_name,cert_name,everything()) %>% 
  glimpse()


## Export data (csv and geojson) ####################
# geojson

# depl_long %>%
#   left_join(ref %>% select(NC_ID),
#             by="NC_ID") %>%
#   st_write(file.path(data_files_dir, 'veh_depl_long.geojson'), delete_dsn = TRUE)


# depl_wide %>%
#   left_join(ref %>% select(NC_ID),
#             by="NC_ID") %>% 
#   st_write(file.path(data_files_dir, 'veh_depl_wide.geojson'), delete_dsn = TRUE)


# csv
# write.csv(depl_long, file = file.path(data_files_dir, 'veh_depl_long.csv'), row.names = F)
# write.csv(depl_wide, file = file.path(data_files_dir, 'veh_depl_wide.csv'), row.names = F)

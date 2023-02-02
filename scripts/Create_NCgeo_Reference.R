################ Clean NC Geos ###
## Set Up ########
## Packages
packages <- c("tidyverse", "readxl", "sf")
lapply(packages, library, character.only = TRUE)
rm(packages)


## File Directories
getwd() # "C:/Project/APP/APP/scripts"
data_path <- file.path('..','data')
output_path <- file.path('..','output','files')

## Read in New NC geos
nc_geos_new = sf::st_read(file.path(data_path,'Neighborhood_Councils_(Certified)','Neighborhood_Councils_(Certified).shp'))


## Read in Old NC geos
nc_geos_old = sf::st_read(file.path(data_path, 'OLD - Neighborhood Councils (Certified)','geo_export_71f6a568-febe-403c-898f-4ce9286a58a2.shp'))
### purpose: we need both the new and old version since the data provided by LADOT uses NC names from the Old NC geos. We wanted to use the names given by the most up to date NC shapefile from LA GeoHub so that others would be able to reproduce our results.


## Obtain NC names from LADOT Data
### May 2019 sheet has all the NC names
data_geos = readxl::read_excel(file.path(data_path,'CPRA #22-10589 Data','2019 Neighborhood Council Trip Matrix.xlsx'), sheet = "May") 


## Connect data & old NC geos ####
temp = data.frame(
  data_nc_name = sort(unique(data_geos$`Origin\\Destination`)),
  old_nc_name = sort(unique(nc_geos_old$name)))
rm(data_geos)


### Check. nearly all have the same name. Only the one NC has a different name, but it is very close. (`UNITED NEIGHBORHOODS OF THE HISTORIC ARLINGTON HEIGHTS` vs `UNITED NEIGHBORHOODS OF THE HISTORIC ARLINGTON HEIGHTS, WEST ADAMS, AND JEFFERSON PARK COMMUNITY UNITED NEIGHBORHOODS NC`)
temp$data_nc_name %in% temp$old_nc_name


## Connect new & old NC geos ####
temp1 = data.frame(nc_geos_old) %>% 
  left_join(data.frame(nc_geos_new), 
            by = c("nc_id" = "NC_ID")) %>% 
  rename(old_nc_name = name,
         new_nc_name = NAME) 


### Check. There are the same number of NCs in each service region. The service regions for each NC are the same.
temp1 %>% count(service_re)
temp1 %>% count(SERVICE_RE)
temp1$service_re %in% temp1$SERVICE_RE


## Create reference table ####
## Join data_nc_names to `temp1` which has old new NC names.
nc_geo_ref = temp %>% 
  left_join(temp1, by = c("old_nc_name" = "old_nc_name")) %>% 
  select(data_nc_name, old_nc_name, new_nc_name, nc_id) %>% 
  left_join(nc_geos_new, by = c("nc_id" = "NC_ID")) %>% 
  select(OBJECTID, data_nc_name, old_nc_name, new_nc_name, nc_id, WADDRESS:geometry)

rm(temp, temp1, nc_geos_new, nc_geos_old)


## Export table #### 
# st_write(nc_geo_ref, 
#          file.path(output_path, 'NC_OldtoNew_Ref.geojson'), 
#          delete_dsn = TRUE)
# 
# nc_geo_ref %>%
#   select(-geometry) %>%
#   write.csv(file.path(output_path, 'NC_OldtoNew_Ref.csv'), row.names = FALSE)


############### Script for Cleaning LADOT Data - OD by NCs ################

# Set Up ########
## set working directory to file location of this script
setwd("C:/Project/APP/APP/scripts/LADOT_working")


## load packages
pkgs <- c("tidyverse", "readxl", "lubridate")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)


# Cleaning ########
years <- c("2019","2020","2021","2022") # the four years of the different workbooks
my_output <- NULL

for (yr in years) {
  # create path to the workbook with a specific year
  path_name <- paste("../../data/LADOT/CPRA #22-10589 Data/", yr, " Neighborhood Council Trip Matrix.xlsx", sep = "")  
  
  # obtain all the sheet names within the specific workbook
  sheet_names <- excel_sheets(path_name)
  
  # read, clean, and append all the sheets from the specific workbook
  for (one_month in sheet_names) {
    one_sheet <- read_excel(path_name, sheet = one_month) %>% 
      rename("Origin NC" = "Origin\\Destination") %>% 
      pivot_longer(cols = -c("Origin NC"), names_to = "Destination NC", values_to = "Trips") %>% 
      mutate(Month = mdy(paste(one_month,"1",yr)))
    
    my_output <- bind_rows(my_output, one_sheet)
  }
  
  rm(one_sheet,yr,one_month,path_name,sheet_names)
}


# Add Geos ########
library(sf)
nc_geos = read_sf("../../data/geos/Neighborhood Councils (Certified)/geo_export_71f6a568-febe-403c-898f-4ce9286a58a2.shp")

library(jsonlite)
lanc_geos = fromJSON(txt = "https://maps.lacity.org/lahub/rest/services/Boundaries/MapServer/18/query?outFields=*&where=1%3D1&f=geojson")
lanc_df = lanc_geos$features$properties

sort(lanc_df$NC_ID) %in% sort(nc_geos$nc_id) # the NC_IDs match, but the names have changed. make sure we do the joins by the IDs not the names.

library(geojsonio)
spdf <- read_sf("https://maps.lacity.org/lahub/rest/services/Boundaries/MapServer/18/query?outFields=*&where=1%3D1&f=geojson")
st_write(spdf, "is_this_.geojson")

# Check Work ########


# Export ########


# Scratch ########


# TO DO ########
## check work
## add NC geos
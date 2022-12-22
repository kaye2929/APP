############### Script for Cleaning LADOT Data - OD by NCs ################

# Set Up ########
## set working directory
setwd("C:/Project/APP/APP/data/LADOT/CPRA #22-10589 Data") 

## load packages
pkgs <- c("tidyverse", "readxl", "lubridate")
lapply(pkgs, library, character.only = TRUE)
rm(pkgs)


# Cleaning ########
years <- c("2019","2020","2021","2022") # the four years of the different workbooks
output <- NULL

for (yr in years) {
  # obtain specific year-workbook and its sheet names
  wkb_name <- paste(yr, "Neighborhood Council Trip Matrix.xlsx")
  sheet_names <- excel_sheets(wkb_name)
  
  # read, clean, and append all the sheets from the specific workbook
  for (one_month in sheet_names) {
    one_sheet <- read_excel(wkb_name, sheet = one_month) %>% 
      rename("Origin NC" = "Origin\\Destination") %>% 
      mutate_at(vars(-c("Origin NC")), as.numeric) %>%
      pivot_longer(cols = -c("Origin NC"), names_to = "Destination NC", values_to = "Trips") %>% 
      mutate(Month = mdy(paste(one_month,"1",yr)))
    
    output <- bind_rows(output, one_sheet)
  }
  rm(one_sheet,yr,one_month,wkb_name,sheet_names)
}

# Check Work ########


# Export ########


# Scratch ########


# TO DO ########
## check work
## add NC geos
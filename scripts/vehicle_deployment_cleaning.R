#Combine Vehicle Deployment Neighborhood Council Destricts Data to one sheet (both long and wide formats)
# Reorganize version

## load packages
pacman::p_load(tidyverse, readxl, lubridate)

## directory paths
data_path <- file.path('.','data')

## loop 
years <- c("2019","2020","2021","2022")
sheet_path <- file.path(data_path, 'CPRA #22-10589 Data', 'Vehicle Deployment. Neighborhood Council Districts.xlsx')
data_output <- NULL
data_output2 <- NULL


for (yr in years) {
  
  # read sheets(pivot long)
  vd <- read_excel(path = sheet_path, sheet = paste('Deployment', yr, sep = " "), skip = 1) %>% 
    pivot_longer(cols = -c('Neighborhood Council District'), names_to = 'month', values_to = 'Avg deployment') 

  data_output <- vd %>% 
    mutate(Month = mdy(paste(month,"1",yr))) %>% 
    
    # append data
    bind_rows(data_output)
  
  # pivot wide
  data_output2 <- data_output %>% 
    mutate(year = year(Month),
           month_year = paste(month, year, sep = '_')) %>% 
    select('Neighborhood Council District', 'Avg deployment', 'month_year') %>% 
    pivot_wider(names_from = month_year, 
                values_from = 'Avg deployment')

  }


## arrange data
data_output <- data_output %>% 
  select('Neighborhood Council District', 'Month', 'Avg deployment')

## Export data
#write_csv(data_output, file = file.path())
#write_csv(data_output2, file = file.path())
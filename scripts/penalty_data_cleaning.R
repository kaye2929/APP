### load packages
pacman::p_load(tidyverse, readxl, lubridate, sf)

### directory paths
file_path <- file.path('.', 'output/files')
excel_path <- "./UCLA Data Request Dockless Violations MyLA311.xlsx"

## read ref file
ref <- st_read(file.path(file_path, 'NC_OldtoNew_Ref.geojson'))
ref_sub <- ref %>% select(data_nc_name, new_nc_name, nc_id)

## loop 
years <- c("2019","2020","2021","2022")

# Loop through each year and read the corresponding sheet
for (yr in years) {
  
  # Construct the sheet name
  sheet_name <- paste0("penalty_", yr)
  
  # Read the sheet
  sheet_data <- read_excel(excel_path, sheet = yr) %>% 
    select(-'Service Request Type') # remove 'Service Request Type' column
  
  # Assign the sheet data to a new object with the desired name
  assign(sheet_name, sheet_data) 
}


### arrange data
## 2019
# check the differences between penalty_2019$`Neighborhood Council` and ref_sub$new_nc_name
setdiff(unique(penalty_2019$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 

penalty_2019 <- penalty_2019 %>% left_join(ref_sub, by = c('Neighborhood Council' = 'data_nc_name'))


## 2020
penalty_2020$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2020$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2020$`Neighborhood Council`, 
                                              paste0(penalty_2020$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2020$`Neighborhood Council` and ref_sub$new_nc_name
setdiff(unique(penalty_2020$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 
# replace different values
penalty_2020 <- penalty_2020 %>%
  mutate(`Neighborhood Council` = case_when(
    grepl("MID CITY WEST", `Neighborhood Council`) ~ "MID CITY WEST CC",
    grepl("PARK MESA HEIGHTS", `Neighborhood Council`) ~ "PARK MESA HEIGHTS CC",
    grepl("VOICES", `Neighborhood Council`) ~ "VOICES OF 90037",
    grepl("DOWNTOWN LOS ANGELES", `Neighborhood Council`) ~ "DOWNTOWN LOS ANGELES",
    grepl("MAR VISTA", `Neighborhood Council`) ~ "MAR VISTA CC",
    grepl("WILSHIRE CENTER KOREATOWN", `Neighborhood Council`) ~ "WILSHIRE CENTER - KOREATOWN NC",
    grepl("EMPOWERMENT CONGRESS NORTH", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS NORTH AREA NDC",
    grepl("UNITED NEIGHBORHOODS", `Neighborhood Council`) ~ "UNITED NEIGHBORHOODS OF THE HISTORIC ARLINGTON HEIGHTS",
    grepl("ARTS DISTRICT LITTLE TOKYO", `Neighborhood Council`) ~ "HISTORIC CULTURAL NC",
    grepl("CANNDU", `Neighborhood Council`) ~ "COMMUNITY AND NEIGHBORS FOR NINTH DISTRICT UNITY (CANNDU)",
    grepl("EMPOWERMENT CONGRESS SOUTHEAST", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS SOUTHEAST AREA NDC",
    grepl("EMPOWERMENT CONGRESS CENTRAL", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS CENTRAL AREA NDC",
    grepl("NORTH HILLS EAST", `Neighborhood Council`) ~ "NORTH HILLS EAST",
    grepl("EMPOWERMENT CONGRESS WEST", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS WEST AREA NDC",
    grepl("NORTHRIDGE EAST", `Neighborhood Council`) ~ "NORTHRIDGE EAST",
    grepl("MID CITY WEST CC NC", `Neighborhood Council`) ~ "MID CITY WEST CC",
    grepl("PARK MESA HEIGHTS CC NC", `Neighborhood Council`) ~ "PARK MESA HEIGHTS CC",
    grepl("EMPOWERMENT CONGRESS CENTRAL AREA NDC NC", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS CENTRAL AREA NDC",
    grepl("VOICES OF 90037 NC", `Neighborhood Council`) ~ "VOICES OF 90037",
    grepl("NORTHRIDGE WEST NC", `Neighborhood Council`) ~ "NORTHRIDGE WEST",
    grepl("EMPOWERMENT CONGRESS NORTH AREA NDC NC", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS NORTH AREA NDC",
    grepl("COMMUNITY AND NEIGHBORS FOR NINTH DISTRICT UNITY (CANNDU) NC", `Neighborhood Council`) ~ "COMMUNITY AND NEIGHBORS FOR NINTH DISTRICT UNITY (CANNDU)",
    TRUE ~ `Neighborhood Council`
  )) 

# check the differences again
setdiff(unique(penalty_2020$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 

# join ref_sub
penalty_2020 <- penalty_2020 %>% left_join(ref_sub, by = c('Neighborhood Council' = 'data_nc_name'))

# check na
table(is.na(penalty_2020))


# 2021
penalty_2021$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2021$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2021$`Neighborhood Council`, 
                                              paste0(penalty_2021$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2021$`Neighborhood Council` and ref_sub$new_nc_name
setdiff(unique(penalty_2021$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 
# replace different values
penalty_2021 <- penalty_2021 %>% 
  mutate(`Neighborhood Council` = case_when(
    grepl("ARTS DISTRICT LITTLE TOKYO NC", `Neighborhood Council`) ~ "HISTORIC CULTURAL NC",
    grepl("WESTCHESTER/PLAYA", `Neighborhood Council`) ~ "NC WESTCHESTER/PLAYA",
    grepl("DOWNTOWN LOS ANGELES", `Neighborhood Council`) ~ "DOWNTOWN LOS ANGELES",
    grepl("MID CITY WEST", `Neighborhood Council`) ~ "MID CITY WEST CC",
    grepl("WILSHIRE CENTER KOREATOWN", `Neighborhood Council`) ~ "WILSHIRE CENTER - KOREATOWN NC",
    grepl("LINCOLN HEIGHTS", `Neighborhood Council`) ~ "LINCOLN HEIGHTS NC",
    grepl("MAR VISTA", `Neighborhood Council`) ~ "MAR VISTA CC",
    grepl("EMPOWERMENT CONGRESS NORTH", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS NORTH AREA NDC",
    grepl("WEST LA - SAWTELLE", `Neighborhood Council`) ~ "WEST LOS ANGELES NC",
    grepl("VALLEY VILLAGE", `Neighborhood Council`) ~ "NC VALLEY VILLAGE",
    grepl("ENCINO", `Neighborhood Council`) ~ "ENCINO NC",
    grepl("UNITED NEIGHBORHOODS", `Neighborhood Council`) ~ "UNITED NEIGHBORHOODS OF THE HISTORIC ARLINGTON HEIGHTS",
    grepl("EMPOWERMENT CONGRESS CENTRAL", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS CENTRAL AREA NDC",
    grepl("EMPOWERMENT CONGRESS SOUTHEAST", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS SOUTHEAST AREA NDC",
    grepl("NORTHRIDGE EAST", `Neighborhood Council`) ~ "NORTHRIDGE EAST",
    grepl("EMPOWERMENT CONGRESS SOUTHWEST", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS SOUTHWEST AREA NDC",
    grepl("GREATER VALLEY GLEN", `Neighborhood Council`) ~ "GREATER VALLEY GLEN COUNCIL",
    grepl("NORTHRIDGE WEST", `Neighborhood Council`) ~ "NORTHRIDGE WEST",
    grepl("VOICES", `Neighborhood Council`) ~ "VOICES OF 90037",
    grepl("EMPOWERMENT CONGRESS WEST", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS WEST AREA NDC",
    grepl("LA32", `Neighborhood Council`) ~ "LA-32 NC",
    grepl("PORTER RANCH", `Neighborhood Council`) ~ "PORTER RANCH NC",
    grepl("CANNDU", `Neighborhood Council`) ~ "COMMUNITY AND NEIGHBORS FOR NINTH DISTRICT UNITY (CANNDU)",
    grepl("PARK MESA HEIGHTS", `Neighborhood Council`) ~ "PARK MESA HEIGHTS CC",
    TRUE ~ `Neighborhood Council`
  ))

# check the differences again
setdiff(unique(penalty_2021$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 

# join ref_sub
penalty_2021 <- penalty_2021 %>% left_join(ref_sub, by = c('Neighborhood Council' = 'data_nc_name'))

# check na
table(is.na(penalty_2021))


# 2022
penalty_2022$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2022$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2022$`Neighborhood Council`, 
                                              paste0(penalty_2022$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2022$`Neighborhood Council` and ref_sub$new_nc_name
setdiff(unique(penalty_2022$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 
# replace different values
penalty_2022 <- penalty_2022 %>% 
  mutate(`Neighborhood Council` = case_when(
    grepl("WEST LA - SAWTELLE", `Neighborhood Council`) ~ "WEST LOS ANGELES NC",
    grepl("MAR VISTA", `Neighborhood Council`) ~ "MAR VISTA CC",
    grepl("DOWNTOWN LOS ANGELES", `Neighborhood Council`) ~ "DOWNTOWN LOS ANGELES",
    grepl("KOREATOWN", `Neighborhood Council`) ~ "WILSHIRE CENTER - KOREATOWN NC",
    grepl("MID", `Neighborhood Council`) ~ "MID CITY WEST CC",
    grepl("ARTS DISTRICT LITTLE TOKYO", `Neighborhood Council`) ~ "HISTORIC CULTURAL NC",
    grepl("EMPOWERMENT CONGRESS NORTH NC", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS NORTH AREA NDC",
    grepl("GREATER VALLEY GLEN", `Neighborhood Council`) ~ "GREATER VALLEY GLEN COUNCIL",
    grepl("VOICES", `Neighborhood Council`) ~ "VOICES OF 90037",
    grepl("WESTCHESTER/PLAYA", `Neighborhood Council`) ~ "NC WESTCHESTER/PLAYA",
    grepl("CANNDU", `Neighborhood Council`) ~ "COMMUNITY AND NEIGHBORS FOR NINTH DISTRICT UNITY (CANNDU)",
    grepl("NORTHRIDGE EAST", `Neighborhood Council`) ~ "NORTHRIDGE EAST",
    grepl("VALLEY VILLAGE", `Neighborhood Council`) ~ "NC VALLEY VILLAGE",
    grepl("LINCOLN HEIGHTS", `Neighborhood Council`) ~ "LINCOLN HEIGHTS NC",
    grepl("EMPOWERMENT CONGRESS SOUTHWEST", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS SOUTHWEST AREA NDC",
    grepl("PARK MESA HEIGHTS", `Neighborhood Council`) ~ "PARK MESA HEIGHTS CC",
    grepl("UNITED NEIGHBORHOODS", `Neighborhood Council`) ~ "UNITED NEIGHBORHOODS OF THE HISTORIC ARLINGTON HEIGHTS",
    grepl("EMPOWERMENT CONGRESS CENTRAL", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS CENTRAL AREA NDC",
    grepl("EMPOWERMENT CONGRESS WEST", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS WEST AREA NDC",
    grepl("ZAPATA-KING NC", `Neighborhood Council`) ~ "ZAPATA KING NC",
    grepl("PORTER RANCH", `Neighborhood Council`) ~ "PORTER RANCH NC",
    grepl("ENCINO", `Neighborhood Council`) ~ "ENCINO NC",
    grepl("NORTHRIDGE WEST NC", `Neighborhood Council`) ~ "NORTHRIDGE WEST",
    grepl("FOOTHILLS TRAILS DISTRICT", `Neighborhood Council`) ~ "FOOTHILL TRAILS DISTRICT NC",
    grepl("NORTH HILLS EAST", `Neighborhood Council`) ~ "NORTH HILLS EAST",
    grepl("EMPOWERMENT CONGRESS SOUTHEAST NC", `Neighborhood Council`) ~ "EMPOWERMENT CONGRESS SOUTHWEST AREA NDC",  
    TRUE ~ `Neighborhood Council`
  ))

# check the differences again
setdiff(unique(penalty_2022$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 

# join ref_sub
penalty_2022 <- penalty_2022 %>% left_join(ref_sub, by = c('Neighborhood Council' = 'data_nc_name'))

# check na
table(is.na(penalty_2022))


### Analyze data
penalty_table <- function(penalty_data){
  penalty_data %>%
    group_by(new_nc_name, `Violation/Infraction/Issue`) %>%
    tally() %>%
    pivot_wider(names_from = `Violation/Infraction/Issue`, values_from = n, values_fill = 0)
}

for (i in 2019:2022) {
  penalty_data <- get(paste0("penalty_", i))
  penalty_table_data <- penalty_table(penalty_data)
  
  #export data(csv, geojson)
  write.csv(penalty_table_data, file = file.path(file_path, paste0("penalty_table_", i, ".csv")), row.names = F)
  
}

## export data(geojson)
#geojosn file is too big to use the loop
st_write(penalty_2019, file.path(file_path, 'penalty_2019.geojson'), delete_dsn = TRUE)
st_write(penalty_2020, file.path(file_path, 'penalty_2020.geojson'), delete_dsn = TRUE) 
st_write(penalty_2021, file.path(file_path, 'penalty_2021.geojson'), delete_dsn = TRUE)
st_write(penalty_2022, file.path(file_path, 'penalty_2022.geojson'), delete_dsn = TRUE)

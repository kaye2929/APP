# Set up #################################
### load packages
pacman::p_load(tidyverse, readxl, lubridate, sf)

### directory paths
data_files_dir <- file.path('.', 'output/files')
excel_path <- file.path('.','data',"UCLA Data Request Dockless Violations MyLA311.xlsx")

## read reference file
ref <- st_read(file.path(data_files_dir, 'NCZone_GeoRef_noSOZ.geojson'))
ref_sub <- 
  ref %>% 
  as.data.frame() %>% 
  select(data_nc_name, cert_name, NC_ID)

# Loop to obtain sheet names ########################### 
# Get sheet names, which are names for each year
years <- readxl::excel_sheets(excel_path) # "2019","2020","2021","2022"

# Loop through each year and read the corresponding sheet
for (yr in years) {
  
  # Construct the sheet name
  sheet_name <- paste0("penalty_", yr)
  
  # Read the sheet
  sheet_data <- read_excel(excel_path, sheet = yr) %>% 
    
    # remove 'Service Request Type' and 'Council District' column
    select(-c('Service Request Type','Council District')) %>% 
    
    # rename the truncated labels (we obtained the full names from LADOT) ###############
  mutate(
    `Violation/Infraction/Issue` = case_when(
      `Violation/Infraction/Issue` %in% "Vehicle improperly parked (lay" ~ "Vehicle improperly parked (lay",
      `Violation/Infraction/Issue` %in% "Unpermitted Company and/or veh" ~ "Unpermitted Company and/or veh",
      `Violation/Infraction/Issue` %in% "Parked on Private Property" ~ "Parked on Private Property",
      `Violation/Infraction/Issue` %in% "Damaged or unsanitary Vehicle" ~ "Damaged or unsanitary Vehicle",
      `Violation/Infraction/Issue` %in% "Low Battery" ~ "Low Battery",
      `Violation/Infraction/Issue` %in% "Other" ~ "Other",
      `Violation/Infraction/Issue` %in% "Sidewalk Riding" ~ "Sidewalk Riding",
      `Violation/Infraction/Issue` %in% "Vehicle listed as available, b" ~ "Vehicle listed as available, b",
      TRUE ~ `Violation/Infraction/Issue`
    )
  ) %>% 
    
    # Create date vars
    mutate(
      `Creation Date` = as.POSIXct(`Creation Date`),
      date_ymday = date(`Creation Date`),
      date_ymo = floor_date(`Creation Date`, unit = "month"),
      date_year = year(`Creation Date`))
  
  
  # Assign the sheet data to a new object with the desired name
  assign(sheet_name, sheet_data) 
}


# Match the NC names ######################
## 2019 ######################
# check the differences between penalty_2019$`Neighborhood Council` and ref_sub$new_nc_name
setdiff(unique(penalty_2019$`Neighborhood Council`), unique(ref_sub$data_nc_name)) # 2 differences

# replace different names
penalty_2019 <- 
  penalty_2019 %>% 
  mutate(`Neighborhood Council` = case_when(
    `Neighborhood Council` %in% "MID-TOWN NORTH HOLLYWOOD NC" ~ "NOHO NC",
    `Neighborhood Council` %in% "GREATER ECHO PARK ELYSIAN NC" ~ "ECHO PARK NC",
    TRUE ~ `Neighborhood Council`
  )) %>% 
  left_join(ref_sub, by = c('Neighborhood Council' = 'data_nc_name'))

table(is.na(penalty_2019)) # check for any remaining NAs

## 2020 ###################
# Add `NC` at the end of the names missing the phrase `NC`
penalty_2020$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2020$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2020$`Neighborhood Council`, 
                                              paste0(penalty_2020$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2020$`Neighborhood Council` and ref_sub$new_nc_name
setdiff(unique(penalty_2020$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 

# replace different names
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

# check NAs
table(is.na(penalty_2020))


## 2021 ##########################
penalty_2021$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2021$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2021$`Neighborhood Council`, 
                                              paste0(penalty_2021$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2021$`Neighborhood Council` and ref_sub$new_nc_name
setdiff(unique(penalty_2021$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 

# replace different names
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


## 2022 #########################
penalty_2022$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2022$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2022$`Neighborhood Council`, 
                                              paste0(penalty_2022$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2022$`Neighborhood Council` and ref_sub$new_nc_name
setdiff(unique(penalty_2022$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 

# replace different names
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
    grepl("NORTH HOLLYWOOD WEST NC", `Neighborhood Council`) ~ "NOHO WEST NC",
    TRUE ~ `Neighborhood Council`
  ))

# check the differences again
setdiff(unique(penalty_2022$`Neighborhood Council`), unique(ref_sub$data_nc_name)) 

# join ref_sub
penalty_2022 <- penalty_2022 %>% left_join(ref_sub, by = c('Neighborhood Council' = 'data_nc_name'))

# check na
table(is.na(penalty_2022))


# Join data to complete list of NCs ###############
# For the NCs that had no penalties, they were not included in the dataset. 
# We want to include them as having 0 penalties so that we can make a time series chart of penalties for all NCs for every month.  

# List all the unique penalities
penalty_desc = unique(penalty_2019$`Violation/Infraction/Issue`)

# Create dataframe with all the penalties for all 99 NCs
full_nc_penalties = 
  data.frame(
    nc_name = unlist(lapply(ref_sub$cert_name, function (x) {rep(x, length(penalty_desc))})), # repeat each NC name 8 times (one for each penalty)
    penalty = rep(penalty_desc,length(ref_sub$cert_name)) # repeat each penalty in sequence 99 times
  )


# Function to summarize each penalty dataframe
penalty_table <- function(penalty_data){
  penalty_data %>%
    
    # summarize and count number of penalities per NC and per month
    group_by(cert_name,date_ymo,`Violation/Infraction/Issue`) %>% 
    tally() %>% 
    
    # if there are no penalties for a month or a NC, replace value with 0
    pivot_wider(names_from = date_ymo,
                values_from = n,
                values_fill = 0) %>% 
    
    # join summarized data to dataframe with full combinations of NC and penalities
    right_join(full_nc_penalties, by=c("cert_name"="nc_name","Violation/Infraction/Issue"="penalty")) %>% 
    
    # replace 0 for any month and NC that did not have penalities
    mutate_at(vars(-c("cert_name","Violation/Infraction/Issue")), list(~replace(.,which(is.na(.)),0))) %>% 
    
    # clean up for clarity
    rename(penalty = `Violation/Infraction/Issue`) %>% 
    arrange(cert_name) %>% 
    select(cert_name,penalty,sort(names(.)))
}


# loop each year dataframe through the function
for (i in 2019:2022) {
  penalty_data <- get(paste0("penalty_", i))
  penalty_table_data <- penalty_table(penalty_data)
  
  # replace previous dataframe
  assign(paste0("penalty_", i),penalty_table_data)
}


# join all dataframes together, add NC_IDs
penalties =
  penalty_2019 %>% 
  left_join(penalty_2020, by=c("cert_name","penalty")) %>% 
  left_join(penalty_2021, by=c("cert_name","penalty")) %>% 
  left_join(penalty_2022, by=c("cert_name","penalty")) %>% 
  left_join(ref %>% as.data.frame() %>% select(NC_ID,cert_name), by="cert_name") %>% 
  select(NC_ID,everything())

View(penalties)

# Export data #####################################
# write.csv(penalties,file = file.path(data_files_dir,"penalties_allyrs.csv"),row.names = F)

### load packages
pacman::p_load(tidyverse, readxl, lubridate, sf)

### directory paths
file_path <- file.path('.', 'output/files')
excel_path <- "./UCLA Data Request Dockless Violations MyLA311.xlsx"

## read file
ref_new <- st_read(file.path(file_path, 'NCZone_GeoRef_wSOZ.geojson'))
ref_newsub <- ref_new %>% select(data_nc_name, cert_name, NC_ID)
trip <- read.csv(file.path(file_path, "Trip_OD_by_NC.csv")) %>% 
  select(-X)

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
# check the differences between penalty_2019$`Neighborhood Council` and ref_new$new_nc_name
setdiff(unique(penalty_2019$`Neighborhood Council`), unique(ref_newsub$data_nc_name)) 

penalty_2019 <- penalty_2019 %>% left_join(ref_newsub, by = c('Neighborhood Council' = 'data_nc_name'))


## 2020
penalty_2020$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2020$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2020$`Neighborhood Council`, 
                                              paste0(penalty_2020$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2020$`Neighborhood Council` and ref_newsub$new_nc_name
setdiff(unique(penalty_2020$`Neighborhood Council`), unique(ref_newsub$data_nc_name)) 
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
setdiff(unique(penalty_2020$`Neighborhood Council`), unique(ref_newsub$data_nc_name)) 

# join ref_newsub
penalty_2020 <- penalty_2020 %>% left_join(ref_newsub, by = c('Neighborhood Council' = 'data_nc_name'))

# check na
table(is.na(penalty_2020))


## 2021
penalty_2021$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2021$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2021$`Neighborhood Council`, 
                                              paste0(penalty_2021$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2021$`Neighborhood Council` and ref_newsub$new_nc_name
setdiff(unique(penalty_2021$`Neighborhood Council`), unique(ref_newsub$data_nc_name)) 
 
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
setdiff(unique(penalty_2021$`Neighborhood Council`), unique(ref_newsub$data_nc_name)) 

# join ref_newsub
penalty_2021 <- penalty_2021 %>% left_join(ref_newsub, by = c('Neighborhood Council' = 'data_nc_name'))

# check na
table(is.na(penalty_2021))


# 2022
penalty_2022$`Neighborhood Council` <- ifelse(grepl("NC", penalty_2022$`Neighborhood Council`, ignore.case = TRUE), 
                                              penalty_2022$`Neighborhood Council`, 
                                              paste0(penalty_2022$`Neighborhood Council`, " NC")) %>% toupper()

# check the differences between penalty_2022$`Neighborhood Council` and ref_newsub$new_nc_name
setdiff(unique(penalty_2022$`Neighborhood Council`), unique(ref_newsub$data_nc_name)) 
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
setdiff(unique(penalty_2022$`Neighborhood Council`), unique(ref_newsub$data_nc_name)) 

# join ref_newsub
penalty_2022 <- penalty_2022 %>% left_join(ref_newsub, by = c('Neighborhood Council' = 'data_nc_name'))

# check na
table(is.na(penalty_2022))

# append data
penalty_output <- bind_rows(penalty_2019, penalty_2020, penalty_2021, penalty_2022)  %>% 
  left_join(ref_new, by = c('nc_id' = 'NC_ID')) %>% 
  select('Creation Date', nc_id, cert_name, 'Neighborhood Council', 'Violation/Infraction/Issue', SFV)


### Analyze data
## penalty type group by nc
penalty_table <- function(penalty_data){
  penalty_data %>%
    group_by(new_nc_name, `Violation/Infraction/Issue`) %>%
    tally() %>%
    pivot_wider(names_from = `Violation/Infraction/Issue`, values_from = n, values_fill = 0)
}

for (i in 2019:2022) {
  penalty_data <- get(paste0("penalty_", i))
  penalty_table_data <- penalty_table(penalty_data)
  
  #export data(csv)
  write.csv(penalty_table_data, file = file.path(file_path, paste0("penalty_table_", i, ".csv")), row.names = F)
  
}

# export data(geojson)
#geojosn file is too big to use the loop
st_write(penalty_2019, file.path(file_path, 'penalty_2019.geojson'), delete_dsn = TRUE)
st_write(penalty_2020, file.path(file_path, 'penalty_2020.geojson'), delete_dsn = TRUE) 
st_write(penalty_2021, file.path(file_path, 'penalty_2021.geojson'), delete_dsn = TRUE)
st_write(penalty_2022, file.path(file_path, 'penalty_2022.geojson'), delete_dsn = TRUE)


## penalty per trip
# Trip data
# adjust the format of month in trip data
trip$month <- as.Date(trip$month) %>%
  format("%Y-%m")

# count the number of trips
trip_origin <- trip %>% 
  select(-c(dest_new_nc_name, dest_nc_id)) %>% 
  group_by(month, origin_nc_id) %>% 
  summarise(ori_trip_n = sum(trips))

trip_dest <- trip %>% 
  select(-c(origin_new_nc_name, origin_nc_id)) %>% 
  group_by(month, dest_nc_id) %>% 
  summarise(dest_trip_n = sum(trips))

# Penalty data
penalty_month <- penalty_output %>% 
  
  # Create Month column
  mutate(Month = format(ymd_hms(penalty_output$`Creation Date`), "%Y-%m")) %>% 
  
  # Monthly penalty number group by nc
  group_by(cert_name, Month) %>% 
  tally() %>%
  pivot_wider(names_from = cert_name, values_from = n, values_fill = 0) %>% 
  pivot_longer(col = -Month, names_to = "nc", values_to = "penalty_n") %>%
  
  # add nc_id and SFV column
  left_join(penalty_output %>% 
              select(cert_name, nc_id, SFV) %>% 
              distinct(), by = c("nc" = "cert_name")) %>% 

  # join monthly penalty data with trip data
  left_join(trip_origin, by = c("Month" = "month", "nc_id" = "origin_nc_id")) %>% 
  right_join(trip_dest, by = c("Month" = "month", "nc_id" = "dest_nc_id")) %>% 
  
  # remove rows without penalty data
  filter(Month > '2019-02') %>%
  
  # create penalty per trip column
  mutate(ppt_ori = penalty_n/ori_trip_n, ppt_dest = penalty_n/dest_trip_n)


## Plotting
# Plot 1 - Yearly penalty number difference between SFV and non-SFV 
penalty_year <- penalty_month %>% 
  select(Month, SFV, penalty_n) %>% 
  mutate(year = as.Date(Month, "%Y") %>% format("%Y")) %>% 
  group_by(year, SFV) %>% 
  summarise(penalty_n = sum(penalty_n)) %>% 
  na.omit()

ggplot(penalty_year, aes(x = year, y = penalty_n, fill = SFV)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_brewer(palette = "Paired", labels = c("Non-SFV", "SFV")) +
  labs(x = "Year", y = "Number of Penalty", fill = "SFV") + 
  theme_bw()  

ggsave("output/plots/yearly_penalties_diff.png")


# Plot 2 - Penalty per trip by month (SFV ncs and non-SFV ncs)
ggplot(penalty_month %>% na.omit(), aes(x = Month, y = log(ppt_ori), color = SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  labs(x = "Month", y = "Penalties per Trip (log)") +
  scale_x_discrete("Month", guide = guide_axis(angle = 45)) + 
  theme_bw()  

ggsave("output/plots/penalties_per_trip.png")

# Plot 3 - Penalty per trip by month (sum of all SFV ncs and sum of all non-SFV ncs)
penalty_sfv <- penalty_month %>% 
  group_by(Month, SFV) %>% 
  summarise(penalty_n = sum(penalty_n), ori_trip_n = sum(ori_trip_n), dest_trip_n = sum(dest_trip_n)) %>% 
  mutate(ppt_ori = penalty_n/ori_trip_n, ppt_dest = penalty_n/dest_trip_n) %>% 
  na.omit()

ggplot(penalty_sfv, aes(x = Month, y = log(ppt_ori), color = SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  labs(x = "Month", y = "Penalties per Trip (log)") +
  scale_x_discrete("Month", guide = guide_axis(angle = 45)) + 
  theme_bw()  

ggsave("output/plots/penalties_per_trip2.png")

# Plot 4 - Penalty per trip by month (SFV regions and non-SFV regions)
penalty_alter <- penalty_month %>% 
  left_join(ref_new %>% select(-SFV), by = c("nc_id" = "NC_ID")) 

pa1 <- penalty_alter %>% 
  select(-c(Geo_Type, area_mi2, geometry, data_nc_name, cert_name)) %>% 
  group_by(Month, SFV, SERVICE_RE) %>% 
  summarise(penalty_n = sum(penalty_n), ori_trip_n = sum(ori_trip_n), dest_trip_n = sum(dest_trip_n)) %>% 
  mutate(ppt_ori = penalty_n/ori_trip_n, ppt_dest = penalty_n/dest_trip_n) %>% 
  na.omit()

ggplot(pa1, aes(x = Month, y = log(ppt_ori), color = SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  labs(x = "Month", y = "Penalties per Trip (log)") +
  scale_x_discrete("Month", guide = guide_axis(angle = 45)) + 
  theme_bw() 

ggsave("output/plots/penalties_per_trip3.png")

# Plot 5 - Penalty per trip by month (group by geo_tyoes)
pa2 <- penalty_alter %>% 
  select(Month, penalty_n, SFV, Geo_Type, ori_trip_n, dest_trip_n) %>% 
  group_by(Month, Geo_Type, SFV) %>% 
  na.omit() %>% 
  group_by(Month, Geo_Type) %>% 
  summarise(penalty_n = sum(penalty_n), ori_trip_n = sum(ori_trip_n), dest_trip_n = sum(dest_trip_n)) %>% 
  mutate(ppt_ori = penalty_n/ori_trip_n, ppt_dest = penalty_n/dest_trip_n) 

ggplot(pa2, aes(x = Month, y = log(ppt_ori), color = Geo_Type)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2", "olivedrab3")) +
  labs(x = "Month", y = "Penalty per Trip (log)") +
  scale_x_discrete("Month", guide = guide_axis(angle = 45)) + 
  theme_bw() 

ggsave("output/plots/penalties_per_trip4.png")

# Plot 6 - Penalties per square mile by month (SFV ncs and non-SFV ncs)
pa3 <- penalty_alter %>% 
  select(Month, nc, nc_id, penalty_n, area_mi2, SFV) %>% 
  group_by(Month, nc, area_mi2, SFV) %>% 
  summarise(penalty_n = sum(penalty_n)) %>% 
  mutate(ppmpnc = penalty_n/area_mi2) %>% 
  na.omit() 

ggplot(pa3, aes(x = Month, y = log(ppmpnc), color = SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  labs(x = "Month", y = "Penalties per mi^2 per NC (log)") +
  scale_x_discrete("Month", guide = guide_axis(angle = 45)) + 
  theme_bw() 

ggsave("output/plots/penalties_per_mi.png")

# Plot 7 - Penalties per square mile by month (sum of all SFV ncs and sum of all non-SFV ncs)
pa3_1 <- penalty_alter %>% 
  select(Month, nc, nc_id, penalty_n, area_mi2, SFV) %>% 
  group_by(Month, SFV) %>% 
  summarise(penalty_n = sum(penalty_n), area_mi2 = sum(area_mi2)) %>% 
  mutate(ppmpnc = penalty_n/area_mi2) %>% 
  na.omit() 

ggplot(pa3_1, aes(x = Month, y = ppmpnc, color = SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  labs(x = "month", y = "Penalties per mi^2 per NC") +
  scale_x_discrete("Month", guide = guide_axis(angle = 45)) + 
  theme_bw() 

ggsave("output/plots/penalties_per_mi2.png")

# Plot 8 - Penalties per square mile by month (group by geo_type)
pa3_2 <- penalty_alter %>% 
  select(Month, nc, nc_id, penalty_n, area_mi2, Geo_Type, SFV) %>% 
  group_by(Month, Geo_Type, SFV) %>% 
  na.omit() %>% 
  group_by(Month, Geo_Type) %>% 
  summarise(penalty_n = sum(penalty_n), area_mi2 = sum(area_mi2)) %>% 
  mutate(ppmpnc = penalty_n/area_mi2) #%>% 
  na.omit() 

ggplot(pa3_2, aes(x = Month, y = log(ppmpnc), color = Geo_Type)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2", "olivedrab3")) +
  labs(x = "month", y = "Penalties per mi^2 per NC (log)")  +
  scale_x_discrete("Month", guide = guide_axis(angle = 45)) + 
  theme_bw() 

ggsave("output/plots/penalties_per_mi3.png")

############## Count Total Vehicles Deployed from API Data #############
# Purpose: recount the number of vehicles in each NC and SOZ. Numbers used to generate summary statistics to see if operators met the 20% distribution threshold.

# Set Up ##############
# libraries
pacman::p_load("tidyverse","sf","scales","lubridate","openxlsx")

# directories
data_files_dir <- file.path('.','output','files')
plots_dir <- file.path(".","output","plots")
data_api_dir <- file.path('.','data','API - Swarm of Scooters')


# Load Data ##############
# load trip data
# trip_df <- read.csv(file.path(data_files_dir,"Trip_OD_by_NC.csv"))
# deploy_df <- read_csv(file.path(data_files_dir,"veh_depl_long.csv"))

# load georeference files
# nc_georef_noSOZ <- sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_noSOZ.geojson")) %>% 
#   st_make_valid() %>% 
#   st_transform(4326) %>% 
#   mutate(OBJ_ID = 1:nrow(.))

nc_georef_wSOZ <- sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_wSOZ.geojson")) %>% 
  st_make_valid() %>% 
  st_transform(4326) %>% # set CRS to WGS 84
  mutate(OBJ_ID = 1:nrow(.)) # add ID variable to join data after counting

# load other files
api_df = st_read(file.path(data_files_dir,"api_avg_deployment.geojson")) # Karen's work. Load to check info

load(file = file.path(data_files_dir,"API_Exploring_Data.RData")) # my saved work. Load so that you don't need to renegerate anything


# Function for Counting Vehicles in NC #################
# 1. Set default values to 12pm
veh_cts <- function(operator = c("Bird","wheels"), hr_24 = 12) {
  
  # 2. list out all the files for the operator
  all_files = list.files(file.path(data_api_dir,operator),recursive = TRUE)
  
  # 3. take subset of all the files for the selected hour
  all_files = all_files[grepl(pattern = str_c(hr_24,"021[6-7].csv"), all_files)]
  
  # 4. define output with object ID
  output = data.frame(OBJ_ID = 1:nrow(nc_georef_wSOZ))
  
  # begin loop
  for (file_num in seq_along(all_files)) {
  
    # 5. obtain date of the file from the folder name. turn date into YYYY-MM-DD
    file_date = str_extract(all_files[file_num], "\\d{8}")
    file_date = as.POSIXct(file_date, format = "%Y%m%d", tz="")
    file_date = format(file_date,"%Y-%m-%d")
  
    # 6. read csv for that specific date
    writeLines(str_c("\nReading file: ",file.path(data_api_dir,operator,all_files[file_num])))
    csv_df = read_csv(file = file.path(data_api_dir,operator,all_files[file_num]))
  
    # 7. turn csv into sf object with geometry column
    points = st_as_sf(csv_df, coords = c("lon","lat"), crs = st_crs(4326))
  
    # 8. Spatial Join. for each bike ID, assign OBJ_ID of the polygon that the bike ID is inside. There are duplicates since some NC geos overlap. st_intersect and st_intersection returns the same result
    within <- st_within(points, nc_georef_wSOZ) %>% as.data.frame()
    
    # 9. Count the number of points/bikes in each polygon ID (aka OBJ_ID).
    counts =
      within %>% 
      count(col.id)
    
    # 10. Join the counted bikes to the output dataframe. Name column with date.
    output = 
      output %>% 
      left_join(counts, by=c("OBJ_ID"="col.id")) %>% 
      rename_with(~str_c(file_date), .cols = n)
  }
  
  # 11. Left join the NC Geo Reference File to output
  output =
    output %>% 
    mutate(
      
      # sum the total count of vehicles across all 30 days
      TotCt = rowSums(output %>% select(-OBJ_ID), na.rm = TRUE),
      
      # calculate month average 
      AvgDepl = TotCt/length(all_files)) %>%
    
    # Left join reference file
    left_join(nc_georef_wSOZ %>% 
                as.data.frame() %>% 
                select(-c(data_nc_name,geometry)), 
              by="OBJ_ID")
  
  return(output)
}

# Run Function ######
bird = veh_cts(operator = "Bird")
wheels = veh_cts(operator = "wheels")

######### summary stats about 20% ########
# both bird and wheels deployed in Venice and Hollywood SOZ. but since they deployed in Venice, this distribution rule overrides the Hollywood rule. If deploy in Venice, must have 20% of total fleet in EFMDDs.

# Report for the entire period of data. Since SOZs are double counted in their respective NCs, I removed SOZs from the total count of deployment. 

# Write function to calculate the % of total fleet in EFMDDs
pct_EFMDD <- function(df) {
  # determine which col goes into function from loop
  cols = c(names(df)[grepl("^2023",names(df))], "TotCt","AvgDepl")
  
  # create dataframe that will be filled with the %
  df_out = data.frame(Date = cols, pct = NA)
  
  # loop through the column names of interest
  for (i in names(df)) {
    if (!i %in% cols) {next}
    pcts = 
      df %>% 
      group_by(Geo_Type) %>% 
      summarise(ct = sum(.data[[i]], na.rm = TRUE)) %>% 
      mutate(
        tot_noSOZ = sum(.data$ct[!grepl("Special",.data$Geo_Type)]),
        pct = ct/tot_noSOZ)
    
    # add the % of vehicles in EFMDD in df_out
    df_out$pct[df_out$Date %in% i] = pcts$pct[pcts$Geo_Type %in% "Equity-Focus Mobility Development District"]
  }
  
  return(df_out)
}

bird_pct = pct_EFMDD(bird) # never compliant
wheels_pct = pct_EFMDD(wheels) # 3 days compliant
# Bird on avg 11.1% and wheels on avg 12.4% in EFMDDs.

# Join the pct tables #################
operators_pct =
  bird_pct %>% 
  rename(Bird=pct) %>% 
  left_join(wheels_pct %>% 
              rename(Wheels=pct),
            by="Date") %>% 
  pivot_longer(cols = c("Bird","Wheels"),
               names_to = "Operator",
               values_to = "pct") %>% 
  glimpse()


# Minimum Deployment #################
bird_tot =
  bird %>% 
  as.data.frame() %>% 
  filter(!grepl("Special",Geo_Type)) %>% 
  select(matches("^2023")) %>% 
  summarise_all(~sum(.,na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Date",
               values_to = "Bird")
  
wheels_tot =
  wheels %>% 
  as.data.frame() %>% 
  filter(!grepl("Special",Geo_Type)) %>% 
  select(matches("^2023")) %>% 
  summarise_all(~sum(.,na.rm = TRUE)) %>% 
  pivot_longer(cols = everything(),
               names_to = "Date",
               values_to = "Wheels")

operators_tot =
  bird_tot %>% 
  left_join(wheels_tot, by="Date")

# Average daily depl
mean(operators_tot$Bird, na.rm = TRUE) # 4750
mean(operators_tot$Wheels, na.rm = TRUE) # 203

# Compare Counts to Karen's work ###########
compare = 
  bird %>% 
  select(cert_name, NC_ID, AvgDepl) %>% 
  rename(bird_AvgDepl = AvgDepl) %>% 
  left_join(wheels %>% 
              select(NC_ID, AvgDepl) %>% 
              rename(wheels_AvgDepl = AvgDepl),
            by = "NC_ID") %>% 
  left_join(api_df %>% 
              select(nc_id, bird_avg, wheels_avg),
            by=c("NC_ID"="nc_id"))
# our percentages are very close


  

# Plot %s ##############
# Join data to one dataframe, leave NAs for wheels. pivot longer. filter out Avg and Tot
depl_pct =
  operators_pct %>% 
  filter(grepl("\\d+",Date)) %>% 
  ggplot(aes(x=Date, y=pct, color=Operator)) +
  geom_line(aes(group=Operator)) +
  geom_point() +
  geom_hline(yintercept = .2,color="grey", linetype="dashed") +
  scale_color_manual(
    values = c(
      "Bird"="plum3",
      "Wheels"="tomato2")) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +
  scale_y_continuous("Percent of Vehicles in EFMDDs\n", breaks = pretty_breaks(4),labels = label_number(scale = 100, suffix = "%")) +
  labs(
    title = str_c("Figure 17: Daily Deployment in EFMDDs (January 18 to February 16, 2023)"),
    subtitle = str_c("Operators deployed less than 20% of their total fleet in EFMDDs."),
    caption = "Source: APIs of Bird and Wheels."
  ) + 
  theme_classic() +
  theme(text = element_text(family = "Century Gothic"))

      
ggsave(plot = depl_pct,filename = file.path(plots_dir,"EFMDD_Depl_API.png"), width = 8,height = 5)

# Save Tables #############
# exclude SOZs in count of total since they are double counted
bird_sumTot = 
  bird %>% 
  group_by(Geo_Type) %>% 
  summarise(ct = sum(TotCt, na.rm = TRUE)) %>% 
  mutate(
    tot_noSOZ = sum(.data$ct[!grepl("Special",.data$Geo_Type)]),
    pct = ct/tot_noSOZ,
    row.id = c(1,2,4,3)) %>% 
  arrange(row.id) # 11.1%

wheels_sumTot = 
  wheels %>% 
  group_by(Geo_Type) %>% 
  summarise(ct = sum(TotCt, na.rm = TRUE)) %>% 
  mutate(
    tot_noSOZ = sum(.data$ct[!grepl("Special",.data$Geo_Type)]),
    pct = ct/tot_noSOZ,
    row.id = c(1,2,4,3)) %>% 
  arrange(row.id) # 12.4%


# Notes #############
# consistently, there are 36 NCs that do not have any Bird vehicles located there

# Export #############
# save(time_all,bird,wheels,operators_pct,operators_tot,file = file.path(data_files_dir,"API_Exploring_Data.RData"))

xl_sheets = list(
  "Bird_DailyCt_NC" = bird,
  "Wheels_DailyCt_NC" = wheels,
  "Bird_Summary_Geos" = bird_sumTot,
  "Wheels_Summary_Geos" = wheels_sumTot,
  "Operators_EFMDD_Pct" = operators_pct,
  "Operators_TotDepl" = operators_tot
)

# openxlsx::write.xlsx(xl_sheets, file = file.path(data_files_dir,"API_BirdWheels_Summary.xlsx"))


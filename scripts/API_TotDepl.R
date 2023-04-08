# Set Up ####
# libraries
pacman::p_load("tidyverse","sf","scales","lubridate")

# directories
data_files_dir <- file.path('.','output','files')
plots_dir <- file.path(".","output","plots")
data_api_dir <- file.path('.','data','API - Swarm of Scooters')

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
  st_transform(4326) %>% 
  mutate(OBJ_ID = 1:nrow(.))

api_df = st_read(file.path(data_files_dir,"api_avg_deployment.geojson"))



# TRY #################
# set objects
operator = c("Bird") # ,"wheels"
hr_24 = 12

# listing out all the files
all_files = list.files(file.path(data_api_dir,operator),recursive = TRUE)
all_files = all_files[grepl(pattern = str_c(hr_24,"021[6-7].csv"), all_files)]

# make it as a function
# define output
output = data.frame(OBJ_ID = 1:nrow(nc_georef_wSOZ))
 ## here we defined DATE. obtain all the CSV dir for dates of interest
# list out all the files that match the timestamp
# in the loop, get the date, turn into 2023-01-19 format
for (file_num in seq_along(all_files)) {
  print(file_num)

file_date = str_extract(all_files[file_num], "\\d{8}")
file_date = as.POSIXct(file_date, format = "%Y%m%d", tz="")
file_date = format(file_date,"%Y-%m-%d")

csv_df = read_csv(file = file.path(data_api_dir,operator,all_files[file_num]))

points =
  st_as_sf(csv_df, coords = c("lon","lat"), crs = st_crs(4326))


points =
  points %>% 
  mutate(
    poly_ID1 = st_intersects(points, nc_georef_wSOZ),
    poly_ID1 = unlist(as.character(poly_ID1)),
  ) %>% 
  glimpse()

set.seed(4832)

points$poly_ID2 = NA
for (j in seq_along(points$poly_ID1)) {
  # Define the input as a string
  input <- points$poly_ID1[j]
  
  # Use eval() and parse() to convert the string into a vector
  vec <- eval(parse(text = input))
  
  # Run through these conditions to identify the number
  if (any(vec %in% 100)) {
    numID = 100
  } else if (any(vec %in% 101)) {
    numID = 101
  } else if (any(vec %in% 102)) {
    numID = 102
  } else if (length(vec) == 0 ) {
    numID = 0
  } else if (length(vec) == 1) {
    numID = vec
  } else {
    numID = sample(vec, 1)
  }
  
  points$poly_ID2[j] = numID
}

counts = 
  points %>% 
  as.data.frame() %>% 
  filter(poly_ID2 != 0) %>% 
  count(poly_ID2)

output = 
  output %>% 
  left_join(counts, by=c("OBJ_ID"="poly_ID2")) %>% 
  rename_with(~str_c(file_date), .cols = n)
}

#### Next
# 


######### summary stats about 20%
output =
  output %>% 
  mutate(TotCt = rowSums(output %>% select(-OBJ_ID), na.rm = TRUE),
         AvgDepl = TotCt/30) %>%
  left_join(nc_georef_wSOZ %>% select(OBJ_ID,NC_ID), by="OBJ_ID") %>% 
  glimpse()


# compare to Karen's work
api_df = 
  api_df %>% 
  left_join(output %>% select(NC_ID,AvgDepl), by=c("nc_id"="NC_ID"))


########## exploration
time_all =
  t06am %>% 
  left_join(t07am, by='poly_ID2', suffix=c('_06am','_07am')) %>% 
  left_join(t08am, by='poly_ID2', suffix=c("",'_08am')) %>% 
  left_join(t09am, by='poly_ID2', suffix=c("", '_09am')) %>% 
  left_join(t10am, by='poly_ID2', suffix=c("", '_10am')) %>% 
  left_join(t11am, by='poly_ID2', suffix=c("", '_11am')) %>% 
  left_join(t12pm, by='poly_ID2', suffix=c("", '_12pm')) 

# consistently, there are 36 NCs that do not have any vehicles located there
nc_georef_wSOZ %>% 
  filter(!OBJ_ID %in% time_all$poly_ID2) %>% 
  as.data.frame() %>% 
  select(cert_name)

# check to see if they intersect
ggplot() +
  geom_sf(data = points) +
  geom_sf(data = org_geos)

# save(time_all, file = file.path(data_files_dir,"API_Exploring_Data.RData"))

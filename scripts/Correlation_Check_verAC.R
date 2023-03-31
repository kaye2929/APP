# Check the correlation between the 3 main variables in our data: penalties, deployment, and trips
# We ran a correlation check for each NC-month combination. 
# We only analyzed between March 2019 and September 2022.
# March 2019 is the earliest month year for the penalties data. Sept 2022 is the latest month year for the trip and deployment data.


# Set up ####
pacman::p_load('tidyverse','readxl','stringr','sf')


# Create directory file paths
# Files are relative to the APP root folder
data_files_dir <- file.path('.','output','files') # location of all transformed and edited data
data_raw_dir <- file.path('.','data') # location of all untransformed and unedited data


# Load 3 data files
depl_long = read_csv(file.path(data_files_dir,"veh_depl_long.csv"))
penalties = read_csv(file.path(data_files_dir,"penalties_allyrs.csv"))
trips <- read_csv(file.path(data_files_dir,"Trip_OD_by_NC.csv")) 


# Prepare Deployment Data #################
# Remove January and February 2019
depl_long_sub =
  depl_long %>% 
  filter(!grepl("2019-0[1-2]",month_yr))
  

# Prepare Penalties Data #######################
penalties_long =
  penalties %>% 
  pivot_longer(cols = -c(NC_ID:penalty),
               names_to = "month",
               values_to = "count") %>% 
  group_by(cert_name,NC_ID,month) %>% 
  summarise(pen_count = sum(count)) %>% 
  filter(!grepl("2022-1[0-2]",month)) %>% 
  mutate(month = as.POSIXct(month)) %>% 
  glimpse()


# Prepare Trip Data #######################
## trip origin dataframe
trips_origin =
  trips %>% 
  
  # count number of trips by origin per month per NC
  group_by(month, origin_new_nc_name, origin_nc_id) %>% 
  summarise(trips = sum(trips)) %>% 
  
  # remove Jan to Feb 2019
  filter(!grepl("2019-0[1-2]",month)) %>% 
  arrange(origin_new_nc_name,month) %>% 
  rename(origin_ct = trips) %>% 
  glimpse()


## trip destination dataframe
trips_dest =
  trips %>% 
  
  # count number of trips by origin per month per NC
  group_by(month, dest_new_nc_name, dest_nc_id) %>% 
  summarise(trips = sum(trips)) %>% 
  
  # remove Jan to Feb 2019
  filter(!grepl("2019-0[1-2]",month)) %>% 
  arrange(dest_new_nc_name,month) %>% 
  rename(dest_ct = trips) %>% 
  glimpse()


# Merging datasets #################################
combined =
  penalties_long %>% 
  left_join(depl_long_sub, by = c("NC_ID", "cert_name", "month"="month_yr")) %>% 
  left_join(trips_dest, by = c("NC_ID"="dest_nc_id","month")) %>% 
  left_join(trips_origin, by = c("NC_ID"="origin_nc_id","month")) %>% 
  select(NC_ID,cert_name,month,pen_count,avg_depl,dest_ct,origin_ct) %>% 
  glimpse()

table(is.na(combined)) # should have no NAs


# Correlation Check #############################
# All variables are not normally distributed, so we used Spearman's rho instead of Pearson's R. 
# We also ran the correlation with Kendall's tau, which confirmed the results for Spearman's rho.
# all 3 variables are highly correlated with one another.

# Check for normality
hist(combined$pen_count)
hist(combined$avg_depl)
hist(combined$dest_ct)
hist(combined$origin_ct)

# Correlation with Spearman's rho
corr_sp <- 
  combined %>% 
  as.data.frame() %>% 
  select(pen_count,avg_depl,dest_ct,origin_ct) %>% 
  cor(method = "spearman") %>% 
  as.data.frame()

# write.csv(corr_sp, file.path(data_files_dir,"correlation_spearman.csv"))


# Correlation with Kendall's tau
corr_kd <- 
  combined %>% 
  as.data.frame() %>% 
  select(pen_count,avg_depl,dest_ct,origin_ct) %>% 
  cor(method = "kendall") %>% 
  as.data.frame()

# write.csv(corr_kd, file.path(data_files_dir,"correlation_kendall.csv"))


# All combinations show that the spearman's rho is statistical significance
cor.test(combined$pen_count, combined$avg_depl, method = "spearman")
cor.test(combined$pen_count, combined$dest_ct, method = "spearman")
cor.test(combined$pen_count, combined$origin_ct, method = "spearman")
cor.test(combined$avg_depl, combined$dest_ct, method = "spearman")
cor.test(combined$avg_depl, combined$origin_ct, method = "spearman")
cor.test(combined$origin_ct, combined$dest_ct, method = "spearman")


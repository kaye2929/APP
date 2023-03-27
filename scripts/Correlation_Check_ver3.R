library(tidyverse)
library(readxl)
library(stringr)
setwd("C:/APP2/APP/") #This is my absolute file path. Change to APP directory on local.

#############################
###
### Data Cleaning
###
#############################

##########################THIS PART IS TO BE CHANGED AFTER OBTAINING THE DEPLOYMENT CSV############################
deployment <- read_excel("deployment.xls") #This data already calculated the average deployment per year
fulldataset <- read_excel("deployment.xls") #Will be merging penalty and trip data to this object.
###################################################################################################################
penalty2022 <- read_csv(file = "./output/files/penalty_table_2022.csv") #Data that was on Github->output->files
penalty2021 <- read_csv(file = "./output/files/penalty_table_2021.csv") #Data that was on Github->output->files
penalty2020 <- read_csv(file = "./output/files/penalty_table_2020.csv") #Data that was on Github->output->files
penalty2019 <- read_csv(file = "./output/files/penalty_table_2019.csv") #Data that was on Github->output->files
trips <- read_csv(file = "./output/files/Trip_OD_by_NC.csv") #Abraham's data on Github

#Data cleaning for penalties
#For Penalties in 2019
names(penalty2019)
penalty2019 <- rename(penalty2019,
                      unpermitted_company = "Unpermitted Company and/or veh",
                      vehicle_improperly_parked = "Vehicle improperly parked (lay",
                      damaged_or_unsanitary_vehicle = "Damaged or unsanitary Vehicle",
                      low_battery = "Low Battery",
                      parked_on_private_property = "Parked on Private Property",
                      vehicle_listed_as_available = "Vehicle listed as available, b",
                      sidewalk_riding = "Sidewalk Riding",
                      other = "Other")
names(penalty2019)
penalty2019 <- penalty2019 %>%
  mutate(pen_tot2019 = other + unpermitted_company + vehicle_improperly_parked + damaged_or_unsanitary_vehicle + low_battery + parked_on_private_property + vehicle_listed_as_available + sidewalk_riding) %>%
  select(new_nc_name, pen_tot2019)
nrow(penalty2019)
#There are 83 observations (82 NC observations and 1 NA.)

#For penalties in 2020
names(penalty2020)
penalty2020 <- rename(penalty2020,
                      unpermitted_company = "Unpermitted Company and/or veh",
                      vehicle_improperly_parked = "Vehicle improperly parked (lay",
                      damaged_or_unsanitary_vehicle = "Damaged or unsanitary Vehicle",
                      low_battery = "Low Battery",
                      parked_on_private_property = "Parked on Private Property",
                      vehicle_listed_as_available = "Vehicle listed as available, b",
                      sidewalk_riding = "Sidewalk Riding",
                      other = "Other")
names(penalty2020)
penalty2020 <- penalty2020 %>%
  mutate(pen_tot2020 = other + unpermitted_company + vehicle_improperly_parked + damaged_or_unsanitary_vehicle + low_battery + parked_on_private_property + vehicle_listed_as_available + sidewalk_riding) %>%
  select(new_nc_name, pen_tot2020)
nrow(penalty2020)
#There are 80 observations (80 NC observations)

#For penalties in 2021
names(penalty2021)
penalty2021 <- rename(penalty2021,
                      unpermitted_company = "Unpermitted Company and/or veh",
                      vehicle_improperly_parked = "Vehicle improperly parked (lay",
                      damaged_or_unsanitary_vehicle = "Damaged or unsanitary Vehicle",
                      low_battery = "Low Battery",
                      parked_on_private_property = "Parked on Private Property",
                      vehicle_listed_as_available = "Vehicle listed as available, b",
                      sidewalk_riding = "Sidewalk Riding",
                      other = "Other")
names(penalty2021)
penalty2021 <- penalty2021 %>%
  mutate(pen_tot2021 = other + unpermitted_company + vehicle_improperly_parked + damaged_or_unsanitary_vehicle + low_battery + parked_on_private_property + vehicle_listed_as_available + sidewalk_riding) %>%
  select(new_nc_name, pen_tot2021)
nrow(penalty2021)
#There are 92 observations (92 NC observations)

#For penalties in 2022
names(penalty2022)
penalty2022 <- rename(penalty2022,
                      unpermitted_company = "Unpermitted Company and/or veh",
                      vehicle_improperly_parked = "Vehicle improperly parked (lay",
                      damaged_or_unsanitary_vehicle = "Damaged or unsanitary Vehicle",
                      low_battery = "Low Battery",
                      parked_on_private_property = "Parked on Private Property",
                      vehicle_listed_as_available = "Vehicle listed as available, b",
                      sidewalk_riding = "Sidewalk Riding",
                      other = "Other")
names(penalty2022)
penalty2022 <- penalty2022 %>%
  mutate(pen_tot2022 = other + unpermitted_company + vehicle_improperly_parked + damaged_or_unsanitary_vehicle + low_battery + parked_on_private_property + vehicle_listed_as_available + sidewalk_riding) %>%
  select(new_nc_name, pen_tot2022)
nrow(penalty2022)
#There are 92 observations (91 NC observations, 1 NA observation)

#Merge the penalty datas
penalty_a <- full_join(x = penalty2019, y = penalty2020, by = "new_nc_name")
penalty_b <- full_join(x = penalty_a, y = penalty2021, by = "new_nc_name")
penalty_c <- full_join(x = penalty_b, y = penalty2022, by = "new_nc_name")
nrow(penalty_c)
#There are 99 observations (98 NC observations, 1 NA observation)

penalty_entire <- penalty_c %>% select(-new_nc_name)
penalty_entire$pen_tot_entire <- rowMeans(penalty_entire, na.rm = TRUE)
penalty_entire$new_nc_name <- penalty_c$new_nc_name

penalty_entire_fin <- penalty_entire %>% select(new_nc_name, pen_tot_entire)

rm(penalty_a, penalty_b, penalty_c, penalty_entire)

#Data cleaning for trips
trips$year <- str_replace(trips$month,
                          pattern = "(\\d{4})-(\\d{2})-(\\d{2})",
                          replacement = "\\1")

trips_NC <- trips %>% group_by(year, origin_new_nc_name) %>%
  summarize(total = sum(trips)) %>%
  pivot_wider(names_from = year, values_from = total, names_prefix = "trips_")

trips_NC2 <- trips_NC %>%
  select(-origin_new_nc_name)
trips_NC2$trips_year_avg <- rowMeans(trips_NC2) #Not using na.rm argument because there were no NAs.
trips_NC2$origin_new_nc_name <- trips_NC$origin_new_nc_name
trips_NC_fin <- trips_NC2 %>% select(origin_new_nc_name, trips_year_avg)

rm(trips_NC, trips_NC2)

#Start Merging
####################THIS PART NEEDS TO BE CHANGED AFTER OBTAINING DEPLOYMENT CSV DATA#########################
fulldataset2 <- fulldataset %>%
  left_join(x = fulldataset, y = penalty_entire_fin, by = c("new_nc_name" = "Neighborhood Council"))

fulldataset3 <- fulldataset2 %>%
  left_join(x = fulldataset2, y = trips_NC_fin, by = c("new_nc_name" = "origin_new_nc_name"))

fulldataset_final <- fulldataset3 %>% select(new_nc_name, Dep_Avg_Entire, pen_tot_entire, trips_year_avg)

write_csv(fulldataset_final, file = "../wholedata.csv")

rm(fulldataset2, fulldataset3)
##############################################################################################################

#################################
###
### Correlation Check. 
###
#################################

###################THE CODES BELOW WOULD ALSO NEED RECODING ACCORDINGLY######################################

#Eliminating the NC name variable because with it you can't perform correlation check
corcheckdata <- fulldataset_final %>% select(-new_nc_name) 

#Checking Normality
hist(corcheckdata$Dep_Avg_Entire)
hist(corcheckdata$pen_tot_entire)
hist(corcheckdata$trips_year_avg)

#All the variables are not normally distributed.
#Therefore, using Spearman's rho.

corr_matrix <- cor(corcheckdata, method = "spearman", use = "pairwise.complete.obs")
corr_matrix2 <- as.data.frame(corr_matrix)
write_csv(corr_matrix2, file = "../correlation_matrix.csv")

#testing the statistical significance of an example of a combination
cor.test(corcheckdata$Dep_Avg_Entire, corcheckdata$pen_tot_entire, method = "spearman", use = "pairwise.complete.obs")
cor.test(corcheckdata$Dep_Avg_Entire, corcheckdata$trips_year_avg, method = "spearman", use = "pairwise.complete.obs")
cor.test(corcheckdata$pen_tot_entire, corcheckdata$trips_year_avg, method = "spearman", use = "pairwise.complete.obs")


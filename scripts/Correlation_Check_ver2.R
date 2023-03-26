library(tidyverse)
library(readxl)
library(stringr)

#############################
###
### Data Cleaning
###
#############################

# NOTE!!!
# The file paths are all absolute file paths because I am working on my personal drive.

deployment <- read_excel("deployment.xls") #This data already calculated the average deployment per year
#                                           by taking the averages of each month in the excel.
fulldataset <- read_excel("deployment.xls") #Will be merging penalty and trip data to this object.
penalty2022 <- read_excel("penalty.xlsx", sheet = "2022") #Karen's data on GoogleDrive
penalty2021 <- read_excel("penalty.xlsx", sheet = "2021")
penalty2020 <- read_excel("penalty.xlsx", sheet = "2020")
penalty2019 <- read_excel("penalty.xlsx", sheet = "2019")
trips <- read_csv("Trip_OD_by_NC.csv") #Abraham's data on Github

#Data cleaning for penalties
penalty2019 <- penalty2019 %>%
  mutate(pen_tot2019 = Damaged_or_unsanitary_Vehicle+Low_Battery+Parked_on_Private_Property+Sidewalk_Riding+Unpermitted_Company+Vehicle_improperly_parked+Vehicle_listed_as_available+Other) %>%
  select(`Neighborhood Council`, pen_tot2019)

penalty2020 <- penalty2020 %>%
  mutate(pen_tot2020 = Damaged_or_unsanitary_Vehicle+Low_Battery+Parked_on_Private_Property+Sidewalk_Riding+Unpermitted_Company+Vehicle_improperly_parked+Vehicle_listed_as_available+Other) %>%
  select(`Neighborhood Council`, pen_tot2020)

penalty2021 <- penalty2021 %>%
  mutate(pen_tot2021 = Damaged_or_unsanitary_Vehicle+Low_Battery+Parked_on_Private_Property+Sidewalk_Riding+Unpermitted_Company+Vehicle_improperly_parked+Vehicle_listed_as_available+Other) %>%
  select(`Neighborhood Council`, pen_tot2021)

penalty2022 <- penalty2022 %>%
  mutate(pen_tot2022 = Damaged_or_unsanitary_Vehicle+Low_Battery+Parked_on_Private_Property+Sidewalk_Riding+Unpermitted_Company+Vehicle_improperly_parked+Vehicle_listed_as_available+Other) %>%
  select(`Neighborhood Council`, pen_tot2022)

penalty_a <- full_join(x = penalty2019, y = penalty2020, by = "Neighborhood Council")
penalty_b <- full_join(x = penalty_a, y = penalty2021, by = "Neighborhood Council")
penalty_c <- full_join(x = penalty_b, y = penalty2022, by = "Neighborhood Council")

penalty_entire <- penalty_c %>% select(-`Neighborhood Council`)
penalty_entire$pen_tot_entire <- rowMeans(penalty_entire, na.rm = TRUE)
penalty_entire$'Neighborhood Council' <- penalty_c$`Neighborhood Council`

penalty_entire_fin <- penalty_entire %>% select(`Neighborhood Council`, pen_tot_entire)

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

trips_NC2$trips_year_avg <- rowMeans(trips_NC2)

trips_NC2$origin_new_nc_name <- trips_NC$origin_new_nc_name

trips_NC_fin <- trips_NC2 %>% select(origin_new_nc_name, trips_year_avg)

rm(trips_NC, trips_NC2)

#Start Merging
fulldataset2 <- fulldataset %>%
  left_join(x = fulldataset, y = penalty_entire_fin, by = c("new_nc_name" = "Neighborhood Council"))

fulldataset3 <- fulldataset2 %>%
  left_join(x = fulldataset2, y = trips_NC_fin, by = c("new_nc_name" = "origin_new_nc_name"))

fulldataset_final <- fulldataset3 %>% select(new_nc_name, Dep_Avg_Entire, pen_tot_entire, trips_year_avg)

write_csv(fulldataset_final, file = "../wholedata.csv")

rm(fulldataset2, fulldataset3)

#################################
###
### Correlation Check
###
#################################

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


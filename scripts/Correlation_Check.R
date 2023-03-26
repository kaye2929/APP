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

#Data cleaning for trips
trips$year <- str_replace(trips$month,
                          pattern = "(\\d{4})-(\\d{2})-(\\d{2})",
                          replacement = "\\1")

trips_NC <- trips %>% group_by(year, origin_new_nc_name) %>%
  summarize(total = sum(trips)) %>%
  pivot_wider(names_from = year, values_from = total, names_prefix = "trips_")

#Start Merging
fulldataset2 <- fulldataset %>%
  left_join(x = fulldataset, y = penalty2022, by = c("new_nc_name" = "Neighborhood Council"))

fulldataset3 <- fulldataset2 %>%
  left_join(x = fulldataset2, y = penalty2021,by = c("new_nc_name" = "Neighborhood Council"))

fulldataset4 <- fulldataset3 %>%
  left_join(x = fulldataset3, y = penalty2020, by = c("new_nc_name" = "Neighborhood Council"))

fulldataset5 <- fulldataset4 %>%
  left_join(x = fulldataset4, y = penalty2019, by = c("new_nc_name" = "Neighborhood Council"))

fulldataset6 <- fulldataset5 %>%
  left_join(x = fulldataset5, y = trips_NC, by = c("new_nc_name" = "origin_new_nc_name"))

fulldatasetfinal <- fulldataset6 %>% select(new_nc_name, Dep_Avg_2022, Dep_Avg_2021, Dep_Avg_2020, Dep_Avg_2019, pen_tot2022, pen_tot2021, pen_tot2020, pen_tot2019, trips_2022, trips_2021, trips_2020, trips_2019)

write_csv(fulldatasetfinal, file = "../wholedata.csv")

rm(fulldataset, fulldataset2, fulldataset3, fulldataset4, fulldataset5, fulldataset6)

#################################
###
### Correlation Check
###
#################################

#Eliminating the NC name variable because with it you can't perform correlation check
corcheckdata <- fulldatasetfinal %>% select(-new_nc_name) 

#Checking Normality
hist(corcheckdata$Dep_Avg_2022)
hist(corcheckdata$Dep_Avg_2021)
hist(corcheckdata$Dep_Avg_2020)
hist(corcheckdata$Dep_Avg_2019)
hist(corcheckdata$pen_tot2022)
hist(corcheckdata$pen_tot2021)
hist(corcheckdata$pen_tot2020)
hist(corcheckdata$pen_tot2019)
hist(corcheckdata$trips_2022)
hist(corcheckdata$trips_2021)
hist(corcheckdata$trips_2020)
hist(corcheckdata$trips_2019)

#All the variables are not normally distributed.
#Therefore, using Spearman's rho.

corr_matrix <- cor(corcheckdata, method = "spearman", use = "pairwise.complete.obs")
corr_matrix2 <- as.data.frame(corr_matrix)
write_csv(corr_matrix2, file = "../correlation_matrix.csv")

#testing the statistical significance of an example of a combination
cor.test(corcheckdata$Dep_Avg_2019, corcheckdata$pen_tot2019, method = "spearman", use = "pairwise.complete.obs")


##### Make Trip Count Figures #######################
# Author: Abraham Cheung
# Purpose of script: make charts and graphs for data using trip counts

# Set Up ####
# libraries
pacman::p_load("tidyverse","sf","scales","lubridate")

# directories
data_files_dir <- file.path('.','output','files')
plots_dir <- file.path(".","output","plots")

# load trip data
trip_df <- read.csv(file.path(data_files_dir,"Trip_OD_by_NC.csv"))
deploy_df <- read_csv(file.path(data_files_dir,"veh_depl_long.csv"))

# load georeference files
nc_georef_noSOZ <- sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_noSOZ.geojson"))


# Clean Trip Data ############
# Prep trip data - we are only using origin counts since origin and destination are very similar.
tr_org =
  trip_df %>% 
  group_by(origin_nc_id,origin_new_nc_name,month) %>% 
  summarise(trips = sum(trips)) %>% 
  left_join(nc_georef_noSOZ %>%
              as.data.frame() %>%
              select(NC_ID,SFV:pop_over18_est),
            by=c("origin_nc_id"="NC_ID")) %>% 
  mutate(pop1000_est = pop_est/1000,
         pop1000_over18_est = pop_over18_est/1000,
         Geo_Type_wSOZ = 
           recode(
             Geo_Type_wSOZ,
             "Standard Permitted District" = "SPD",
             "Equity-Focus Mobility Development District" = "EFMDD",
             "Mobility Development District" = "MDD",
             "Special Operations Zone" = "SOZ")) %>% 
  glimpse()

# tr_des =
#   trip_df %>% 
#   group_by(dest_nc_id,dest_new_nc_name,month) %>% 
#   summarise(trips = sum(trips)) %>% 
#   left_join(nc_georef_noSOZ %>%
#               as.data.frame() %>%
#               select(NC_ID,SFV:pop_over18_est),
#             by=c("dest_nc_id"="NC_ID")) %>% 
#   mutate(pop1000_est = pop_est/1000,
#          pop1000_over18_est = pop_over18_est/1000,
#          Geo_Type_wSOZ = recode(Geo_Type_wSOZ,
#                                 "Standard Permitted District" = "SPD",
#                                 "Equity-Focus Mobility Development District" = "EFMDD",
#                                 "Mobility Development District" = "MDD",
#                                 "Special Operations Zone" = "SOZ"
#          )) %>% 
#   glimpse()

# Color Info ####
# four colors: dodgerblue3 - EFMDD or non-SFV, tomato2 - MDD or SFV, olivedrab3  - SPD, plum3 - SOZ

# Plots 1: Pilot Program Year (PPYR) Trip Totals  ##############
# the code below generates the data and plot for trip totals. However, we decided to plot and present the results for average trips per month by NC because the average smooths out variation between NCs.
# we kept the code below just in case.

# 1. only keep trip counts for the months in the specific years (Pilot Program)
yr_regex = "2019-0[4-9]|2019-1[0-2]|2020-0[1-3]"
tr_org_ppyr =
  tr_org %>%
  filter(grepl(yr_regex,month)) %>%
  mutate(days_mo = lubridate::days_in_month(month))

# 2. calculate average for SFV and non SFV
tr_org_ppyr_sum =
  tr_org_ppyr %>%
  group_by(SFV,month,days_mo) %>%
  summarise(sum = sum(trips, na.rm=TRUE)) %>%
  group_by(SFV) %>%
  summarise(avg_tr = weighted.mean(sum, days_mo, na.rm=TRUE))

# 3. plot deployment by SFV and not
tr_org_ppyr_plot =
  tr_org_ppyr %>%
  group_by(SFV,month) %>%
  summarise(sum = sum(trips, na.rm=TRUE)) %>%
  ggplot(aes(x=as.character(month),y=sum,color=SFV)) +
  geom_line(aes(group=SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  # geom_hline(yintercept = c(tr_org_ppyr_sum$avg_tr[1],tr_org_ppyr_sum$avg_tr[2]), linetype='dashed', color=c('dodgerblue3','tomato2')) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Trips\n", breaks = pretty_breaks(10),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Figure X: SFV and Non-SFV Total Trips (Pilot Program)"),
    subtitle = str_c("Trips are reported as total monthly trips starting in the SFV"),
    caption = "Source: LADOT CPRA Data"
  ) +
  theme_classic()

ggsave(plot = tr_org_ppyr_plot,filename = file.path(plots_dir,"Trips_Pilot_Tot.png"), width = 6,height = 5)


# # 4. plot log transformed y-axis
# tr_org_ppyr_log_plot =
#   tr_org_ppyr_plot +
#   scale_y_continuous("Trips (Logged) \n",trans = scales::log_trans(),labels = label_number(scale = .001,suffix = "K",big.mark = ",")) +
#   labs(
#     title = str_c("Figure X: ","SFV and Non-SFV Total Trips (","Pilot Program",")"),
#     subtitle = str_c("Trips are reported as logged total monthly trips starting in the SFV")
#   )
# 
# ggsave(plot = tr_org_ppyr_log_plot,filename = file.path(plots_dir,"Trips_Origin_Log_Total_Pilot.png"), width = 6,height = 5)

# Plots 2: PPYR Trips Averaged ##############
# 1. reuse df from previous section. kept trips for the months in the specific years (Pilot Program). 
tr_org_ppyr

# 2. calculate average trips for SFV and non SFV
tr_org_ppyr_avg =
  tr_org_ppyr %>% 
  group_by(SFV) %>% 
  summarise(avg_tr = weighted.mean(trips, days_mo, na.rm=TRUE))

# 3. plot trips by SFV and not
tr_org_ppyr_avg_plot =
  tr_org_ppyr %>% 
  group_by(SFV,month) %>% 
  summarise(avg = mean(trips, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month),y=avg,color=SFV)) +
  geom_line(aes(group=SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  # geom_hline(yintercept = c(tr_org_ppyr_avg$avg_tr[1],tr_org_ppyr_avg$avg_tr[2]), linetype='dashed', color=c('dodgerblue3','tomato2')) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Trips\n", breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Figure X: SFV and Non-SFV Average Trips (Pilot Program)"),
    subtitle = str_c("Trips are reported as average monthly trips for a trip starting in the SFV"),
    caption = "Source: LADOT CPRA Data"
  ) +
  theme_classic()

ggsave(plot = tr_org_ppyr_avg_plot,filename = file.path(plots_dir,"Trips_Pilot_Avg.png"), width = 6,height = 5)


# 4. plot log transformed y-axis
# we are no longer presenting the log transformed plots since it may be hard for readers to understand. But we are keeping the code for future reference.
# tr_org_ppyr_logavg_plot =
#   tr_org_ppyr_avg_plot +
#   scale_y_continuous("Average Trips (Logged) \n",trans = scales::log_trans(),labels = label_number(accuracy = 1, scale = .001,suffix = "K",big.mark = ",")) +
#   labs(
#     title = str_c("Figure X: ","SFV and Non-SFV Average Monthly Trips (","Pilot Program",")"),
#     subtitle = str_c("Trips are reported as logged average monthly trips for a trip starting in the SFV")
#   )
# 
# ggsave(plot = tr_org_ppyr_logavg_plot,filename = file.path(plots_dir,"Trips_Origin_LogAvg_Pilot.png"), width = 6,height = 5)


# Plots 3: Trips Current Program ############
# 1. filter for months April 2020 to Sept 2022
yr_regex = "2020-0[4-9]|2020-1[0-2]|202[1-2]"
tr_curr =
  tr_org %>% 
  filter(grepl(yr_regex,month)) %>% 
  mutate(days_mo = lubridate::days_in_month(month))


# 2. plot deployment by 4 geo types with average
tr_curr_avg =
  tr_curr %>% 
  group_by(Geo_Type_wSOZ,month) %>% 
  summarise(avg = mean(trips, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month),y=avg,color=Geo_Type_wSOZ)) +
  geom_line(aes(group=Geo_Type_wSOZ)) +
  geom_point() +
  scale_color_manual(
    values = c(
      "SOZ"="plum3",
      "MDD"="tomato2",
      "EFMDD"="dodgerblue3",
      "SPD"="olivedrab3"
    ),
    breaks = c("SOZ","MDD","EFMDD","SPD")) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Trips\n", breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Figure X: ","Avergae Trips by Geography Types (Current Program",")"),
    subtitle = str_c("Trips are reported as average monthly trips for a trip starting in a Program Geography"),
    color = "Program Geographies",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic()

ggsave(plot = tr_curr_avg,filename = file.path(plots_dir,"Trips_Current_Avg.png"), width = 8,height = 5)


# trip totals during program geos
tr_curr_tot =
  tr_curr %>% 
  group_by(Geo_Type_wSOZ,month) %>% 
  summarise(avg = sum(trips, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month),y=avg,color=Geo_Type_wSOZ)) +
  geom_line(aes(group=Geo_Type_wSOZ)) +
  geom_point() +
  scale_color_manual(
    values = c(
      "SOZ"="plum3",
      "MDD"="tomato2",
      "EFMDD"="dodgerblue3",
      "SPD"="olivedrab3"
    ),
    breaks = c("SOZ","MDD","EFMDD","SPD")) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Total Trips\n", breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Figure X: ","Total Trips by Geography Types (Current Program",")"),
    subtitle = str_c("Trips are reported as total monthly trips for a trip starting in a Program Geography"),
    color = "Program Geographies",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic()

ggsave(plot = tr_curr_tot,filename = file.path(plots_dir,"Trips_Current_Total.png"), width = 8,height = 5)


# Exploratory
Dec21=
  tr_org %>% 
  filter(grepl("2021-12-01",month)) %>% 
  select(origin_new_nc_name,trips) %>% 
  glimpse()

Nov21=tr_org %>% 
  filter(grepl("2021-11-01",month)) %>% 
  select(origin_new_nc_name,trips) %>% 
  glimpse()


# Plot 4: Trips across all months ###########
# for appendix, no filtering by Program or years
# TOTAL
tr_all_tot =
  tr_org %>% 
  group_by(Geo_Type_wSOZ,month) %>% 
  summarise(avg = sum(trips, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month),y=avg,color=Geo_Type_wSOZ)) +
  geom_line(aes(group=Geo_Type_wSOZ)) +
  geom_point() +
  scale_color_manual(
    values = c(
      "SOZ"="plum3",
      "MDD"="tomato2",
      "EFMDD"="dodgerblue3",
      "SPD"="olivedrab3"
    ),
    breaks = c("SOZ","MDD","EFMDD","SPD")) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Total Trips\n", breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Appendix Figure X: Total Trips by Geography Types (2019-2022)"),
    subtitle = str_c("Trips are reported as total monthly trips for a trip starting in a Program Geography."),
    color = "Program Geographies",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic()

ggsave(plot = tr_all_tot,filename = file.path(plots_dir,"Trips_AllYrs_Tot.png"), width = 10,height = 6)


# AVERAGE
tr_all_avg =
  tr_org %>% 
  group_by(Geo_Type_wSOZ,month) %>% 
  summarise(avg = mean(trips, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month),y=avg,color=Geo_Type_wSOZ)) +
  geom_line(aes(group=Geo_Type_wSOZ)) +
  geom_point() +
  scale_color_manual(
    values = c(
      "SOZ"="plum3",
      "MDD"="tomato2",
      "EFMDD"="dodgerblue3",
      "SPD"="olivedrab3"
    ),
    breaks = c("SOZ","MDD","EFMDD","SPD")) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Trips\n", breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Appendix Figure X: Average Trips by Geography Types (2019-2022)"),
    subtitle = str_c("Trips are reported as average monthly trips for a trip starting in a Program Geography."),
    color = "Program Geographies",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic()

ggsave(plot = tr_all_avg,filename = file.path(plots_dir,"Trips_AllYrs_Avg.png"), width = 10,height = 6)

# Plot 5: Trips across all months by SFV ###########
# for appendix, no filtering by Program or years
# for penalties analysis
# TOT
tr_allSFV_tot =
  tr_org %>% 
  group_by(SFV,month) %>% 
  summarise(avg = sum(trips, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month),y=avg,color=SFV)) +
  geom_line(aes(group=SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Total Trips\n", breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Appendix Figure X: Total Trips by SFV (2019-2022)"),
    subtitle = str_c("Trips are reported as total monthly trips for a trip starting in a Program Geography"),
    color = "SFV",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic()

ggsave(plot = tr_allSFV_tot,filename = file.path(plots_dir,"Trips_AllYrs_SFV_Tot.png"), width = 10,height = 6)

# AVG
tr_allSFV_avg =
  tr_org %>% 
  group_by(SFV,month) %>% 
  summarise(avg = mean(trips, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month),y=avg,color=SFV)) +
  geom_line(aes(group=SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Trips\n", breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Appendix Figure X: Average Trips by SFV (2019-2022)"),
    subtitle = str_c("Trips are reported as average monthly trips for a trip starting in a Program Geography"),
    color = "SFV",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic()

ggsave(plot = tr_allSFV_avg,filename = file.path(plots_dir,"Trips_AllYrs_SFV_Avg.png"), width = 10,height = 6)

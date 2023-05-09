##### Make Trip Count Figures #######################
# Author: Abraham Cheung
# Purpose of script: make charts and graphs for data using trip counts

# Set Up ####
# libraries
pacman::p_load("tidyverse","sf","scales","lubridate","openxlsx","extrafont", install = F, update = F)

# directories
data_files_dir <- file.path('.','output','files')
plots_dir <- file.path(".","output","plots")
plots_svg_dir <- file.path(".","output","plots_svg")

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
             "Special Operations Zone" = "SOZ"),
         days_mo = lubridate::days_in_month(month)) %>% 
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


# Plot 1: All Total Program Geos Trips ###########
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
    title = str_c("Appendix Figure 6: Total Trips by Geography Types (2019-2022)"),
    subtitle = str_c("Trips are reported as total monthly trips for a trip starting in a Program Geography."),
    color = "Program Geographies",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic() +
  theme(text = element_text(family = "Century Gothic"))

ggsave(plot = tr_all_tot,filename = file.path(plots_dir,"Trips_AllYrs_Tot.png"), width = 10,height = 6)

ggsave(plot = tr_all_tot,filename = file.path(plots_svg_dir,"Appendix6_Trips_AllYrs_Tot.svg"), width = 10,height = 6)

# Plot 2: All Avg Program Geos Trips ###########
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
    title = str_c("Appendix Figure 8: Average Trips by Geography Types (2019-2022)"),
    subtitle = str_c("Trips are reported as average monthly trips for a trip starting in a Program Geography."),
    color = "Program Geographies",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic() +
  theme(text = element_text(family = "Century Gothic"))

ggsave(plot = tr_all_avg,filename = file.path(plots_dir,"Trips_AllYrs_Avg.png"), width = 10,height = 6)

ggsave(plot = tr_all_avg,filename = file.path(plots_svg_dir,"Appendix8_Trips_AllYrs_Avg.svg"), width = 10,height = 6)

# Plot 3: All Total SFV Trips ###########
# for appendix, no filtering by Program or years
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
    title = str_c("Appendix Figure 5: Total Trips by SFV (2019-2022)"),
    subtitle = str_c("Trips are reported as total monthly trips for a trip starting in a Program Geography"),
    color = "SFV",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic() +
  theme(text = element_text(family = "Century Gothic"))

ggsave(plot = tr_allSFV_tot,filename = file.path(plots_dir,"Trips_AllYrs_SFV_Tot.png"), width = 10,height = 6)

ggsave(plot = tr_allSFV_tot,filename = file.path(plots_svg_dir,"Appendix5_Trips_AllYrs_SFV_Tot.svg"), width = 10,height = 6)

# Plot 4: All Avg SFV Trips ###########
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
    title = str_c("Appendix Figure 7: Average Trips by SFV (2019-2022)"),
    subtitle = str_c("Trips are reported as average monthly trips for a trip starting in a Program Geography"),
    color = "SFV",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic() +
  theme(text = element_text(family = "Century Gothic"))

ggsave(plot = tr_allSFV_avg,filename = file.path(plots_dir,"Trips_AllYrs_SFV_Avg.png"), width = 10,height = 6)

ggsave(plot = tr_allSFV_avg,filename = file.path(plots_svg_dir,"Appendix7_Trips_AllYrs_SFV_Avg.svg"), width = 10,height = 6)

# Plot 5: PP Total Trips  ##############
# 1. only keep trip counts for the months in the specific years (Pilot Program)
yr_regex = "2019-0[4-9]|2019-1[0-2]|2020-0[1-3]"
pp_months = unique(tr_org$month) %>% 
  grep(yr_regex,.,value = TRUE)

# 2. edit the previous complete plot, change labels
tr_org_ppyr_plot =
  tr_allSFV_tot +
  scale_x_discrete("\nMonth", limits = pp_months, guide = guide_axis(angle = 45)) +
  labs(
    title = str_c("Figure 5: SFV and Non-SFV Total Trips (Pilot Program)"),
    subtitle = str_c("Trips are reported as total monthly trips starting in the SFV."),
    caption = "Source: LADOT CPRA Data")
  
# 3. save plot
ggsave(plot = tr_org_ppyr_plot,filename = file.path(plots_dir,"Trips_Pilot_Tot.png"), width = 6,height = 5)

ggsave(plot = tr_org_ppyr_plot,filename = file.path(plots_svg_dir,"Figure5_Trips_Pilot_Tot.svg"), width = 6,height = 5)

# Plot 6: PP Avg Trips ##############
# 1. reuse this plot and list of months. 
tr_allSFV_avg
pp_months

# 2. edit the previous complete plot, change labels
tr_org_ppyr_avg_plot =
  tr_allSFV_avg +
  scale_x_discrete("\nMonth", limits = pp_months, guide = guide_axis(angle = 45)) +
  labs(
    title = str_c("Figure 6: SFV and Non-SFV Average Trips (Pilot Program)"),
    subtitle = str_c("Trips are reported as average monthly trips starting in the SFV."),
    caption = "Source: LADOT CPRA Data")

# 3. save plot
ggsave(plot = tr_org_ppyr_avg_plot,filename = file.path(plots_dir,"Trips_Pilot_Avg.png"), width = 6,height = 5)

ggsave(plot = tr_org_ppyr_avg_plot,filename = file.path(plots_svg_dir,"Figure6_Trips_Pilot_Avg.svg"), width = 6,height = 5)

# Plot 7: Current Total Trips ############
# 1. make list of months for the current program
yr_regex = "2020-0[4-9]|2020-1[0-2]|202[1-2]"
curr_months = unique(tr_org$month) %>% 
  grep(yr_regex,.,value = TRUE)

# 2. edit the previous complete plot, change labels
tr_curr_tot =
  tr_all_tot +
  scale_x_discrete("\nMonth", limits = curr_months, guide = guide_axis(angle = 45)) +
  scale_y_continuous("Total Trips\n", limits = c(0,350000), breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Figure 11: Total Trips by Geography Types (Current Program)"),
    subtitle = str_c("Trips are reported as total monthly trips starting in a Program Geography."),
    caption = "Source: LADOT CPRA Data")

# 3. save plot
ggsave(plot = tr_curr_tot,filename = file.path(plots_dir,"Trips_Current_Total.png"), width = 8,height = 5)

ggsave(plot = tr_curr_tot,filename = file.path(plots_svg_dir,"Figure11_Trips_Current_Total.svg"), width = 8,height = 5)

# Plot 8: Current Avg Trips ############
# 1. reuse previous plot and list of months
curr_months
tr_all_avg

# 2. edit the previous complete plot, change labels
tr_curr_avg =
  tr_all_avg +
  scale_x_discrete("\nMonth", limits = curr_months, guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Trips\n", limits = c(0,70000), breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Figure 12: Average Trips by Geography Types (Current Program)"),
    subtitle = str_c("Trips are reported as average monthly trips starting in a Program Geography."),
    caption = "Source: LADOT CPRA Data")

# 3. save plot
ggsave(plot = tr_curr_avg,filename = file.path(plots_dir,"Trips_Current_Avg.png"), width = 8,height = 5)

ggsave(plot = tr_curr_avg,filename = file.path(plots_svg_dir,"Figure12_Trips_Current_Avg.svg"), width = 8,height = 5)

# Summary Stats ##############
# SS 1: PP SFV ############
tr_pilot =
  tr_org %>% 
  filter(month %in% pp_months)

# average and total trips in SFV
summ_PP_avg =
  tr_pilot %>% 
  group_by(SFV) %>% 
  summarise(avg_12mo = weighted.mean(trips, days_mo, na.rm = TRUE),
            tot_12mo = sum(trips, na.rm = TRUE))

# max total trip in any NC for any month by SFV
summ_PP_max =
  tr_pilot %>% 
  group_by(SFV) %>% 
  slice_max(trips) %>% 
  select(origin_nc_id:SFV,Geo_Type_wSOZ)

# Plot 9: PP Top 3 Trips ################
# NCs with Top avg depl, help show the range
summ_pp_top_tr =
  tr_pilot %>% 
  group_by(origin_new_nc_name, SFV) %>% 
  summarise(wtavg = weighted.mean(trips, days_mo, na.rm = TRUE),
            total = sum(trips, na.rm = TRUE)) %>% 
  group_by(SFV) %>% 
  slice_max(total, n=3) %>% 
  arrange(desc(total)) %>% 
  glimpse()

# plot
tr_ppTop3_plot =
  summ_pp_top_tr %>% 
  ggplot(aes(x=reorder(origin_new_nc_name,total), y=total, fill=SFV)) +
  geom_col(aes(group=SFV)) +
  geom_label(
    aes(label = comma(total)),hjust = -.1,size=3, fill = grey(0.95), label.size = 0, family = "Century Gothic") +
  scale_fill_manual(
    values = c("dodgerblue3", "tomato2"), 
    labels = c("Non-SFV", "SFV")) +
  scale_x_discrete("Neighborhood Councils", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("\nTotal Trips", limits = c(0,2250000), breaks = pretty_breaks(8),labels = label_number(scale = .000001, suffix = "M", big.mark = ",")) +
  labs(
    title = "Figure 4: Top 3 Neighborhood Councils with Highest Total Trip for SFV and Non-SFV Areas",
    subtitle = "Trips are reported as the total number of trips during the Pilot Program (April 2019 to March 2020).",
    caption = "Source: LADOT CPRA Data"
  ) +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic"))

# save
ggsave(plot = tr_ppTop3_plot, filename = file.path(plots_dir,"Trips_Pilot_Top3.png"),width = 10,height = 7)

ggsave(plot = tr_ppTop3_plot, filename = file.path(plots_svg_dir,"Figure4_Trips_Pilot_Top3.svg"),width = 10,height = 7)


# SS 2: Current Prog Geos ###############
tr_curr =
  tr_org %>% 
  filter(month %in% curr_months, !month %in% '2021-12-01')

# average and total trips in SFV
summ_curr_avg =
  tr_curr %>% 
  group_by(Geo_Type_wSOZ) %>% 
  summarise(avg_12mo = weighted.mean(trips, days_mo, na.rm = TRUE),
            tot_12mo = sum(trips, na.rm = TRUE)) %>% 
  glimpse()

# max total trip in any NC for any month by SFV
summ_curr_max =
  tr_curr %>% 
  group_by(Geo_Type_wSOZ) %>% 
  slice_max(trips) %>% 
  select(origin_nc_id:SFV,Geo_Type_wSOZ) %>% 
  glimpse()

# Plot 10: Current Top 3 Trips ###########
# NCs with Top avg depl, help show the range
summ_curr_top_tr =
  tr_curr %>% 
  group_by(origin_new_nc_name, Geo_Type_wSOZ) %>% 
  summarise(wtavg = weighted.mean(trips, days_mo, na.rm = TRUE),
            total = sum(trips, na.rm = TRUE)) %>% 
  group_by(Geo_Type_wSOZ) %>% 
  slice_max(total, n=3) %>% 
  arrange(desc(total)) %>% 
  glimpse()

# plot
tr_currTop3_plot =
  summ_curr_top_tr %>% 
  ggplot(aes(x=reorder(origin_new_nc_name,total), y=total, fill=Geo_Type_wSOZ)) +
  geom_col(aes(group=Geo_Type_wSOZ)) +
  geom_label(
    aes(label = comma(total)),hjust = -.1,size=3, fill = grey(0.95), label.size = 0, family = "Century Gothic") +
  scale_fill_manual(
    name = "Program Geographies",
    values = c(
      "SOZ"="plum3",
      "MDD"="tomato2",
      "EFMDD"="dodgerblue3",
      "SPD"="olivedrab3"),
    breaks = c("SOZ","MDD","EFMDD","SPD")) +
  scale_x_discrete("Neighborhood Councils", labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous("\nTotal Trips", limits = c(0,3750000), breaks = pretty_breaks(8),labels = label_number(scale = .000001, suffix = "M")) +
  labs(
    title = "Figure 13: Top 3 Neighborhood Councils with Highest Total Trips for Each Program Geography",
    subtitle = "Trips are reported as the total number of trips during the Current Program (April 2020 to September 2022).",
    caption = "Source: LADOT CPRA Data"
  ) +
  coord_flip() +
  theme_bw() +
  theme(text = element_text(family = "Century Gothic"))

# save
ggsave(plot = tr_currTop3_plot, filename = file.path(plots_dir,"Trips_Current_Top3.png"),width = 10,height = 7)

ggsave(plot = tr_currTop3_plot, filename = file.path(plots_svg_dir,"Figure13_Trips_Current_Top3.svg"),width = 10,height = 7)


# Export DFs as excel sheets ##############
xl_sheets =
  list(
    "Pilot_Summary" = summ_PP_avg,
    "Pilot_NCMax" = summ_PP_max,
    "CurrProg_Summary" = summ_curr_avg,
    "CurrProg_NCMax" = summ_curr_max
  )

# write.xlsx(xl_sheets, file = file.path(data_files_dir, "TripCt_Summary.xlsx"))


# Plot 11: April 2021 Range ########
# 1. only keep trip counts for the months in the specific months
yr_regex = "2021-0[2-6]"
apr_months = unique(tr_org$month) %>% 
  grep(yr_regex,.,value = TRUE)

# 2. edit the previous complete plot, change labels
tr_apr21_plot =
  tr_all_tot +
  scale_x_discrete("\nMonth", limits = apr_months, guide = guide_axis(angle = 45)) +
  scale_y_continuous("Total Trips\n", breaks = pretty_breaks(8),limits = c(0,350000), labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  labs(
    title = str_c("Figure 21: Total Trips from February to June 2021"),
    subtitle = str_c("Deployment is reported as the average number of vehicles\navailable each day by month."),
    caption = "Source: LADOT CPRA Data")

# 3. save plot
ggsave(plot = tr_apr21_plot,filename = file.path(plots_dir,"Trips_Apr21_Tot.png"), width = 7,height = 7)

ggsave(plot = tr_apr21_plot,filename = file.path(plots_svg_dir,"Figure21_Trips_Apr21_Tot.svg"), width = 7,height = 7)

# Storage #############
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

tr_org %>% 
  group_by(Geo_Type_wSOZ,month) %>% 
  summarise(avg = sum(trips, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month),y=avg,color=Geo_Type_wSOZ)) +
  geom_line(aes(group=Geo_Type_wSOZ)) +
  geom_point() +
  # scale_color_manual(
  #   values = c(
  #     "SOZ"="plum3",
  #     "MDD"="tomato2",
  #     "EFMDD"="dodgerblue3",
  #     "SPD"="olivedrab3"
  #   ),
  #   breaks = c("SOZ","MDD","EFMDD","SPD")) +
  # scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  # scale_y_continuous("Total Trips\n", breaks = pretty_breaks(8),labels = label_number(scale = .001, suffix = "K", big.mark = ",")) +
  # labs(
  #   title = str_c("Appendix Figure X: Total Trips by Geography Types (2019-2022)"),
  #   subtitle = str_c("Trips are reported as total monthly trips for a trip starting in a Program Geography."),
  #   color = "Program Geographies",
  #   caption = "Source: LADOT CPRA Data"
  # ) + 
  theme(text = element_text(family = "Century Gothic")) +
  # theme_classic()

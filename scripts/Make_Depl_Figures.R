##### Make Deployment Figures #######################
# Author: Abraham Cheung
# Purpose of script: make charts, tables, and graphs for data using deployment averages 

# Set Up ####
# libraries
pacman::p_load("tidyverse","sf","scales","lubridate","openxlsx")

# directories
data_files_dir <- file.path('.','output','files')
plots_dir <- file.path(".","output","plots")

# load trip data
trip_df <- read.csv(file.path(data_files_dir,"Trip_OD_by_NC.csv"))
deploy_df <- read_csv(file.path(data_files_dir,"veh_depl_long.csv"))

# load georeference files
nc_georef_noSOZ <- sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_noSOZ.geojson"))


# Clean Depl Data ###############
# join NC_ID info to deployment df
depl_joined <-
  deploy_df %>%
  left_join(nc_georef_noSOZ %>%
              as.data.frame() %>%
              select(NC_ID,SFV:pop_over18_est),
            by="NC_ID") %>% 
  mutate(pop1000_est = pop_est/1000,
         pop1000_over18_est = pop_over18_est/1000,
         Geo_Type_wSOZ = recode(Geo_Type_wSOZ,
           "Standard Permitted District" = "SPD",
           "Equity-Focus Mobility Development District" = "EFMDD",
           "Mobility Development District" = "MDD",
           "Special Operations Zone" = "SOZ"),
           days_mo = lubridate::days_in_month(month_yr))

# Color Info ####
# four colors: dodgerblue3 - EFMDD or non-SFV, tomato2 - MDD or SFV, olivedrab3  - SPD, plum3 - SOZ

# PLOTS ###################
# P1: All Mo. by Prog Geos###########
# for appendix, no filtering, by program geographies (EFMDD, MDD, SOZ, SPD) for all months
depl_all_plot =
  depl_joined %>% 
  group_by(Geo_Type_wSOZ,month_yr) %>% 
  summarise(avg = mean(avg_depl, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month_yr),y=avg,color=Geo_Type_wSOZ)) +
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
  scale_y_continuous("Average Deployment\n", breaks = pretty_breaks(8)) +
  labs(
    title = "Appendix Figure X: Deployment All Months (2019-2022)",
    subtitle = "Deployment is reported as the average number of vehicles available each day by month.",
    color = "Program Geographies",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic()

ggsave(plot = depl_all_plot,filename = file.path(plots_dir,"Deployment_AllYrs.png"), width = 10,height = 6)


# P2: All Mo. by SFV ###########
# for appendix, no filtering, by SFV or non SFV, for all months
depl_allSFV_plot =
  depl_joined %>% 
  group_by(SFV,month_yr) %>% 
  summarise(avg = mean(avg_depl, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month_yr),y=avg,color=SFV)) +
  geom_line(aes(group=SFV)) +
  geom_point() +
  scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Deployment\n", breaks = pretty_breaks(8)) +
  labs(
    title = "Appendix Figure X: Deployment All Months (2019-2022) by SFV",
    subtitle = "Deployment is reported as the average number of vehicles available each day by month.",
    color = "SFV",
    caption = "Source: LADOT CPRA Data"
  ) + 
  theme_classic()

ggsave(plot = depl_allSFV_plot,filename = file.path(plots_dir,"Deployment_AllYrs_SFV.png"), width = 10,height = 6)

# P3: PP Avg by SFV ####
# plot deployment during Pilot Program (Apr 2019-Mar2020), group by SFV or not

# 1. only keep deployment for the months in the specific years (Pilot Program)
yr_regex = "2019-0[4-9]|2019-1[0-2]|2020-0[1-3]"
pp_months = unique(depl_joined$month_yr) %>% 
  grep(yr_regex,.,value = TRUE)

# 2. edit the previous complete plot, change labels
depl_ppSFV_plot =
  depl_allSFV_plot +
  scale_x_discrete("\nMonth", limits = pp_months, guide = guide_axis(angle = 45)) +
  labs(
    title = str_c("Figure X: SFV and Non-SFV Deployment (Pilot Program)"),
    subtitle = str_c("Deployment is reported as the average number of vehicles available \neach day by month."),
    caption = "Source: LADOT CPRA Data")
  
# 3. save plot
ggsave(plot = depl_ppSFV_plot,filename = file.path(plots_dir,"Deployment_Pilot_SFV.png"), width = 6,height = 5)


# P4: Curr Avg Depl by Program Geos ####
# 1. only keep deployment for the months in the specific years of the "current" program (Apr 2020 to Sept 2022)
yr_regex = "2020-0[4-9]|2020-1[0-2]|202[1-2]"
curr_months = unique(depl_joined$month_yr) %>% 
  grep(yr_regex,.,value = TRUE)


# 2. edit previous complete plot, add label changes
depl_curr_plot =
  depl_all_plot +
  scale_x_discrete("\nMonth", limits = curr_months, guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Deployment\n", limits = c(0,1300), breaks = pretty_breaks(8)) +
  labs(
    title = str_c("Figure X: Deployment by Geography Types (Current Program)"),
    subtitle = str_c("Deployment is reported as the average number of vehicles available each day by month."),
    caption = "Source: LADOT CPRA Data")

# 3. save plot
ggsave(plot = depl_curr_plot,filename = file.path(plots_dir,"Deployment_Current.png"), width = 8,height = 5)


# SUMMARY STATS #############
# SS1: PP Depl ###############
depl_pilot = 
  depl_joined %>% 
  filter(as.character(month_yr) %in% pp_months)

# weighted average across all months of pilot program
summ_pp_avg =
  depl_pilot %>% 
  group_by(SFV) %>% 
  summarise(avg_12mo = weighted.mean(avg_depl, days_mo, na.rm = TRUE))

# max deployment NC
summ_pp_max =
  depl_pilot %>% 
  group_by(SFV) %>% 
  slice_max(avg_depl) %>% 
  select(NC_ID:SFV,Geo_Type_wSOZ,-data_nc_name)

# P5: PP Top 3 Depl #############
# NCs with Top avg depl, help show the range
summ_pp_top_depl =
  depl_pilot %>% 
  group_by(cert_name, SFV) %>% 
  summarise(wtavg = weighted.mean(avg_depl, days_mo, na.rm = TRUE)) %>% 
  group_by(SFV) %>% 
  slice_max(wtavg, n=3) %>% 
  arrange(desc(wtavg)) %>% 
  glimpse()

# plot
depl_ppTop3_plot =
  summ_pp_top_depl %>% 
  ggplot(aes(x=reorder(cert_name,wtavg), y=wtavg, fill=SFV)) +
  geom_col(aes(group=SFV)) +
  geom_label(
    aes(label = comma(round(wtavg))),hjust = -.1,size=3, fill = grey(0.95), label.size = 0) +
  scale_fill_manual(
    values = c("dodgerblue3", "tomato2"), 
    labels = c("Non-SFV", "SFV")) +
  scale_x_discrete("Neighborhood Council", labels = function(x) str_wrap(x, width = 10)) +
  scale_y_continuous("\nAverage Deployment") +
  labs(
    title = "Figure X: Top 3 Neighborhood Councils with Highest  Deployment for SFV and Non-SFV Areas",
    subtitle = "Deployment is the average deployment during the Pilot Program (April 2019 to March 2020).",
    caption = "Source: LADOT CPRA Data"
  ) +
  coord_flip() +
  theme_bw()

ggsave(plot = depl_ppTop3_plot, filename = file.path(plots_dir,"Deployment_Pilot_Top3.png"),width = 10,height = 7)


# SS2: Curr Depl ##############
depl_curr = 
  depl_joined %>% 
  filter(as.character(month_yr) %in% curr_months)

# weighted average across all months of current program
summ_curr_avg =
  depl_curr %>% 
  group_by(Geo_Type_wSOZ) %>% 
  summarise(avg_12mo = weighted.mean(avg_depl, days_mo, na.rm = TRUE)) %>% 
  glimpse()

# max deployment NC
summ_curr_max =
  depl_curr %>% 
  group_by(Geo_Type_wSOZ) %>% 
  slice_max(avg_depl) %>% 
  select(NC_ID:SFV,Geo_Type_wSOZ,-data_nc_name)

# P6: Current Top 3 Depl #############
# NCs with Top avg depl, help show the range
summ_curr_top_depl =
  depl_curr %>% 
  group_by(cert_name, Geo_Type_wSOZ) %>% 
  summarise(wtavg = weighted.mean(avg_depl, days_mo, na.rm = TRUE)) %>% 
  group_by(Geo_Type_wSOZ) %>% 
  slice_max(wtavg, n=3) %>% 
  arrange(desc(wtavg)) %>% 
  glimpse()

# plot
depl_currTop3_plot =
  summ_curr_top_depl %>% 
  ggplot(aes(x= reorder(cert_name,wtavg), y=wtavg, fill=Geo_Type_wSOZ)) +
  geom_col(aes(group=Geo_Type_wSOZ)) +
  geom_label(
    aes(label = comma(round(wtavg))),hjust = -.1,size=3, fill = grey(0.95), label.size = 0) +
  scale_fill_manual(
    name = "Program Geographies",
    values = c(
      "SOZ"="plum3",
      "MDD"="tomato2",
      "EFMDD"="dodgerblue3",
      "SPD"="olivedrab3"
    )) +
  scale_x_discrete("Neighborhood Council", labels = function(x) str_wrap(x, width = 15)) +
  scale_y_continuous("\nAverage Deployment") +
  labs(
    title = "Figure X: Top 3 Neighborhood Councils with Highest Deployment for Each Program Geography",
    subtitle = "Deployment is the average deployment during the current program (April 2020 to September 2022).",
    caption = "Source: LADOT CPRA Data"
  ) +
  coord_flip() +
  theme_bw()

ggsave(plot = depl_currTop3_plot, filename = file.path(plots_dir,"Deployment_Current_Top3.png"),width = 10.5,height = 7)


# Export to Excel ##########
xl_sheets =
  list(
    "Pilot_Summary" = summ_pp_avg,
    # "Pilot_NCMax" = summ_pp_max,
    "CurrProg_Summary" = summ_curr_avg
    # "CurrProg_NCMax" = summ_curr_max
  )

# write.xlsx(xl_sheets, file = file.path(data_files_dir, "Depl_Summary.xlsx"))


# Normality check for: ######
# sq mi2, per capita, and per capita >18
# conclusion: the distribution of NC areas is right skewed, but the population distribution of NCs are about normally distributed. There are 2 outliers for having significantly larger areas. And only 1 outlier for having significantly larger population. The zones types are about equally distributed in all NC areas and population sizes. As a result, we do not determine there to be significant changes in the trip, deployment, and penalty counts when divided by area or population. Additionally, our unit of analysis is at the San Fernando Valley (or non SFV) level and the 4 program geography types. Aggregating to such a high level of geography eliminates differences in the areas and population.

## square miles
# plot
nrm_chk_mi2 =
  nc_georef_noSOZ %>% 
  ggplot(aes(area_mi2, fill=Geo_Type_wSOZ)) +
  geom_histogram(binwidth = 1, color="white") +
  scale_x_continuous(name = "Square Miles") +
  ylab("Count of Neighborhood Councils") +
  labs(title = "Distribution of Neighborhood Council Area") +
  theme_classic()
ggsave(filename = file.path(plots_dir,"Normality_Check_SqMi.png"),plot = nrm_chk_mi2,width = 6,height = 5)

# outliers where z >= 3 or z <= -3
nc_georef_noSOZ %>% 
  as.data.frame() %>% 
  mutate(
    zscore = (area_mi2 - mean(area_mi2))/sd(area_mi2)
  ) %>% 
  select(cert_name,area_mi2,zscore) %>% 
  filter(zscore >=3 | zscore <= -3) %>% 
  arrange(zscore) 
# only 3 neighborhood councils with significantly larger geographies than average
#   cert_name                   area_mi2   zscore
# 1    BEL AIR-BEVERLY CREST NC 17.04331 3.096032
# 2 FOOTHILL TRAILS DISTRICT NC 19.60131 3.735471


## per capita
# plot
nrm_chk_pop =
  nc_georef_noSOZ %>% 
  ggplot(aes(pop_est, fill=Geo_Type_wSOZ)) +
  geom_histogram(binwidth = 10000, color="white") +
  scale_x_continuous(name = "Population") +
  ylab("Count of Neighborhood Councils") +
  labs(title = "Distribution of Neighborhood Councils by Population") +
  theme_classic()
ggsave(filename = file.path(plots_dir,"Normality_Check_Pop.png"),plot = nrm_chk_pop,width = 6,height = 5)

# outliers 
nc_georef_noSOZ %>% 
  as.data.frame() %>% 
  mutate(
    zscore = (pop_est - mean(pop_est))/sd(pop_est)
  ) %>% 
  select(cert_name,pop_est,zscore) %>% 
  filter(zscore >=3 | zscore <= -3) %>% 
  arrange(zscore) 
# only one NC with significantly larger population
#   cert_name                      pop_est    zscore
# 1 WILSHIRE CENTER - KOREATOWN NC 96431.34 3.061989


## per capita over 18
# plot
nrm_chk_pop18 =
  nc_georef_noSOZ %>% 
  ggplot(aes(pop_over18_est,fill=Geo_Type_wSOZ)) +
  geom_histogram(binwidth = 10000, color="white") +
  scale_x_continuous(name = "Population") +
  ylab("Count of Neighborhood Councils") +
  labs(title = "Distribution of Neighborhood Councils by Population") +
  theme_classic()
ggsave(filename = file.path(plots_dir,"Normality_Check_Pop18.png"),plot = nrm_chk_pop18,width = 6,height = 5)

# outliers
nc_georef_noSOZ %>% 
  as.data.frame() %>% 
  mutate(
    zscore = (pop_over18_est - mean(pop_over18_est))/sd(pop_over18_est)
  ) %>% 
  select(cert_name,pop_over18_est,zscore) %>% 
  filter(zscore >=3 | zscore <= -3) %>% 
  arrange(zscore) 
# only one NC with significantly larger population over 18
#   cert_name                      pop_over18_est  zscore
# 1 WILSHIRE CENTER - KOREATOWN NC        78015.4 3.25778


# Storage: ####
## plotting with linear model ##########
# plot2 =
#   depl_ppyr %>% 
#   ggplot(aes(x=as.character(month_yr),y=avg_depl,group=SFV)) +
#   geom_point(aes(shape=SFV)) +
#   scale_shape(solid = FALSE) +
#   geom_smooth(method="lm",aes(linetype=SFV)) +
#   scale_x_discrete("Month",guide = guide_axis(angle = 45)) +
#   scale_y_continuous("Average Deployment \n (Natural Log)", trans = scales::log_trans(), labels = label_number(accuracy = .01, big.mark = ",")) +
#   scale_colour_hue(labels = c("Non-SFV NC", "SFV NC")) +  
#   ggtitle("Deployment by Neighborhood Council during Pilot Program") +
#   theme_classic()
#   
# ggsave(plot = plot2,filename = file.path(plots_dir,"Deployment_PilotProg.png"),
#        width = 6, height = 6)

# don't log values before averaging #########
## log is not be good way to analyze bc if 0, then it is dropped, any average of logged numbers is then an overstatement of the real change
# but you can log the final average to see more of the variation
# this table shows that when you log an estimate, you lose observations
depl_per_pop1000_est_df %>% 
  filter(!is.na(depl_p_log)) %>% 
  group_by(SFV,month_yr) %>% 
  count() %>% 
  print(n=nrow(.))

# A tibble: 24 × 3
# Groups:   SFV, month_yr [24]
# SFV   month_yr       n
# <lgl> <date>     <int>
#   1 FALSE 2019-04-01    64
# 2 FALSE 2019-05-01    64
# 3 FALSE 2019-06-01    64
# 4 FALSE 2019-07-01    64
# 5 FALSE 2019-08-01    63
# 6 FALSE 2019-09-01    64
# 7 FALSE 2019-10-01    60
# 8 FALSE 2019-11-01    57
# 9 FALSE 2019-12-01    59
# 10 FALSE 2020-01-01    56
# 11 FALSE 2020-02-01    56
# 12 FALSE 2020-03-01    59
# 13 TRUE  2019-04-01    26
# 14 TRUE  2019-05-01    31
# 15 TRUE  2019-06-01    30
# 16 TRUE  2019-07-01    32
# 17 TRUE  2019-08-01    34
# 18 TRUE  2019-09-01    33
# 19 TRUE  2019-10-01    31
# 20 TRUE  2019-11-01    30
# 21 TRUE  2019-12-01    27
# 22 TRUE  2020-01-01    20
# 23 TRUE  2020-02-01    19
# 24 TRUE  2020-03-01    21


# log transformed y-axis #########
# we initially plotted log transformed plots using the same data, but we decided that the non transformed plots are easier to read. 
# We commented out this code just in case we want to use this again in the future
# depl_plot_log = 
#   depl_ppSFV_plot +
#   scale_y_continuous("Average Deployment\n",trans = scales::log_trans(),labels = label_number(accuracy = .01)) +
#   labs(
#     title = str_c("Figure X: ","SFV and Non-SFV Deployment (","Pilot Program",")"),
#     subtitle = str_c("Deployment is reported as logged average monthly deployment")
#   )
#   
# ggsave(plot = depl_plot_log,filename = file.path(plots_dir,"Deployment_SFV_Pilot_Logged.png"), width = 6,height = 5)


# Plotting Depl Function ###########
# average deployment by area, per capita, and per capita over 18
# however, since these plots are very similar to the avg deployment (untransformed), we will not be plotting these variables anymore
# we commented out this code to keep it for future use

# plot_depl <- function(df = depl_ppyr, denom, loop_obj = i) {
# 
#   # make dataframe for the average deployment per mi2, capita, capita >=18
#   df =
#     df %>%
#     mutate(
#       depl_per = avg_depl/{{denom}},
#       depl_p_log = log(depl_per),
#       
#       # log(0)=-Inf, so return as NA
#       depl_p_log = ifelse(is.infinite(depl_p_log),NA,depl_p_log))
# 
#   # making plot for depl_per
#   # make a summary table for average across the whole period
#   depl_per_sum =
#     df %>%
#     group_by(SFV) %>%
#     summarise(avg_SFV_depl = weighted.mean(depl_per, days_mo))
# 
#   # make a plot with depl_per
#   depl_per_plot =
#     df %>%
#     group_by(SFV, month_yr) %>%
#     summarise(avg = mean(depl_per, na.rm=TRUE)) %>%
#     ggplot(aes(x=as.character(month_yr),y=avg,color=SFV)) +
#     geom_point() +
#     geom_line(aes(group=SFV)) +
#     scale_color_manual(values = c("dodgerblue3", "tomato2"), labels = c("Non-SFV", "SFV")) +
#     geom_hline(yintercept = c(depl_per_sum$avg_SFV_depl[1],depl_per_sum$avg_SFV_depl[2]), linetype='dashed', color=c("dodgerblue3", "tomato2")) +
#     scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
#     ylab(str_c("Deployment per ", loop_obj)) +
#     labs(
#       title = str_c("Figure X: ","SFV and Non-SFV Deployment during ","Pilot Program"),
#       subtitle = str_c("Average deployment is calculated per ",loop_obj),
#       caption = "Source: LADOT CPRA Data"
#     ) +
#     theme_classic()
# 
#   # making plot with log transformed axes to show more variation in the lower count values
#   depl_p_log_plot =
#     depl_per_plot +
#     scale_y_continuous(str_c("Deployment per log(", loop_obj, ")"), trans = scales::log_trans(), labels = label_number(accuracy = .01)) +
#     labs(
#       title = str_c("Figure X: ","SFV and Non-SFV Deployment (","Pilot Program",")"),
#       subtitle = str_c("Deployment is reported as logged average monthly deployment per ",loop_obj))
#       
#   # return objects as list
#   return(list(df,depl_per_sum,depl_per_plot,depl_p_log_plot))
# 
# }


## Loop thru plot_depl
# words <- c("area_mi2","pop1000_est","pop1000_over18_est")
# for (i in words) {
#   writeLines(str_c("\nMaking tables and plots for deployment per ",i))
#   res = plot_depl(denom = get(i))
#   
#   # assign unique objects 
#   assign(str_c("depl_per_",i,"_df"),res[[1]])
#   assign(str_c("depl_per_",i,"_sum"),res[[2]])
#   assign(str_c("depl_per_",i,"_plot"),res[[3]])
#   assign(str_c("depl_per_",i,"_log_plot"),res[[4]])
#   
#   # save plots
#   ggsave(file.path(plots_dir,str_c("depl_per_",i,"_pilot.png")),plot = res[[3]],width = 6,height = 5)
#   ggsave(file.path(plots_dir,str_c("depl_per_",i,"_log_pilot.png")),plot = res[[4]],width = 6,height = 5)
#   }




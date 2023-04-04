##### Make Deployment Figures #######################
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
nc_georef_wSOZ <- sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_wSOZ.geojson"))

# join NC_ID info to deployment df
depl_joined <-
  deploy_df %>%
  left_join(nc_georef_noSOZ %>%
              as.data.frame() %>%
              select(NC_ID,SFV:pop_over18_est),
            by="NC_ID") %>% 
  mutate(pop1000_est = pop_est/1000,
         pop1000_over18_est = pop_over18_est/1000)

# Plot 1: Working Plot ####
June_depl = deploy_df %>% 
  select(new_nc_name,nc_id,June_2019) %>% 
  inner_join(nc_georef_noSOZ %>% 
               as.data.frame() %>% 
               select(NC_ID,SERVICE_RE,SFV,area_mi2,Geo_Type,pop_est,pop_over18_est),
             by=c("nc_id"="NC_ID")) %>% 
  mutate(June_2019_tot = June_2019*30) %>% 
  as.data.frame()  

city_area = sum(nc_georef_noSOZ$area_mi2)

June_depl %>% 
  summarise(avg_avg = mean(June_2019, na.rm=TRUE), # average of average deployment, WRONG
            tot_depl = sum(June_2019_tot), # total depl after finding total count each month
            avg_depl_city = tot_depl/length(June_2019_tot), # avg depl per NC
            tot_mi2 = sum(area_mi2), # total sq mi in city
            avg_tot_mi2 = tot_depl/tot_mi2, # 
            avg_tot_wt_mi2 = sum(June_2019*area_mi2)/tot_mi2, # avg daily depl wt by mi2
            avg_tot_wt_day_mi2 = sum(June_2019_tot*area_mi2)/tot_mi2) %>% # avg monthly depl wt by mi, can transf to prev one by dividing by # days in month. This is lower than `avg_depl_city`
  glimpse()


ggplot(data = nc_georef_noSOZ,aes(x=area_mi2))+
  geom_histogram()+
  geom_vline(aes(xintercept = median(area_mi2))) # 3.29 mi2


# avg depl per sq mi
June_depl %>% 
  mutate(AvgDepl_mi2 = June_2019/area_mi2) %>% 
  summarise(avg_avg = mean(AvgDepl_mi2), # wrong
            AvgDepl_wt_mi2 = sum(AvgDepl_mi2*area_mi2)/city_area # avg depl per mi2 wt by NC mi2
            ) %>% 
  glimpse()

# as area increases, the average deployment decreases
June_depl %>% 
  mutate(AvgDepl_mi2 = June_2019/area_mi2) %>% 
  ggplot(aes(x=area_mi2,y=AvgDepl_mi2)) +
  geom_point(aes(color=SFV))+
  geom_smooth(method = "lm",se=FALSE,color="black")


# Plot 2: Average Deployment over time, by SFV ####
# x: months Apr 2019 - Mar 2020
# y: average deployment; make additional charts with avg depl per capita and per mi2
# grouping: SFV or not
# plot: connected dots of averages for SFV and for not SFV

  
# 1. only keep deployment for the months in the specific years (Pilot Program)
yr_regex = "2019-0[4-9]|2019-1[0-2]|2020-0[1-3]"
depl_ppyr =
  depl_joined %>% 
  filter(grepl(yr_regex,month_yr)) %>% 
  mutate(days_mo = days_in_month(month_yr))
  
# 2. calculate average for SFV and non SFV
depl_ppyr_sum =
  depl_ppyr %>% 
  group_by(SFV) %>% 
  summarise(avg_SFV_depl = weighted.mean(avg_depl, days_mo, na.rm=TRUE))

# 3. plot deployment by SFV and not
depl_plot = 
  depl_ppyr %>% 
  group_by(SFV,month_yr) %>% 
  summarise(avg = mean(avg_depl, na.rm=TRUE)) %>% 
  ggplot(aes(x=as.character(month_yr),y=avg,group=SFV)) + 
  geom_point(aes(shape=SFV)) +
  geom_line(aes(color=SFV)) +
  geom_hline(yintercept = c(depl_ppyr_sum$avg_SFV_depl[1],depl_ppyr_sum$avg_SFV_depl[2]), linetype='dashed', color=c('red','blue')) +
  scale_shape(solid = FALSE) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  ylab("Average Deployment\n") +
  labs(
    title = str_c("SFV and Non-SFV Deployment (","Pilot Program",")"),
    subtitle = str_c("Deployment is reported as average monthly deployment")
  ) + 
  theme_classic()

ggsave(plot = depl_plot,filename = file.path(plots_dir,"Deployment_SFV_Pilot.png"), width = 6,height = 5)


# 4. plot log transformed y-axis
depl_plot_log = 
  depl_plot +
  scale_y_continuous("Average Deployment\n",trans = scales::log_trans(),labels = label_number(accuracy = .01)) +
  labs(
    title = str_c("SFV and Non-SFV Deployment (","Pilot Program",")"),
    subtitle = str_c("Deployment is reported as logged average monthly deployment")
  )
  
ggsave(plot = depl_plot_log,filename = file.path(plots_dir,"Deployment_SFV_Pilot_Logged.png"), width = 6,height = 5)



# Plot 3: Plotting Deploying Function ###########
# average deployment by area, per capita, and per capita over 18
plot_depl <- function(df = depl_ppyr, denom, loop_obj = i) {

  # make dataframe for the average deployment per mi2, capita, capita >=18
  df =
    df %>%
    mutate(
      depl_per = avg_depl/{{denom}},
      depl_p_log = log(depl_per),
      
      # log(0)=-Inf, so return as NA
      depl_p_log = ifelse(is.infinite(depl_p_log),NA,depl_p_log))

  # making plot for depl_per
  # make a summary table for average across the whole period
  depl_per_sum =
    df %>%
    group_by(SFV) %>%
    summarise(avg_SFV_depl = weighted.mean(depl_per, days_mo))

  # make a plot with depl_per
  depl_per_plot =
    df %>%
    group_by(SFV, month_yr) %>%
    summarise(avg = mean(depl_per)) %>%
    ggplot(aes(x=as.character(month_yr),y=avg,group=SFV)) +
    geom_point(aes(shape=SFV)) +
    geom_line(aes(color=SFV)) +
    geom_hline(yintercept = c(depl_per_sum$avg_SFV_depl[1],depl_per_sum$avg_SFV_depl[2]), linetype='dashed', color=c('red','blue')) +
    scale_shape(solid = FALSE) +
    scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
    ylab(str_c("Deployment per ", loop_obj)) +
    labs(
      title = str_c("SFV and Non-SFV Deployment during ","Pilot Program"),
      subtitle = str_c("Average deployment is calculated per ",loop_obj)
    ) +
    theme_classic()

  # making plot with log transformed axes to show more variation in the lower count values
  depl_p_log_plot =
    depl_per_plot +
    scale_y_continuous(str_c("Deployment per log(", loop_obj, ")"), trans = scales::log_trans(), labels = label_number(accuracy = .01)) +
    labs(
      title = str_c("SFV and Non-SFV Deployment (","Pilot Program",")"),
      subtitle = str_c("Deployment is reported as logged average monthly deployment per ",loop_obj))
      
  # return objects as list
  return(list(df,depl_per_sum,depl_per_plot,depl_p_log_plot))

}


## Loop thru plot_depl ####
words <- c("area_mi2","pop1000_est","pop1000_over18_est")
for (i in words) {
  writeLines(str_c("\nMaking tables and plots for deployment per ",i))
  res = plot_depl(denom = get(i))
  
  # assign unique objects 
  assign(str_c("depl_per_",i,"_df"),res[[1]])
  assign(str_c("depl_per_",i,"_sum"),res[[2]])
  assign(str_c("depl_per_",i,"_plot"),res[[3]])
  assign(str_c("depl_per_",i,"_log_plot"),res[[4]])
  
  # save plots
  ggsave(file.path(plots_dir,str_c("depl_per_",i,"_pilot.png")),plot = res[[3]],width = 6,height = 5)
  ggsave(file.path(plots_dir,str_c("depl_per_",i,"_log_pilot.png")),plot = res[[4]],width = 6,height = 5)
  }






# Next Steps ############





# Plot 3: ####




# Plot 4: ####


## Normality check for: ######
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
## plotting with linear model
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



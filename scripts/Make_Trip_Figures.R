##### Make Trip Figures #######################
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
deploy_df <- sf::st_read(file.path(data_files_dir,"vehicle_deployment_wide.geojson"))

# load georeference files
nc_georef_noSOZ <- sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_noSOZ.geojson"))
nc_georef_wSOZ <- sf::st_read(file.path(data_files_dir,"NCZone_GeoRef_wSOZ.geojson"))

depl_joined <- 
  deploy_df %>% 
  as.data.frame() %>% 
  select(-c(new_nc_name,geometry)) %>% 
  inner_join(nc_georef_noSOZ %>% 
               as.data.frame() %>% 
               select(-data_nc_name),
             by=c("nc_id"="NC_ID")) 

# Depl Work ####
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


# Plot 2: Average Deployment over time, by SFV####
# x: months Jan 2019 - Mar 2020
# y: average deployment; make additional charts with avg depl per capita and per mi2
# grouping: SFV or not
# plot: connected dots of averages for SFV and for not SFV; scatter plot with line for SFV and non SFV
str_replace(names(depl_joined),"_(?=\\d)"," ") # obtain the month and year in colnames

# only keep deployment for the months in the Pilot Program Year
depl_ppyr =
  depl_joined %>% 
  select(-matches("202[12]"),-c(April_2020:December_2020)) %>% 
  pivot_longer(cols = c(January_2020:December_2019),
               names_to = "Month",
               values_to = "avg_depl") %>% 
  mutate(Month = floor_date(mdy(Month),unit = "month")) %>% 
  arrange(nc_id,Month)

## plotting with linear model
plot2 =
  depl_ppyr %>% 
  ggplot(aes(x=as.character(Month),y=avg_depl,group=SFV)) +
  geom_point(aes(shape=SFV)) +
  scale_shape(solid = FALSE) +
  geom_smooth(method="lm",aes(linetype=SFV)) +
  scale_x_discrete("Month",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Deployment \n (Natural Log)", trans = scales::log_trans(), labels = label_number(accuracy = .01, big.mark = ",")) +
  scale_colour_hue(labels = c("Non-SFV NC", "SFV NC")) +  
  ggtitle("Deployment by Neighborhood Council during Pilot Program") +
  theme_classic()
  
ggsave(plot = plot2,filename = file.path(plots_dir,"Deployment_PilotProg.png"),
       width = 6, height = 6)


## sum the averages by SFV and non SFV ####
# monthly average for SFV and non SFV
plot2b =
  depl_ppyr %>% 
  group_by(SFV,Month) %>% 
  summarise(avg = mean(avg_depl)) %>% 
  ggplot(aes(x=as.character(Month),y=avg,group=SFV)) + 
  geom_point(aes(shape=SFV)) +
  geom_line(aes(color=SFV)) +
  scale_shape(solid = FALSE) +
  scale_x_discrete("\nMonth",guide = guide_axis(angle = 45)) +
  ylab("Average Deployment\n") +
  ggtitle("SFV and Non-SFV Deployment during Pilot Program\n") +
  theme_classic()

ggsave(plot = plot2b,filename = file.path(plots_dir,"Deployment_SFV_Pilot.png"), width = 6,height = 5)

## normality of DV ####
## cannot find average across subdivided space. we can find averages per month, but we cant find the average within a month by a geo
depl_ppyr %>% 
  mutate(
    depl_per = avg_depl/{{denom}},
    depl_per_log = log(depl_per)) %>% 
  
depl_ppyr %>% 
  ggplot(aes(x=as.character(Month),y=avg_depl,group=SFV)) +
  geom_point(aes(shape=SFV)) +
  scale_shape(solid = FALSE) +
  geom_smooth(method="lm",aes(linetype=SFV)) +
  scale_x_discrete("Month",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Deployment \n (Natural Log)", trans = scales::log_trans(), labels = label_number(accuracy = .01, big.mark = ",")) +
  scale_colour_hue(labels = c("Non-SFV NC", "SFV NC")) +  
  ggtitle("Deployment by Neighborhood Council during Pilot Program") +
  theme_classic()

  
## distribution of DV
# no transf, very skewed
depl_ppyr %>% 
  ggplot(aes(x=avg_depl)) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(depl_ppyr$avg_depl),
                            sd = sd(depl_ppyr$avg_depl)))

# natural log transf, better, but there are lots of obs with HIGH depl
ggplot(data=depl_ppyr, aes(x=log(avg_depl))) +
  geom_histogram(aes(y = after_stat(density))) +
  stat_function(fun = dnorm, 
                args = list(
                  mean = mean(log(depl_ppyr$avg_depl[which(depl_ppyr$avg_depl!=0)])),
                  sd = sd(log(depl_ppyr$avg_depl[which(depl_ppyr$avg_depl!=0)]))))

chk_norm <- function(df = depl_ppyr, denom, plot_phrase = plot_title) {
  
  df = 
    df %>% 
    filter(avg_depl != 0) %>% 
    mutate(
      depl_per = avg_depl/{{denom}},
      depl_per_log = log(depl_per)) %>% 
    glimpse()
  
  # show the skewed distribution, not logged
  print(
    ggplot(data = df,
           aes(x=depl_per)) +
      geom_histogram(aes(y = after_stat(density))) +
      stat_function(fun = dnorm,
                    args = list(
                      mean = mean(df$depl_per),
                      sd = sd(df$depl_per)))
    + ggtitle(str_c("Histogram of Avg Depl Divided by ",paste(plot_phrase)))
  )
  
  print(
    ggplot(data = df, 
           aes(x=depl_per_log)) +
      geom_histogram(aes(y = after_stat(density))) +
      stat_function(fun = dnorm,
                    args = list(
                      mean = mean(df$depl_per_log),
                      sd = sd(df$depl_per_log)))
    + ggtitle(str_c("Histogram of Log Avg Depl Divided by ",paste(plot_phrase)))
    )
}

# loop through colnames
words <- c("area_mi2","pop_est","pop_over18_est")
for (i in words) {
  writeLines(str_c("\nChecking normality of ",i, " variable."))
  plot_title = str_c(i)
  chk_norm(denom = get(i))
}
# once the per_ something values are logged, the distribution is more normal, but not really
# we can run some tests to check for normality



# Plot 3: ####
depl_allyr =
  depl_joined %>% 
  # select(-matches("202[12]"),-c(April_2020:December_2020)) %>% 
  pivot_longer(cols = c(January_2022:December_2019),
               names_to = "Month",
               values_to = "avg_depl") %>% 
  mutate(Month = floor_date(mdy(Month),unit = "month")) %>% 
  arrange(nc_id,Month)

## plotting with linear model
plot3 =
  depl_allyr %>% 
  ggplot(aes(x=as.character(Month),y=avg_depl,group=Geo_Type)) +
  # geom_point(aes(shape=Geo_Type)) +
  # scale_shape(solid = FALSE) +
  geom_smooth(method="lm",aes(linetype=Geo_Type)) +
  scale_x_discrete("Month",guide = guide_axis(angle = 45)) +
  scale_y_continuous("Average Deployment \n (Natural Log)", trans = scales::log_trans(), labels = label_number(accuracy = .01, big.mark = ",")) +
  scale_colour_hue(labels = c("Non-SFV NC", "SFV NC")) +  
  ggtitle("Deployment by Neighborhood Council during Pilot Program") +
  theme_classic()



# Plot 4: ####




# Plot 5: ####





########## Plot trip cost per operator count by city ###
# Purpose: for the policy evaluation section in the Admin Enhancement section 

# Setup
pacman::p_load('tidyverse','extrafont','scales')

# directory path objects
data_raw_dir <- file.path('.','data')
plots_dir <- file.path(".","output","plots")

# find the relevant sheets with data 
list.files(data_raw_dir)
readxl::excel_sheets(file.path(data_raw_dir,"Operator Size, Trip, and Price.xlsx")) # operators and LA_price

# operator data from Ride Report
ops <- readxl::read_excel(file.path(data_raw_dir,"Operator Size, Trip, and Price.xlsx"), sheet = 5)

# Self collected scooter trip times in LA
la_times <- readxl::read_excel(file.path(data_raw_dir,"Operator Size, Trip, and Price.xlsx"), sheet = 6, range = "A1:B31")

# self collected prices in LA for 4 companies
la_prices <- readxl::read_excel(file.path(data_raw_dir,"Operator Size, Trip, and Price.xlsx"), sheet = 6, range = "D1:E5")


# Calculate trip cost per operator
# convert distance per trip to $ per trip, by using miles per hour and average price per minute. then add $1 unlocking fee
operators =
  ops %>% 
  mutate(p_avg = rowMeans(select(.,contains("p_")), na.rm = TRUE),
         p_avg_trip = dist_trip/mph*60*p_avg+1) %>% glimpse()

# calc trip cost for LA
# average price
la_p_avg = mean(la_prices$price)
la_avg_time = mean(unlist(la_times))

operators =
  operators %>% 
  bind_rows(
    data.frame(
      "city" = "Los Angeles",
      "operator_ct" = 4,
      "p_avg" = la_p_avg,
      "p_avg_trip" = la_avg_time*la_p_avg+1
    ))

# plot data
plot_operators =
  ggplot(operators, aes(x=operator_ct,y=p_avg_trip)) + 
  geom_point(size = 3,
             color = "dodgerblue3") +
  xlab("\nNumber of Operators\n") +
  scale_y_continuous("Average Trip Cost\n", labels = label_number(accuracy = .01, prefix = "$")) +
  labs(
    title = "Figure X: Trip Cost by Number of Operators",
    subtitle = "Cities include San Francisco, Santa Monica, and Washington, D.C.\n",
    caption = "Source: Scooter prices collected individually from mobile apps.\nTrip information from Global Mobility Dashbard."
  ) +
  theme(text = element_text(family = "Century Gothic", size = 12)) +
  theme_bw()

# add a different color for the LA point
plot_operators =
  plot_operators +
  geom_point(data = operators %>% 
               filter(city %in% "Los Angeles"),
             aes(x=operator_ct, y=p_avg_trip, color=city),
             size = 3,
             show.legend = FALSE) +
  geom_label(data = operators %>% 
               filter(city %in% "Los Angeles"),
             aes(label = city),
             hjust = -.1) +
  theme(legend.position = "bottom")
  
ggsave(plot = plot_operators, filename = file.path(plots_dir,"Operators_TripCost.png"), height = 5, width = 6)




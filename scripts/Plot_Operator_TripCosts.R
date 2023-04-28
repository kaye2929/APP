########## Plot trip cost per operator count by city ###
# Purpose: for the policy evaluation section in the Admin Enhancement section 
# Author: Abraham Cheung

# Setup ####
pacman::p_load('tidyverse','extrafont','scales', install = FALSE, update = FALSE)

# directory path objects
data_raw_dir <- file.path('.','data')
plots_dir <- file.path(".","output","plots")
plots_svg_dir <- file.path(".","output","plots_svg")

# Load Data ####
# find the relevant sheets with data 
list.files(data_raw_dir)
readxl::excel_sheets(file.path(data_raw_dir,"Operator Size, Trip, and Price.xlsx")) # operators and LA_price

# operator data from Ride Report
ops <- readxl::read_excel(file.path(data_raw_dir,"Operator Size, Trip, and Price.xlsx"), sheet = 5)

# Self collected scooter trip times in LA
la_times <- readxl::read_excel(file.path(data_raw_dir,"Operator Size, Trip, and Price.xlsx"), sheet = 6, range = "A1:B31")

# self collected prices in LA for 4 companies
la_prices <- readxl::read_excel(file.path(data_raw_dir,"Operator Size, Trip, and Price.xlsx"), sheet = 6, range = "D1:E5")

# Calculations ####
# Calculate trip cost per operator
# convert distance per trip to $ per trip, by using miles per hour and average price per minute. then add $1 unlocking fee
# Conversion mathematically: Price per Trip = (__ Miles / 1 Trip) X (__ Miles / 1 Hour) X (60 Minutes / 1 Hour) X ($__ Price / 1 Minute)
# Miles, Hours, Minutes cancel out

operators =
  ops %>% 
  mutate(p_avg = rowMeans(select(.,contains("p_")), na.rm = TRUE),
         p_avg_trip = dist_trip/mph*60*p_avg + 1) %>% glimpse()

# Calculate trip cost for LA, collected by hand
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

# Plot Data ####
plot_operators =
  ggplot(operators, aes(x=operator_ct,y=p_avg_trip)) + 
  geom_point(size = 3,
             color = "dodgerblue3") +
  xlab("\nNumber of Operators\n") +
  scale_y_continuous("Average Trip Cost\n", labels = label_number(accuracy = .01, prefix = "$")) +
  labs(
    title = "Figure 24: Trip Cost by Number of Operators",
    subtitle = "Cities include Austin, Santa Monica, and Portland\n",
    caption = "Source: Scooter prices collected individually from mobile apps.\nTrip information from Global Mobility Dashbard."
  ) +
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
             family = "Century Gothic",
             hjust = -.1) +
  theme(text = element_text(family = "Century Gothic", size = 12),
        legend.position = "bottom")
  
ggsave(plot = plot_operators, filename = file.path(plots_dir,"Operators_TripCost.png"), height = 5, width = 6)

ggsave(plot = plot_operators, filename = file.path(plots_svg_dir,"Figure24_Operators_TripCost.svg"), height = 5, width = 6)

# Elasticity Graph ####
# Make graph showing two kinds of elasticity estimates
# make synthetic data
df_el =
  data.frame(
    p_incr = c(0,2,4,6,8,10),
    high_el = c(0,-6,-12,-18,-24,-30),
    norm_el = c(0,-2,-4,-6,-8,-10)
  ) %>% 
  pivot_longer(cols = c(high_el,norm_el),
               names_to = "Elasticity",
               values_to = "d_decr")

# plot
plot_ElEst =
  ggplot(data = df_el, aes(x=p_incr, y=d_decr, color=Elasticity)) +
  geom_line(linewidth=1.5) +
  geom_ribbon(aes(ymin = d_decr, ymax = max(d_decr), fill = Elasticity),alpha = 0.5, linetype=0) +
  scale_color_manual(
    values = c("norm_el"="dodgerblue3","high_el"="tomato2"),
    breaks = c("norm_el","high_el"),
    labels = c("Model 1: Normal Elasticity Range",
               "Model 2: High Elasticity Range")
  ) +
  scale_fill_manual(
    values = c("norm_el"="dodgerblue3","high_el"="tomato2"),
    guide = "none"
  ) +
  scale_x_continuous("\nPrice Increase", labels = label_number(accuracy = 1, suffix = "%")) +
  scale_y_continuous("Demand Decrease\n", labels = label_number(suffix = "%")) +
  labs(
    title = "Figure 22: Scooter Price Elasticity Estimates\n",
  ) +
  theme_classic() +
  theme(text = element_text(family = "Century Gothic")) +
  guides(color = guide_legend(override.aes = list(fill = NA)))

# save
ggsave(plot = plot_ElEst, filename = file.path(plots_dir,"Elasticity_Ranges.png"), width = 6, height = 4)

ggsave(plot = plot_ElEst, filename = file.path(plots_svg_dir,"Figure22_Elasticity_Ranges.svg"), width = 6, height = 4)

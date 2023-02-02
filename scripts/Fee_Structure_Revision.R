#Masa currently working. Please do not alter the content below.

###Policy Option (Fee Structure Revision)###

##################################################
### Preliminary Steps
##################################################
library(tidyverse)
library(readxl)
getwd()
setwd("C:/APP/APP")  #Absolute file path. Switch to relative file path.
trips_per_zone_fy21to22_init <- read_excel(path = "./data/LADOT/CPRA #22-10589 Data/New Trip Geography Origin-Dest.xlsx",
                                           sheet = "FY 21-22",
                                           skip = 2)

trips_per_zone_fy21to22_init2 <- trips_per_zone_fy21to22_init %>%
  select("Trip Origin and Destination", "Total")

trips_per_zone_fy21to22 <- trips_per_zone_fy21to22_init2[-c(26),] # Deleted the Totals row

rm(trips_per_zone_fy21to22_init, trips_per_zone_fy21to22_init2) #Remove unnecessary objects

##################################################
### Calculate LADOT's Revenue (Status quo version)
##################################################
trips_per_zone_fy21to22_status_quo <- trips_per_zone_fy21to22 %>% mutate(trip_fee = case_when(
  `Trip Origin and Destination` == "EFMDD - EFMDD" ~ 0,
  `Trip Origin and Destination` == "EFMDD - MDD" ~ 0,
  `Trip Origin and Destination` == "EFMDD - Out_of_Bound" ~ 0,
  `Trip Origin and Destination` == "EFMDD - SOZ" ~ 0,
  `Trip Origin and Destination` == "EFMDD -SPD" ~ 0,
  `Trip Origin and Destination` == "MDD - EFMDD" ~ 0,
  `Trip Origin and Destination` == "MDD - MDD" ~ 0.06,
  `Trip Origin and Destination` == "MDD - Out_of_Bound" ~ 0.06,
  `Trip Origin and Destination` == "MDD - SOZ" ~ 0.06,
  `Trip Origin and Destination` == "MDD - SPD" ~ 0.06,
  `Trip Origin and Destination` == "Out_of_Bound - EFMDD" ~ 0,
  `Trip Origin and Destination` == "Out_of_Bound - MDD" ~ 0.06,
  `Trip Origin and Destination` == "Out_of_Bound - Out_of_Bound" ~ 0,
  `Trip Origin and Destination` == "Out_of_Bound - SOZ" ~ 0.40,
  `Trip Origin and Destination` == "Out_of_Bound - SPD" ~ 0.20,
  `Trip Origin and Destination` == "SOZ - EFMDD" ~ 0,
  `Trip Origin and Destination` == "SOZ - MDD" ~ 0.06,
  `Trip Origin and Destination` == "SOZ - Out_of_Bound" ~ 0.40,
  `Trip Origin and Destination` == "SOZ - SOZ" ~ 0.40,
  `Trip Origin and Destination` == "SOZ - SPD" ~ 0.20,
  `Trip Origin and Destination` == "SPD - EFMDD" ~ 0,
  `Trip Origin and Destination` == "SPD - MDD" ~ 0.06,
  `Trip Origin and Destination` == "SPD - Out_of_Bound" ~ 0.20,
  `Trip Origin and Destination` == "SPD - SOZ" ~ 0.20,
  `Trip Origin and Destination` == "SPD - SPD" ~ 0.20))  #Setting current fee

trips_per_zone_fy21to22_status_quo <- trips_per_zone_fy21to22_status_quo %>%
  mutate(LADOT_revenue = trip_fee * Total) #Calculating LADOT's revenue for each group under current fee

LADOT_revenue_fy21to22_status_quo <- sum(trips_per_zone_fy21to22_status_quo$LADOT_revenue) #This is LADOT's current revenue.

###########################################################
###Calculate LADOT's Increase in Revenue (SOZ fees doubled)
###########################################################
trips_per_zone_fy21to22_SOZ_double <- trips_per_zone_fy21to22 %>% mutate(trip_fee_soz_doubled = case_when(
  `Trip Origin and Destination` == "EFMDD - EFMDD" ~ 0,
  `Trip Origin and Destination` == "EFMDD - MDD" ~ 0,
  `Trip Origin and Destination` == "EFMDD - Out_of_Bound" ~ 0,
  `Trip Origin and Destination` == "EFMDD - SOZ" ~ 0,
  `Trip Origin and Destination` == "EFMDD -SPD" ~ 0,
  `Trip Origin and Destination` == "MDD - EFMDD" ~ 0,
  `Trip Origin and Destination` == "MDD - MDD" ~ 0.06,
  `Trip Origin and Destination` == "MDD - Out_of_Bound" ~ 0.06,
  `Trip Origin and Destination` == "MDD - SOZ" ~ 0.06,
  `Trip Origin and Destination` == "MDD - SPD" ~ 0.06,
  `Trip Origin and Destination` == "Out_of_Bound - EFMDD" ~ 0,
  `Trip Origin and Destination` == "Out_of_Bound - MDD" ~ 0.06,
  `Trip Origin and Destination` == "Out_of_Bound - Out_of_Bound" ~ 0,
  `Trip Origin and Destination` == "Out_of_Bound - SOZ" ~ 0.80,
  `Trip Origin and Destination` == "Out_of_Bound - SPD" ~ 0.20,
  `Trip Origin and Destination` == "SOZ - EFMDD" ~ 0,
  `Trip Origin and Destination` == "SOZ - MDD" ~ 0.06,
  `Trip Origin and Destination` == "SOZ - Out_of_Bound" ~ 0.80,
  `Trip Origin and Destination` == "SOZ - SOZ" ~ 0.80,
  `Trip Origin and Destination` == "SOZ - SPD" ~ 0.20,
  `Trip Origin and Destination` == "SPD - EFMDD" ~ 0,
  `Trip Origin and Destination` == "SPD - MDD" ~ 0.06,
  `Trip Origin and Destination` == "SPD - Out_of_Bound" ~ 0.20,
  `Trip Origin and Destination` == "SPD - SOZ" ~ 0.20,
  `Trip Origin and Destination` == "SPD - SPD" ~ 0.20)) #Doubling SOZ fee

# ASSUMPTION 1 (SOZ fees doubled. Could be other fees as well.Also possible to increase fees on SPZ but personally against it)

ped <- -0.45
#ASSUMPTION 2 (assuming that the price elasticity of demand is -0.45,meaning that 1% increase in the price is associated with -0.45% increase in ridership)

fare_status_quo <- 4
increase_in_soz_fee <- 0.4
new_fare_soz <- fare_status_quo+increase_in_soz_fee
#ASSUMPTION 3 (assuming that fare, including fees imposed by LADOT, for each trip is 4. As a result in $0.40 rise in SOZ fees, the fare for SOZ is $4.40)
#What value to set status quo fare is TBD

change_in_soz_ridership <- (100+ped*(100*(new_fare_soz-fare_status_quo)/fare_status_quo))/100
print(change_in_soz_ridership)
#This means that the ridership will be 0.955 times in response to the price increase in SOZ

trips_per_zone_fy21to22_SOZ_double <- trips_per_zone_fy21to22_SOZ_double %>%
  mutate(new_ridership = case_when(
    `Trip Origin and Destination` == "EFMDD - EFMDD" ~ Total,
    `Trip Origin and Destination` == "EFMDD - MDD" ~ Total,
    `Trip Origin and Destination` == "EFMDD - Out_of_Bound" ~ Total,
    `Trip Origin and Destination` == "EFMDD - SOZ" ~ Total,
    `Trip Origin and Destination` == "EFMDD -SPD" ~ Total,
    `Trip Origin and Destination` == "MDD - EFMDD" ~ Total,
    `Trip Origin and Destination` == "MDD - MDD" ~ Total,
    `Trip Origin and Destination` == "MDD - Out_of_Bound" ~ Total,
    `Trip Origin and Destination` == "MDD - SOZ" ~ Total,
    `Trip Origin and Destination` == "MDD - SPD" ~ Total,
    `Trip Origin and Destination` == "Out_of_Bound - EFMDD" ~ Total,
    `Trip Origin and Destination` == "Out_of_Bound - MDD" ~ Total,
    `Trip Origin and Destination` == "Out_of_Bound - Out_of_Bound" ~ Total,
    `Trip Origin and Destination` == "Out_of_Bound - SOZ" ~ Total*change_in_soz_ridership,
    `Trip Origin and Destination` == "Out_of_Bound - SPD" ~ Total,
    `Trip Origin and Destination` == "SOZ - EFMDD" ~ Total,
    `Trip Origin and Destination` == "SOZ - MDD" ~ Total,
    `Trip Origin and Destination` == "SOZ - Out_of_Bound" ~ Total*change_in_soz_ridership,
    `Trip Origin and Destination` == "SOZ - SOZ" ~ Total*change_in_soz_ridership,
    `Trip Origin and Destination` == "SOZ - SPD" ~ Total,
    `Trip Origin and Destination` == "SPD - EFMDD" ~ Total,
    `Trip Origin and Destination` == "SPD - MDD" ~ Total,
    `Trip Origin and Destination` == "SPD - Out_of_Bound" ~ Total,
    `Trip Origin and Destination` == "SPD - SOZ" ~ Total,
    `Trip Origin and Destination` == "SPD - SPD" ~ Total)) #Derived each groups new ridership. Groups with SOZ decreased in ridership by 0.955 times.

trips_per_zone_fy21to22_SOZ_double <- trips_per_zone_fy21to22_SOZ_double %>%
  mutate(LADOT_new_revenue = new_ridership * trip_fee_soz_doubled) #Derived each group's revenue for LADOT.

LADOT_revenue_fy21to22_SOZ_doubled <- sum(trips_per_zone_fy21to22_SOZ_double$LADOT_new_revenue) #LADOT's revenue after SOZ fees doubled

rev_increase_soz_doubled <- LADOT_revenue_fy21to22_SOZ_doubled - LADOT_revenue_fy21to22_status_quo #Increase in LADOT's revenue after SOZ fees doubled

#####################################################################
###Calculating LADOT's Cost to Subsidize EFMDD Trips
#####################################################################
trips_per_zone_fy21to22_EFMDD_subsidized <- trips_per_zone_fy21to22[c(1,2,3,4,5,6,11,16,21),] #Filtering only EFMDD trips

subsidy_amount <- -0.1
trips_per_zone_fy21to22_EFMDD_subsidized <- trips_per_zone_fy21to22_EFMDD_subsidized %>%
  mutate(fee_reduction = subsidy_amount)
new_fare_EFMDD <- fare_status_quo+subsidy_amount
#ASSUMPTION 5 (assume that each EFMDD trip is subsidized by 10 cents. As a result, fare for EFMDD is $3.90. Could be other amount as well. TBD)

change_in_EFMDD_ridership <- (100+ped*(100*(new_fare_EFMDD-fare_status_quo)/fare_status_quo))/100
print(change_in_EFMDD_ridership)
#This means that the ridership will be 1.01125 times in response to fare being subsidized.

trips_per_zone_fy21to22_EFMDD_subsidized <- trips_per_zone_fy21to22_EFMDD_subsidized %>%
  mutate(new_ridership = Total * change_in_EFMDD_ridership) #Calculating increased ridership due to subsidy

trips_per_zone_fy21to22_EFMDD_subsidized <- trips_per_zone_fy21to22_EFMDD_subsidized %>%
  mutate(cost_for_subsidy = new_ridership * fee_reduction)

total_cost_for_subsidy <- sum(trips_per_zone_fy21to22_EFMDD_subsidized$cost_for_subsidy)

#####################################################################
###Compare cost for subsidy and increase in revenue
#####################################################################
print(total_cost_for_subsidy)
print(rev_increase_soz_doubled)
print(rev_increase_soz_doubled+total_cost_for_subsidy)

#####################################################################
###Compare Operator's Revenues
#####################################################################
trips_per_zone_fy21to22_operator_revenue_post_fee_rev <- trips_per_zone_fy21to22_SOZ_double %>%
  select("Trip Origin and Destination", trip_fee_soz_doubled) %>%
  mutate(fare = recode(trip_fee_soz_doubled,
                       "0.00" = fare_status_quo+subsidy_amount,
                       "0.06" = fare_status_quo,
                       "0.20" = fare_status_quo,
                       "0.80" = fare_status_quo+increase_in_soz_fee)) %>% #Derived average fares for each group.
  select(-trip_fee_soz_doubled) #Delete unnecessary column.

trips_per_zone_fy21to22_operator_revenue_post_fee_rev <- trips_per_zone_fy21to22_operator_revenue_post_fee_rev %>%
  mutate(new_ridership = trips_per_zone_fy21to22_SOZ_double$new_ridership)

trips_per_zone_fy21to22_operator_revenue_post_fee_rev <- trips_per_zone_fy21to22_operator_revenue_post_fee_rev %>%
  mutate(new_ridership2 = ifelse(fare == fare_status_quo+subsidy_amount, NA, new_ridership))


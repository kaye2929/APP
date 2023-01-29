###Policy Option (Fee Structure Revision)###

#####################################################
### Preliminary Steps
#####################################################
library(tidyverse)
library(readxl)
getwd()
setwd("C:/APP/APP")  #Absolute file path. Switch to relative file path.
trips_per_zone_fy21to22_init <- read_excel(path = "./data/LADOT/CPRA #22-10589 Data/New Trip Geography Origin-Dest.xlsx",
                                           sheet = "FY 21-22",
                                           skip = 2)

trips_per_zone_fy21to22_init2 <- trips_per_zone_fy21to22_init %>%
  select("Trip Origin and Destination", "Total")

trips_per_zone_fy21to22 <- trips_per_zone_fy21to22_init2[-c(13,26),] # Deleted the Out-of-bound~Out^of-bound and Totals rows

rm(trips_per_zone_fy21to22_init, trips_per_zone_fy21to22_init2) #Remove unnecessary objects

trips_per_zone_fy21to22 <- rename(trips_per_zone_fy21to22, Ridership = Total) #Renamed "Total" to "Ridership"

#####################################################
### Making Assumptions
#####################################################
current_fare <- 4.00
change_in_soz_fee <- 0.50
change_in_spd_fee <- 0.00
change_in_mdd_fee <- 0.00
change_in_efmdd_fee <- -0.10
ped <- -0.45 #Price Elasticity of Demand

#####################################################
### Current LADOT Fees on Operators
#####################################################
current_soz_fee <- 0.40
current_spd_fee <- 0.20
current_mdd_fee <- 0.06
current_efmdd_fee <- 0.00

#####################################################
### Calculating Changes in Ridership
#####################################################
change_in_soz_ridership <- (((change_in_soz_fee/current_fare)*100)*ped+100)/100
change_in_spd_ridership <- (((change_in_spd_fee/current_fare)*100)*ped+100)/100
change_in_mdd_ridership <- (((change_in_mdd_fee/current_fare)*100)*ped+100)/100
change_in_efmdd_ridership <- (((change_in_efmdd_fee/current_fare)*100)*ped+100)/100


#####################################################
### Deriving Current Revenues for LADOT and Operators
#####################################################
trips_per_zone_fy21to22 <- trips_per_zone_fy21to22 %>%
  mutate(Current_Fare = current_fare) %>%
  mutate(Current_Fee = case_when(
    `Trip Origin and Destination` == "EFMDD - EFMDD" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "EFMDD - MDD" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "EFMDD - Out_of_Bound" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "EFMDD - SOZ" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "EFMDD -SPD" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "MDD - EFMDD" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "MDD - MDD" ~ current_mdd_fee,
    `Trip Origin and Destination` == "MDD - Out_of_Bound" ~ current_mdd_fee,
    `Trip Origin and Destination` == "MDD - SOZ" ~ current_mdd_fee,
    `Trip Origin and Destination` == "MDD - SPD" ~ current_mdd_fee,
    `Trip Origin and Destination` == "Out_of_Bound - EFMDD" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "Out_of_Bound - MDD" ~ current_mdd_fee,
    `Trip Origin and Destination` == "Out_of_Bound - SOZ" ~ current_soz_fee,
    `Trip Origin and Destination` == "Out_of_Bound - SPD" ~ current_spd_fee,
    `Trip Origin and Destination` == "SOZ - EFMDD" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "SOZ - MDD" ~ current_mdd_fee,
    `Trip Origin and Destination` == "SOZ - Out_of_Bound" ~ current_soz_fee,
    `Trip Origin and Destination` == "SOZ - SOZ" ~ current_soz_fee,
    `Trip Origin and Destination` == "SOZ - SPD" ~ current_spd_fee,
    `Trip Origin and Destination` == "SPD - EFMDD" ~ current_efmdd_fee,
    `Trip Origin and Destination` == "SPD - MDD" ~ current_mdd_fee,
    `Trip Origin and Destination` == "SPD - Out_of_Bound" ~ current_spd_fee,
    `Trip Origin and Destination` == "SPD - SOZ" ~ current_spd_fee,
    `Trip Origin and Destination` == "SPD - SPD" ~ current_spd_fee)) %>%
  mutate(Current_LADOT_Revenue = Ridership*Current_Fee) %>%
  mutate(Current_Operator_Revenue = Ridership*Current_Fare)

#####################################################
### Making Revised Fee and Fare Column
#####################################################
revised_fee_temp <- c(current_efmdd_fee+change_in_efmdd_fee,
                      current_efmdd_fee+change_in_efmdd_fee,
                      current_efmdd_fee+change_in_efmdd_fee,
                      current_efmdd_fee+change_in_efmdd_fee,
                      current_efmdd_fee+change_in_efmdd_fee,
                      current_efmdd_fee+change_in_efmdd_fee,
                      current_mdd_fee+change_in_mdd_fee,
                      current_mdd_fee+change_in_mdd_fee,
                      current_mdd_fee+change_in_mdd_fee,
                      current_mdd_fee+change_in_mdd_fee,
                      current_efmdd_fee+change_in_efmdd_fee,
                      current_mdd_fee+change_in_mdd_fee,
                      current_soz_fee+change_in_soz_fee,
                      current_spd_fee+change_in_spd_fee,
                      current_efmdd_fee+change_in_efmdd_fee,
                      current_mdd_fee+change_in_mdd_fee,
                      current_soz_fee+change_in_soz_fee,
                      current_soz_fee+change_in_soz_fee,
                      current_spd_fee+change_in_spd_fee,
                      current_efmdd_fee+change_in_efmdd_fee,
                      current_mdd_fee+change_in_mdd_fee,
                      current_spd_fee+change_in_spd_fee,
                      current_spd_fee+change_in_spd_fee,
                      current_spd_fee+change_in_spd_fee)
trips_per_zone_fy21to22 <- trips_per_zone_fy21to22 %>%
  mutate(Revised_Fee = revised_fee_temp)
revised_fare_temp <- c(change_in_efmdd_fee,
                       change_in_efmdd_fee,
                       change_in_efmdd_fee,
                       change_in_efmdd_fee,
                       change_in_efmdd_fee,
                       change_in_efmdd_fee,
                       change_in_mdd_fee,
                       change_in_mdd_fee,
                       change_in_mdd_fee,
                       change_in_mdd_fee,
                       change_in_efmdd_fee,
                       change_in_mdd_fee,
                       change_in_soz_fee,
                       change_in_spd_fee,
                       change_in_efmdd_fee,
                       change_in_mdd_fee,
                       change_in_soz_fee,
                       change_in_soz_fee,
                       change_in_spd_fee,
                       change_in_efmdd_fee,
                       change_in_mdd_fee,
                       change_in_spd_fee,
                       change_in_spd_fee,
                       change_in_spd_fee)
trips_per_zone_fy21to22 <- trips_per_zone_fy21to22 %>%
  mutate(Revised_Fare = Current_Fare+revised_fare_temp)

#####################################################
### Making the New Ridership Column
#####################################################
change_in_ridership_temp <- c(change_in_efmdd_ridership,
                              change_in_efmdd_ridership,
                              change_in_efmdd_ridership,
                              change_in_efmdd_ridership,
                              change_in_efmdd_ridership,
                              change_in_efmdd_ridership,
                              change_in_mdd_ridership,
                              change_in_mdd_ridership,
                              change_in_mdd_ridership,
                              change_in_mdd_ridership,
                              change_in_efmdd_ridership,
                              change_in_mdd_ridership,
                              change_in_soz_ridership,
                              change_in_spd_ridership,
                              change_in_efmdd_ridership,
                              change_in_mdd_ridership,
                              change_in_soz_ridership,
                              change_in_soz_ridership,
                              change_in_spd_ridership,
                              change_in_efmdd_ridership,
                              change_in_mdd_ridership,
                              change_in_spd_ridership,
                              change_in_spd_ridership,
                              change_in_spd_ridership)
trips_per_zone_fy21to22 <- trips_per_zone_fy21to22 %>%
  mutate(New_Ridership = Ridership*change_in_ridership_temp)

#####################################################
### Deriving New Revenues for LADOT and Operators
#####################################################
trips_per_zone_fy21to22 <- trips_per_zone_fy21to22 %>%
  mutate(New_LADOT_Revenue = New_Ridership*Revised_Fee) %>%
  mutate(New_Operator_Revenue = New_Ridership*Revised_Fare)

#####################################################
### Showing the Results
#####################################################
change_in_LADOT_Revenue <- sum(trips_per_zone_fy21to22$New_LADOT_Revenue)-sum(trips_per_zone_fy21to22$Current_LADOT_Revenue)
change_in_Operator_Revenue <- sum(trips_per_zone_fy21to22$New_Operator_Revenue)-sum(trips_per_zone_fy21to22$Current_Operator_Revenue)
change_in_Ridership <- sum(trips_per_zone_fy21to22$New_Ridership)-sum(trips_per_zone_fy21to22$Ridership)

#####################################################
### Exporting Excel
#####################################################
write_csv(x = trips_per_zone_fy21to22,
          path = "./output_figures/Fee_Revision_Output.csv")


#####################################################
### Printing the Results
#####################################################

print(change_in_LADOT_Revenue)
print(change_in_Operator_Revenue)
print(change_in_Ridership)

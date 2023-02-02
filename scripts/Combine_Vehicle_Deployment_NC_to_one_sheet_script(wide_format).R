#Combine Vehicle Deployment Neighborhood Council Destricts Data to one sheet (Wide_format)

#getwd()
#setwd("../data/CPRA #22-10589 Data")
library(tidyverse)
library(readxl)
list.files()

df2019 <- read_excel(path = "./Vehicle Deployment. Neighborhood Council Districts.xlsx",
                 sheet = "Deployment 2019",
                 skip = 1)
df2020 <- read_excel(path = "./Vehicle Deployment. Neighborhood Council Districts.xlsx",
                     sheet = "Deployment 2020",
                     skip = 1)
df2021 <- read_excel(path = "./Vehicle Deployment. Neighborhood Council Districts.xlsx",
                    sheet = "Deployment 2021",
                    skip = 1)
df2022 <- read_excel(path = "./Vehicle Deployment. Neighborhood Council Districts.xlsx",
                     sheet = "Deployment 2022",
                     skip = 1)
df2019 <- df2019 %>%
  mutate(Total_2019 = January+February+March+April+May+June+July+August+September+October+November+December)
df2020 <- df2020 %>%
  mutate(Total_2020 = January+February+March+April+May+June+July+August+September+October+November+December)
df2021 <- df2021 %>%
  mutate(Total_2021 = January+February+March+April+May+June+July+August+September+October+November+December)
df2022 <- df2022 %>%
  mutate(Total_2022 = January+February+March+April+May+June+July+August+September+October+November+December)


df2019 <- rename(df2019,
                 January_2019 = January,
                 February_2019 = February,
                 March_2019 = March,
                 April_2019 = April,
                 May_2019 = May,
                 June_2019 = June,
                 July_2019 = July,
                 August_2019 = August,
                 September_2019 = September,
                 October_2019 = October,
                 November_2019 = November,
                 December_2019 = December)

df2020 <- rename(df2020,
                 January_2020 = January,
                 February_2020 = February,
                 March_2020 = March,
                 April_2020 = April,
                 May_2020 = May,
                 June_2020 = June,
                 July_2020 = July,
                 August_2020 = August,
                 September_2020 = September,
                 October_2020 = October,
                 November_2020 = November,
                 December_2020 = December)

df2021 <- rename(df2021,
                 January_2021 = January,
                 February_2021 = February,
                 March_2021 = March,
                 April_2021 = April,
                 May_2021 = May,
                 June_2021 = June,
                 July_2021 = July,
                 August_2021 = August,
                 September_2021 = September,
                 October_2021 = October,
                 November_2021 = November,
                 December_2021 = December)

df2022 <- rename(df2022,
                 January_2022 = January,
                 February_2022 = February,
                 March_2022 = March,
                 April_2022 = April,
                 May_2022 = May,
                 June_2022 = June,
                 July_2022 = July,
                 August_2022 = August,
                 September_2022 = September,
                 October_2022 = October,
                 November_2022 = November,
                 December_2022 = December)

df <- df2019 %>%
  left_join(df2020, by = "Neighborhood Council District") %>%
  left_join(df2021, by = "Neighborhood Council District") %>%
  left_join(df2022, by = "Neighborhood Council District")

write_csv(df, file = "../../output/files/vehicle_deployment_wide.csv")

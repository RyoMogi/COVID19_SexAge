#===============================================================================
# 2020/04/12
# COVID-19 data collection project
# Ryohei Mogi, rmogi@ced.uab.es
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#===============================================================================
# upd: 2020/04/14
# upd: 2020/04/15
# upd: 2020/04/17
# upd: 2020/05/05

library(tidyverse)
library(lubridate)
`%out%` = Negate(`%in%`)

jpn_cases <- read.csv("Data/JPN_COVID19_Cases.csv")

#### ---- By region ----
region <- c("Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima", "Ibaraki", "Tochigi", "Gunma", 
            "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata", "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", 
            "Gifu", "Shizuoka", "Aichi", "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori", 
            "Shimane", "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa", "Ehime", "Kochi", "Fukuoka", "Saga", 
            "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima", "Okinawa")

region_id <- 1:47 
region_id <- if_else(region_id < 10, paste("0", region_id, sep = ""), as.character(region_id))

Region <- cbind(region, region_id)
Region <- Region %>% 
  as.data.frame()

jpn_cases_long <- jpn_cases %>% 
  as.data.frame() %>% 
  filter(Residential.Pref %in% region) %>% 
  select(Date = `確定日`, Age = `年代`, Sex = `Gender`, Region = Residential.Pref, Value = `人数`) %>% 
  mutate(Date = mdy(Date),
         Age = as.character(Age),
         #Age = ifelse(Age == "0-10", "0", Age),
         Age = case_when(Age == "0-10" ~ "0",
                         Age == "不明" ~ "UNK",
                         Age == "" ~ "UNK",
                         T ~ Age),
         Age = gsub(" ", "", Age, fixed = T),
         Age = factor(Age, levels = c("0", "10", "20", "30", "40", "50",
                                      "60", "70", "80", "90", "UNK")),
         Sex = case_when(Sex == "Male" ~ "m",
                         Sex == "Female" ~ "f",
                         T ~ "UNK")) %>% 
  select(Region, Date, Sex, Age, Value) %>% 
  group_by(Region, Date, Sex, Age) %>% 
  summarise(Value = sum(Value)) %>%
  ungroup(Region, Date, Sex, Age)

min_date_region <- jpn_cases_long %>% 
  group_by(Region) %>% 
  summarise(min_date = min(Date))

saveRDS(min_date_region, "Data/min_date_region.Rds")

## Create a box
mindate <- min(jpn_cases_long$Date)
maxdate <- max(jpn_cases_long$Date)
period <- seq(mindate, maxdate, 1)
age <- c("0", "10", "20", "30", "40", "50",
         "60", "70", "80", "90", "UNK")
sex <- unique(jpn_cases_long$Sex)

full_cases <- data.frame("Date" = rep(period, length(region) * length(age) * length(sex)),
                         "Region" = rep(region, each = length(period), times = length(sex) * length(age)),
                         "Age" = rep(age, each = length(period) * length(region), times = length(sex)),
                         "Sex" = rep(sex, each = length(period) * length(region) * length(age))
)

## Merge box with the main
D_cases_jpn_region <- full_cases %>% 
  left_join(Region, by = c("Region" = "region")) %>% 
  mutate(Country = "Japan",
         AgeInt = case_when(Age == "90" ~ "15",
                            Age == "UNK" ~ "NA",
                            T ~ "10"),
         Metric = "Count",
         Measure = "Cases",
         Datex = format(Date, format = "%d.%m.%Y"),
         Code = paste("JP", region_id, Datex, sep = "_")) %>% 
  left_join(jpn_cases_long, by = c("Region", "Date", "Age", "Sex")) %>% 
  mutate(Value = ifelse(is.na(Value), 0, Value)) %>% 
  select(Country, Region, PrefNo = region_id, Code, Date, Sex, Age, AgeInt, Metric, Measure, Value) %>% 
  group_by(Region, Age, Sex) %>% 
  mutate(Value = cumsum(Value)) %>% 
  ungroup()

D_cases_jpn_region_total <- D_cases_jpn_region %>% 
  group_by(Country, Region, PrefNo, Code, Date, Age, AgeInt, Metric, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Sex = "b")

#### ---- All Japan ----
D_cases_jpn_all <- D_cases_jpn_region %>% 
  group_by(Country, Date, Sex, Age, AgeInt, Metric, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>%
  mutate(Region = "All",
         PrefNo = "00",
         Datex = format(Date, format = "%d.%m.%Y"),
         Code = paste("JP", Datex, sep = "_")) %>% 
  select(Country, Region, PrefNo, Code, Date, Sex, Age, AgeInt, Metric, Measure, Value)

D_cases_jpn_all_total <- D_cases_jpn_all %>% 
  group_by(Country, Region, PrefNo, Code, Date, Age, AgeInt, Metric, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Sex = "b")

#### ---- Japanese confirmed cases data-set ----
D_cases_jpn <- bind_rows(D_cases_jpn_region, D_cases_jpn_region_total, D_cases_jpn_all, D_cases_jpn_all_total)

saveRDS(D_cases_jpn, "Data/D_cases_jpn.Rds")

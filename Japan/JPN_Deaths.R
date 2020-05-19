#===============================================================================
# 2020/04/14
# COVID-19 data collection project
# Ryohei Mogi, rmogi@ced.uab.es
# Fumiya Uchikoshi, uchikoshi@princeton.edu
#===============================================================================
# upd: 2020/04/15
# upd: 2020/04/17

library(tidyverse)
library(lubridate)
library(readxl)
`%out%` = Negate(`%in%`)

region <- c("Hokkaido", "Aomori", "Iwate", "Miyagi", "Akita", "Yamagata", "Fukushima", "Ibaraki", "Tochigi", "Gunma", 
            "Saitama", "Chiba", "Tokyo", "Kanagawa", "Niigata", "Toyama", "Ishikawa", "Fukui", "Yamanashi", "Nagano", 
            "Gifu", "Shizuoka", "Aichi", "Mie", "Shiga", "Kyoto", "Osaka", "Hyogo", "Nara", "Wakayama", "Tottori", 
            "Shimane", "Okayama", "Hiroshima", "Yamaguchi", "Tokushima", "Kagawa", "Ehime", "Kochi", "Fukuoka", "Saga", 
            "Nagasaki", "Kumamoto", "Oita", "Miyazaki", "Kagoshima", "Okinawa")

region_id <- 1:47
Region <- cbind(region, region_id)
Region <- Region %>% 
  as.data.frame() %>% 
  mutate(region_id = as.numeric(as.character(region_id)))

jpn_death <- read_excel("Data/JPN_COVID19_Deaths.xlsx", sheet = 1, col_types = c("numeric", "date", "text",
                                                                                 "numeric", "text", "text",
                                                                                 "numeric", "text", "text",
                                                                                 "text", "text", "text",
                                                                                 "text"))
jpn_death_long <- jpn_death %>% 
  select(-id, -URL, -Note, -基礎疾患,-チェック日,-職業) %>% 
  filter(PrefNo > 0) %>% 
  group_by(Date, Pref, PrefNo, Sex, Age) %>%
  summarise(Check = sum(Check)) %>% 
  ungroup(Date, Pref, PrefNo, Sex, Age) %>% 
  left_join(Region, by = c("PrefNo" = "region_id")) %>% 
  mutate(Sex = case_when(Sex == "M" ~ "m",
                         Sex == "F" ~ "f",
                         T ~ "UNK"),
         Age = factor(Age, levels = c("0", "10", "20", "30", "40", "50",
                                      "60", "70", "80", "90", "UNK")),
         Date = ymd(Date)) %>% 
  select(Region = region, Date, Age, Sex, Value = Check)


## Create a box
mindate <- min(jpn_death_long$Date, na.rm = T)
maxdate <- max(jpn_death_long$Date, na.rm = T)
period <- seq(mindate, maxdate, 1)
#age <- unique(jpn_death_long$Age)
age <- c("0", "10", "20", "30", "40", "50",
         "60", "70", "80", "90", "UNK")
sex <- unique(jpn_death_long$Sex)

full_death <- data.frame("Date" = rep(period, length(region) * length(age) * length(sex)),
                         "Region" = rep(region, each = length(period), times = length(sex) * length(age)),
                         "Age" = rep(age, each = length(period) * length(region), times = length(sex)),
                         "Sex" = rep(sex, each = length(period) * length(region) * length(age))
)


## Merge box with the main
D_death_jpn_region <- full_death %>% 
  left_join(Region, by = c("Region" = "region")) %>% 
  left_join(jpn_death_long, by = c("Region", "Date", "Age", "Sex")) %>% 
  mutate(Country = "Japan",
         AgeInt = case_when(Age == "90" ~ "15",
                            Age == "UNK" ~ "NA",
                            T ~ "10"),
         Metric = "Count",
         Measure = "Deaths",
         Datex = format(Date, format = "%d.%m.%Y"),
         PrefNo = if_else(region_id < 10, paste("0", region_id, sep = ""), as.character(region_id)),
         Code = paste("JP", PrefNo, Datex, sep = "_"),
         Value = ifelse(is.na(Value), 0, Value)) %>% 
  group_by(Region, Age, Sex) %>% 
  mutate(Value = cumsum(Value)) %>% 
  ungroup() %>% 
  mutate(Value = ifelse(is.na(Value), 0, Value)) %>% 
  select(Country, Region, PrefNo, Code, Date, Sex, Age, AgeInt, Metric, Measure, Value)

D_death_jpn_region_total <- D_death_jpn_region %>% 
  group_by(Country, Region, PrefNo, Code, Date, Age, AgeInt, Metric, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Sex = "b")

#### ---- All Japan ----
D_death_jpn_all <- D_death_jpn_region %>% 
  group_by(Country, Date, Sex, Age, AgeInt, Metric, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  ungroup() %>% 
  mutate(Region = "All",
         PrefNo = "00",
         Datex = format(Date, format = "%d.%m.%Y"),
         Code = paste("JP", Datex, sep = "_")) %>% 
  select(Country, Region, PrefNo, Code, Date, Sex, Age, AgeInt, Metric, Measure, Value)

D_death_jpn_all_total <- D_death_jpn_all %>% 
  group_by(Country, Region, PrefNo, Code, Date, Age, AgeInt, Metric, Measure) %>% 
  summarise(Value = sum(Value)) %>% 
  mutate(Sex = "b")

#### ---- Japanese confirmed cases data-set ----
D_death_jpn <- bind_rows(D_death_jpn_region, D_death_jpn_region_total, D_death_jpn_all, D_death_jpn_all_total)

saveRDS(D_death_jpn, "Data/D_death_jpn.Rds")


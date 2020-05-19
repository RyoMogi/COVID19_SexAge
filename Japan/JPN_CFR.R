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
library(devtools)

D_cases_jpn <- readRDS("Data/D_cases_jpn.Rds")
D_death_jpn <- readRDS("Data/D_death_jpn.Rds")
min_date_region <- readRDS("Data/min_date_region.Rds")

#### ---- For Database ----

min_date_jpn <- ymd("2020-01-15")

D_jpn_share <- bind_rows(D_cases_jpn, D_death_jpn) %>% 
  arrange(PrefNo) %>%
  select(-PrefNo) %>% 
  filter(Sex != "b") %>% 
  left_join(min_date_region, by = "Region") %>% 
  mutate(min_date = if_else(Region == "All", min_date_jpn, min_date)) %>% 
  filter(Date >= min_date) %>% 
  mutate(Date = format(Date, format = "%d.%m.%Y"),
         AgeInt = as.integer(AgeInt)) %>% 
  select(-min_date)

write.csv(D_jpn_share,  file = "Data/JPN_Deaths_Cases.csv", row.names = F)

#### ---- Check ----
sort_input_data <- function(X){
  X %>% 
    mutate(Date2 = dmy(Date)) %>% 
    arrange(Country,
            Region,
            Date2,
            Code,
            Sex, 
            Measure,
            Metric,
            suppressWarnings(as.integer(Age))) %>% 
    select(-Date2)
}

outgoing <- D_jpn_share %>% 
  sort_input_data()


source("../../covid_age/R_checks/inputDB_check.R")

region_id <- sprintf(1:47, fmt = "%02d")

Short <- c("JP", paste("JP", region_id, sep = "_"))
run_checks(outgoing, Short, logfile = "log.txt")


#### ---- Submit to database ----
ss <- "https://docs.google.com/spreadsheets/d/1hnetIRO2r3pdNW-eEpSZHXImZAuXDOXe_7_adVrjW7Y/edit#gid=0"

remotes::install_github("tidyverse/googlesheets4")
write_sheet(outgoing, ss = ss, sheet = "database")



#e_check <- D_jpn_share %>% 
#  group_by(Region, Date, Sex, Measure) %>% 
#  summarise(sum_ageint = sum(AgeInt, na.rm = T)) %>% 
#  ungroup(Region, Date, Sex, Measure) %>% 
#  mutate(check = ifelse(sum_ageint == 105, 1, 0))
#  
#table(e_check$check)

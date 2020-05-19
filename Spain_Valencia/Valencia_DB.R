# =======================================================================
# 2020/04/22
# Ryohei Mogi
# COVID-19 database Project
# Valencia data
# Source: https://dadesobertes.gva.es/va/dataset/dades-covid-19-comunitat-valenciana-ultimes-dades
# Source/Cases: https://dadesobertes.gva.es/va/dataset/dades-covid-19-percentatge-i-nombre-de-casos-per-rang-edat-i-sexe
# Source/Deaths: https://dadesobertes.gva.es/va/dataset/dades-covid-19-percentatge-i-numero-de-morts-per-rang-edat-i-sexe
# =======================================================================
# upd 2020/05/06

library(tidyverse)
library(lubridate)

list_case <- list.files(path = "Data/Cases")
list_death <- list.files(path = "Data/Deaths")

D <- c()

for(i in 1:length(list_death)){
  date <- str_extract(list_death[i], "^.{10}")
  date <- ymd(date)
  
  if(date <= ymd("2020-04-29")){
    d_Cases <- read.csv(paste("Data/Cases/", list_case[i], sep = ""), sep =";")
    d_Cases <- d_Cases %>% 
      as.data.frame() %>% 
      mutate(Measure = "Cases")
    
    d_Deaths <- read.csv(paste("Data/Deaths/", list_death[i], sep = ""), sep =";")
    d <- d_Deaths %>% 
      as.data.frame() %>% 
      mutate(Measure = "Deaths") %>% 
      bind_rows(d_Cases) %>% 
      mutate(Age = str_sub(G.Edad, 2, 3),
             Age = ifelse(Age == "0-", "0", Age),
             AgeInt = ifelse(Age == 90, 15, 10),
             Sex = case_when(Sexo == "Hombre" ~ "m",
                             Sexo == "Mujer" ~ "f"),
             Metric = "Count",
             Country = "Spain",
             Region = "Valencia",
             Date = format(date, format = "%d.%m.%Y"),
             Short = "ES_VC",
             Code = paste(Short, Date, sep = "")) %>% 
      rename(Value = NCasos) %>% 
      select(Country, Region, Code, Date, Sex, Age, AgeInt, Metric, Measure, Value)
    
  } else {
    d_Cases <- read.csv(paste("Data/Cases/", list_case[i], sep = ""), sep =";")
    d_Cases <- d_Cases %>% 
      as.data.frame() %>% 
      mutate(Measure = "Cases") %>% 
      rename(G.Edad = Grupo.de.edad, Porc = Porcentaje,
             NCasos = Casos.acumulados.desde.el.31.01.2020)
    
    d_Deaths <- read.csv(paste("Data/Deaths/", list_death[i], sep = ""), sep =";")
    d <- d_Deaths %>% 
      as.data.frame() %>% 
      mutate(Measure = "Deaths") %>% 
      rename(G.Edad = Grupo.de.edad, Porc = Porcentaje,
             NCasos = Casos.acumulados.desde.el.31.01.2020) %>% 
      bind_rows(d_Cases) %>% 
      mutate(Age = str_sub(G.Edad, 2, 3),
             Age = ifelse(Age == "0-", "0", Age),
             AgeInt = ifelse(Age == 90, 15, 10),
             Sex = case_when(Sexo == "Hombre" ~ "m",
                             Sexo == "Mujer" ~ "f"),
             Metric = "Count",
             Country = "Spain",
             Region = "Valencia",
             Date = format(date, format = "%d.%m.%Y"),
             Short = "ES_VC",
             Code = paste(Short, Date, sep = "")) %>% 
      rename(Value = NCasos) %>% 
      select(Country, Region, Code, Date, Sex, Age, AgeInt, Metric, Measure, Value)
  }
  
  
  D <- rbind(D, d)
}

write.csv(D, "Result/DB_Valencia.csv")

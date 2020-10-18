###
install.packages('tidyverse')
update.packages('tidyverse')

library('tidyverse')

setwd('.')

load('country__other_data.RData')
load('country__pop_coord.RData')
load('country_gen_info.RData')
load('country__other_data.RData')
load('covid100__2020-10-02.RData')


glimpse(crt_data)
glimpse(country_gen_info)
glimpse(country_gen_info_init)
glimpse(country__other_data)


#I1. populatia fiecare regiuni geografice

country_gen_info %>%
  select(population, region) %>%
  group_by(region) %>%
  summarise(pop_regiune=sum(population))



#I2 tara cu cea mai mare populatie

country_gen_info %>%
  arrange(desc(population)) %>%
  slice_head()



#I.3 tara cu cel mai mare procent de varstnici

country_gen_info %>%
  arrange(desc(pop_65)) %>%
  select(country_name) %>%
  slice_head()


#I.4 tara cu cel mai mare PIB pe cap de locuitor

country_gen_info %>%
  arrange(desc(gdp_per_capita)) %>%
  select(country_name) %>%
  slice_head()
  

#I.5 tara cu cel mai mare procent de femei
  
  country_gen_info %>%
    arrange(desc(pop_female)) %>%
    select(country_name) %>%
    slice_head()
  
#I.6 tara cu cel mai mare procent de barbati fumatori
  
  country_gen_info %>%
    arrange(desc(smoking_males)) %>%
    select(country_name, smoking_males) %>%
    slice_head()
  
#I.7 zona geografica cea mai populata
  
  country_gen_info %>%
    select(population, region) %>%
    group_by(region) %>%
    summarise(pop_regiune=sum(population)) %>%
    arrange(desc(pop_regiune)) %>%
    slice_head()
    
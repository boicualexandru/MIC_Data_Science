###############################################################
###       Al treilea laborator MIC & DS - SIA - 20 oct. 2020
###############################################################

#install.packages('tidyverse')
#update.packages('tidyverse')

library(tidyverse)
options(scipen = 999)

#setwd('/Users/marinfotache/Google Drive/Baze de date 2020/Studii de caz/covid19(2020)/covid-100-curs/R')

###############################################################
###                    Incarcarea datelor
###############################################################

###         cadrul de date `country__other_data`
load('all_data_covid_2020-10-13.RData')



# Rezolvate in episoadele anterioare 
# I.1 populatia fiecarei regiuni geografice

# I.2 tara cu cea mai mare populatie

# I.3 tara cu cel mai mare procent de varstnici
# your turn!

# I.4 tara cu cel mai mare PIB pe cap de locuitor
# your turn!

# I.5 tara cu cel mai mare procent de femei
# your turn!

# I.6 tara cu cel mai mare procent de barbati fumatori
# your turn!


# I.8 TOP 3 tari cu cea mai mare populatie
#  I.13 TOP 3 zone geografica cele mai populate

 


# II. Adaugati/calculati:

# II.1 atributul nr. de teste din ziua respectiva (teste noi)
# II.2 atributul nr. de cazuri (`confirmed`) din ziua respectiva
# II.3 atributul nr. de decese din ziua respectiva

    



# III. Interogari (mai) complexe tidyverse
# 
# Obtineti:
# 
# III.1 Ziua cu cele mai multe noi cazuri in Romania
rezultat <- covid_new %>%
     inner_join(country_gen_info) %>%
     filter (country_name == 'Romania') %>%
     select (country_name, report_date, confirmed, confirmed_new) %>%
     top_n(1, confirmed_new) 



# III.7 Tara cu cea mai mare rata zilnica de imbolnaviri (raportat la total populatie)

# solutia 1:
rezultat <- covid_new %>%
     inner_join(country_gen_info) %>%
     inner_join(country__pop_coord)  %>%
     select (country_name, report_date, confirmed_new, population) %>%
     mutate (new_cases_rate = round(confirmed_new / population, 6)) %>%
     arrange(desc(new_cases_rate))  %>%
     top_n(1, new_cases_rate)


# solutia 2:
rezultat <- covid_new %>%
     inner_join(country_gen_info) %>%
     inner_join(country__pop_coord)  %>%
     select (country_name, report_date, confirmed_new, population) %>%
     mutate (new_cases_rate = round(confirmed_new / population, 6)) %>%
     filter (new_cases_rate == max(new_cases_rate))  





##################################################################
####                     Interogari 20 octombrie


##################################################################
# I.7 zona geografica cea mai populata


# solutie 1 - folosind functia `max`
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     ungroup() %>%
     filter (region_population == max(region_population))


# solutie 2 - folosind jonctiunea cu un soi de subconsultare
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     ungroup() %>%
     inner_join (country_gen_info %>%
                  inner_join(country__pop_coord) %>%
                  group_by(region) %>%
                  summarise(region_population = sum(population)) %>%
                  ungroup() %>%
                  select (region_population) %>%
                  arrange (desc(region_population)) %>%
                  head(1)
     )
                

# solutie 3 - folosind un soi de subconsultare
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     ungroup() %>%
     filter ( region_population == 
                (country_gen_info %>%
                  inner_join(country__pop_coord) %>%
                  group_by(region) %>%
                  summarise(region_population = sum(population)) %>%
                  ungroup() %>%
                  select (region_population) %>%
                  arrange (desc(region_population)) %>%
                  head(1) %>%
                  pull())
     )


# solutie 4 - generalizabila
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     ungroup() %>%
     top_n(1, region_population)



##################################################################
# I.7' Care sunt zonele geografice cu populatie mai mare decat 
#               `Europe and  Central Asia`

# sol. 1
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     ungroup() %>%
     filter (region_population > 
              (country_gen_info %>%
                  inner_join(country__pop_coord) %>%
                  group_by(region) %>%
                  summarise(region_population = sum(population)) %>%
                  ungroup() %>%
                  filter (str_detect(region, "Europe" )) %>%
                  pull(region_population)))


# sol. 2
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     ungroup() %>%
     mutate (pop_europe = if_else(str_detect(region, "Europe" ), 
                                  region_population, 0)) %>%
     mutate (pop_europe = max(pop_europe)) %>%
     filter (region_population > pop_europe)


# sol. 2'
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     ungroup() %>%
     mutate (pop_europe = if_else(str_detect(region, "Europe" ), 
                                  region_population, 0)) %>%
     filter (region_population > max(pop_europe))



##################################################################
# II.4 Calculati rata zilnica de testare (procent), 
#       raportat la total populatie, pentru fiecare ara si zi
rezultat <- covid_new %>%
    select (country_code, report_date, tests_new) %>%
    inner_join(country__pop_coord) %>%
    select (-latitude, -longitude) %>%
    mutate (daily_test_rate_1000 = (tests_new / population) * 1000 ) %>%
    inner_join(country_gen_info %>%
                 select (country_code, country_name))


test <- rezultat %>%
    filter (country_name == 'Romania')


# II.4' Care a fost ziua cu cea mai mare rata de testare (procent), 
#     pentru Romania
rezultat <- covid_new %>%
    select (country_code, report_date, tests_new) %>%
    filter(country_code == 'ROU') %>%
    inner_join(country__pop_coord) %>%
    select (-latitude, -longitude) %>%
    mutate (daily_test_rate_1000 = (tests_new / population) * 1000 ) %>%
    top_n(1, daily_test_rate_1000)



# II.4'' Afisati, pentru fiecare tara (in ordine alfabetica), 
#       ziua cu cea mai mare rata de testare (procent)
rezultat <- covid_new %>%
    select (country_code, report_date, tests_new) %>%
    inner_join(country__pop_coord) %>%
    select (-latitude, -longitude) %>%
    mutate (daily_test_rate_1000 = (tests_new / population) * 1000 ) %>%
    inner_join(country_gen_info %>%
                 select (country_code, country_name)) %>%
    group_by(country_name) %>%
    arrange(desc(daily_test_rate_1000)) %>%
    filter (row_number() == 1) %>%
    ungroup() %>%
    arrange(country_name)
  

# II.4''' Afisati, pentru fiecare tara (in ordine alfabetica), 
#       top 5 zile cu cea mai mare rata de testare (procent)

# sol. 1
rezultat <- covid_new %>%
    select (country_code, report_date, tests_new) %>%
    inner_join(country__pop_coord) %>%
    select (-latitude, -longitude) %>%
    mutate (daily_test_rate_1000 = (tests_new / population) * 1000 ) %>%
    inner_join(country_gen_info %>%
                 select (country_code, country_name)) %>%
    group_by(country_name) %>%
    arrange(desc(daily_test_rate_1000)) %>%
    filter (row_number() <= 5) %>%
    ungroup() %>%
    arrange(country_name)


# sol. 2
rezultat <- covid_new %>%
    select (country_code, report_date, tests_new) %>%
    inner_join(country__pop_coord) %>%
    select (-latitude, -longitude) %>%
    mutate (daily_test_rate_1000 = (tests_new / population) * 1000 ) %>%
    filter (daily_test_rate_1000 > 0) %>%
    inner_join(country_gen_info %>%
                 select (country_code, country_name)) %>%
    group_by(country_name) %>%
    top_n(5, daily_test_rate_1000) %>%
    ungroup() %>%
    arrange(country_name, desc(daily_test_rate_1000))


test <- rezultat %>%
    inner_join(
      rezultat %>%
          group_by(country_name) %>%
          summarise (n = n()) %>%
          filter (n > 5) 
    )



# II.5 rata zilnica a cazurilor depistate (procent), raportat la total populatie
# II.6 rata zilnica a cazurilor depistate (procent), raportat la numarul de teste
# II.7 rata zilnica a mortalitatii (procent), raportat la total populatie
# II.8 rata zilnica a mortalitatii (procent), raportat la numarul de teste
# II.9 Calculati, pentru fiecare tara si data, numarul de zile de la momentul inceputului pandemiei in tara respectiva
# II.9 Calculati, pentru fiecare tara si data, numarul de zile de la momentul inceputului pandemiei la nivel mondial


# III.2 TOP 3 zile cu cele mai multe noi cazuri in Romania
# III.3 Ziua cu cele mai multe noi decese in Romania
# III.4 TOP 3 zile cu cele mai multe noi decese in Romania
# III.5 Ziua cu cele mai multe noi testari in Romania
# III.6 TOP 3 zile cu cele mai multe noi testari in Romania



# III.8 TOP 3 tari cu cea mai mare rata de imbolnaviri (raportat la total populatie)

# III.9 Tara cu cea mai mare rata a mortalitatii, raportat la total populatie
# III.10 TOP 3 tari cu cea mai mare rata a mortalitatii, raportat la total populatie
# III.11 Tara cu cea mai mare rata a mortalitatii, raportat la numar de cazuri
# III.12 TOP 3 tari cu cea mai mare rata a mortalitatii, raportat la numar de cazuri
# III.13 Tara si data cu cele mai multe noi cazuri
# 
# III.14 TOP 3 tari si date cu cele mai multe noi cazuri
# III.15 Tara si data cu cele mai mari rate ale imbolnavirilor
# III.16 Pentru fiecare tara extrageti TOP 3 zile cu cea mai mare crestere a numarului de noi cazuri
# III.17 Calculati corelatia dintre numarul de cazuri zilnic si PIB pe locuitor
# III.18 Calculati corelatia dintre numarul de cazuri zilnic si gradul de restrictie (`stringency_index`)
# III.19 Calculati, pentru fiecare tara, corelatia dintre numarul de cazuri zilnic si PIB pe locuitor
# III.20 Calculati, pentru fiecare tara, corelatia dintre numarul de cazuri zilnic si gradul de restrictie (`stringency_index`)
# 


#### 
## Care sunt tarile care au avut in luna septembrie o medie de noi cazuri 
##  mai mare decat Romania 





# I.9 TOP 3 tari cu cel mai mare procent de varstnici
# I.10 TOP 3 tari cu cel mai mare PIB pe cap de locuitor
# I.11 TOP 3 tari cu cel mai mare procent de femei
# I.12 TOP 3 tari cu cel mai mare procent de barbati fumatori


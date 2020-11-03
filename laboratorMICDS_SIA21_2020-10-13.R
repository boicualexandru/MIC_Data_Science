###############################################################
###       Al doilea laborator MIC & DS - SIA - 13 oct. 2020
###############################################################

#install.packages('tidyverse')
#update.packages('tidyverse')

library(tidyverse)
#setwd('/Users/marinfotache/Google Drive/Baze de date 2020/Studii de caz/covid19(2020)/covid-100-curs/R')
options(scipen = 999)

###############################################################
###                    Incarcarea datelor
###############################################################

###         cadrul de date `country__other_data`
load('country__other_data.RData')

###         cadrul de date `country__pop_coord`
load('country__pop_coord.RData')
country__pop_coord <- country__pop_coord %>%
     distinct(.)

###         cadrul de date `country_gen_info`
load('country_gen_info.RData')

###         cadrul de date `crt_data`
load('covid100__2020-10-02.RData')

## copiem data frame-ul `crt_data` in `covid`
covid <- crt_data

# stergem `crt_data`
rm(crt_data)



###############################################################
###                      Interogari
###############################################################

### Cate tari sunt in dataframe-ul `country__other_data`?
rezultat <- country__other_data %>%
     count()

### Afisati evolutia cazurilor in cazul Romaniei
rezultat <- covid %>%
     filter (country_code == 'ROU') %>%
     arrange(report_date)


#library(rio)
#rio::export(rezultat, file = 'COVID_Romania_2020-10-01.xlsx', format = 'xlsx')

 
# I.1 populatia fiecarei regiuni geografice
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     arrange(desc(region_population))


# I.2 tara cu cea mai mare populatie
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     top_n(1, population)
     
     
# I.3 tara cu cel mai mare procent de varstnici
# your turn!


# I.4 tara cu cel mai mare PIB pe cap de locuitor
# your turn!


# I.5 tara cu cel mai mare procent de femei
# your turn!


# I.6 tara cu cel mai mare procent de barbati fumatori
# your turn!


# I.7 zona geografica cea mai populata

 
# I.8 TOP 3 tari cu cea mai mare populatie
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     top_n(3, population)



# I.9 TOP 3 tari cu cel mai mare procent de varstnici
# I.10 TOP 3 tari cu cel mai mare PIB pe cap de locuitor
# I.11 TOP 3 tari cu cel mai mare procent de femei
# I.12 TOP 3 tari cu cel mai mare procent de barbati fumatori


# I.13 TOP 3 zone geografica cele mai populate
rezultat <- country_gen_info %>%
     inner_join(country__pop_coord) %>%
     group_by(region) %>%
     summarise(region_population = sum(population)) %>%
     ungroup() %>%
     top_n(3, region_population)
     
 


# II. Adaugati/calculati:

# II.1 atributul nr. de teste din ziua respectiva (teste noi)
# II.2 atributul nr. de cazuri (`confirmed`) din ziua respectiva
# II.3 atributul nr. de decese din ziua respectiva

covid_new <- covid %>%
     arrange(country_code, report_date) %>%
     group_by(country_code) %>%
     mutate (
          tests_new = tests - coalesce(lag(tests, 1), 0),
          confirmed_new = confirmed - coalesce(lag(confirmed, 1), 0),
          recevered_new = recovered - coalesce(lag(recovered, 1), 0),
          deaths_new = deaths - coalesce(lag(deaths, 1), 0)
          ) %>%
     ungroup()

      
test  <- rezultat %>%
     filter (country_code == 'ROU') %>%
     arrange(report_date) %>%
     select (country_code:confirmed, confirmed_new)


# salvare cadru nou de date
rm(rezultat, test)
getwd()
#save.image('all_data_covid_2020-10-13.RData')
#load('all_data_covid_2020-10-13.RData')


# II.4 rata zilnica de testare (procent), raportat la total populatie
# II.5 rata zilnica a cazurilor depistate (procent), raportat la total populatie
# II.6 rata zilnica a cazurilor depistate (procent), raportat la numarul de teste
# II.7 rata zilnica a mortalitatii (procent), raportat la total populatie
# II.8 rata zilnica a mortalitatii (procent), raportat la numarul de teste
# II.9 Calculati, pentru fiecare tara si data, numarul de zile de la momentul inceputului pandemiei in tara respectiva
# II.9 Calculati, pentru fiecare tara si data, numarul de zile de la momentul inceputului pandemiei la nivel mondial
    



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



# III.2 TOP 3 zile cu cele mai multe noi cazuri in Romania
# III.3 Ziua cu cele mai multe noi decese in Romania
# III.4 TOP 3 zile cu cele mai multe noi decese in Romania
# III.5 Ziua cu cele mai multe noi testari in Romania
# III.6 TOP 3 zile cu cele mai multe noi testari in Romania


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



## Care sunt tarile care au avut in luna septembrie o medie de noi cazuri 
##  mai mare decat Romania 




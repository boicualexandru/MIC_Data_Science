###############################################################
###       Al treilea laborator MIC & DS - SIA - 26 oct. 2020
###############################################################

#install.packages('tidyverse')
#update.packages('tidyverse')

library(tidyverse)
library(lubridate)
options(scipen = 999)

#setwd('/Users/marinfotache/Google Drive/Baze de date 2020/Studii de caz/covid19(2020)/covid-100-curs/R')

###############################################################
###                    Incarcarea datelor
###############################################################

###         cadrul de date `country__other_data`
load('all_data_covid_2020-10-13.RData')



###############################################################
###             Interogari 2020-10-27
###############################################################



##############################################################################
## Sa se obtina un raport in care pe fiecare linie sa fie afisata cate o tara
## si pe coloane, in afara numelui tarii, sa se afiseze numarul de 
## cazuri COVID pe fiecare din primele trei trimestre ale anului 2020
## 
###    Tara              cazuri_trim1    cazuri_trim2   cazuri_trim3
##############################################################################


rezultat <- country_gen_info %>%
    filter(!is.na(country_name)) %>%
    select (country_name, country_code) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(1, 2, 3)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim1 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(4, 5, 6)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim2 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(7, 8, 9)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim3 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    mutate (cazuri_trim1 = coalesce(cazuri_trim1, 0),
            cazuri_trim2 = coalesce(cazuri_trim2, 0),
            cazuri_trim3 = coalesce(cazuri_trim3, 0)
            )


##############################################################################
## Sa se obtina un raport in care pe fiecare linie sa fie afisata cate o tara
## si pe coloane, in afara numelui tarii, sa se afiseze numarul de 
## cazuri COVID pe fiecare din primele trei trimestre ale anului 2020
## 
###   In plus, adaugati o coloana si o linie cu totaluri 
##############################################################################


## sol. 1
rezultat <- bind_rows(
  country_gen_info %>%
    filter(!is.na(country_name)) %>%
    select (country_name, country_code) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(1, 2, 3)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim1 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(4, 5, 6)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim2 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(7, 8, 9)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim3 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) <= 9) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim1_2_3 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    mutate (cazuri_trim1 = coalesce(cazuri_trim1, 0),
            cazuri_trim2 = coalesce(cazuri_trim2, 0),
            cazuri_trim3 = coalesce(cazuri_trim3, 0),
            cazuri_trim1_2_3 = coalesce(cazuri_trim1_2_3, 0)
            ),
    tibble(
        country_name = ' T O T A L ',
        country_code = '',
        cazuri_trim1 = (covid_new %>%
              filter (year(report_date) == 2020 & month(report_date) %in% c(1, 2, 3)) %>%
              summarise (cazuri_trim1 = sum(confirmed_new)) %>%
              pull(cazuri_trim1))   , 
        cazuri_trim2 = (covid_new %>%
              filter (year(report_date) == 2020 & month(report_date) %in% c(4, 5, 6)) %>%
              summarise (cazuri_trim2 = sum(confirmed_new)) %>%
              pull(cazuri_trim2))   , 
        cazuri_trim3 = (covid_new %>%
              filter (year(report_date) == 2020 & month(report_date) %in% c(7, 8, 9)) %>%
              summarise (cazuri_trim3 = sum(confirmed_new)) %>%
              pull(cazuri_trim3))   , 
        cazuri_trim1_2_3 = (covid_new %>%
              filter (year(report_date) == 2020 & month(report_date) <= 9) %>%
              summarise (cazuri_trim1_2_3 = sum(confirmed_new)) %>%
              pull(cazuri_trim1_2_3))   
      )
)


## sol. 2
rezultat <- bind_rows (
  country_gen_info %>%
    filter(!is.na(country_name)) %>%
    select (country_name, country_code) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(1, 2, 3)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim1 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(4, 5, 6)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim2 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    left_join( covid_new %>%
            filter (year(report_date) == 2020 & month(report_date) %in% c(7, 8, 9)) %>%
            group_by(country_code) %>%
            summarise(cazuri_trim3 = sum(confirmed_new)) %>%
            ungroup()
    ) %>%
    mutate (cazuri_trim1 = coalesce(cazuri_trim1, 0),
            cazuri_trim2 = coalesce(cazuri_trim2, 0),
            cazuri_trim3 = coalesce(cazuri_trim3, 0),
            cazuri_trim1_2_3 = cazuri_trim1 + cazuri_trim2 + cazuri_trim3
            ),
    bind_cols(
        tibble(country_name = ' T O T A L '),
        tibble(country_code = ''),
        covid_new %>%
              filter (year(report_date) == 2020 & month(report_date) %in% c(1, 2, 3)) %>%
              summarise (cazuri_trim1 = sum(confirmed_new)),
        covid_new %>%
              filter (year(report_date) == 2020 & month(report_date) %in% c(4, 5, 6)) %>%
              summarise (cazuri_trim2 = sum(confirmed_new)),
        covid_new %>%
              filter (year(report_date) == 2020 & month(report_date) %in% c(7, 8, 9)) %>%
              summarise (cazuri_trim3 = sum(confirmed_new)),
        covid_new %>%
              filter (year(report_date) == 2020 & month(report_date) < 9) %>%
              summarise (cazuri_trim1_2_3 = sum(confirmed_new))
      )
)
  



##############################################################################
## Sa se obtina un raport in care pe fiecare linie sa fie afisata cate o tara
## si pe coloane, in afara numelui tarii, sa se afiseze numarul de 
## cazuri COVID pe fiecare luna ale primelor trei trimestre ale anului 2020
## 

rezultat <- covid_new %>%
    filter (year(report_date) == 2020 & month(report_date) <= 9) %>%
    transmute (country_code, month = month(report_date), confirmed_new) %>%
    group_by(country_code, month) %>%
    summarise(new_cases = sum(confirmed_new)) %>%
    ungroup() %>%
    pivot_wider(names_from = month, values_from = new_cases)
  






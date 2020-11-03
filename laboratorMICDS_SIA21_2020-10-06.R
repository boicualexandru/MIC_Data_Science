###############################################################
###       Primul laborator MIC & DS - SIA - 6 oct. 2020
###############################################################

#install.packages('tidyverse')
#update.packages('tidyverse')

library(tidyverse)

#setwd('/Users/marinfotache/Downloads/COVID19-20201006')


###############################################################
###                    Incarcarea datelor
###############################################################

###         cadrul de date `country__other_data`
load('country__other_data.RData')

###         cadrul de date `country__pop_coord`
load('country__pop_coord.RData')

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







rezultat <- covid_new %>%
  group_by(country_code) %>%
  summarise(totalDeaths = sum(deaths)) %>%
  arrange(desc(totalDeaths)) %>%
  filter(row_number() <= 10)




rezultat <- covid_new %>%
  inner_join(country_gen_info) %>%
  group_by(country_name) %>%
  summarise(totalDeaths = sum(deaths)) %>%
  arrange(desc(totalDeaths)) %>%
  filter(row_number() <= 10)
  
  inner_join(country_gen_info) %>%
  filter (country_name == 'Romania') %>%
  select (country_name, report_date, confirmed, confirmed_new) %>%
  top_n(1, confirmed_new) 










rezultat <- country_gen_info %>%
  filter(!is.na(country_name)) %>%
  select(country_name, country_code)
  left_join(
    covid_new %>%
      filter(year(report_date) == 2020 & month report_date %in% c(1, 2, 3)) %>%
      group_by(country_code)
  )
  
  
  rezultat <- country_gen_info %>%
    
    filter(!is.na(country_name) %>%
  
  select (country_name, country_code) %>%
  
  left_join( covid_new %>%
    filter (yearCreport_date) == 2020 & month(report_date) %in® c(1, J, 1)) %>%
    group_by((country_code) %>%
    summarise(cazuri_triml = sum(confirmed_new)) %>%
    ungroup()
  ) %>%
    left_join( covid_new %>%
     filter (yearCreport_date) == 2020 & month(report_date) %in® c(1, J, 1)) %>%
   group_by((country_code) %>%
              summarise(cazuri_triml = sum(confirmed_new)) %>%
              ungroup()
   ) %>%
     left_join( covid_new %>%
      filter (yearCreport_date) == 2020 & month(report_date) %in® c(1, J, 1)) %>%
    group_by((country_code) %>%
               summarise(cazuri_triml = sum(confirmed_new)) %>%
               ungroup()
    ) %>%

left_join( covid_new %>%
             filter CyearCreport_date) == J02¢ & monthCreport_date) %in®% c(4, %, &)) %%
                                                                            group_byCcountry_code) %>%
  summariseCcazuri_trim2 = surCconfirmed_new)) %>%
  ungroup()

De So.)

left joinC covid_new %>%
  filter CyearCreport_date) == [0/6 & monthCreport_date) *in® c(*, 8, )) %%
  group byCcountry_code) %>%
  summariseCcazuri_trim3 = surCconfirmed_new)) %%
  ungroup()

eso.)

mutate Ccazuri_triml = coalesce(cazuri_triml, “),
cazuri_trim2 = coalesceCcazuri_trim2, ),
cazuri_trim3 = coalesce(cazuri_trim3, +)
)

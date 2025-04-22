library(tidyverse)

county_data <- read_csv("county_data.csv")
coc_data <- read_csv("coc_data.csv")


coc_data |>
  ggplot() +
  geom_jitter(aes(x = med_hh_income, y = funding_tot/total_population)) +
  scale_y_log10()

coc_data |>
  ggplot() +
  geom_jitter(aes(x = total_population, y = funding_tot/total_population)) +
  scale_y_log10() +
  scale_x_log10()



coc_data |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/total_population, y = `Overall Homeless`/total_population, color = `CoC Category`)) +
  scale_x_log10() +
  scale_y_log10()

coc_data |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/total_population, y = `Overall Homeless`/total_population, color = percent_rural)) +
  scale_x_log10() +
  scale_y_log10()

coc_data |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/total_population, y = `Overall Homeless`/total_population, color = log10(poverty_rate))) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_continuous(type = "viridis")

coc_data |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/total_population, y = `Overall Homeless`/total_population, color = poverty_rate)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_color_continuous(type = "viridis")





coc_data |> 
  ggplot() +
  geom_histogram(aes(x = `Overall Homeless`/total_population))

coc_data |>
  ggplot() +
  geom_jitter(aes(x = med_hh_income, y = funding_tot/`Overall Homeless`)) +
  scale_y_log10()


### Maps

coc_data |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR") |>
  ggplot() +
  geom_sf(aes(fill = log10(funding_tot/`Overall Homeless`))) +
  scale_fill_continuous(name = NULL)









### Color points based on other variables


CE <- read_csv("county_equity_metrics.csv")

CE2 <- ifelse(CE$`program_doi_water_storage-percent_rank_water_system_violations` < 0, 'Underfunded', ifelse(CE$`program_doi_water_storage-percent_rank_poverty_rate` > 0, 'Overfunded', 'Just right'))
?scale_fill_manual

CE3 <- ifelse(CE$`program_doi_water_storage-percent_rank_poverty_rate` < 0, 'Underfunded', ifelse(CE$`program_doi_water_storage-percent_rank_water_system_violations` > 0, 'Overfunded', 'Just right'))


CE |>
  ggplot() +
  geom_histogram(aes(x = `program_doi_water_storage-percent_rank_water_system_violations`, fill = CE2), 
                 color = "black") +
  labs(title = "Equity Metrics for Water Infrastructure Funds", x = "Equity Metric: Percentile funding - percentile water system violations") +
  scale_fill_manual(name = "Over/under-funded?", values = c('Just right' = 'skyblue',
                                                            'Overfunded' = 'green4',
                                                            'Underfunded' = 'orange3'))

CE |>
  ggplot() +
  geom_histogram(aes(x = `program_doi_water_storage-percent_rank_poverty_rate`, fill = CE3), color = "black") +
  labs(title = "Equity Metrics for Water Infrastructure Funds", x = "Equity Metric: Percentile funding - percentile poverty rate") +
  scale_fill_manual(name = "Over/under-funded?", values = c('Just right' = 'skyblue',
                                                            'Overfunded' = 'green4',
                                                            'Underfunded' = 'orange3'))



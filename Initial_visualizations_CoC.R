library(tidyverse)
library(sf)
library(plotly)

county_data <- read_csv("county_data.csv")
coc_data <- read_csv("coc_data.csv")

stupid_simple_GIS <- st_read("stupid_simple_GIS.shp")



coc_data <- stupid_simple_GIS |>
  right_join(coc_data, by = c("COCNUM" = "coc_num"))





























### Scatterplots

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
  scale_color_continuous(type = "viridis")



### Histograms

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
  # scale_fill_continuous(name = "$$$ per homeless person") +
  scale_fill_gradientn(transform = "log10",
                     colours = c("lightgrey", "orange2", "blue4"),
                     limits = c(2, 6.1), breaks = c(3, 4, 5, 6),
                     labels = c("$1,000", "$10,000", "$100,000", "$1,000,000"),
                     name = "Funding per homeless person")







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



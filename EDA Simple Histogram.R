library(tidyverse)

SE <- read_csv("state_equity_metrics.csv")

CE <- read_csv("county_equity_metrics.csv")


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




CE2 <- ifelse(CE$`program_doi_water_storage-percent_rank_water_system_violations` < 0, 'Underfunded', ifelse(CE$`program_doi_water_storage-percent_rank_poverty_rate` > 0, 'Overfunded', 'Just right'))
?scale_fill_manual

CE3 <- ifelse(CE$`program_doi_water_storage-percent_rank_poverty_rate` < 0, 'Underfunded', ifelse(CE$`program_doi_water_storage-percent_rank_water_system_violations` > 0, 'Overfunded', 'Just right'))

library(tidyverse)

sdat <- read_csv("state_data.csv")

sdat |>
  ggplot() +
  geom_jitter(aes(x = funding_tot, y = `Overall Homeless`)) +
  geom_jitter(aes(x = funding_tot, y = `Sheltered Total Homeless`), alpha = 0.5, color = "blue") +
  scale_x_log10()

sdat |>
  ggplot() +
  geom_jitter(aes(x = funding_tot, y = `Sheltered Total Homeless`))

sdat |>
  ggplot() +
  geom_jitter(aes(y = funding_tot, x = `Overall Homeless`)) +
  geom_jitter(aes(y = funding_tot, x = `Sheltered Total Homeless`), alpha = 0.5, color = "blue") +
  scale_y_log10()


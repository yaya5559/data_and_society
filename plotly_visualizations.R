library(tidyverse)
library(plotly)
library(sf)
library(rmapshaper)

coc_data <- read_csv("coc_data.csv")

GIS <- sf::st_read("CoC_GIS_National_Boundary.gdb")

stupid_simple_GIS <- GIS |>
  rmapshaper::ms_simplify(keep = 0.001, keep_shapes = FALSE)

hud_gis <- stupid_simple_GIS |>
  right_join(coc_data, by = c("COCNUM" = "coc_num"))

hud_gis <- sf::st_cast(hud_gis, "MULTIPOLYGON")




p1 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Overall Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate, 
                  text = paste(coc_name))) +
  scale_x_log10() +
  scale_y_log10()

ggplotly(p1)


e <- hud_gis |> 
  filter(state_abv == "MA") |>
  ggplot() +
  geom_sf()



p2 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR") |>
  ggplot() +
  geom_sf(aes(fill = log10(funding_tot/`Overall Homeless`),
                           text = paste(coc_name,
                                        '</br>Funding per homeless person: $', funding_tot/`Overall Homeless`))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       limits = c(2, 6.01), breaks = c(2, 3, 4, 5, 6),
                       labels = c("$100", "$1,000", "$10,000", "$100,000", "$1,000,000"),
                       name = "Funding per homeless person")

ggplotly(p2, hoveron = "fills")







p3 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Overall Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate,
              text = paste(coc_name,
                           '</br>Funding per homeless person: $', funding_tot/`Overall Homeless`))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10() +
  scale_y_log10()

ggplotly(p3, hoveron = "color")







p4 <- hud_gis |>
  filter(funding_tot/`Overall Homeless` >= 300000) |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Overall Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '</br>Funding per homeless person: $', funding_tot/`Overall Homeless`,
                               "</br>Overall Homeless: ", `Overall Homeless`))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.3), breaks = c(0, .1, .2, .3),
                        labels = c("0%", "10%", "20%", "30%"),
                        name = "Poverty Rate") +
  scale_x_log10(limits = c(300000, 3100000), breaks = c(300000, 1000000, 3000000),
                labels = c("$300,000", "$1,000,000", "$3,000,000")) +
  scale_y_log10(limits = c(0.0001, 0.003), breaks = c(0.0003, 0.003),
                labels = c("0.03%", "0.3%")) +
  labs(title = "CoCs With More than $300,000 per Homeless Individual", x = "Funding per Homeless Person (USD)",
       y = "Homelessness Rate")

ggplotly(p4, hoveron = "color")






p5 <- hud_gis |>
  filter(funding_tot/`Sheltered Total Homeless` >= 300000) |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Sheltered Total Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '</br>Funding per homeless person: $', funding_tot/`Sheltered Total Homeless`,
                               "</br>Sheltered Homeless: ", `Sheltered Total Homeless`))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.3), breaks = c(0, .1, .2, .3),
                        labels = c("0%", "10%", "20%", "30%"),
                        name = "Poverty Rate") +
  scale_x_log10() +
  scale_y_log10()

ggplotly(p5, hoveron = "color")








p6 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Sheltered Total Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '</br>Funding per homeless person: $', funding_tot/`Sheltered Total Homeless`,
                               "</br>Sheltered Homeless: ", `Sheltered Total Homeless`))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.3), breaks = c(0, .1, .2, .3),
                        labels = c("0%", "10%", "20%", "30%"),
                        name = "Poverty Rate") +
  labs(Title = "Funding per Sheltered Homeless Person", x = "Funding per Sheltered Individual (USD)", )
  scale_x_log10() +
  scale_y_log10()

ggplotly(p6, hoveron = "color")




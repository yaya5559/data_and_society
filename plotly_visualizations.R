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
                  text = paste())) +
  scale_x_log10() +
  scale_y_log10()

ggplotly(p1)






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

ggplotly(p2, hoveron = "fill")





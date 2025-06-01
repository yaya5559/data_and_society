library(tidyverse)
library(plotly)
library(sf)
library(rmapshaper)
library(scales)
library(weights)
library(htmlwidgets)

ppp <- read_csv("ppp.csv")
ppp$race_better <- factor(ppp$race_better, levels = c("Native Hawaiian/Other Pacific Islander", 
                                                      "Black, African American, or African", 
                                                      "American Indian/Alaska Native", 
                                                      "White", 
                                                      "Multi-Racial", 
                                                      "Asian"))
cocstuff4pca <- read_csv("too_many_cocs.csv")
vvv <- stupid_simple_GIS |>
  left_join(too_many_cocs, by = c("COCNUM" = "coc_num"))
vvv <- vvv |>
  left_join(coc_data, by = c("COCNUM" = "coc_num"))
vvv <- sf::st_cast(thingy, "MULTIPOLYGON")

coc_data <- read_csv("coc_data.csv")
GIS <- sf::st_read("CoC_GIS_National_Boundary.gdb")
stupid_simple_GIS <- GIS |>
  rmapshaper::ms_simplify(keep = 0.001, keep_shapes = FALSE)
hud_gis <- stupid_simple_GIS |>
  right_join(coc_data, by = c("COCNUM" = "coc_num"))
hud_gis <- sf::st_cast(hud_gis, "MULTIPOLYGON")


# Look into Alpine, Inyo, and Mono Counties CoC

p25 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = `Overall Homeless`/total_population,
              text = paste(coc_name,
                           '<br>Homeless rate: ', round((`Overall Homeless`/total_population)*100, digits = 2), "%"))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Homeless Rate",
                       labels = scales::percent_format())

ggplotly(p25, tooltip = "text") |>
  style(hoveron = "fills")


#$/ person map (same scale as $/homeless map)

p7 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = fnd_per_person,
              text = paste(coc_name,
                           '<br>Funding per person: $', round(fnd_per_person), digits = 2))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       labels = scales::comma_format(prefix = "$"),
                       name = "Funding per Person"
  )

ggplotly(p7, tooltip = "text") |>
  style(hoveron = "fills")





# Median HH income

p27 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = med_hh_income,
              text = paste(coc_name,
                           '<br>Median Household Income: $', round(med_hh_income, digits = 2)))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Median Household<br>Income",
                       labels = scales::comma_format(prefix = "$"))

ggplotly(p27, tooltip = "text") |>
  style(hoveron = "fills")




ppp |>
  ggplot() + 
  geom_col(aes(x = race_better, y = median_scale_of_diff, fill = race,
               text = paste(race_better,
                            "<br>Median racial disparity gap: ", round(median_scale_of_diff, digits = 3)))) +
  theme(axis.text.x = element_text(vjust = .5)) +
  scale_x_discrete(labels = c("Native Hawaiian/\nOther Pacific\nIslander",
                              "Black, African\nAmerican, or\nAfrican",
                              "American Indian/\nAlaska Native",
                              "White",
                              "Multi-Racial",
                              "Asian"),
                   name = "Race Group") +
  scale_y_continuous(name = "Median Outcome Gap") +
  labs(title = "Racial Disparities In Homelessness",
       subtitle = "Gap is how many times more likely a given group is to be homeless than average homelessness rate for whole population") +
  scale_fill_discrete(guide = FALSE)




p3a <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = fnd_per_person, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per person: $', round(fnd_per_person, digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Funding per Person", x = "Funding per Person (USD)", y = "Homelessness Rate (%)")

ggplotly(p3a, tooltip = "text")







# Homeless rate by overcrowded scatterplot (vacancy rate = color)

p13a <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = overcrowded_housing, y = `Overall Homeless`/total_population, color = vacancy_rate,
                  text = paste(coc_name,
                               '<br>Houses with 2 or more occupants per room: ', round(overcrowded_housing*100, digits = 3), "%",
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Vacancy rate: ', round((vacancy_rate)*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Vacancy Rate",
                        labels = scales::percent_format()) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Overcrowded Housing", x = "Share of Houses With 2 or More Occupants per Room (%)", 
       y = "Homelessness Rate (%)")

ggplotly(p13a, tooltip = "text")





# fnd/impoverished person by median hh income

p32 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = med_hh_income, y = funding_tot/total_population, color = `Overall Homeless`/total_population,
                  text = paste(coc_name,
                               '<br>Median Household Income: $', med_hh_income,
                               '<br>Funding per impoverished person: $', round(funding_tot/total_population, digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%'))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Homelessness rate",
                        labels = scales::percent_format()) +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::comma_format(prefix = "$")) +
  labs(title = "Funding per Person by Median Household Income", x = "Median Household Income (USD)", 
       y = "Funding per Person") +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p32, tooltip = "text")






# Poverty rate map

p14 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = poverty_rate,
              text = paste(coc_name,
                           '<br>Share of People Below Poverty Line: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("lightblue1", "maroon4"),
                       labels = scales::percent_format(),
                       name = "Poverty Rate")

ggplotly(p14, tooltip = "text") |>
  style(hoveron = "fills")





# Chronically homeless map (near end)

p19 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill =`Overall Chronically Homeless`/`Overall Homeless`,
              text = paste(coc_name,
                           '<br>Homeless people that are chronically homeless: ', round(`Overall Chronically Homeless`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("lightblue1", "maroon4"),
                       name = "Experienceing Chronic<br>Homelessness",
                       labels = scales::percent_format())

ggplotly(p19, tooltip = "text") |>
  style(hoveron = "fills")  





#pc maps

y1 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc1,
              text = paste(coc_name,
                           '<br>Pc1 score: ', round(pc1, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f03b20", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc1 score")

ggplotly(y1, tooltip = "text") |>
  style(hoveron = "fills") 


y2 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc2,
              text = paste(coc_name,
                           '<br>Pc2 score: ', round(pc2, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f03b20", "#fd8d3c", "#feb24c", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc2 score",
                       # limits = c(-5,5)
  )

ggplotly(y2, tooltip = "text") |>
  style(hoveron = "fills") 


y3 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc3,
              text = paste(coc_name,
                           '<br>Pc3 score: ', round(pc3, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#ffffcc", "#feb24c", "#fd8d3c", "#f03b20"),
                       name = "Pc3 score")

ggplotly(y3, tooltip = "text") |>
  style(hoveron = "fills")


y4 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc4,
              text = paste(coc_name,
                           '<br>Pc4 score: ', round(pc4, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#7fcdbb", "#a1dab4", "#ffffcc", "#feb24c", "#f03b20"),
                       name = "Pc4 score")

ggplotly(y4, tooltip = "text") |>
  style(hoveron = "fills")


y5 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc5,
              text = paste(coc_name,
                           '<br>Pc5 score: ', round(pc5, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f3330c", "#fd8d3c", "#fecc5c", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc5 score")

ggplotly(y5, tooltip = "text") |>
  style(hoveron = "fills")



y6 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc6,
              text = paste(coc_name,
                           '<br>Pc6 score: ', round(pc6, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#ffffcc", "#fecc5c", "#fd8d3c", "#f3330c"),
                       name = "Pc6 score")

ggplotly(y6, tooltip = "text") |>
  style(hoveron = "fills")


y7 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc7,
              text = paste(coc_name,
                           '<br>Pc7 score: ', round(pc7, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f3330c", "#fecc5c", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc7 score")

ggplotly(y7, tooltip = "text") |>
  style(hoveron = "fills")


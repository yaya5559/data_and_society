




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


p3b <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = fnd_per_impov, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per impoverished person: $', round(fnd_per_impov, digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Funding per Impoverished Person", x = "Funding per Person (USD)", y = "Homelessness Rate (%)")

ggplotly(p3b, tooltip = "text")


p3b <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = fnd_per_hmls, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per homeless person: $', round(fnd_per_hmls, digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Funding per Homeless Person", x = "Funding per Person (USD)", y = "Homelessness Rate (%)")

ggplotly(p3b, tooltip = "text")





# Homeless rate by overcrowded scatterplot (poverty rate = color)

p11 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = overcrowded_housing, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Houses with 2 or more occupants per room', round(overcrowded_housing*100, digits = 2), "%",
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty Rate: ', round(poverty_rate*100, digits = 2), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Poverty Rate",
                        labels = scales::percent_format()) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Overcrowded Housing", x = "Share of Houses with Two or More Occupants per Room (%)", 
       y = "Homelessness Rate (%)") 

ggplotly(p11, tooltip = "text")




# fnd/impoverished person by median hh income

p32 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = med_hh_income, y = funding_tot/(poverty_rate*total_population), color = `Overall Homeless`/total_population,
                  text = paste(coc_name,
                               '<br>Median Household Income: $', med_hh_income,
                               '<br>Funding per impoverished person: $', round(funding_tot/(poverty_rate*total_population), digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%'))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Homelessness rate",
                        labels = scales::percent_format()) +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::comma_format(prefix = "$")) +
  labs(title = "Funding per Impoverished Person by Median Household Income", x = "Median Household Income (USD)", 
       y = "Funding per Impoverished Person") +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p32, tooltip = "text")





p13a <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = overcrowded_housing, y = `Overall Homeless`/total_population, color = vacancy_rate,
                  text = paste(coc_name,
                               '<br>Houses with 2 or more occupants per room: ', round(overcrowded_housing*100, digits = 3), "%",
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Vacancy rate: ', round(vacancy_rate, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Vacancy Rate") +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Poverty Rate", x = "Share of Houses With 2 or More Occupants per Room (%)", 
       y = "Homelessness Rate (%)") 

ggplotly(p13a, tooltip = "text")






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
  scale_fill_gradientn(colours = c("lightblue", "maroon4"),
                       name = "Experienceing Chronic<br>Homelessness",
                       labels = scales::percent_format())

ggplotly(p19, tooltip = "text") |>
  style(hoveron = "fills")  

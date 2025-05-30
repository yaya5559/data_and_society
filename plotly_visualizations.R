library(tidyverse)
library(plotly)
library(sf)
library(rmapshaper)
library(scales)
library(weights)
library(htmlwidgets)

coc_data <- read_csv("coc_data.csv")

GIS <- sf::st_read("CoC_GIS_National_Boundary.gdb")

stupid_simple_GIS <- GIS |>
  rmapshaper::ms_simplify(keep = 0.001, keep_shapes = FALSE)

hud_gis <- stupid_simple_GIS |>
  right_join(coc_data, by = c("COCNUM" = "coc_num"))

### consider right_join

hud_gis <- sf::st_cast(hud_gis, "MULTIPOLYGON")

hud_gis <- hud_gis |>
  mutate(fnd_per_impov = funding_tot/(poverty_rate*total_population),
         fnd_per_person = funding_tot/total_population,
         fnd_per_hmls = funding_tot/`Overall Homeless`,
         homeless_rate = `Overall Homeless`/total_population)






# Homeless rate by $/homeless individual scatterplot

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


# $/homeless person map

p2 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = log10(funding_tot/`Overall Homeless`),
                           text = paste(coc_name,
                                        '<br>Funding per homeless person: $', round((funding_tot/`Overall Homeless`), digits = 2)))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       limits = c(2, 6.01), breaks = c(2, 3, 4, 5, 6),
                       labels = c("$100", "$1,000", "$10,000", "$100,000", "$1,000,000"),
                       name = "Funding per homeless person")

ggplotly(p2, tooltip = "text") |>
  style(hoveron = "fills")





# Homeless rate by $/homeless individual scatterplot (poverty rate = color)

p3 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Overall Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate,
              text = paste(coc_name,
                           '<br>Funding per homeless person: $', round((funding_tot/`Overall Homeless`), digits = 2),
                           '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                           '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Funding per Homeless Person", x = "Funding per Homeless Individual (USD)", y = "Homelessness Rate (%)")

ggplotly(p3, tooltip = "text")





# Homeless rate by $/homeless individual scatterplot (>$300,000 per person)

p4 <- hud_gis |>
  filter(funding_tot/`Overall Homeless` >= 300000) |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Overall Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per homeless person: $', round(funding_tot/`Overall Homeless`, digits = 2),
                               "<br>Overall Homeless: ", `Overall Homeless`,
                               '<br>Poverty Rate: ', round(poverty_rate*100, digits = 3), "%"))) +
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

ggplotly(p4, tooltip = "text")




# Homeless rate by $/sheltered homeless scatterplot (>$300,000 per person)

p5 <- hud_gis |>
  filter(funding_tot/`Sheltered Total Homeless` >= 300000) |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Sheltered Total Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per homeless person: $', round(funding_tot/`Sheltered Total Homeless`, digits = 2),
                               "<br>Sheltered Homeless: ", `Sheltered Total Homeless`,
                               '<br>Poverty Rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.3), breaks = c(0, .1, .2, .3),
                        labels = c("0%", "10%", "20%", "30%"),
                        name = "Poverty Rate") +
  scale_x_log10() +
  scale_y_log10()

ggplotly(p5, tooltip = "text")



# Homeless rate by $/sheltered homeless scatterplot   @

p6 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Sheltered Total Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per homeless person: $', round(funding_tot/`Sheltered Total Homeless`, digits = 2),
                               "<br>Sheltered Homeless: ", `Sheltered Total Homeless`,
                               '<br>Poverty Rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.3), breaks = c(0, .1, .2, .3),
                        labels = c("0%", "10%", "20%", "30%"),
                        name = "Poverty Rate") +
  labs(Title = "Funding per Sheltered Homeless Person", x = "Funding per Sheltered Individual (USD)", y = "Homeless Rate") +
  scale_x_log10() +
  scale_y_log10()

ggplotly(p6, tooltip = "text")











# $/impoverished person map (same scale as $/homeless map)

p7 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = log10(funding_tot/(poverty_rate*total_population)),
              text = paste(coc_name,
                           '<br>Funding per impoverished person: $', round(funding_tot/(poverty_rate*total_population), digits = 2)))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       limits = c(2, 6.01), breaks = c(2, 3, 4, 5, 6),
                       labels = c("$100", "$1,000", "$10,000", "$100,000", "$1,000,000"),
                       name = "Funding per Impoverished Person"
                       )

ggplotly(p7, tooltip = "text") |>
  style(hoveron = "fills")



# $/impoverished person map (new scale)

p8 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = log10(funding_tot/(poverty_rate*total_population)),
              text = paste(coc_name,
                           '<br>Funding per impoverished person: $', round(funding_tot/(poverty_rate*total_population), digits = 2)))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       limits = c(1, 4.01), breaks = c(1, 2, 3, 4),
                       labels = c("$10", "$100", "$1,000", "$10,000"),
                       name = "Funding per Impoverished Person")

ggplotly(p8, tooltip = "text") |>
  style(hoveron = "fills")



# Homeless rate by $/impoverished individual scatterplot (poverty rate = color)

p9 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/(poverty_rate*total_population), y = `Overall Homeless`/total_population, color = med_hh_income,
                  text = paste(coc_name,
                               '<br>Funding per impoverished person: $', round(funding_tot/(poverty_rate*total_population), digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Median Household Income: $', round(med_hh_income, digits = 2)))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Median Household Income",
                        labels = scales::comma_format(prefix = "$")) +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Funding per Impoverished Person", x = "Funding per Impoverished Individual (USD)", 
       y = "Homelessness Rate (%)") +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p9, tooltip = "text")








# Overcrowded housing map

p10 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = overcrowded_housing, text = paste(coc_name,
                           '<br>Share of Houses With 2 or More Occupants per Room: ', 
                           round(overcrowded_housing*100, digits = 3), "%"))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Overcrowded Housing (%)",
                       labels = scales::percent_format())

ggplotly(p10, tooltip = "text") |>
  style(hoveron = "fills")



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


# funding per overcrowded house scatterplot(not super useful)

p12 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/overcrowded_housing*housing_units*total_population, y = `Overall Homeless`/total_population, 
                  color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per Overcrowded House: $', 
                               round(funding_tot/overcrowded_housing*housing_units*total_population, digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty Rate: ', round(poverty_rate*100, digits = 2), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Poverty Rate",
                        labels = scales::percent_format()) +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Overcrowded Housing", x = "Funding per House With 2 or More Occupants per Room", 
       y = "Homelessness Rate (%)") +
  theme(axis.text.x = element_text(angle = 45))

ggplotly(p12, tooltip = "text")



# Homeless rate by poverty rate scatterplot (overcrowded = color)

p13 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = poverty_rate, y = `Overall Homeless`/total_population, color = overcrowded_housing,
                  text = paste(coc_name,
                               '<br>Poverty Rate: ', round(poverty_rate*100, digits = 2), "%",
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Houses with 2 or more occupants per room: ', round(overcrowded_housing*100, digits = 2), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Overcrowded Housing (%)",
                        labels = scales::percent_format()) +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Poverty Rate", x = "Share of People Living Below Federal Poverty Line (%)", 
       y = "Homelessness Rate (%)") 

ggplotly(p13, tooltip = "text")




# Poverty rate map

p14 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = poverty_rate,
              text = paste(coc_name,
                           '<br>Share of People Below Poverty Line: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("lightblue", "maroon4"),
                       labels = scales::percent_format(),
                       name = "Poverty Rate")

ggplotly(p14, tooltip = "text") |>
  style(hoveron = "fills")




# % of homeless people race map

p15 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = `Overall Homeless - Black, African American, or African` / `Overall Homeless`,
              text = paste(coc_name,
                           '<br>Percent of people in coc who identify as Black: ', round(`Overall Homeless - Black, African American, or African`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "pink3", "purple"),
                       labels = scales::percent_format(),
                       name = "Share of homeless <br>population that <br>idenifies as Black")

ggplotly(p15, tooltip = "text") |>
  style(hoveron = "fills")


# % of homeless people race map

p15 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = `Overall Homeless - Black, African American, or African`/`Overall Homeless`,
              text = paste(coc_name,
                           '<br>Percent of people in coc who identify as Black: ', round(`Overall Homeless - Black, African American, or African`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "pink3", "purple"),
                       labels = scales::percent_format(),
                       name = "Share of homeless <br>population that <br>idenifies as Black")

ggplotly(p15, tooltip = "text") |>
  style(hoveron = "fills")





  
  
# Housing units per person (map) 
  
p16 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = housing_units,
              text = paste(coc_name,
                           '<br>Housing Units per Person: ', round(housing_units, digits = 3)))) +
  scale_fill_gradientn(colours = c("lightblue", "maroon4"),
                       name = "Housing Units per Person")

ggplotly(p16, tooltip = "text") |>
  style(hoveron = "fills")  
  


# Beds in shelter/homeless people map

p17 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = `Total Year-Round Beds (ES, TH, SH)`/`Overall Homeless`,
              text = paste(coc_name,
                           '<br>Year-round beds available in shelters: ', round(`Total Year-Round Beds (ES, TH, SH)`/`Overall Homeless`, digits = 3)))) +
  scale_fill_gradientn(colours = c("maroon4", "lightblue"),
                       name = "Beds per Homeless Individual")

ggplotly(p17, tooltip = "text") |>
  style(hoveron = "fills")  





# Year round beds and housing units per person scatterplot

p18 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = housing_units, y = `Total Year-Round Beds (ES, TH, SH)`/`Overall Homeless`, color = overcrowded_housing,
                  text = paste(coc_name,
                               '<br>Housing units per person (general population): ', round(housing_units, digits = 23),
                               '<br>Shelter beds per homeless individual: ', round((`Total Year-Round Beds (ES, TH, SH)`/`Overall Homeless`), digits = 3),
                               '<br>Houses with 2 or more occupants per room: ', round(overcrowded_housing*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Overcrowded Housing (%)",
                        labels = scales::percent_format()) +
  scale_x_log10() +
  scale_y_log10() +
  labs(title = "Housing Units per Person and Shelter Beds per Homeless Individual", x = "Housing Units (Houses/Apartments) per Person in CoC", 
       y = "Shelter Beds Available Year-Round per Homeless Person in CoC") 

ggplotly(p18, tooltip = "text")





# Chronically homeless map

p19 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill =`Overall Chronically Homeless`/`Overall Homeless`,
              text = paste(coc_name,
                           '<br>Homeless people that are chronically homeless: ', round(`Overall Chronically Homeless`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("lightblue", "maroon4"),
                       name = "Experienceing Chronic Homelessness",
                       labels = scales::percent_format())

ggplotly(p19, tooltip = "text") |>
  style(hoveron = "fills")  




# Parenting youth map

p20 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = (`Overall Homeless Parenting Youth (Under 25)` / `Overall Homeless`),
              text = paste(coc_name,
                           '<br>Percent of homeless people parenting youth: ', round(`Overall Homeless Parenting Youth (Under 25)`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("lightgrey", "gold3", "red4"),
                       labels = scales::percent_format(),
                       name = "% of homeless people <br>who are parenting youth")

ggplotly(p20, tooltip = "text") |>
  style(hoveron = "fills")




# unaccompanied youth map

p21 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = (`Overall Homeless Unaccompanied Youth (Under 25)` / `Overall Homeless`),
              text = paste(coc_name,
                           '<br>Percent of homeless people who <br>are unaccompanied youth: ', round(`Overall Homeless Unaccompanied Youth (Under 25)`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "gold3", "red4"),
                       labels = scales::percent_format(),
                       name = "% of homeless people who <br>are unaccompanied youth")
  


ggplotly(p21, tooltip = "text") |>
  style(hoveron = "fills")




# Homeless rate by $/homeless individual scatterplot (poverty rate = color), size = population

p22 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/`Overall Homeless`, y = `Overall Homeless`/total_population, color = poverty_rate, size = total_population,
                  text = paste(coc_name,
                               '<br>Funding per homeless person: $', round((funding_tot/`Overall Homeless`), digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Funding per Homeless Person", x = "Funding per Homeless Individual (USD)", y = "Homelessness Rate (%)")

ggplotly(p22, tooltip = "text")





# Rename

p23 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = `Overall Homeless Unaccompanied Youth (Under 25)`/`Overall Homeless`, y = funding_tot/`Overall Homeless`, color = poverty_rate, size = total_population,
                  text = paste(coc_name,
                               '<br>Funding per homeless person: $', round((funding_tot/`Overall Homeless`), digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(title = "Homeless Rate by Funding per Homeless Person", x = "% of Homeless who are Unacc. Youth", y = "Funding/homeless person")

ggplotly(p23, tooltip = "text")










# Rename

p24 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = capacity_housing, y = funding_tot/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per homeless person: $', round((funding_tot/`Overall Homeless`), digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::comma_format()) +
  labs(title = "Homeless Rate by Funding per Homeless Person", x = "Government officers working to secure housing per capita", 
       y = "Funding/capita")

ggplotly(p24, tooltip = "text")






# Look into Alpine, Inyo, and Mono Counties CoC

p25 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = `Overall Homeless`/total_population,
              text = paste(coc_name,
                           '<br>Homeless rate: ', round((`Overall Homeless`/total_population)*100, digits = 2), "%"))) +
  scale_fill_gradientn(colours = c("lightgrey", "orange2", "blue4"),
                       name = "Homeless Rate")

ggplotly(p25, tooltip = "text") |>
  style(hoveron = "fills")








p25 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = total_population,
              text = paste(coc_name,
                           '<br>POP: ', round((total_population), digits = 2)))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Pop")

ggplotly(p25, tooltip = "text") |>
  style(hoveron = "fills")


p25 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = funding_tot,
              text = paste(coc_name,
                           '<br>Fnd: ', round((funding_tot), digits = 2)))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Fnd")

ggplotly(p25, tooltip = "text") |>
  style(hoveron = "fills")



p26 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = age_over_64,
              text = paste(coc_name,
                           '<br>Population over 64: ', round((age_over_64*100), digits = 2), "%"))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Share of Population<br>Over 64",
                       labels = percent_format())

ggplotly(p26, tooltip = "text") |>
  style(hoveron = "fills")




p27 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = median_age,
              text = paste(coc_name,
                           '<br>Median age in CoC: ', round((median_age), digits = 2)))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Median age in CoC")

ggplotly(p27, tooltip = "text") |>
  style(hoveron = "fills")






# Weighted map of homelessness by Latinx enthnicity

p28 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = (`Overall Homeless - Hispanic/Latina/e/o`/total_population) - pct_hispanic_latinx,
              text = paste(coc_name,
                           '<br>Homeless Hispanic/Latinx: ', round(((`Overall Homeless - Hispanic/Latina/e/o`/total_population) - pct_hispanic_latinx)*100, digits = 2), "%"))) +
  scale_fill_gradientn(colours = c("lightgrey", "orange2", "blue4"),
                       name = "Homeless Hispanic/Latinx",
                       labels = percent_format())

ggplotly(p28, tooltip = "text") |>
  style(hoveron = "fills")





# Weighted map of homelessness by *Bblack, African American, or African race

p29 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = (`Overall Homeless - Black, African American, or African`/total_population) - pct_black,
              text = paste(coc_name,
                           '<br>Homeless Hispanic/Latinx: ', round(((`Overall Homeless - Black, African American, or African`/total_population) - pct_black)*100, digits = 2), "%"))) +
  scale_fill_gradientn(colours = c("lightgrey", "orange2", "blue4"),
                       name = "Homeless Hispanic/Latinx",
                       labels = percent_format())

ggplotly(p29, tooltip = "text") |>
  style(hoveron = "fills")



# homeless rate by funding per capita scatterplot

p30 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = funding_tot/total_population, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per capita: $', round((funding_tot/`Overall Homeless`), digits = 2),
                               '<br>Homelessness Rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Funding per Capita", x = "Funding per Capita (USD)", y = "Homelessness Rate (%)")

ggplotly(p30, tooltip = "text")





# 

p31 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = hud_esg/total_population, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per capita: $', round(hud_esg/total_population, digits = 2),
                               "<br>Sheltered Homeless: ", `Sheltered Total Homeless`,
                               '<br>Poverty Rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.3), breaks = c(0, .1, .2, .3),
                        labels = c("0%", "10%", "20%", "30%"),
                        name = "Poverty Rate") +
  labs(Title = "Emergency Solutions Grand Funding per Sheltered Homeless Person", x = "Funding per Sheltered Individual (USD)", y = "Homeless Rate") +
  scale_x_log10() +
  scale_y_log10(labels = scales::percent_format())

ggplotly(p31, tooltip = "text")





#

p31 <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = `Unsheltered Homeless`/total_population, y = `Sheltered Total Homeless`/total_population, color = funding_tot/total_population,
                  text = paste(coc_name,
                               '<br>% of people homeless w/o shelter: ', round(`Unsheltered Homeless`/total_population* 100, digits = 2), "%",
                               "<br>% of people homeless with shelter ", round(`Sheltered Total Homeless`/total_population *100, digits = 2), "%",
                               '<br>Funding per capita: $', funding_tot/total_population))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        name = "Funding") +
  labs(Title = "Emergency Solutions Grand Funding per Sheltered Homeless Person", x = "% of people homeless w/o shelter", y = "% of people homeless with shelter") +
  scale_x_log10(labels = scales::percent_format()) +
  scale_y_log10(labels = scales::percent_format()) +
  stat_smooth(method = "lm")

ggplotly(p31, tooltip = "text")



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



# homeless women map

p33 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill =`Overall Homeless - Woman`/`Overall Homeless`,
              text = paste(coc_name,
                           '<br>Homeless people - women: ', round(`Overall Homeless - Woman`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("lightblue", "maroon4"),
                       name = "% of Homeless People Who Are Women",
                       labels = scales::percent_format())

ggplotly(p33, tooltip = "text") |>
  style(hoveron = "fills")  



# Trying to do a binned map of some sort

hud_gis <- hud_gis |>
  mutate(hmls_female = `Overall Homeless - Woman`/ `Overall Homeless`,
         hmls_male = `Overall Homeless - Man`/`Overall Homeless`,
         hmls_trans = `Overall Homeless - Transgender`/`Overall Homeless`,
         hmls_nonbi = `Overall Homeless - Non Binary`/`Overall Homeless`,
         hmls_cultural_id = `Overall Homeless - Culturally Specific Identity`/`Overall Homeless`,
         hmls_gender_questioning = `Overall Homeless - Gender Questioning`/`Overall Homeless`,
         hmls_more_than_1_gend = `Overall Homeless - More Than One Gender`/`Overall Homeless`,
         hmls_other_gend = `Overall Homeless - Different Identity`/`Overall Homeless`)

my_cutpoints <- c(0.25, 0.30, 0.35, 0.40, 0.45, 0.49, 0.51, 0.55, 0.60, 0.65, 0.70, 0.75)
my_labels <- c(">75% Women", "70-75% Women", "65-70% Women", "60-65% Women", "55-60% Women", "51-55% Women",
               "50/50% Split", "51-55% Men", "55-60% Men", "60-65% Men", "65-70% Men", "70-75% Men", ">75% Men")
my_colors <- c("#543005", "#8c510a", "#bf812d", "#dfc27d", "#f6e8c3", "#f5f5f5", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30")

length(my_cutpoints)
length(my_labels)
length(my_colors)

hud_gis <- hud_gis |>
  mutate(gender_scale = cut(hmls_male, breaks = my_cutpoints, labels = my_labels))





p34 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = gender_scale,
              text = paste(coc_name,
                           '<br>Homeless Population: ', round(hmls_male*100, digits = 3), "% Men",
                           '<br>                     ', round(hmls_female*100, digits = 3), "% Women"))) +
  scale_fill_manual("", values = my_colors)

ggplotly(p34, tooltip = "text") |>
  style(hoveron = "fills")  






p35 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = hmls_male,
              text = paste(coc_name,
                           '<br>Homeless Population: ', round(hmls_male*100, digits = 3), "% Men",
                           '<br>                     ', round(hmls_female*100, digits = 3), "% Women"))) +
  scale_fill_gradientn(name = "",
                    colors = c("#543005", "#bf812d", "#f6e8c3", "#f5f5f5", "#c7eae5", "#80cdc1", "#35978f", "#01665e", "#003c30"),
                    limits = c(0.35,0.75),
  )

ggplotly(p35, tooltip = "text") |>
  style(hoveron = "fills") 




# homeless trans map

p36 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill =`Overall Homeless - Transgender`/`Overall Homeless`,
              text = paste(coc_name,
                           '<br>Homeless people - transgender: ', round(`Overall Homeless - Transgender`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("#ffe0d0", "#11408a"),
                       name = "% of Homeless People Who Are Transgender",
                       labels = scales::percent_format(),
                       limits = c(0,0.02))

ggplotly(p36, tooltip = "text") |>
  style(hoveron = "fills")  





p37 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = (funding_tot/(total_population)),
              text = paste(coc_name,
                           '<br>Funding per person: $', round(funding_tot/(poverty_rate*total_population), digits = 2)))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Funding per Person")


ggplotly(p37, tooltip = "text") |>
  style(hoveron = "fills")



"#ff704c", "#115b8a"

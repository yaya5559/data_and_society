library(tidyverse)
library(plotly)
library(sf)
library(rmapshaper)
library(scales)
library(weights)
library(htmlwidgets)

### code for background
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



### Slide 1 
# visual that we will send via teams - probably a picture



### Slide 2
# final data frame
# side by side with names of different data sets
    # - County indicators
    # - County funding
    # - County to CoC
    # - CoC Homelessness PIT
    # - CoC Shelter HIC
    # - American Community Survey
    # - GIS Shapefiles for CoCs
# Below list is diagram of how we linked them(?)



### Slide 3 
# able to switch between all three maps

# homeless rate map
p25 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = `Overall Homeless`/total_population,
              text = paste(coc_name,
                           '<br>Homeless rate: ', round((`Overall Homeless`/total_population)*100, digits = 2), "%"))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       name = "Homeless Rate",
                       labels = scales::percent_format()) +
  labs(title = "Homelessness in the United States - Continuum of Care View (2024)")

ggplotly(p25, tooltip = "text") |>
  style(hoveron = "fills")


# funding per person map (same scale as $/homeless map)
p7 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = fnd_per_person,
              text = paste(coc_name,
                           '<br>Funding per person: $', round(fnd_per_person), digits = 2))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       labels = scales::comma_format(prefix = "$"),
                       name = "Funding per Person<br>(USD)") +
  labs(title = "Federal Funding for Housing per Person - Continuum of Care View (2023)")

ggplotly(p7, tooltip = "text") |>
  style(hoveron = "fills")


# scatterplot of funding per person and homeless rate
p3a <- hud_gis |>
  ggplot() +
  geom_jitter(aes(x = fnd_per_person, y = `Overall Homeless`/total_population, color = poverty_rate,
                  text = paste(coc_name,
                               '<br>Funding per person: $', round(fnd_per_person, digits = 2),
                               '<br>Homelessness rate: ', round((`Overall Homeless`/total_population)*100, digits = 3), '%',
                               '<br>Poverty rate: ', round(poverty_rate*100, digits = 3), "%"))) +
  scale_color_gradient2(low = "lightgrey", mid = "orange2", high = "blue4",
                        limits = c(0, 0.5), breaks = c(0, .1, .2, .3, .4),
                        labels = c("0%", "10%", "20%", "30%", "40%"),
                        name = "Poverty Rate") +
  scale_x_log10(labels = scales::comma_format(prefix = "$")) +
  scale_y_log10(labels = scales::percent_format()) +
  labs(title = "Homeless Rate by Funding per Person - Continuum of Care View", x = "Funding per Person (USD - 2023)", y = "Homelessness Rate (% - 2024)")

ggplotly(p3a, tooltip = "text")





### Slide 4

# drop down for race map, all categories below (be sure to change text so it says "percent of people who are [insert race here]")
# need to change in fill, text string, text round(), and color bar name
p15 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = `Overall Homeless - Black, African American, or African` / `Overall Homeless`,
              text = paste(coc_name,
                           '<br>Percent of people in coc who identify as Black: ', round(`Overall Homeless - Black, African American, or African`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("#fcf1fc", "pink1", "#e778a1", "#ae017e", "#400060"),
                       labels = scales::percent_format(),
                       name = "Share of homeless <br>population that <br>idenifies as Black") +
  labs(title = "Homelessness by Race/Ethnicity - CoC View (2024)")

ggplotly(p15, tooltip = "text") |>
  style(hoveron = "fills")

`Overall Homeless - Hispanic/Latina/e/o`
`Overall Homeless - American Indian, Alaska Native, or Indigenous` 
`Overall Homeless - Asian or Asian American` 
`Overall Homeless - Black, African American, or African` 
`Overall Homeless - Middle Eastern or North African` 
`Overall Homeless - White` 
`Overall Homeless - Native Hawaiian or Other Pacific Islander`
`Overall Homeless - Multi-Racial`



# median gap bar plot
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


### Slide 5
# Our variables put into the model (I will send you a picture)
# process
    # - choose which variables we want
    # - check for/ remove NAs,
    # - make sure each variable is numeric
    # - standardize each variable
    # - run through pca



### slide 6
# principal component maps (dropdown) (be sure to change which variable in both text string and text round().)

y1 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc1,
              text = paste(coc_name,
                           '<br>Principal Component 1 score: ', round(pc1, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f03b20", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "PC 1 score")

ggplotly(y1, tooltip = "text") |>
  style(hoveron = "fills") 


y2 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc2,
              text = paste(coc_name,
                           '<br>Principal Component 2 score: ', round(pc2, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f03b20", "#fd8d3c", "#feb24c", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "PC 2 score",
                       # limits = c(-5,5)
  )

ggplotly(y2, tooltip = "text") |>
  style(hoveron = "fills") 


y3 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc3,
              text = paste(coc_name,
                           '<br>Principal Component 3 score: ', round(pc3, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#ffffcc", "#feb24c", "#fd8d3c", "#f03b20"),
                       name = "PC 3 score")

ggplotly(y3, tooltip = "text") |>
  style(hoveron = "fills")


y4 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc4,
              text = paste(coc_name,
                           '<br>Principal Component 4 score: ', round(pc4, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#7fcdbb", "#a1dab4", "#ffffcc", "#feb24c", "#f03b20"),
                       name = "PC 4 score")

ggplotly(y4, tooltip = "text") |>
  style(hoveron = "fills")


y5 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc5,
              text = paste(coc_name,
                           '<br>Principal Component 5 score: ', round(pc5, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f3330c", "#fd8d3c", "#fecc5c", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc 5 score")

ggplotly(y5, tooltip = "text") |>
  style(hoveron = "fills")



y6 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc6,
              text = paste(coc_name,
                           '<br>Principal Component 6 score: ', round(pc6, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#ffffcc", "#fecc5c", "#fd8d3c", "#f3330c"),
                       name = "PC 6 score")

ggplotly(y6, tooltip = "text") |>
  style(hoveron = "fills")






### Slide 7
# Models (dropdown, I will send you pictures of both models), 
# also put side by side with principal component maps again.







### Slide 8

# funding per person map (same scale as $/homeless map)
p7 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = fnd_per_person,
              text = paste(coc_name,
                           '<br>Funding per person: $', round(fnd_per_person), digits = 2))) +
  scale_fill_gradientn(transform = "log10",
                       colours = c("lightgrey", "orange2", "blue4"),
                       labels = scales::comma_format(prefix = "$"),
                       name = "Funding per Person<br>(USD)") +
  labs(title = "Federal Funding for Housing per Person - Continuum of Care View (2023)")

ggplotly(p7, tooltip = "text") |>
  style(hoveron = "fills")



# Sheltered homeless map

p54 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = `Sheltered Total Homeless`/`Overall Homeless`,
              text = paste(coc_name,
                           '<br>Share of Homeless People Sleeping in Shelters', round(`Sheltered Total Homeless`/`Overall Homeless`*100, digits = 2), "%",
                           "<br>Homeless rate: ", round(`Overall Homeless`/total_population*100, digits = 2), "%",
                           "<br>Funding per Person: $", round(fnd_per_person, digits = 2), "<br>",
                           `CoC Category`))) +
  scale_fill_gradientn(colours = c("#f3f3f3", "orange2", "blue4"),
                       name = "Share of Homeless<br>People Sleeping in<br>Shelters",
                       labels = scales::percent_format()) +
  labs(title = "Homeless People Sleeping in Shelters - CoC View (2024)")

ggplotly(p54, tooltip = "text") |>
  style(hoveron = "fills")



# Chronically homeless map 

p19 <- hud_gis |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill =`Overall Chronically Homeless`/`Overall Homeless`,
              text = paste(coc_name,
                           '<br>Homeless people that are chronically homeless: ', round(`Overall Chronically Homeless`/`Overall Homeless`*100, digits = 3), "%"))) +
  scale_fill_gradientn(colours = c("#f3f3f3", "orange2", "blue4"),
                       name = "Share of Homeless<br>People Who Are<br>Chronically Homeless",
                       labels = scales::percent_format()) +
  labs(title = "Chronic Homelessness - CoC View (2024)")

ggplotly(p19, tooltip = "text") |>
  style(hoveron = "fills")  









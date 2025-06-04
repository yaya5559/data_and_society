#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Load libraries 
 
library(shiny)
library(tidyverse)
library(sf)
library(plotly)
library(janitor)

# === Load and clean data ===
ppp <- read_csv("ppp.csv") %>%
  mutate(race_better = factor(race_better,
                              levels = c("Native Hawaiian/Other Pacific Islander", 
                                         "Black, African American, or African", 
                                         "American Indian/Alaska Native", 
                                         "White", 
                                         "Multi-Racial", 
                                         "Asian")))

coc_data <- read_csv("coc_data (2).csv")
too_many_cocs <- read_csv("too_many_cocs.csv")
county_data_take2 <- read_csv("county_data_take2.csv") |> janitor::clean_names()

# Simplified GIS (preprocessed externally)
GIS <- readRDS("gis_data.rds")

hud_gis <- GIS %>%
  right_join(coc_data, by = c("COCNUM" = "coc_num")) %>%
  st_cast("MULTIPOLYGON")

vvv <- GIS %>%
  left_join(too_many_cocs, by = c("COCNUM" = "coc_num")) %>%
  left_join(coc_data, by = c("COCNUM" = "coc_num")) %>%
  st_cast("MULTIPOLYGON")

# === Create racial disparity data ===
ccc <- county_data_take2 |>
  group_by(coc_num) |>
  summarise(
    hmls_AIAN = mean(overall_homeless_american_indian_alaska_native_or_indigenous, na.rm = TRUE) / mean(overall_homeless, na.rm = TRUE),
    hmls_asian = mean(overall_homeless_asian_or_asian_american, na.rm = TRUE) / mean(overall_homeless, na.rm = TRUE),
    hmls_black = mean(overall_homeless_black_african_american_or_african, na.rm = TRUE) / mean(overall_homeless, na.rm = TRUE),
    hmls_multi = mean(overall_homeless_multi_racial, na.rm = TRUE) / mean(overall_homeless, na.rm = TRUE),
    hmls_NHPI = mean(overall_homeless_native_hawaiian_or_other_pacific_islander, na.rm = TRUE) / mean(overall_homeless, na.rm = TRUE),
    hmls_white = mean(overall_homeless_white + overall_homeless_middle_eastern_or_north_african, na.rm = TRUE) / mean(overall_homeless, na.rm = TRUE),
    
    pop_AIAN = sum(amer_indian_ak_native, na.rm = TRUE) / sum(popest - other_race, na.rm = TRUE),
    pop_asian = sum(asian, na.rm = TRUE) / sum(popest - other_race, na.rm = TRUE),
    pop_black = sum(black, na.rm = TRUE) / sum(popest - other_race, na.rm = TRUE),
    pop_multi = sum(multi_racial, na.rm = TRUE) / sum(popest - other_race, na.rm = TRUE),
    pop_NHPI = sum(native_hi_other_pi, na.rm = TRUE) / sum(popest - other_race, na.rm = TRUE),
    pop_white = sum(white, na.rm = TRUE) / sum(popest - other_race, na.rm = TRUE)
  )

# === UI ===
ui <- fluidPage(
  titlePanel("Racial Disparities in Homelessness"),
  
  tabsetPanel(
    tabPanel("Racial Density Plots",
             sidebarLayout(
               sidebarPanel(
                 selectInput("race_density", "Select a Racial Group:", 
                             choices = c("Black", "Asian", "White (incl. MENA)", 
                                         "American Indian/Alaska Native", 
                                         "Multi-Racial", 
                                         "Native Hawaiian or Other Pacific Islander"))
               ),
               mainPanel(
                 plotOutput("density_plot")
               )
             )
    ),
    tabPanel("Racial Bar Plot",
             plotlyOutput("racial_bar_plot")
    ),
    tabPanel("Racial Maps",
             sidebarLayout(
               sidebarPanel(
                 selectInput("map_race", "Select a racial group for map:", 
                             choices = c(
                               "Overall Homeless - Black, African American, or African",
                               "Overall Homeless - Hispanic/Latina/e/o",
                               "Overall Homeless - American Indian, Alaska Native, or Indigenous",
                               "Overall Homeless - Asian or Asian American",
                               "Overall Homeless - Middle Eastern or North African",
                               "Overall Homeless - White",
                               "Overall Homeless - Native Hawaiian or Other Pacific Islander",
                               "Overall Homeless - Multi-Racial"
                             )
                 )
               ),
               mainPanel(
                 plotlyOutput("racial_map_plot")
               )
             )
    )
  )
)

# === Server ===
server <- function(input, output) {
  
  output$racial_map_plot <- renderPlotly({
    selected_col <- input$map_race
    
    race_labels <- c(
      "Overall Homeless - Black, African American, or African" = "Black, African American, or African",
      "Overall Homeless - Hispanic/Latina/e/o" = "Hispanic/Latina/e/o",
      "Overall Homeless - American Indian, Alaska Native, or Indigenous" = "American Indian/Alaska Native",
      "Overall Homeless - Asian or Asian American" = "Asian",
      "Overall Homeless - Middle Eastern or North African" = "Middle Eastern or North African",
      "Overall Homeless - White" = "White",
      "Overall Homeless - Native Hawaiian or Other Pacific Islander" = "Native Hawaiian/Other Pacific Islander",
      "Overall Homeless - Multi-Racial" = "Multi-Racial"
    )
    race_label <- race_labels[[selected_col]]
    
    plot_data <- hud_gis %>%
      filter(!ST_1 %in% c("AK", "HI", "PR", "GU", "VI", "MP")) %>%
      mutate(race_share = .data[[selected_col]] / `Overall Homeless`)
    
    p <- ggplot(plot_data) +
      geom_sf(aes(fill = race_share,
                  text = paste0(coc_name,
                                "<br>Percent of people in CoC who identify as ", race_label, ": ",
                                round(race_share * 100, 3), "%"))) +
      scale_fill_gradientn(
        colours = c("#fcf1fc", "pink1", "#e778a1", "#ae017e", "#400060"),
        labels = scales::percent_format(),
        name = paste0("Share of homeless<br>population that<br>identifies as ", race_label)
      ) +
      labs(title = paste("Homelessness by Race/Ethnicity (2024) —", race_label)) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> style(hoveron = "fills")
  })
}

shinyApp(ui, server)

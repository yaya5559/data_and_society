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
library(rmapshaper)

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

GIS <- st_read("CoC_GIS_National_Boundary.gdb")
stupid_simple_GIS <- ms_simplify(GIS, keep = 0.001, keep_shapes = FALSE)

hud_gis <- stupid_simple_GIS %>%
  right_join(coc_data, by = c("COCNUM" = "coc_num")) %>%
  st_cast("MULTIPOLYGON") %>%
  filter(!st_is_empty(.) & !is.na(st_dimension(.)))

vvv <- stupid_simple_GIS %>%
  left_join(too_many_cocs, by = c("COCNUM" = "coc_num")) %>%
  left_join(coc_data, by = c("COCNUM" = "coc_num")) %>%
  st_cast("MULTIPOLYGON") %>%
  filter(!st_is_empty(.) & !is.na(st_dimension(.)))

# === Create racial disparity data (ccc) ===
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
ui <- navbarPage("Capstone Project",
                 
                 tabPanel("Racial Density Plots",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_race", "Select Racial Group:",
                                          choices = c("White", "Black", "Asian", "American Indian/Alaska Native", 
                                                      "Native Hawaiian/Other Pacific Islander", "Multi-Racial"))
                            ),
                            mainPanel(
                              plotlyOutput("race_density_plot")
                            )
                          )
                 ),
                 
                 tabPanel("Racial Maps",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("map_race", "Select Racial Group:",
                                          choices = c(
                                            "Black, African American, or African" = "Overall Homeless - Black, African American, or African",
                                            "Hispanic/Latina/e/o" = "Overall Homeless - Hispanic/Latina/e/o",
                                            "American Indian/Alaska Native" = "Overall Homeless - American Indian, Alaska Native, or Indigenous",
                                            "Asian" = "Overall Homeless - Asian or Asian American",
                                            "Middle Eastern or North African" = "Overall Homeless - Middle Eastern or North African",
                                            "White" = "Overall Homeless - White",
                                            "Native Hawaiian/Other Pacific Islander" = "Overall Homeless - Native Hawaiian or Other Pacific Islander",
                                            "Multi-Racial" = "Overall Homeless - Multi-Racial"
                                          )
                              )
                            ),
                            mainPanel(
                              plotlyOutput("racial_map_plot")
                            )
                          )
                 ),
                 
                 tabPanel("Racial Bar Plot",
                          fluidPage(
                            h3("Racial Disparities in Homelessness"),
                            plotlyOutput("racial_bar_plot")
                          )
                 ),
                 
                 tabPanel("PCA Maps",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_pc", "Select Principal Component:",
                                          choices = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"))
                            ),
                            mainPanel(
                              plotlyOutput("pca_map_plot")
                            )
                          )
                 )
)

# === SERVER ===
server <- function(input, output) {
  
  output$race_density_plot <- renderPlotly({
    race <- input$selected_race
    
    plot_config <- list(
      "Black" = list(hmls = ccc$hmls_black, pop = ccc$pop_black, color = "blue"),
      "White" = list(hmls = ccc$hmls_white, pop = ccc$pop_white, color = "gray60"),
      "Asian" = list(hmls = ccc$hmls_asian, pop = ccc$pop_asian, color = "turquoise4"),
      "Multi-Racial" = list(hmls = ccc$hmls_multi, pop = ccc$pop_multi, color = "darkgreen"),
      "Native Hawaiian/Other Pacific Islander" = list(hmls = ccc$hmls_NHPI, pop = ccc$pop_NHPI, color = "skyblue"),
      "American Indian/Alaska Native" = list(hmls = ccc$hmls_AIAN, pop = ccc$pop_AIAN, color = "firebrick")
    )
    
    config <- plot_config[[race]]
    
    p <- ggplot(ccc) +
      geom_density(aes(x = config$hmls, fill = "Homeless Share"), alpha = 0.4) +
      geom_density(aes(x = config$pop, fill = "Population Share"), alpha = 0.4) +
      scale_x_continuous(labels = scales::percent_format()) +
      scale_fill_manual(
        name = "Group",
        values = c("Homeless Share" = config$color, "Population Share" = "black")
      ) +
      labs(title = race, x = "Share", y = "Density") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$racial_map_plot <- renderPlotly({
    selected_col <- input$map_race
    race_label <- names(which(sapply(input$map_race, function(x) {
      c(
        "Black, African American, or African" = "Overall Homeless - Black, African American, or African",
        "Hispanic/Latina/e/o" = "Overall Homeless - Hispanic/Latina/e/o",
        "American Indian/Alaska Native" = "Overall Homeless - American Indian, Alaska Native, or Indigenous",
        "Asian" = "Overall Homeless - Asian or Asian American",
        "Middle Eastern or North African" = "Overall Homeless - Middle Eastern or North African",
        "White" = "Overall Homeless - White",
        "Native Hawaiian/Other Pacific Islander" = "Overall Homeless - Native Hawaiian or Other Pacific Islander",
        "Multi-Racial" = "Overall Homeless - Multi-Racial"
      )[x] == selected_col
    })))
    
    plot_data <- hud_gis |> filter(!ST_1 %in% c("AK", "HI", "PR", "GU", "VI", "MP")) |> 
      mutate(race_share = .data[[selected_col]] / `Overall Homeless`)
    
    p <- ggplot(plot_data) +
      geom_sf(aes(fill = race_share,
                  text = paste0(coc_name,
                                "<br>Percent of people in CoC who identify as ", race_label, ": ",
                                round(race_share * 100, 3), "%"))) +
      scale_fill_gradientn(
        colours = c("#fcf1fc", "pink1", "#e778a1", "#ae017e", "#400060"),
        labels = scales::percent_format(),
        name = paste("Share of homeless\npopulation that\nidentifies as", race_label)
      ) +
      labs(title = paste("Homelessness by Race/Ethnicity -", race_label, "(2024)")) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> style(hoveron = "fills")
  })
  
  output$racial_bar_plot <- renderPlotly({
    p <- ppp |>
      ggplot() + 
      geom_col(aes(x = race_better, y = median_scale_of_diff, fill = race,
                   text = paste(race_better,
                                "<br>Median racial disparity gap: ", round(median_scale_of_diff, digits = 3)))) +
      theme(axis.text.x = element_text(vjust = .5)) +
      scale_x_discrete(labels = c(
        "Native Hawaiian/\nOther Pacific\nIslander",
        "Black, African\nAmerican, or\nAfrican",
        "American Indian/\nAlaska Native",
        "White",
        "Multi-Racial",
        "Asian"
      ), name = "Race Group") +
      scale_y_continuous(name = "Median Outcome Gap") +
      labs(title = "Racial Disparities In Homelessness",
           subtitle = "Gap is how many times more likely a given group is to be homeless than average homelessness rate for whole population") +
      scale_fill_discrete(guide = FALSE)
    
    ggplotly(p, tooltip = "text")
  })
  
  output$pca_map_plot <- renderPlotly({
    pc_col <- tolower(input$selected_pc)
    
    pc_palette <- list(
      pc1 = c("#f03b20", "#ffffcc", "#41b6c4", "#225ea8"),
      pc2 = c("#f03b20", "#fd8d3c", "#feb24c", "#ffffcc", "#41b6c4", "#225ea8"),
      pc3 = c("#225ea8", "#41b6c4", "#ffffcc", "#feb24c", "#fd8d3c", "#f03b20"),
      pc4 = c("#225ea8", "#41b6c4", "#7fcdbb", "#a1dab4", "#ffffcc", "#feb24c", "#f03b20"),
      pc5 = c("#f3330c", "#fd8d3c", "#fecc5c", "#ffffcc", "#41b6c4", "#225ea8"),
      pc6 = c("#225ea8", "#41b6c4", "#ffffcc", "#fecc5c", "#fd8d3c", "#f3330c")
    )
    
    plot_data <- vvv |> filter(!ST_1 %in% c("AK", "HI", "PR", "GU", "VI", "MP")) |>
      mutate(pc_value = .data[[pc_col]])
    
    p <- ggplot(plot_data) +
      geom_sf(aes(fill = pc_value,
                  text = paste(coc_name,
                               "<br>", input$selected_pc, "score: ",
                               round(pc_value, 3)))) +
      scale_fill_gradientn(
        colours = pc_palette[[pc_col]],
        name = paste(input$selected_pc, "score")
      ) +
      labs(title = paste("Principal Component Map -", input$selected_pc)) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text") |> style(hoveron = "fills")
  })
}

# === Run App ===
shinyApp(ui, server)

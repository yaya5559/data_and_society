#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(plotly)
library(sf)
library(viridis)

# Define racial group options
racial_vars <- c(
  "Overall Homeless - Non-Hispanic/Latina/e/o",
  "Overall Homeless - Hispanic/Latina/e/o",
  "Overall Homeless - American Indian, Alaska Native, or Indigenous",
  "Overall Homeless - Asian or Asian American",
  "Overall Homeless - Black, African American, or African",
  "Overall Homeless - Middle Eastern or North African",
  "Overall Homeless - White",
  "Overall Homeless - Native Hawaiian or Other Pacific Islander",
  "Overall Homeless - Multi-Racial"
)

ui <- navbarPage("CoC Homelessness Visualizations",
                 
                 tabPanel("Funding per Capita vs Homeless Rate",
                          plotlyOutput("plot1")
                 ),
                 
                 tabPanel("Funding per Homeless vs Homeless Rate",
                          plotlyOutput("plot2")
                 ),
                 
                 tabPanel("Total Population vs Homeless Rate",
                          plotlyOutput("plot3")
                 ),
                 
                 tabPanel("Map by Racial Group",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("selected_group", "Select Racial Group:", choices = racial_vars)
                            ),
                            mainPanel(
                              plotlyOutput("race_map")
                            )
                          )
                 )
)

server <- function(input, output) {
  
  # Plot 1: Funding per capita
  output$plot1 <- renderPlotly({
    coc_data_1 <- coc_data |> 
      mutate(homeless_rate = `Overall Homeless` / total_population,
             funding_per_capita = funding_tot / total_population)
    
    p <- ggplot(coc_data_1) +
      geom_jitter(aes(
        x = funding_per_capita,
        y = homeless_rate,
        color = poverty_rate,
        text = paste0("CoC: ", coc_name, "<br>",
                      "Poverty Rate: ", scales::percent(poverty_rate, accuracy = 0.1))
      )) +
      scale_x_log10() +
      scale_y_log10(
        breaks = c(0.1, 0.01, 0.001, 0.0001),
        labels = scales::percent_format(accuracy = 0.1)
      ) +
      scale_color_continuous(type = "viridis", labels = scales::percent_format(accuracy = 0.1)) +
      labs(
        title = "Federal funding per capita vs. Homelessness for each CoC",
        x = "Funding per capita",
        y = "Rate of homelessness",
        color = "Poverty rate"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 2: Funding per homeless
  output$plot2 <- renderPlotly({
    coc_data_1 <- coc_data |> 
      mutate(homeless_rate = `Overall Homeless` / total_population,
             funding_per_homeless = funding_tot / `Overall Homeless`)
    
    p <- ggplot(coc_data_1) +
      geom_jitter(aes(
        x = funding_per_homeless,
        y = homeless_rate,
        color = poverty_rate,
        text = paste0("CoC: ", coc_name, "<br>",
                      "Poverty Rate: ", scales::percent(poverty_rate, accuracy = 0.1))
      )) +
      scale_x_log10(breaks = c(100, 1000, 10000, 100000, 1000000),
                    labels = scales::label_dollar()) +
      scale_y_log10(
        breaks = c(0.1, 0.01, 0.001, 0.0001),
        labels = scales::percent_format(accuracy = 0.1)
      ) +
      scale_color_continuous(type = "viridis", labels = scales::percent_format(accuracy = 0.1)) +
      labs(
        title = "Federal funding per homeless individual vs. Homelessness for each CoC",
        x = "Funding per homeless individual",
        y = "Rate of homelessness",
        color = "Poverty rate"
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  # Plot 3: Population vs Homeless Rate
  output$plot3 <- renderPlotly({
    coc_data_1 <- coc_data |> 
      mutate(homeless_rate = `Overall Homeless` / total_population,
             funding_per_homeless = funding_tot / `Overall Homeless`)
    
    p <- ggplot(coc_data_1) +
      geom_jitter(aes(
        x = funding_per_homeless,
        y = homeless_rate,
        color = log10(total_population),
        text = paste0("CoC: ", coc_name, "<br>",
                      "Funding: $", round(funding_tot), "<br>",
                      "Homeless: ", `Overall Homeless`)
      )) +
      scale_x_log10(breaks = c(100, 1000, 10000, 100000, 1000000),
                    labels = scales::label_dollar()) +
      scale_y_log10(
        breaks = c(0.1, 0.01, 0.001, 0.0001),
        labels = scales::percent_format(accuracy = 0.1)
      ) +
      scale_color_continuous(
        type = "viridis",
        breaks = log10(c(1e5, 1e6, 1e7)),
        labels = c("100K", "1M", "10M")
      ) +
      labs(
        title = "Federal funding per homeless individual vs. Homelessness for each CoC",
        x = "Funding per homeless individual",
        y = "Rate of homelessness",
        color = "Total population"
      ) +
      theme(plot.title = element_text(size = 11))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Map tab: Racial group selection
  output$race_map <- renderPlotly({
    req(input$selected_group)
    
    filtered <- gis_coc |> 
      filter(!ST_1 %in% c("AK", "HI", "PR", "GU", "VI", "MP")) |>
      mutate(percent_group = .data[[input$selected_group]] / `Overall Homeless`)
    
    p <- ggplot(filtered) +
      geom_sf(aes(fill = percent_group,
                  text = paste0(
                    coc_name, "<br>",
                    "Percent: ", round(percent_group * 100, 2), "%"
                  ))) +
      scale_fill_gradientn(
        colours = c("lightgrey", "pink3", "purple3"),
        labels = scales::percent_format(accuracy = 1),
        name = "% of population"
      ) +
      labs(title = "Percent of Homeless Population by Selected Racial Group")
    
    ggplotly(p, tooltip = "text") |> style(hoveron = "fills")
  })
}


# Run the application 
shinyApp(ui = ui, server = server)

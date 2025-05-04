#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
coc_shapes <- st_read("C:/Users/yahya/OneDrive/Bureau/data_and_society/demographic/Continuum_of_Care_Grantee_Areas.shp")

coc_shapes <- coc_shapes |> rename(coc_num = COCNUM)


coc_shapes <- coc_shapes |> st_transform()

coc_shapes_map <- coc_shapes |>select(geometry, coc_num, COCNAME,ES_CN_HWAC,  RRH_CN_VET)
coc_data <- read.csv("C:/Users/yahya/OneDrive/Bureau/data_and_society/demographic/coc_data.csv")

coc_merged <- left_join(coc_shapes, coc_data, by = "coc_num")
library(shiny)

# First, make a cop

# Then rename only the columns you provided mappings for:
names(coc_merged)[c(
  3, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73,
  74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 
  89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103
)] <- c(
  "coc_number",
  "coc_name",
  "coc_category",
  "count_types",
  "fips_state",
  "state_full",
  "state_abbreviation",
  "overall_homeless",
  "homeless_under_18",
  "homeless_18_24",
  "homeless_25_34",
  "homeless_35_44",
  "homeless_45_54",
  "homeless_55_64",
  "homeless_over_64",
  "homeless_women",
  "homeless_men",
  "homeless_transgender",
  "homeless_non_binary",
  "homeless_multiple_genders",
  "homeless_gender_questioning",
  "homeless_culturally_specific",
  "homeless_different_identity",
  "homeless_non_hispanic_latino",
  "homeless_hispanic_latino",
  "homeless_american_indian_alaska_native",
  "homeless_asian",
  "homeless_black_african_american",
  "homeless_middle_eastern_north_african",
  "homeless_white",
  "homeless_native_hawaiian_pacific_islander",
  "homeless_multiracial",
  "homeless_veterans",
  "chronically_homeless",
  "homeless_families",
  "homeless_individuals",
  "unaccompanied_youth_under_25",
  "parenting_youth_under_25",
  "sheltered_total_homeless",
  "sheltered_emergency_shelter",
  "sheltered_transitional_housing",
  "sheltered_safe_haven",
  "unsheltered_homeless",
  "year_round_beds_total",
  "year_round_beds_emergency"
)







names(coc_data) <- c(
  "id",
  "coc_number",
  "coc_name",
  "coc_category",
  "count_types",
  "fips_state",
  "state_full",
  "state_abbreviation",
  "overall_homeless",
  "homeless_under_18",
  "homeless_18_24",
  "homeless_25_34",
  "homeless_35_44",
  "homeless_45_54",
  "homeless_55_64",
  "homeless_over_64",
  "homeless_women",
  "homeless_men",
  "homeless_transgender",
  "homeless_non_binary",
  "homeless_multiple_genders",
  "homeless_gender_questioning",
  "homeless_culturally_specific",
  "homeless_different_identity",
  "homeless_non_hispanic_latino",
  "homeless_hispanic_latino",
  "homeless_american_indian_alaska_native",
  "homeless_asian",
  "homeless_black_african_american",
  "homeless_middle_eastern_north_african",
  "homeless_white",
  "homeless_native_hawaiian_pacific_islander",
  "homeless_multiracial",
  "homeless_veterans",
  "chronically_homeless",
  "homeless_families",
  "homeless_individuals",
  "unaccompanied_youth_under_25",
  "parenting_youth_under_25",
  "sheltered_total_homeless",
  "sheltered_emergency_shelter",
  "sheltered_transitional_housing",
  "sheltered_safe_haven",
  "unsheltered_homeless",
  "year_round_beds_total",
  "year_round_beds_emergency_shelter",
  "year_round_beds_transitional_housing",
  "year_round_beds_safe_haven",
  "year_round_beds_other_permanent_housing",
  "year_round_beds_permanent_supportive_housing",
  "year_round_beds_rapid_rehousing",
  "total_population",
  "water_area_sq_meters",
  "land_area_sq_meters",
  "disadvantaged_county",
  "persistent_poverty_county",
  "percent_rural",
  "percent_urban",
  "percent_people_of_color",
  "poverty_rate",
  "population_density",
  "median_household_income",
  "employment_access_index",
  "housing_cost_burden",
  "overcrowded_housing",
  "vacancy_rate",
  "incomplete_plumbing",
  "incomplete_kitchen",
  "total_housing_units",
  "building_permits",
  "hud_supportive_housing_202",
  "hud_housing_choice_vouchers",
  "hud_project_based_section_8",
  "hud_public_housing",
  "housing_capacity",
  "population_under_18",
  "population_over_64",
  "continuum_of_care",
  "cdbg_entitlement",
  "elderly_population",
  "grrp_completion",
  "grrp_elements",
  "grrp_leading",
  "housing_choice_vouchers_funding",
  "home_investment_partnerships",
  "hud_disability_programs",
  "hud_esg_funding",
  "hud_hopwa_programs",
  "public_housing_funding",
  "public_housing_capacity",
  "section_8_project_based_funding",
  "total_funding"
)

coc_data <- coc_data %>% filter(total_funding > 0, homeless_under_18> 0)
coc_data <- coc_data %>%filter(total_funding > 0, homeless_18_24 > 0)
coc_data <- coc_data %>% filter(total_funding > 0, homeless_25_34> 0)
coc_data <- coc_data %>%filter(total_funding > 0, homeless_35_44 > 0)
coc_data <- coc_data %>% filter(total_funding > 0, homeless_45_54> 0)
coc_data <- coc_data %>%filter(total_funding > 0, homeless_55_64 > 0)
coc_data <- coc_data %>% filter(total_funding > 0, homeless_over_64> 0)
coc_data <- coc_data %>% filter(total_funding > 0, homeless_veterans> 0)
coc_data <- coc_data %>% filter(total_funding > 0, homeless_families> 0)
coc_data <- coc_data %>% filter(total_funding > 0, homeless_transgender> 0)
coc_data <- coc_data %>% filter(total_funding > 0, homeless_hispanic_latino> 0)
coc_data <- coc_data %>% filter(total_funding > 0, homeless_middle_eastern_north_african> 0)



axis_options <- c(
  "Total Funding" = "",
  "Homeless Under 18" = "homeless_under_18",
  "Homeless 18 to 24" = "homeless_18_24",
  "Homeless 25 to 34" = "homeless_25_34",
  "Homeless 35 to 44" = "homeless_35_44",
  "Homeless 45 to 54" = "homeless_45_54",
  "Homeless 55 to 64" = "homeless_55_64",
  "Homeless Over 64" = "homeless_over_64",
  "Homeless Veterans" = "homeless_veterans",
  "Homeless Families" = "homeless_families",
  "Homeless Transgender" = "homeless_transgender",
  "Homeless Hispanic/Latino" = "homeless_hispanic_latino",
  "Homeless MENA (Middle Eastern or North African)" = "homeless_middle_eastern_north_african"
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Impact of Federal Housing Funding on Youth Homelessness"),

    # Sidebar with a slider input for number of bins 
    tabsetPanel(
      tabPanel("Scatter Plot",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("yvar", "Select Y-axis Variable:", choices = axis_options, selected = "homeless_under_18")
                 ),
                 mainPanel(
                   plotlyOutput("scatterPlot")
                 )
               )
        ),
      
      tabPanel("Map",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("fillvar", "Select Fill Variable for Map:", choices = axis_options, selected = "homeless_under_18")
                 ),
                 mainPanel(
                   plotlyOutput("mapPlot")
                 )
               )
      )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$scatterPlot <- renderPlotly({
    p <- ggplot(coc_data, aes(
      x = total_funding,
      y = .data[[input$yvar]],
      text = coc_name
    )) +
      geom_point(aes(color = poverty_rate, size = total_population)) +
      scale_y_log10() +
      scale_x_log10() +
      geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "red") +
      scale_color_gradient(low = "lightblue", high = "darkblue") +
      labs(
        title = "Impact of Federal Housing Funding on Vulnerable Groups",
        subtitle = "Colored by Poverty Rate",
        x = "Total Funding",
        y = input$yvar,
        color = "Poverty Rate",
        size = "Total Population"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("text", "x", "y", "color", "size"))
  })
  
  output$mapPlot <- renderPlotly({
    p <- ggplot(coc_merged) +
      geom_sf(aes(fill = .data[[input$fillvar]]), color = NA) +
      scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
      coord_sf(expand = FALSE) +
      labs(
        title = "Homeless Population Under Age 18 (by Continuum of Care Region)",
        subtitle = "Source: HUD CoC Dataset | Year: 2023",
        fill = "Under 18 (Count)",
        caption = "Grey areas represent missing data."
      ) +
      theme_minimal()
    
    ggplotly(p)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

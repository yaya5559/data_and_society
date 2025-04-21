library(readxl)
table1_countyprogram <- read_excel("C:/Users/yahya/OneDrive/Bureau/data_and_society/table1_countyprogram.xlsx")
View(table1_countyprogram)


library(readxl)
table2_stateprogram <- read_excel("C:/Users/yahya/OneDrive/Bureau/data_and_society/table2_stateprogram.xlsx")
View(table2_stateprogram)


countyhousingProgram <- table1_countyprogram |> 
  select(state,county, GEOID, total_county_pop, cdbg_entitlement, coc,elderly,
         grrp_comp, grrp_elements, grrp_leading, hcv,  home, hud_disability,
         hud_esg, hud_hopwa )

countyhousingTidy <- countyhousingProgram |> 
  pivot_longer(
    cols = -c(county, state, GEOID,total_county_pop),
    names_to = "Organization",
    values_to= "Spending"
)

countyHousing_v2 <- countyhousingTidy[countyhousingTidy$Spending != 0,]

countyHousing_v2 <- countyHousing_v2|>
  rename(county_fips = GEOID)

cocHousing <- countyHousing_v2 |>
  inner_join(county_coc_match, by = "county_fips", relationship = "many-to-many")


cocHousing_v2 <- 
  cocHousing |> 
  select(state, county, coc_name, coc_number, county_fips, total_county_pop, Organization, Spending, pct_cnty_pop_coc)

cocHousing_v2 <- 
  cocHousing_v2 |> mutate(adjusted_spending = Spending * pct_cnty_pop_coc)





coc_level_spending <- cocHousing_v2 |>
  group_by(coc_number, coc_name, Organization) |>
  summarise(total_spending_coc = sum(adjusted_spending, na.rm = TRUE)) |>
  arrange(desc(total_spending_coc))
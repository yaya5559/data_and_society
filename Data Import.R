library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(broom)
library(rgeos)

sind <- read_csv("table4_state-indicator.csv")
spro <- read_excel("table2_stateprogram redo.xlsx")
spit <- read_csv("2024-PIT-Counts-by-State.csv")
shic <- read_csv("2024-HIC-Counts-by-State.csv", 
                                      col_types = cols(`Total Year-Round Beds (ES, TH, SH)` = col_number(), 
                                                       `Total Non-DV Year-Round Beds (ES, TH, SH)` = col_number(), 
                                                       `Total HMIS Year-Round Beds (ES, TH, SH)` = col_number(), 
                                                       `HMIS Participation Rate for Year-Round Beds (ES, TH, SH)` = col_number(), 
                                                       `Total Year-Round Beds (ES)...6` = col_number(), 
                                                       `Total Year-Round Beds (TH)...7` = col_number(), 
                                                       `Total Year-Round Beds (SH)...8` = col_number(), 
                                                       `Total Units for Households with Children (ES, TH, SH)` = col_number(), 
                                                       `Total Beds for Households with Children (ES, TH, SH)` = col_number(), 
                                                       `Total Beds for Households without Children (ES, TH, SH)` = col_number(), 
                                                       `Total Beds for Households with only Children (ES, TH, SH)` = col_number(), 
                                                       `Dedicated Veteran Beds (ES, TH, SH)` = col_number(), 
                                                       `Dedicated Youth Beds (ES, TH, SH)` = col_number(), 
                                                       `Total Year-Round Beds (ES)...15` = col_number(), 
                                                       `Total Non-DV Year-Round Beds (ES)` = col_number(), 
                                                       `Total HMIS Year-Round Beds (ES)` = col_number(), 
                                                       `HMIS Participation Rate for Year-Round Beds (ES)` = col_number(), 
                                                       `Total Seasonal Beds (ES)` = col_number(), 
                                                       `Total Overflow Beds (ES)` = col_number(), 
                                                       `Total Units for Households with Children (ES)` = col_number(), 
                                                       `Total Beds for Households with Children (ES)` = col_number(), 
                                                       `Total Beds for Households without Children (ES)` = col_number(), 
                                                       `Total Beds for Households with only Children (ES)` = col_number(), 
                                                       `Dedicated Veteran Beds (ES)` = col_number(), 
                                                       `Dedicated Youth Beds (ES)` = col_number(), 
                                                       `Total Year-Round Beds (TH)...27` = col_number(), 
                                                       `Total Non-DV Year-Round Beds (TH)` = col_number(), 
                                                       `Total HMIS Year-Round Beds (TH)` = col_number(), 
                                                       `HMIS Participation Rate for Year-Round Beds (TH)` = col_number(), 
                                                       `Total Units for Households with Children (TH)` = col_number(), 
                                                       `Total Beds for Households with Children (TH)` = col_number(), 
                                                       `Total Beds for Households without Children (TH)` = col_number(), 
                                                       `Total Beds for Households with only Children (TH)` = col_number(), 
                                                       `Dedicated Veteran Beds (TH)` = col_number(), 
                                                       `Dedicated Youth Beds (TH)` = col_number(), 
                                                       `Total Year-Round Beds (SH)...37` = col_number(), 
                                                       `Total Non-DV Year-Round Beds (SH)` = col_number(), 
                                                       `Total HMIS Year-Round Beds (SH)` = col_number(), 
                                                       `HMIS Participation Rate for Year-Round Beds (SH)` = col_number(), 
                                                       `Total Units for Households with Children (SH)` = col_number(), 
                                                       `Total Beds for Households with Children (SH)` = col_number(), 
                                                       `Total Beds for Households without Children (SH)` = col_number(), 
                                                       `Total Beds for Households with only Children (SH)` = col_number(), 
                                                       `Dedicated Veteran Beds (SH)` = col_number(), 
                                                       `Dedicated Youth Beds (SH)` = col_number(), 
                                                       `Total Year-Round Beds (RRH)` = col_number(), 
                                                       `Total Non-DV Year-Round Beds (RRH)` = col_number(), 
                                                       `Total HMIS Year-Round Beds (RRH)` = col_number(), 
                                                       `HMIS Participation Rate for Year-Round Beds (RRH)` = col_number(), 
                                                       `Total Units for Households with Children (RRH)` = col_number(), 
                                                       `Total Beds for Households with Children (RRH)` = col_number(), 
                                                       `Total Beds for Households without Children (RRH)` = col_number(), 
                                                       `Total Beds for Households with only Children (RRH)` = col_number(), 
                                                       `Dedicated Veteran Beds (RRH)` = col_number(), 
                                                       `Dedicated Youth Beds (RRH)` = col_number(), 
                                                       `Total Year-Round Beds (PSH)` = col_number(), 
                                                       `Total Non-DV Year-Round Beds (PSH)` = col_number(), 
                                                       `Total HMIS Year-Round Beds (PSH)` = col_number(), 
                                                       `HMIS Participation Rate for Year-Round Beds (PSH)` = col_number(), 
                                                       `Total Units for Households with Children (PSH)` = col_number(), 
                                                       `Total Beds for Households with Children (PSH)` = col_number(), 
                                                       `Total Beds for Households without Children (PSH)` = col_number(), 
                                                       `Total Beds for Households with only Children (PSH)` = col_number(), 
                                                       `Dedicated Veteran Beds (PSH)` = col_number(), 
                                                       `Dedicated Youth Beds (PSH)` = col_number(), 
                                                       `Dedicated Chronically Homeless Beds (PSH)` = col_number(), 
                                                       `Total Year-Round Beds (OPH)` = col_number(), 
                                                       `Total Non-DV Year-Round Beds (OPH)` = col_number(), 
                                                       `Total HMIS Year-Round Beds (OPH)` = col_number(), 
                                                       `HMIS Participation Rate for Year-Round Beds (OPH)` = col_number(), 
                                                       `Total Units for Households with Children (OPH)` = col_number(), 
                                                       `Total Beds for Households with Children (OPH)` = col_number(), 
                                                       `Total Beds for Households without Children (OPH)` = col_number(), 
                                                       `Total Beds for Households with only Children (OPH)` = col_number(), 
                                                       `Dedicated Veteran Beds (OPH)` = col_number(), 
                                                       `Dedicated Youth Beds (OPH)` = col_number()), 
                                      skip = 1)


# programs to look at: coc, cdbg_entitlement, elderly, grrp_comp, grrp_elements, grrp_leading, hvc, home, 
# hud_disability, hud_esg, hud_hopwa, hud_htf, public_hsg, public_hsg_cap, s8_project

# indicators to look at: GEOID, NAME, total_pop, ALAND, AWATER, percent_urban, percent_rural, percent_poc,
# poverty_rate, pop_density, med_hh_income, employment_access_index, housing_cost_burden, overcrowded_housing,
# vacancy_rate, homelessness, homelessness, incomplete_plumbing, incomplete_kitchen, housing_units, permits,
# capacity_housing, hud_pbs8, hud_hcv, hud_202, hud_ph, age_under_18, age_over_64

spro_names <- c('state', 
                'GEOID', 
                'coc', 
                'cdbg_entitlement', 
                'elderly', 
                'grrp_comp', 
                'grrp_elements', 
                'grrp_leading', 
                'home', 
                'hcv',
                'hud_disability', 
                'hud_esg', 
                'hud_hopwa', 
                'hud_htf', 
                'public_hsg', 
                'public_hsg_cap', 
                's8_project')


spro_cut <- spro |>
  select(state, 
         GEOID, 
         coc, 
         cdbg_entitlement, 
         elderly, 
         grrp_comp, 
         grrp_elements, 
         grrp_leading, 
         home, 
         hcv,
         hud_disability, 
         hud_esg, 
         hud_hopwa, 
         hud_htf, 
         public_hsg, 
         public_hsg_cap, 
         s8_project)


sind_cut <- sind |>
  select(GEOID, 
         NAME, 
         total_pop, 
         ALAND, 
         AWATER, 
         percent_urban, 
         percent_rural, 
         percent_poc,
         poverty_rate,
         pop_density, 
         med_hh_income, 
         employment_access_index, 
         housing_cost_burden, 
         overcrowded_housing,
         vacancy_rate,
         incomplete_plumbing, 
         incomplete_kitchen, 
         housing_units, 
         permits,
         capacity_housing, 
         hud_pbs8, 
         hud_hcv, 
         hud_202, 
         hud_ph, 
         age_under_18, 
         age_over_64)

sind_cut$GEOID <- as.numeric(sind_cut$GEOID)

spit_cut <- spit |>
  select(State,
         `Number of CoCs`,
         `Overall Homeless`,
         `Overall Homeless - Under 18`,
         `Overall Homeless - Age 18 to 24`,
         `Overall Homeless - Age 25 to 34`,
         `Overall Homeless - Age 35 to 44`,
         `Overall Homeless - Age 45 to 54`,
         `Overall Homeless - Age 55 to 64`,
         `Overall Homeless - Over 64`,
         `Overall Homeless - Woman`,
         `Overall Homeless - Man`,
         `Overall Homeless - Transgender`,
         `Overall Homeless - Non Binary`,
         `Overall Homeless - More Than One Gender`,
         `Overall Homeless - Gender Questioning`,
         `Overall Homeless - Culturally Specific Identity`,
         `Overall Homeless - Different Identity`,
         `Overall Homeless - Non-Hispanic/Latina/e/o`,
         `Overall Homeless - Hispanic/Latina/e/o`,
         `Overall Homeless - Asian or Asian American`,
         `Overall Homeless - Black, African American, or African`,
         `Overall Homeless - Middle Eastern or North African`,
         `Overall Homeless - White`,
         `Overall Homeless - Native Hawaiian or Other Pacific Islander`,
         `Overall Homeless Veterans`,
         `Overall Chronically Homeless`,
         `Overall Homeless People in Families`,
         `Overall Homeless Individuals`,
         `Overall Homeless Unaccompanied Youth (Under 25)`,
         `Overall Homeless Parenting Youth (Under 25)`,
         `Sheltered Total Homeless`,
         `Sheltered ES Homeless`,
         `Sheltered TH Homeless`,
         `Sheltered SH Homeless`,
         `Unsheltered Homeless`
         )

shic_cut <- shic |>
  select(State,
         `Total Year-Round Beds (ES, TH, SH)`,
         `Total Year-Round Beds (ES)...6`,
         `Total Year-Round Beds (TH)...7`,
         `Total Year-Round Beds (SH)...8`,
         `Total Year-Round Beds (OPH)`,
         `Total Year-Round Beds (PSH)`,
         `Total Year-Round Beds (RRH)`)



nohome <- shic_cut |>
  left_join(spit_cut, by=c("State" = "State")) |>
  arrange(State)

program <- spro_cut |>
  left_join(sind_cut, by=c("GEOID" = "GEOID"))

sdat <- program |>
  left_join(nohome, by=c("state" = "State"))

sdat <- mutate(sdat,
               funding_tot = coc + 
                 cdbg_entitlement +
                 elderly +
                 grrp_comp +
                 grrp_elements +
                 grrp_leading +
                 home +
                 hud_disability +
                 hud_esg +
                 hud_hopwa +
                 hud_htf +
                 public_hsg +
                 public_hsg_cap +
                 s8_project)

write.csv(sdat, file = "~/Data and Society/data_and_society/state_data.csv")





### Below is for county data



cind <- read_csv("table3_county-indicator.csv")
cpro <- read_csv("table1_countyprogram.csv", 
                 col_types = cols(GEOID = col_character()))
cpit <- read_csv("2024-PIT-Counts-by-CoC.csv")
chic <- read_csv("2024-HIC-Counts-by-CoC.csv", 
                 col_types = cols(`Total Year-Round Beds (ES, TH, SH)` = col_number(), 
                                  `Total Non-DV Year-Round Beds (ES, TH, SH)` = col_number(), 
                                  `Total HMIS Year-Round Beds (ES, TH, SH)` = col_number(), 
                                  `HMIS Participation Rate for Year-Round Beds (ES, TH, SH)` = col_number(), 
                                  `Total Year-Round Beds (ES)...6` = col_number(), 
                                  `Total Year-Round Beds (TH)...7` = col_number(), 
                                  `Total Year-Round Beds (SH)...8` = col_number(), 
                                  `Total Units for Households with Children (ES, TH, SH)` = col_number(), 
                                  `Total Beds for Households with Children (ES, TH, SH)` = col_number(), 
                                  `Total Beds for Households without Children (ES, TH, SH)` = col_number(), 
                                  `Total Beds for Households with only Children (ES, TH, SH)` = col_number(), 
                                  `Dedicated Veteran Beds (ES, TH, SH)` = col_number(), 
                                  `Dedicated Youth Beds (ES, TH, SH)` = col_number(), 
                                  `Total Year-Round Beds (ES)...15` = col_number(), 
                                  `Total Non-DV Year-Round Beds (ES)` = col_number(), 
                                  `Total HMIS Year-Round Beds (ES)` = col_number(), 
                                  `HMIS Participation Rate for Year-Round Beds (ES)` = col_number(), 
                                  `Total Seasonal Beds (ES)` = col_number(), 
                                  `Total Overflow Beds (ES)` = col_number(), 
                                  `Total Units for Households with Children (ES)` = col_number(), 
                                  `Total Beds for Households with Children (ES)` = col_number(), 
                                  `Total Beds for Households without Children (ES)` = col_number(), 
                                  `Total Beds for Households with only Children (ES)` = col_number(), 
                                  `Dedicated Veteran Beds (ES)` = col_number(), 
                                  `Dedicated Youth Beds (ES)` = col_number(), 
                                  `Total Year-Round Beds (TH)...27` = col_number(), 
                                  `Total Non-DV Year-Round Beds (TH)` = col_number(), 
                                  `Total HMIS Year-Round Beds (TH)` = col_number(), 
                                  `HMIS Participation Rate for Year-Round Beds (TH)` = col_number(), 
                                  `Total Units for Households with Children (TH)` = col_number(), 
                                  `Total Beds for Households with Children (TH)` = col_number(), 
                                  `Total Beds for Households without Children (TH)` = col_number(), 
                                  `Total Beds for Households with only Children (TH)` = col_number(), 
                                  `Dedicated Veteran Beds (TH)` = col_number(), 
                                  `Dedicated Youth Beds (TH)` = col_number(), 
                                  `Total Year-Round Beds (SH)...37` = col_number(), 
                                  `Total Non-DV Year-Round Beds (SH)` = col_number(), 
                                  `Total HMIS Year-Round Beds (SH)` = col_number(), 
                                  `HMIS Participation Rate for Year-Round Beds (SH)` = col_number(), 
                                  `Total Units for Households with Children (SH)` = col_number(), 
                                  `Total Beds for Households with Children (SH)` = col_number(), 
                                  `Total Beds for Households without Children (SH)` = col_number(), 
                                  `Total Beds for Households with only Children (SH)` = col_number(), 
                                  `Dedicated Veteran Beds (SH)` = col_number(), 
                                  `Dedicated Youth Beds (SH)` = col_number(), 
                                  `Total Year-Round Beds (RRH)` = col_number(), 
                                  `Total Non-DV Year-Round Beds (RRH)` = col_number(), 
                                  `Total HMIS Year-Round Beds (RRH)` = col_number(), 
                                  `HMIS Participation Rate for Year-Round Beds (RRH)` = col_number(), 
                                  `Total Units for Households with Children (RRH)` = col_number(), 
                                  `Total Beds for Households with Children (RRH)` = col_number(), 
                                  `Total Beds for Households without Children (RRH)` = col_number(), 
                                  `Total Beds for Households with only Children (RRH)` = col_number(), 
                                  `Dedicated Veteran Beds (RRH)` = col_number(), 
                                  `Dedicated Youth Beds (RRH)` = col_number(), 
                                  `Total Year-Round Beds (PSH)` = col_number(), 
                                  `Total Non-DV Year-Round Beds (PSH)` = col_number(), 
                                  `Total HMIS Year-Round Beds (PSH)` = col_number(), 
                                  `HMIS Participation Rate for Year-Round Beds (PSH)` = col_number(), 
                                  `Total Units for Households with Children (PSH)` = col_number(), 
                                  `Total Beds for Households with Children (PSH)` = col_number(), 
                                  `Total Beds for Households without Children (PSH)` = col_number(), 
                                  `Total Beds for Households with only Children (PSH)` = col_number(), 
                                  `Dedicated Veteran Beds (PSH)` = col_number(), 
                                  `Dedicated Youth Beds (PSH)` = col_number(), 
                                  `Dedicated Chronically Homeless Beds (PSH)` = col_number(), 
                                  `Total Year-Round Beds (OPH)` = col_number(), 
                                  `Total Non-DV Year-Round Beds (OPH)` = col_number(), 
                                  `Total HMIS Year-Round Beds (OPH)` = col_number(), 
                                  `HMIS Participation Rate for Year-Round Beds (OPH)` = col_number(), 
                                  `Total Units for Households with Children (OPH)` = col_number(), 
                                  `Total Beds for Households with Children (OPH)` = col_number(), 
                                  `Total Beds for Households without Children (OPH)` = col_number(), 
                                  `Total Beds for Households with only Children (OPH)` = col_number(), 
                                  `Dedicated Veteran Beds (OPH)` = col_number(), 
                                  `Dedicated Youth Beds (OPH)` = col_number()), 
                 skip = 1)


# programs to look at: coc, cdbg_entitlement, elderly, grrp_comp, grrp_elements, grrp_leading, hvc, home, 
# hud_disability, hud_esg, hud_hopwa, hud_htf, public_hsg, public_hsg_cap, s8_project

# indicators to look at: GEOID, NAME, total_pop, ALAND, AWATER, percent_urban, percent_rural, percent_poc,
# poverty_rate, pop_density, med_hh_income, employment_access_index, housing_cost_burden, overcrowded_housing,
# vacancy_rate, homelessness, homelessness, incomplete_plumbing, incomplete_kitchen, housing_units, permits,
# capacity_housing, hud_pbs8, hud_hcv, hud_202, hud_ph, age_under_18, age_over_64



cpro_cut <- cpro |>
  select(state, 
         GEOID, 
         coc, 
         cdbg_entitlement, 
         elderly, 
         grrp_comp, 
         grrp_elements, 
         grrp_leading, 
         home, 
         hcv,
         hud_disability, 
         hud_esg, 
         hud_hopwa, 
         public_hsg, 
         public_hsg_cap, 
         s8_project)

cpro_cut <- cpro_cut |>
  mutate(GEOID = case_when(
    str_length(GEOID) < 5 ~ paste("0", GEOID, sep = "")
  ))

cpro_cut$GEOID <- as.character(cind_cut$GEOID)



cind_cut <- cind |>
  select(GEOID, 
         NAME, 
         county,
         state,
         total_pop, 
         ALAND, 
         AWATER, 
         percent_urban, 
         percent_rural, 
         percent_poc,
         poverty_rate,
         pop_density, 
         med_hh_income, 
         employment_access_index, 
         housing_cost_burden, 
         overcrowded_housing,
         vacancy_rate,
         incomplete_plumbing, 
         incomplete_kitchen, 
         housing_units, 
         permits,
         capacity_housing, 
         hud_pbs8, 
         hud_hcv, 
         hud_202, 
         hud_ph, 
         age_under_18, 
         age_over_64)

cind_cut$GEOID <- as.character(cind_cut$GEOID)


cpit_cut <- cpit |>
  select(`CoC Number`,
         `CoC Name`,
         `CoC Category`,
         `Count Types`,
         `Overall Homeless`,
         `Overall Homeless - Under 18`,
         `Overall Homeless - Age 18 to 24`,
         `Overall Homeless - Age 25 to 34`,
         `Overall Homeless - Age 35 to 44`,
         `Overall Homeless - Age 45 to 54`,
         `Overall Homeless - Age 55 to 64`,
         `Overall Homeless - Over 64`,
         `Overall Homeless - Woman`,
         `Overall Homeless - Man`,
         `Overall Homeless - Transgender`,
         `Overall Homeless - Non Binary`,
         `Overall Homeless - More Than One Gender`,
         `Overall Homeless - Gender Questioning`,
         `Overall Homeless - Culturally Specific Identity`,
         `Overall Homeless - Different Identity`,
         `Overall Homeless - Non-Hispanic/Latina/e/o`,
         `Overall Homeless - Hispanic/Latina/e/o`,
         `Overall Homeless - Asian or Asian American`,
         `Overall Homeless - Black, African American, or African`,
         `Overall Homeless - Middle Eastern or North African`,
         `Overall Homeless - White`,
         `Overall Homeless - Native Hawaiian or Other Pacific Islander`,
         `Overall Homeless Veterans`,
         `Overall Chronically Homeless`,
         `Overall Homeless People in Families`,
         `Overall Homeless Individuals`,
         `Overall Homeless Unaccompanied Youth (Under 25)`,
         `Overall Homeless Parenting Youth (Under 25)`,
         `Sheltered Total Homeless`,
         `Sheltered ES Homeless`,
         `Sheltered TH Homeless`,
         `Sheltered SH Homeless`,
         `Unsheltered Homeless`
  )

chic_cut <- chic |>
  select(`CoC Number`,
         `Total Year-Round Beds (ES, TH, SH)`,
         `Total Year-Round Beds (ES)...6`,
         `Total Year-Round Beds (TH)...7`,
         `Total Year-Round Beds (SH)...8`,
         `Total Year-Round Beds (OPH)`,
         `Total Year-Round Beds (PSH)`,
         `Total Year-Round Beds (RRH)`)



nohome2 <- cpit_cut |>
  left_join(chic_cut, by=c("CoC Number" = "CoC Number")) |>
  arrange(`CoC Number`)

nohome2 <- nohome2[-c(1, 2, 388, 389, 390), ]

nohome2$`CoC Number`[209] <- "MO-604"
nohome2$`CoC Name`[209] <- "Kansas City (MO&KS), Independence, Lees Summit/Jackson,
Wyandotte Counties CoC"
nohome2$`CoC Name`[25] <- "Contra Costa County CoC"
nohome2$`CoC Name`[32] <- "Daly City/San Mateo County CoC"
nohome2$`CoC Name`[35] <- "Roseville, Rocklin/Placer County CoC"
nohome2$`CoC Name`[36] <- "Redding/Shasta, Siskiyou, Lassen, Plumas, Del Norte, Modoc, Sierra Counties CoC"
nohome2$`CoC Name`[75] <- "Lakeland/Polk County CoC"
nohome2$`CoC Name`[94] <- "Charlotte County CoC"
nohome2$`CoC Name`[117] <- "Rockford/DeKalb, Winnebago, Boone Counties CoC"
nohome2$`CoC Name`[144] <- "Lafayette/Acadiana Regional CoC"
nohome2$`CoC Name`[196] <- "Dakota, Anoka, Washington, Scott, Carver Counties CoC"
nohome2$`CoC Name`[201] <- "Duluth/St. Louis County CoC"
nohome2$`CoC Name`[205] <- "St. Charles, Lincoln, Warren Counties CoC"
nohome2$`CoC Name`[314] <- "Bristol, Bensalem/Bucks County CoC"
nohome2$`CoC Name`[316] <- "Pittsburgh, McKeesport, Penn Hills/Allegheny County CoC"
nohome2$`CoC Name`[324] <- "Greenville, Anderson, Spartanburg/Upstate CoC"
nohome2$`CoC Name`[326] <- "Sumter City & County CoC"
nohome2$`CoC Name`[333] <- "Upper Cumberland CoC"
nohome2$`CoC Name`[340] <- "Dallas City & County, Irving CoC"
nohome2$`CoC Name`[341] <- "Fort Worth, Arlington/Tarrant County CoC"
nohome2$`CoC Name`[344] <- "Texas Balance of State CoC"
nohome2$`CoC Name`[347] <- "Houston, Pasadena, Conroe/Harris, Fort Bend, Montgomery Counties CoC"
nohome2$`CoC Name`[348] <- "Bryan, College Station/Brazos Valley CoC"
nohome2$`CoC Name`[]






program2 <- cind_cut |>
  left_join(cpro_cut, by=c("GEOID" = "GEOID")) |>
  arrange(state.y, NAME)

program2$GEOID <- as.character(program2$GEOID)




cdat <- mutate(cdat,
               funding_tot = coc + 
                 cdbg_entitlement +
                 elderly +
                 grrp_comp +
                 grrp_elements +
                 grrp_leading +
                 home +
                 hud_disability +
                 hud_esg +
                 hud_hopwa +
                 hud_htf +
                 public_hsg +
                 public_hsg_cap +
                 s8_project)




### I FINALLY have the goddamned CoC to County table. Working on merging the data here.

cnt_to_coc <- read_csv("County-to-CoC.csv")

cnt_to_coc <- cnt_to_coc |>
  rename(geoCode = GEOID)


cnt_to_coc <- cnt_to_coc |>
  separate(`CoC Name`, c("coc_num", "coc_name"), sep = " - ")

cnt_to_coc <- cnt_to_coc |>
  select(coc_num, geoCode, coc_name, `County Name`)

a <- cnt_to_coc |>
  left_join(nohome2, by=c("coc_num" = "CoC Number")) |>
  arrange(geoCode)

a$geoCode <- as.character(a$geoCode)



###Chopping things up by state to sort counties so I can match with cnt-to-coc


a <- a |>
  mutate(state  = str_extract(coc_num, "[A-Z]{1,2}"))







b <- a |>
  right_join(program2, by = c("state" = "state.y", "County Name" = "county"))







  





  
  us_county_v2 <- us_county |>
  mutate(county = str_replace(county, " County", ""),
         county = str_replace(county, " Bourough", ""),
         county = str_replace(county, " city", ""),
         county = str_replace(county, " Parish", ""))




### Pulling the shapefiles

GIS <- sf::st_read("CoC_GIS_National_Boundary.gdb")

GIS |>
ggplot() +
  geom_sf()

slim_GIS |>
  ggplot() +
    geom_sf()


# Need to turn this off for simplify and on when plotting map
sf_use_s2(TRUE)

slim_GIS <- st_simplify(GIS, preserveTopology = FALSE, dTolerance = 1000)



### Notes:
# Regional access point <- look this up (something to do with coordinated entry)
# Balance of state CoC is managed by department of commerce for each state
# 

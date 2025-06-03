library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(broom)
library(rmapshaper)


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
         `Overall Homeless Individuals - American Indian, Alaska Native, or Indigenous`,
         `Overall Homeless - Asian or Asian American`,
         `Overall Homeless - Black, African American, or African`,
         `Overall Homeless - Middle Eastern or North African`,
         `Overall Homeless - White`,
         `Overall Homeless - Native Hawaiian or Other Pacific Islander`,
         `Overall Homeless - Multi-Racial`,
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

# write.csv(sdat, file = "~/Data and Society/data_and_society/state_data.csv")





### Below is for county data



cind <- read_csv("table3_county-indicator.csv")
cpro <- read_csv("table1_countyprogram.csv", 
                 col_types = cols(GEOID = col_character()))
cpit <- read_csv("2024-PIT-Counts-by-CoC.csv")
chic <- read_csv("2024-HIC-Counts-by-CoC.csv",
                 skip = 1)

acs <- read_csv("ACSDP5Y2023.DP05-Data.csv", 
                skip = 1)

acs <- acs |>
  separate(Geography, c("nada", "GEOID"), sep = "US")

acs <- acs |>
  separate(`Geographic Area Name`, c("county", "state"), sep = ", ")

acs <- acs |>
  select(GEOID,
         county,
         state,
         `Estimate!!SEX AND AGE!!Total population`,
         `Estimate!!SEX AND AGE!!Total population!!Female`,
         `Estimate!!SEX AND AGE!!Total population!!Male`,
         `Estimate!!SEX AND AGE!!Total population!!Under 18 years`,
         `Estimate!!SEX AND AGE!!Total population!!65 years and over...59`,
         `Estimate!!SEX AND AGE!!Total population!!Under 5 years`,
         `Estimate!!SEX AND AGE!!Total population!!5 to 9 years`,
         `Estimate!!SEX AND AGE!!Total population!!10 to 14 years`,
         `Estimate!!SEX AND AGE!!Total population!!15 to 19 years`,
         `Estimate!!SEX AND AGE!!Total population!!20 to 24 years`,
         `Estimate!!SEX AND AGE!!Total population!!25 to 34 years`,
         `Estimate!!SEX AND AGE!!Total population!!35 to 44 years`,
         `Estimate!!SEX AND AGE!!Total population!!45 to 54 years`,
         `Estimate!!SEX AND AGE!!Total population!!55 to 59 years`,
         `Estimate!!SEX AND AGE!!Total population!!60 to 64 years`,
         `Estimate!!SEX AND AGE!!Total population!!65 to 74 years`,
         `Estimate!!SEX AND AGE!!Total population!!75 to 84 years`,
         `Estimate!!SEX AND AGE!!Total population!!85 years and over`,
         `Estimate!!SEX AND AGE!!Total population!!Median age (years)`,
         `Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)`,
         `Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino`,
         `Estimate!!RACE!!Total population!!One race!!American Indian and Alaska Native`,
         `Estimate!!RACE!!Total population!!One race!!Asian`,
         `Estimate!!RACE!!Total population!!One race!!Black or African American`,
         `Estimate!!RACE!!Total population!!One race!!Native Hawaiian and Other Pacific Islander`,
         `Estimate!!RACE!!Total population!!One race!!Some Other Race`,
         `Estimate!!RACE!!Total population!!One race!!White`,
         `Estimate!!RACE!!Total population!!Two or More Races...71`
         )

acs <- acs |>
  group_by(GEOID) |>
  summarise(county,
            state,
            popest = `Estimate!!SEX AND AGE!!Total population`,
            female = `Estimate!!SEX AND AGE!!Total population!!Female`,
            male = `Estimate!!SEX AND AGE!!Total population!!Male`,
            under18 = `Estimate!!SEX AND AGE!!Total population!!Under 18 years`,
            over65 = `Estimate!!SEX AND AGE!!Total population!!65 years and over...59`,
            under5 = `Estimate!!SEX AND AGE!!Total population!!Under 5 years`,
            fiveto9 = `Estimate!!SEX AND AGE!!Total population!!5 to 9 years`,
            tento14 = `Estimate!!SEX AND AGE!!Total population!!10 to 14 years`,
            fifteento19 = `Estimate!!SEX AND AGE!!Total population!!15 to 19 years`,
            twentyto24 = `Estimate!!SEX AND AGE!!Total population!!20 to 24 years`,
            age25to34 = `Estimate!!SEX AND AGE!!Total population!!25 to 34 years`,
            age35to44 = `Estimate!!SEX AND AGE!!Total population!!35 to 44 years`,
            age45to54 = `Estimate!!SEX AND AGE!!Total population!!45 to 54 years`,
            fifty5to = `Estimate!!SEX AND AGE!!Total population!!55 to 59 years`,
            sxtyto64 = `Estimate!!SEX AND AGE!!Total population!!60 to 64 years`,
            sxty5to74 = `Estimate!!SEX AND AGE!!Total population!!65 to 74 years`,
            svy5to84 = `Estimate!!SEX AND AGE!!Total population!!75 to 84 years`,
            over85 = `Estimate!!SEX AND AGE!!Total population!!85 years and over`,
            age18to24 = under5 + fiveto9 + tento14 + fifteento19 + twentyto24 - under18,
            age55to64 = fifty5to + sxtyto64,
            median_age = `Estimate!!SEX AND AGE!!Total population!!Median age (years)`,
            hispaniclatinx = `Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Hispanic or Latino (of any race)`,
            non_hispaniclatinx = `Estimate!!HISPANIC OR LATINO AND RACE!!Total population!!Not Hispanic or Latino`,
            amer_indian_ak_native = `Estimate!!RACE!!Total population!!One race!!American Indian and Alaska Native`,
            asian = `Estimate!!RACE!!Total population!!One race!!Asian`,
            black = `Estimate!!RACE!!Total population!!One race!!Black or African American`,
            native_hi_other_pi = `Estimate!!RACE!!Total population!!One race!!Native Hawaiian and Other Pacific Islander`,
            other_race = `Estimate!!RACE!!Total population!!One race!!Some Other Race`,
            white = `Estimate!!RACE!!Total population!!One race!!White`,
            multi_racial = `Estimate!!RACE!!Total population!!Two or More Races...71`
            )





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
         # grrp_comp, 
         # grrp_elements, 
         # grrp_leading, 
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
    str_length(GEOID) < 5 ~ paste("0", GEOID, sep = ""),
    .default = as.character(GEOID)
  ))

cpro_cut$GEOID <- as.character(cpro_cut$GEOID)





cind_cut <- cind |>
  select(GEOID, 
         NAME, 
         county,
         state,
         total_pop, 
         ALAND, 
         AWATER, 
         metro,
         urban,
         persistent_poverty_county,
         disadvantaged_county,
         percent_urban, 
         percent_rural, 
         percent_poc,
         percent_poc_bucket,
         poverty_rate,
         poverty_rate_bucket,
         pop_density, 
         med_hh_income, 
         med_hh_income_bucket,
         employment_access_index, 
         housing_cost_burden, 
         overcrowded_housing,
         vacancy_rate,
         incomplete_plumbing, 
         incomplete_kitchen, 
         housing_units, 
         permits,
         capacity_housing, 
         capacity_environment,
         capacity_transport,
         hud_pbs8, 
         hud_hcv, 
         hud_202, 
         hud_ph, 
         age_under_18, 
         age_over_64,
         cnw_500,
         contaminated_sites,
         stations_subway,
         stations_elevated,
         airports_primary,
         docks,
         rail_transit_km,
         transport_trade_jobs,
         highway_miles
  )

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
         `Overall Homeless - American Indian, Alaska Native, or Indigenous`,
         `Overall Homeless - Asian or Asian American`,
         `Overall Homeless - Black, African American, or African`,
         `Overall Homeless - Middle Eastern or North African`,
         `Overall Homeless - White`,
         `Overall Homeless - Native Hawaiian or Other Pacific Islander`,
         `Overall Homeless - Multi-Racial`,
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

nohome3 <- nohome2 |>
  filter(`CoC Number` == "CA-600" | `CoC Number` == "CA-606" | `CoC Number` == "CA-607" | `CoC Number` == "CA-612"
         | `CoC Number` == "GA-500" | `CoC Number` == "GA-502" | `CoC Number` == "IL-510" | `CoC Number` == "IL-511" 
         | `CoC Number` == "MA-500" | `CoC Number` == "MA-502" | `CoC Number` == "MA-505" | `CoC Number` == "MA-509"
         | `CoC Number` == "MA-515" | `CoC Number` == "MA-516" | `CoC Number` == "MA-519" | `CoC Number` == "MI-501" 
         | `CoC Number` == "MI-502" | `CoC Number` == "NH-501" | `CoC Number` == "NH-502")

nohome3$`CoC Name`[2] <- "Los Angeles City & County CoC"
nohome3$`CoC Number`[2] <- "CA-600"
nohome3$`CoC Name`[3] <- "Los Angeles City & County CoC"
nohome3$`CoC Number`[3] <- "CA-600"
nohome3$`CoC Name`[4] <- "Los Angeles City & County CoC"
nohome3$`CoC Number`[4] <- "CA-600"
nohome3$`CoC Name`[5] <- "Fulton County CoC"
nohome3$`CoC Number`[5] <- "GA-502"
nohome3$`CoC Name`[7] <- "Cook County CoC"
nohome3$`CoC Number`[7] <- "IL-511"
nohome3$`CoC Name`[9] <- "Massachusetts Balance of State CoC"
nohome3$`CoC Number`[9] <- "MA-516"
nohome3$`CoC Name`[10] <- "Massachusetts Balance of State CoC"
nohome3$`CoC Number`[10] <- "MA-516"
nohome3$`CoC Name`[11] <- "New Bedford, Attleboro, Taunton/Bristol County CoC"
nohome3$`CoC Number`[11] <- "MA-505"
nohome3$`CoC Name`[12] <- "Massachusetts Balance of State CoC"
nohome3$`CoC Number`[12] <- "MA-516"
nohome3$`CoC Name`[13] <- "New Bedford, Attleboro, Taunton/Bristol County CoC"
nohome3$`CoC Number`[13] <- "MA-505"
nohome3$`CoC Name`[15] <- "Dearborn, Dearborn Heights, Westland/Wayne County CoC"
nohome3$`CoC Number`[15] <- "MI-502"
nohome3$`CoC Name`[17] <- "Nashua/Hillsborough County CoC"
nohome3$`CoC Number`[17] <- "NH-502"

nohome3$`CoC Category` <- "Other Largely Urban CoC"

nohome3 <- nohome3 |>
  group_by(`CoC Number`, `CoC Name`, `CoC Category`, `Count Types`) |>
      summarise(`Overall Homeless` = sum(`Overall Homeless`),
          `Overall Homeless - Under 18` = sum(`Overall Homeless - Under 18`),
          `Overall Homeless - Age 18 to 24` = sum(`Overall Homeless - Age 18 to 24`),
          `Overall Homeless - Age 25 to 34` = sum(`Overall Homeless - Age 25 to 34`),
          `Overall Homeless - Age 35 to 44` = sum(`Overall Homeless - Age 35 to 44`),
          `Overall Homeless - Age 45 to 54` = sum(`Overall Homeless - Age 45 to 54`),
          `Overall Homeless - Age 55 to 64` = sum(`Overall Homeless - Age 55 to 64`),
          `Overall Homeless - Over 64` = sum(`Overall Homeless - Over 64`),
          `Overall Homeless - Woman` = sum(`Overall Homeless - Woman`),
          `Overall Homeless - Man` = sum(`Overall Homeless - Man`),
          `Overall Homeless - Transgender` = sum(`Overall Homeless - Transgender`),
          `Overall Homeless - Non Binary` = sum(`Overall Homeless - Non Binary`),
          `Overall Homeless - More Than One Gender` = sum(`Overall Homeless - More Than One Gender`),
          `Overall Homeless - Gender Questioning` = sum(`Overall Homeless - Gender Questioning`),
          `Overall Homeless - Culturally Specific Identity` = sum(`Overall Homeless - Culturally Specific Identity`),
          `Overall Homeless - Different Identity` = sum(`Overall Homeless - Different Identity`),
          `Overall Homeless - Non-Hispanic/Latina/e/o` = sum(`Overall Homeless - Non-Hispanic/Latina/e/o`),
          `Overall Homeless - Hispanic/Latina/e/o` = sum(`Overall Homeless - Hispanic/Latina/e/o`),
          `Overall Homeless - American Indian, Alaska Native, or Indigenous` = sum(`Overall Homeless - American Indian, Alaska Native, or Indigenous`),
          `Overall Homeless - Asian or Asian American` = sum(`Overall Homeless - Asian or Asian American`),
          `Overall Homeless - Black, African American, or African` = sum(`Overall Homeless - Black, African American, or African`),
          `Overall Homeless - Middle Eastern or North African` = sum(`Overall Homeless - Middle Eastern or North African`),
          `Overall Homeless - White` = sum(`Overall Homeless - White`),
          `Overall Homeless - Native Hawaiian or Other Pacific Islander` = sum(`Overall Homeless - Native Hawaiian or Other Pacific Islander`),
          `Overall Homeless - Multi-Racial` = sum(`Overall Homeless - Multi-Racial`),
          `Overall Homeless Veterans` = sum(`Overall Homeless Veterans`),
          `Overall Chronically Homeless` = sum(`Overall Chronically Homeless`),
          `Overall Homeless People in Families` = sum(`Overall Homeless People in Families`),
          `Overall Homeless Individuals` = sum(`Overall Homeless Individuals`),
          `Overall Homeless Unaccompanied Youth (Under 25)` = sum(`Overall Homeless Unaccompanied Youth (Under 25)`),
          `Overall Homeless Parenting Youth (Under 25)` = sum(`Overall Homeless Parenting Youth (Under 25)`),
          `Sheltered Total Homeless` = sum(`Sheltered Total Homeless`),
          `Sheltered ES Homeless` = sum(`Sheltered ES Homeless`),
          `Sheltered TH Homeless` = sum(`Sheltered TH Homeless`),
          `Sheltered SH Homeless` = sum(`Sheltered SH Homeless`),
          `Unsheltered Homeless` = sum(`Unsheltered Homeless`),
          `Total Year-Round Beds (ES, TH, SH)` = sum(`Total Year-Round Beds (ES, TH, SH)`),
          `Total Year-Round Beds (ES)` = sum(`Total Year-Round Beds (ES)...6`),
          `Total Year-Round Beds (TH)` = sum(`Total Year-Round Beds (TH)...7`),
          `Total Year-Round Beds (SH)` = sum(`Total Year-Round Beds (SH)...8`),
          `Total Year-Round Beds (OPH)` = sum(`Total Year-Round Beds (OPH)`),
          `Total Year-Round Beds (PSH)` = sum(`Total Year-Round Beds (PSH)`),
          `Total Year-Round Beds (RRH)` = sum(`Total Year-Round Beds (RRH)`))


nohome4 <- nohome2[-c(51, 56, 57, 61, 99, 101, 124, 125, 151, 152, 155, 158, 160, 161, 174, 175, 232, 233), ]

nohome2 <- bind_rows(nohome3, nohome4)

nohome2 <- nohome2 |>
  arrange(`CoC Number`)






program2 <- cind_cut |>
  left_join(cpro_cut, by=c("GEOID" = "GEOID"))

program2 <- program2 |>
  left_join(acs, by=c("GEOID" = "GEOID", "county" = "county", "state.x" = "state"))

program2 <- program2 |>
  mutate(fips_state = str_extract(GEOID, "[0-9]{1,2}"))


program2$GEOID <- as.character(program2$GEOID)







### I FINALLY have the goddamned CoC to County table. Working on merging the data here.


# Detroit CoC = Wayne County, Atlanta CoC = Fulton County, Chicago CoC = Cook County, Long Beach CoC = Los Angeles County, 
# Pasedena CoC = Los Angeles County, Glendale CoC = Los Angeles County, Boston CoC = (BoS) Suffolk County, Lynn CoC = (BoS) Essex County,
# New Bedford CoC = Bristol County, Cambridge CoC = (BoS) Middlesex County, Fall River CoC = Bristol County,(there is no bristol County CoC)

cnt_to_coc <- read_csv("County-to-CoC.csv")

cnt_to_coc <- cnt_to_coc |>
  rename(geoCode = GEOID)

cnt_to_coc$`County Name`[2610] <- "Doña Ana County"
cnt_to_coc$`CoC Name`[2475] <- "NE-502 - Lincoln CoC"
cnt_to_coc$`CoC Name`[2604] <- "NM-500 - Albuquerque CoC"
cnt_to_coc$`CoC Name`[2915] <- "OK-502 - Oklahoma City CoC"
cnt_to_coc$`CoC Name`[3665] <- "TX-611 - Amarillo CoC"
cnt_to_coc$`CoC Name`[3668] <- "TX-611 - Amarillo CoC"
cnt_to_coc$`CoC Name`[1649] <- "MA-505 - New Bedford, Attleboro, Taunton/Bristol County CoC"
cnt_to_coc$`CoC Name`[1685] <- "MA-505 - New Bedford, Attleboro, Taunton/Bristol County CoC"
cnt_to_coc$`CoC Name`[1686] <- "MA-505 - New Bedford, Attleboro, Taunton/Bristol County CoC"
cnt_to_coc$`CoC Name`[1687] <- "MA-505 - New Bedford, Attleboro, Taunton/Bristol County CoC"
cnt_to_coc$`CoC Name`[1688] <- "MA-505 - New Bedford, Attleboro, Taunton/Bristol County CoC"






cnt_to_coc <- cnt_to_coc |>
  separate(`CoC Name`, c("coc_num", "coc_name"), sep = " - ")

cnt_to_coc <- cnt_to_coc |>
  select(coc_num, geoCode, coc_name, `County Name`)

a <- cnt_to_coc |>
  right_join(nohome2, by=c("coc_num" = "CoC Number"))

a$geoCode <- as.character(a$geoCode)


### Chopping things up by state to sort counties so I can match with cnt-to-coc


a <- a |>
  mutate(state  = str_extract(coc_num, "[A-Z]{1,2}"))


a <- a |>
  mutate(`County Name` = str_replace(`County Name`, " Municipality", ""),
         `County Name` = str_replace(`County Name`, " ", " "))

program2 <- program2 |>
  mutate(county = str_replace(county, " Municipality", ""),
         county = str_replace(county, " ", " "))




### No matches: FL-Baker, FL-Dixie, FL-Union, OK-Logan, MA-Bristol
### Changes: OK-Pottawatomie = KS, KS-Wyandotte = MO, IA-Pottawattamie = NE, NE-Dakota = MN, 


program2$state.y[851] <- "NE"
program2$state.y[933] <- "MO"
program2$state.y[1663] <- "MN"
program2$state.y[2162] <- "KS"








b <- a |>
  right_join(program2, by = c("state" = "state.y", "County Name" = "county"))


cdat <- mutate(b,
               funding_tot = coc + 
                 cdbg_entitlement +
                 elderly +
                 # grrp_comp +
                 # grrp_elements +
                 # grrp_leading +
                 home +
                 hud_disability +
                 hud_esg +
                 hud_hopwa +
                 public_hsg +
                 public_hsg_cap +
                 s8_project)






cdat <- cdat |>
  rename(state_abv = state) |>
  rename(state_full = state.x)

write.csv(cdat, file = "~/Data and Society/data_and_society/county_data_take2.csv")



# cnt_chkr <- cnt_to_coc |>
#   group_by(coc_num, coc_name) |>
#   summarise()
# 
# chkr <- cnt_chkr |>
#   left_join(cocdat, by = c("coc_num" = "coc_num", "coc_name" = "coc_name"))
# 
# chkd <- chkr |>
#   filter(is.na(state_full))


### Now to group by coc

cocdat <- cdat |>
  group_by(coc_num, coc_name, `CoC Category`, `Count Types`, fips_state, state_full, state_abv) |>
  summarise(`Overall Homeless` = mean(`Overall Homeless`),
            `Overall Homeless - Under 18` = mean(`Overall Homeless - Under 18`),
            `Overall Homeless - Age 18 to 24` = mean(`Overall Homeless - Age 18 to 24`),
            `Overall Homeless - Age 25 to 34` = mean(`Overall Homeless - Age 25 to 34`),
            `Overall Homeless - Age 35 to 44` = mean(`Overall Homeless - Age 35 to 44`),
            `Overall Homeless - Age 45 to 54` = mean(`Overall Homeless - Age 45 to 54`),
            `Overall Homeless - Age 55 to 64` = mean(`Overall Homeless - Age 55 to 64`),
            `Overall Homeless - Over 64` = mean(`Overall Homeless - Over 64`),
            `Overall Homeless - Woman` = mean(`Overall Homeless - Woman`),
            `Overall Homeless - Man` = mean(`Overall Homeless - Man`),
            `Overall Homeless - Transgender` = mean(`Overall Homeless - Transgender`),
            `Overall Homeless - Non Binary` = mean(`Overall Homeless - Non Binary`),
            `Overall Homeless - More Than One Gender` = mean(`Overall Homeless - More Than One Gender`),
            `Overall Homeless - Gender Questioning` = mean(`Overall Homeless - Gender Questioning`),
            `Overall Homeless - Culturally Specific Identity` = mean(`Overall Homeless - Culturally Specific Identity`),
            `Overall Homeless - Different Identity` = mean(`Overall Homeless - Different Identity`),
            `Overall Homeless - Non-Hispanic/Latina/e/o` = mean(`Overall Homeless - Non-Hispanic/Latina/e/o`),
            `Overall Homeless - Hispanic/Latina/e/o` = mean(`Overall Homeless - Hispanic/Latina/e/o`),
            `Overall Homeless - American Indian, Alaska Native, or Indigenous` = mean(`Overall Homeless - American Indian, Alaska Native, or Indigenous`),
            `Overall Homeless - Asian or Asian American` = mean(`Overall Homeless - Asian or Asian American`),
            `Overall Homeless - Black, African American, or African` = mean(`Overall Homeless - Black, African American, or African`),
            `Overall Homeless - Middle Eastern or North African` = mean(`Overall Homeless - Middle Eastern or North African`),
            `Overall Homeless - White` = mean(`Overall Homeless - White`),
            `Overall Homeless - Native Hawaiian or Other Pacific Islander` = mean(`Overall Homeless - Native Hawaiian or Other Pacific Islander`),
            `Overall Homeless - Multi-Racial` = mean(`Overall Homeless - Multi-Racial`),
            `Overall Homeless Veterans` = mean(`Overall Homeless Veterans`),
            `Overall Chronically Homeless` = mean(`Overall Chronically Homeless`),
            `Overall Homeless People in Families` = mean(`Overall Homeless People in Families`),
            `Overall Homeless Individuals` = mean(`Overall Homeless Individuals`),
            `Overall Homeless Unaccompanied Youth (Under 25)` = mean(`Overall Homeless Unaccompanied Youth (Under 25)`),
            `Overall Homeless Parenting Youth (Under 25)` = mean(`Overall Homeless Parenting Youth (Under 25)`),
            `Sheltered Total Homeless` = mean(`Sheltered Total Homeless`),
            `Sheltered ES Homeless` = mean(`Sheltered ES Homeless`),
            `Sheltered TH Homeless` = mean(`Sheltered TH Homeless`),
            `Sheltered SH Homeless` = mean(`Sheltered SH Homeless`),
            `Unsheltered Homeless` = mean(`Unsheltered Homeless`),
            `Total Year-Round Beds (ES, TH, SH)` = mean(`Total Year-Round Beds (ES, TH, SH)`),
            `Total Year-Round Beds (ES)` = mean(`Total Year-Round Beds (ES)`),
            `Total Year-Round Beds (TH)` = mean(`Total Year-Round Beds (TH)`),
            `Total Year-Round Beds (SH)` = mean(`Total Year-Round Beds (SH)`),
            `Total Year-Round Beds (OPH)` = mean(`Total Year-Round Beds (OPH)`),
            `Total Year-Round Beds (PSH)` = mean(`Total Year-Round Beds (PSH)`),
            `Total Year-Round Beds (RRH)` = mean(`Total Year-Round Beds (RRH)`),
            total_population = sum(total_pop),
            AWATER = sum(AWATER),
            ALAND = sum(ALAND),
            disadvantaged_county = mean(disadvantaged_county),
            persistent_poverty_county = mean(persistent_poverty_county),
            percent_rural = sum(percent_rural*ALAND)/sum(ALAND),
            percent_urban = sum(percent_urban*ALAND)/sum(ALAND),
            percent_poc = sum(percent_poc*total_pop)/sum(total_pop),
            acs_popest = sum(popest),
            pct_amer_indian_ak_native = sum(amer_indian_ak_native)/sum(popest),
            pct_asian = sum(asian)/sum(popest),
            pct_black = sum(black)/sum(popest),
            pct_multi_racial = sum(multi_racial)/sum(popest),
            pct_native_hi_other_pi = sum(native_hi_other_pi)/sum(popest),
            pct_other_race = sum(other_race)/sum(popest),
            pct_white = sum(white)/sum(popest),
            pct_male = sum(male)/sum(popest),
            pct_female = sum(female)/sum(popest),
            pct_hispanic_latinx = sum(hispaniclatinx)/sum(popest),
            pct_non_hispanic_latinx = sum(non_hispaniclatinx)/sum(popest),
            # est_amer_indian_ak_native = sum(amer_indian_ak_native),
            # est_asian = sum(asian),
            # est_black = sum(black),
            # est_multi_racial = sum(multi_racial),
            # est_native_hi_other_pi = sum(native_hi_other_pi),
            # est_other_race = sum(other_race),
            # est_white = sum(white),
            # est_male = sum(male),
            # est_female = sum(female),
            # est_hispanic_latinx = sum(hispaniclatinx),
            # est_non_hispanic_latinx = sum(non_hispaniclatinx),
            poverty_rate = sum(poverty_rate*total_pop)/sum(total_pop),
            pop_density = sum(total_pop)/sum(ALAND),
            med_hh_income = mean(sum(med_hh_income*total_pop)/sum(total_pop)),
            employment_access_index = mean(sum(employment_access_index*total_pop)/sum(total_pop)),
            housing_cost_burden = mean(sum(housing_cost_burden*total_pop)/sum(total_pop)),
            overcrowded_housing = sum(housing_units*total_pop*overcrowded_housing)/sum(housing_units*total_pop),
            vacancy_rate = sum(housing_units*total_pop*vacancy_rate)/sum(housing_units*total_pop),
            incomplete_plumbing = sum(incomplete_plumbing*total_pop)/sum(total_pop),
            incomplete_kitchen = sum(incomplete_kitchen*total_pop)/sum(total_pop),
            housing_units = sum(housing_units*total_pop)/sum(total_pop),
            permits = sum(permits*total_pop)/sum(total_pop),
            ind_hud_202 = sum(hud_202*total_pop)/sum(total_pop),
            ind_hud_hcv = sum(hud_hcv*total_pop)/sum(total_pop),
            ind_hud_pbs8 = sum(hud_pbs8*total_pop)/sum(total_pop),
            ind_hud_ph = sum(hud_ph*total_pop)/sum(total_pop),
            ind_hud_housing = sum(hud_hcv*total_pop + hud_ph*total_pop)/sum(total_pop),
            ind_hud_all = sum(hud_hcv*total_pop + hud_ph*total_pop + hud_pbs8*total_pop + hud_202*total_pop)/sum(total_pop),
            capacity_housing = sum(capacity_housing*total_pop)/sum(total_pop),
            capacity_environment = sum(capacity_environment*total_pop)/sum(total_pop),
            capacity_transport = sum(capacity_transport*total_pop)/sum(total_pop),
            cnw_500 = sum(cnw_500*ALAND)/sum(ALAND),
            contaminated_sites = sum(contaminated_sites*total_pop)/sum(total_pop),
            stations_elevated = sum(stations_elevated*total_pop)/sum(total_pop),
            stations_subway = sum(stations_subway*total_pop)/sum(total_pop),
            airports_primary = sum(airports_primary*total_pop)/sum(total_pop),
            docks = sum(docks*total_pop)/sum(AWATER),
            transport_trade_jobs = sum(transport_trade_jobs*total_pop)/sum(total_pop),
            rail_transit_km = sum(rail_transit_km*total_pop)/sum(ALAND),
            highway_miles = sum(highway_miles*total_pop*ALAND)/sum(ALAND),
            pct_under_18 = sum(under18)/sum(popest),
            pct_18_to_24 = sum(age18to24)/sum(popest),
            pct_25_to_34 = sum(age25to34)/sum(popest),
            pct_35_to_44 = sum(age35to44)/sum(popest),
            pct_45_to_54 = sum(age45to54)/sum(popest),
            pct_55_to_64 = sum(age55to64)/sum(popest),
            pct_over_64 = sum(over65)/sum(popest),
            # est_under_18 = sum(under18),
            # est_18_to_24 = sum(age18to24),
            # est_25_to_34 = sum(age25to34),
            # est_35_to_44 = sum(age35to44),
            # est_45_to_54 = sum(age45to54),
            # est_55_to_64 = sum(age55to64),
            # est_over_64 = sum(over65)/sum(popest),
            median_age = mean(sum(median_age*total_pop)/sum(total_pop)),
            fnd_per_impov = sum(funding_tot)/sum(poverty_rate*total_population),
            fnd_per_person = sum(funding_tot)/sum(total_population),
            fnd_per_hmls = sum(funding_tot)/mean(`Overall Homeless`),
            homeless_rate = mean(`Overall Homeless`)/sum(total_population),
            coc = sum(coc),
            cdbg_entitlement = sum(cdbg_entitlement),
            elderly = sum(elderly),
            # grrp_comp = sum(grrp_comp),
            # grrp_elements = sum(grrp_elements),
            # grrp_leading = sum(grrp_leading),
            hcv = sum(hcv),
            home = sum(home),
            hud_disability = sum(hud_disability),
            hud_esg = sum(hud_esg),
            hud_hopwa = sum(hud_hopwa),
            public_hsg = sum(public_hsg),
            public_hsg_cap = sum(public_hsg_cap),
            s8_project = sum(s8_project),
            funding_tot = sum(funding_tot),

            
            
  )




write.csv(cocdat, file = "~/Data and Society/data_and_society/coc_data.csv")




us_county_v2 <- us_county |>
  mutate(county = str_replace(county, " County", ""),
         county = str_replace(county, " Bourough", ""),
         county = str_replace(county, " city", ""),
         county = str_replace(county, " Parish", ""))




### Pulling the shapefiles

GIS <- sf::st_read("CoC_GIS_National_Boundary.gdb")

stupid_simple_GIS <- GIS |>
  rmapshaper::ms_simplify(keep = 0.001, keep_shapes = FALSE)

stupid_simple_GIS |>
  ggplot() + 
  geom_sf()



hud_gis <- stupid_simple_GIS |>
  right_join(cocdat, by = c("COCNUM" = "coc_num"))

# write.csv(hud_gis, file = "~/Data and Society/data_and_society/coc_data_with_GIS.csv")

# write_csv(hud_gis, file = "~/Data and Society/data_and_society/coc_data_with_GIS.csv")

# sf::st_write(stupid_simple_GIS, "~/Data and Society/data_and_society/stupid_simple_GIS.shp")

# sf::st_write(stupid_simple_GIS, "~/Data and Society/data_and_society/stupid_simple_GIS.gdb")


?st_write.data.frame
### Notes:
# Regional access point <- look this up (something to do with coordinated entry)
# Balance of state CoC is managed by department of commerce for each state


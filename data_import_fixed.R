library(tidyverse)
library(readxl)
library(sf)
library(mapview)
library(broom)


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




program2 <- cind_cut |>
  left_join(cpro_cut, by=c("GEOID" = "GEOID"))

program2 <- program2 |>
  mutate(fips_state = str_extract(GEOID, "[0-9]{1,2}"))


program2$GEOID <- as.character(program2$GEOID)







### I FINALLY have the goddamned CoC to County table. Working on merging the data here.

cnt_to_coc <- read_csv("County-to-CoC.csv")

cnt_to_coc <- cnt_to_coc |>
  rename(geoCode = GEOID)

cnt_to_coc$`County Name`[2610] <- "Doña Ana County"


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
                 grrp_comp +
                 grrp_elements +
                 grrp_leading +
                 home +
                 hud_disability +
                 hud_esg +
                 hud_hopwa +
                 public_hsg +
                 public_hsg_cap +
                 s8_project)

write.csv(cdat, file = "~/Data and Society/data_and_society/county_data_take2.csv")









colnames(cdat)


### Now to group by coc

cocdat <- cdat |>
  group_by(coc_num, coc_name, `CoC Category`, `Count Types`, fips_state) |>
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
            `Overall Homeless - Asian or Asian American` = mean(`Overall Homeless - Asian or Asian American`),
            `Overall Homeless - Black, African American, or African` = mean(`Overall Homeless - Black, African American, or African`),
            `Overall Homeless - Middle Eastern or North African` = mean(`Overall Homeless - Middle Eastern or North African`),
            `Overall Homeless - White` = mean(`Overall Homeless - White`),
            `Overall Homeless - Native Hawaiian or Other Pacific Islander` = mean(`Overall Homeless - Native Hawaiian or Other Pacific Islander`),
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
            `Total Year-Round Beds (ES)...6` = mean(`Total Year-Round Beds (ES)...6`),
            `Total Year-Round Beds (TH)...7` = mean(`Total Year-Round Beds (TH)...7`),
            `Total Year-Round Beds (SH)...8` = mean(`Total Year-Round Beds (SH)...8`),
            `Total Year-Round Beds (OPH)` = mean(`Total Year-Round Beds (OPH)`),
            `Total Year-Round Beds (PSH)` = mean(`Total Year-Round Beds (PSH)`),
            `Total Year-Round Beds (RRH)` = mean(`Total Year-Round Beds (RRH)`),
            total_population = sum(total_pop),
            AWATER = sum(AWATER),
            ALAND = sum(ALAND),
            disadvantaged_county = mean(disadvantaged_county),
            persistent_poverty_county = mean(persistent_poverty_county),
            percent_rural = sum(percent_rural*total_pop)/sum(total_pop),
            percent_urban = sum(percent_urban*total_pop)/sum(total_pop),
            percent_poc = sum(percent_poc*total_pop)/sum(total_pop),
            poverty_rate = sum(poverty_rate*total_pop)/sum(total_pop),
            pop_density = sum(total_pop)/sum(ALAND),
            med_hh_income = mean(med_hh_income),
            employment_access_index = mean(employment_access_index),
            housing_cost_burden = mean(housing_cost_burden),
            overcrowded_housing = sum(housing_units*total_pop*overcrowded_housing)/sum(housing_units*total_pop),
            vacancy_rate = sum(housing_units*total_pop*vacancy_rate)/sum(housing_units*total_pop),
            incomplete_plumbing = sum(incomplete_plumbing*total_pop)/sum(total_pop),
            incomplete_kitchen = sum(incomplete_kitchen*total_pop)/sum(total_pop),
            housing_units = sum(housing_units*total_pop)/sum(total_pop),
            permits = sum(permits*total_pop)/sum(total_pop),
            capacity_housing = sum(capacity_housing*total_pop)/sum(total_pop),
            age_under_18 = sum(age_under_18*total_pop)/sum(total_pop),
            age_over_64 = sum(age_over_64*total_pop)/sum(total_pop),
            coc = sum(coc),
            cdbg_entitlement = sum(cdbg_entitlement),
            elderly = sum(elderly),
            grrp_comp = sum(grrp_comp),
            grrp_elements = sum(grrp_elements),
            grrp_leading = sum(grrp_leading),
            hcv = sum(hcv),
            home = sum(home),
            hud_202 = sum(hud_202),
            hud_disability = sum(hud_disability),
            hud_esg = sum(hud_esg),
            hud_hcv = sum(hud_hcv),
            hud_hopwa = sum(hud_hopwa),
            hud_pbs8 = sum(hud_pbs8),
            hud_ph = sum(hud_ph),
            public_hsg = sum(public_hsg),
            public_hsg_cap = sum(public_hsg_cap),
            s8_project = sum(s8_project),
            funding_tot = sum(funding_tot)
            
            
  )




write.csv(cocdat, file = "~/Data and Society/data_and_society/coc_data.csv")




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

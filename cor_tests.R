
cor_coc <- cdat |>
  group_by(coc_num) |>
  summarise(`Overall Homeless - Under 18` = mean(`Overall Homeless - Under 18`),
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
                        est_amer_indian_ak_native = sum(amer_indian_ak_native),
                        est_asian = sum(asian),
                        est_black = sum(black),
                        est_multi_racial = sum(multi_racial),
                        est_native_hi_other_pi = sum(native_hi_other_pi),
                        est_other_race = sum(other_race),
                        est_white = sum(white),
                        est_male = sum(male),
                        est_female = sum(female),
                        est_hispanic_latinx = sum(hispaniclatinx),
                        est_non_hispanic_latinx = sum(non_hispaniclatinx),
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
                        capacity_housing = sum(capacity_housing*total_pop)/sum(total_pop),
                        capacity_environment = sum(capacity_environment*total_pop)/sum(total_pop),
                        capacity_transport = sum(capacity_transport*total_pop)/sum(total_pop),
                        cnw_500 = sum(cnw_500*ALAND)/sum(ALAND),
                        contaminated_sites = sum(contaminated_sites*total_pop)/sum(total_pop),
                        stations_elevated = sum(stations_elevated*total_pop)/sum(total_pop),
                        stations_subway = sum(stations_subway*total_pop)/sum(total_pop),
                        airports_primary = sum(airports_primary*total_pop)/sum(total_pop),
                        docks = sum(docks*total_pop)/sum(AWATER),
                        # transport_trade_jobs = sum(transport_trade_jobs*total_pop)/sum(total_pop),
                        rail_transit_km = sum(rail_transit_km*total_pop)/sum(ALAND),
                        highway_miles = sum(highway_miles*total_pop*ALAND)/sum(ALAND),
                        pct_under_18 = sum(under18)/sum(popest),
                        pct_18_to_24 = sum(age18to24)/sum(popest),
                        pct_25_to_34 = sum(age25to34)/sum(popest),
                        pct_35_to_44 = sum(age35to44)/sum(popest),
                        pct_45_to_54 = sum(age45to54)/sum(popest),
                        pct_55_to_64 = sum(age55to64)/sum(popest),
                        pct_over_64 = sum(over65)/sum(popest),
                        est_under_18 = sum(under18),
                        est_18_to_24 = sum(age18to24),
                        est_25_to_34 = sum(age25to34),
                        est_35_to_44 = sum(age35to44),
                        est_45_to_54 = sum(age45to54),
                        est_55_to_64 = sum(age55to64),
                        est_over_64 = sum(over65)/sum(popest),
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
                        s8_project = sum(s8_project)
  )


# check for NAs

cor_coc <- cor_coc[-c(371), ]

colSums(is.na(cor_coc))
temp <- filter(cor_coc, rowSums(is.na(cor_coc))>0)

cor_coc <- drop_na(cor_coc)





# make sure everything is numeric

sapply(cor_coc, class)

cor_coc <- cor_coc |>
  select(-coc_num)





# Correlation testing variables for fnd_per_person

d <- data.frame()
d <- data.frame(sapply(names(cor_coc), function(x) 
  cor.test(cor_coc[[x]], cor_coc$ind_hud_housing, use="pairwise.complete.obs")))

d <- data.frame(t(d))


pov <- data.frame()
pov <- data.frame(sapply(names(cor_coc), function(x) 
  cor.test(cor_coc[[x]], cor_coc$pop_density, use="pairwise.complete.obs")))

pov <- data.frame(t(pov))


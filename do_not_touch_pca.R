library(tidyverse)
library(plotly)
library(supernova)
library(car)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)
library(Metrics)
library(pls)

cdat <- read_csv("county_data_take2.csv")

cocstuff4pca <- cdat |>
  group_by(coc_num) |>
  summarise(total_population = sum(total_pop),
            total_homeless = mean(`Overall Homeless`),
            homeless_rate = mean(`Overall Homeless`)/sum(total_pop),
            hmls_AIAN = mean(`Overall Homeless - American Indian, Alaska Native, or Indigenous`)/mean(`Overall Homeless`),
            hmls_asian = mean(`Overall Homeless - Asian or Asian American`)/mean(`Overall Homeless`),
            hmls_black = mean(`Overall Homeless - Black, African American, or African`)/mean(`Overall Homeless`),
            hmls_multi = mean(`Overall Homeless - Multi-Racial`)/mean(`Overall Homeless`),
            hmls_NHPI = mean(`Overall Homeless - Native Hawaiian or Other Pacific Islander`)/mean(`Overall Homeless`),
            hmls_white = mean(`Overall Homeless - White` + `Overall Homeless - Middle Eastern or North African`)/mean(`Overall Homeless`),
            hmls_under_18 = mean(`Overall Homeless - Under 18`)/mean(`Overall Homeless`),
            hmls_18_to_24 = mean(`Overall Homeless - Age 18 to 24`)/mean(`Overall Homeless`),
            hmls_25_to_34 = mean(`Overall Homeless - Age 25 to 34`)/mean(`Overall Homeless`),
            hmls_35_to_44 = mean(`Overall Homeless - Age 35 to 44`)/mean(`Overall Homeless`),
            hmls_45_to_54 = mean(`Overall Homeless - Age 45 to 54`)/mean(`Overall Homeless`),
            hmls_55_to_64 = mean(`Overall Homeless - Age 55 to 64`)/mean(`Overall Homeless`),
            hmls_over_64 = mean(`Overall Homeless - Over 64`)/mean(`Overall Homeless`),
            hmls_female = mean(`Overall Homeless - Woman`)/mean(`Overall Homeless`),
            hmls_male = mean(`Overall Homeless - Man`)/mean(`Overall Homeless`),
            hmls_trans = mean(`Overall Homeless - Transgender`)/mean(`Overall Homeless`),
            hmls_nonbi = mean(`Overall Homeless - Non Binary`)/mean(`Overall Homeless`),
            hmls_cultural_id = mean(`Overall Homeless - Culturally Specific Identity`)/mean(`Overall Homeless`),
            hmls_gender_questioning = mean(`Overall Homeless - Gender Questioning`)/mean(`Overall Homeless`),
            hmls_more_than_1_gend = mean(`Overall Homeless - More Than One Gender`)/mean(`Overall Homeless`),
            hmls_other_gend = mean(`Overall Homeless - Different Identity`)/mean(`Overall Homeless`),
            hmls_hisp_latx = mean(`Overall Homeless - Hispanic/Latina/e/o`)/mean(`Overall Homeless`),
            hmls_non_hisp_latx = mean(`Overall Homeless - Non-Hispanic/Latina/e/o`)/mean(`Overall Homeless`),
            chronic_hmls = mean(`Overall Chronically Homeless`)/mean(`Overall Homeless`),
            veteran_hmls = mean(`Overall Homeless Veterans`)/mean(`Overall Homeless`),
            unaccompanied_yth_hmls = mean(`Overall Homeless Unaccompanied Youth (Under 25)`)/mean(`Overall Homeless`),
            parenting_yth_hmls = mean(`Overall Homeless Parenting Youth (Under 25)`)/mean(`Overall Homeless`),
            individual_hmls = mean(`Overall Homeless Individuals`)/mean(`Overall Homeless`),
            ppl_in_families_hmls = mean(`Overall Homeless People in Families`)/mean(`Overall Homeless`),
            pop_AIAN = sum(amer_indian_ak_native)/sum(popest - other_race),
            pop_asian = sum(asian)/sum(popest - other_race),
            pop_black = sum(black)/sum(popest - other_race),
            pop_multi = sum(multi_racial)/sum(popest - other_race),
            pop_NHPI = sum(native_hi_other_pi)/sum(popest - other_race),
            pop_white = sum(white)/sum(popest - other_race),
            pop_under_18 = sum(under18)/sum(popest),
            pop_18_to_24 = sum(age18to24)/sum(popest),
            pop_25_to_34 = sum(age25to34)/sum(popest),
            pop_35_to_44 = sum(age35to44)/sum(popest),
            pop_45_to_54 = sum(age45to54)/sum(popest),
            pop_55_to_64 = sum(age55to64)/sum(popest),
            pop_over_64 = sum(over65)/sum(popest),
            pop_female = sum(female)/sum(popest),
            pop_male = sum(male)/sum(popest),
            pop_hisp_latx = sum(hispaniclatinx)/sum(popest),
            pop_non_hisp_latx = sum(non_hispaniclatinx)/sum(popest),
            AWATER = sum(AWATER),
            ALAND = sum(ALAND),
            disadvantaged_county = mean(disadvantaged_county),
            persistent_poverty_county = mean(persistent_poverty_county),
            percent_rural = sum(percent_rural*ALAND)/sum(ALAND),
            percent_urban = sum(percent_urban*ALAND)/sum(ALAND),
            percent_poc = sum(percent_poc*total_pop)/sum(total_pop),
            acs_popest = sum(popest),
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
            capacity_housing = sum(capacity_housing*total_pop)/sum(total_pop),
            capacity_environment = sum(capacity_environment*total_pop)/sum(total_pop),
            capacity_transport = sum(capacity_transport*total_pop)/sum(total_pop),
            cnw_500 = sum(cnw_500*ALAND)/sum(ALAND),
            contaminated_sites = sum(contaminated_sites*total_pop)/sum(total_pop),
            stations_elevated = sum(stations_elevated*total_pop)/sum(total_pop),
            stations_subway = sum(stations_subway*total_pop)/sum(total_pop),
            airports_primary = sum(airports_primary*total_pop)/sum(total_pop),
            docks = sum(docks*total_pop)/sum(AWATER),
            rail_transit_km = sum(rail_transit_km*total_pop)/sum(ALAND),
            transport_trade_jobs = sum(transport_trade_jobs*total_pop)/sum(total_pop),
            highway_miles = sum(highway_miles*total_pop*ALAND)/sum(ALAND),
            median_age = mean(sum(median_age*total_pop)/sum(total_pop)),
            hcv = sum(hcv),
            hud_esg = sum(hud_esg),
            public_hsg = sum(public_hsg),
            funding_tot = sum(funding_tot),
            fnd_per_person = sum(funding_tot)/sum(total_pop),
            fnd_per_impov = sum(funding_tot)/sum(total_pop*poverty_rate),
            fnd_per_hmls = sum(funding_tot)/sum(`Overall Homeless`)
  )


# check for NAs

cocstuff4pca <- cocstuff4pca[-c(371), ]

colSums(is.na(cocstuff4pca))

cocstuff4pca <- drop_na(cocstuff4pca)





# make sure everything is numeric

sapply(cocstuff4pca, class)

too_many_cocs <- cocstuff4pca

cocstuff4pca <- cocstuff4pca |>
  select(-coc_num)





# Correlation testing variables for fnd_per_person

list_data <- data.frame()

list_data <- data.frame(sapply(names(cocstuff4pca), function(x) 
  cor.test(cocstuff4pca[[x]], cocstuff4pca$fnd_per_person, use="pairwise.complete.obs")))

data_list <- data.frame()
data_list <- data.frame(t(list_data))



# estimates of more than |0.20| (for predicting fnd_per_person)


###### This one

coc_pca3 <- cocstuff4pca |>
  select(pop_density,
         employment_access_index,
         rail_transit_km,
         pop_black,
         # docks,
         # stations_subway,
         poverty_rate,
         # stations_elevated,
         capacity_transport,
         # permits,
         pop_AIAN,
         # pop_asian,
         pop_white,
         pop_under_18,
         # pop_25_to_34,
         # pop_35_to_44,
         # pop_55_to_64,
         pop_over_64,
         # ALAND,
         # percent_rural,
         # percent_poc,
         # pop_female,
         # incomplete_kitchen,
         transport_trade_jobs)

out_coc3 <- princomp(coc_pca3)

summary(out_coc3)

out_coc3$loadings[, 1:3]




fviz_eig(out_coc3, addlabels = TRUE)

fviz_pca_var(out_coc3, col.var = "black")

fviz_cos2(out_coc3, choice = "var", axes = 1:2)

fviz_pca_var(out_coc3, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)



barplot(out_coc3$loadings[,1], main = "")
barplot(out_coc3$loadings[,2], main = "")
barplot(out_coc3$loadings[,3], main = "")
barplot(out_coc3$loadings[,4], main = "")


help3 <- lm(fnd_per_person ~ out_coc3$scores[, 1:3], data = cocstuff4pca)

summary(help3)
supernova(help3)










# for predicting homeless_rate
cor_hmls <- data.frame()

cor_hmls <- data.frame(sapply(names(cocstuff4pca), function(x) 
  cor.test(cocstuff4pca[[x]], cocstuff4pca$homeless_rate, use="pairwise.complete.obs")))

cor_hmls <- data.frame(t(cor_hmls))

# estimates of more than |0.10| (for predicting homeless_rate)

coc_pca4 <- cocstuff4pca |>
  select(pop_AIAN,
         pop_asian,
         pop_black,
         pop_multi,
         pop_NHPI,
         pop_white,
         pop_under_18,
         pop_25_to_34,
         pop_35_to_44,
         pop_45_to_54,
         pop_55_to_64,
         pop_over_64,
         pop_male,
         pop_hisp_latx,
         poverty_rate,
         pop_density,
         housing_cost_burden,
         overcrowded_housing,
         # incomplete_plumbing,
         cnw_500,
         # contaminated_sites,
         # stations_subway
  )


out_coc4 <- princomp(coc_pca4)

summary(out_coc4)

out_coc4$loadings[, 1:6]




fviz_eig(out_coc4, addlabels = TRUE)

fviz_pca_var(out_coc4, col.var = "black")

fviz_cos2(out_coc4, choice = "var", axes = 1:2)

fviz_pca_var(out_coc4, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)



barplot(out_coc4$loadings[,1], main = "")      # any latinx (non-white)
barplot(out_coc4$loadings[,2], main = "")      # latinx (non-black)
barplot(out_coc4$loadings[,3], main = "")      # no asian, yes black, yes white, yes hispanic, poverty, high housing cost burden... south?
barplot(out_coc4$loadings[,4], main = "")      # low housing cost burden, young, latinx, not asian
barplot(out_coc4$loadings[,5], main = "")      # high poverty rate (not black, not white, young, near water(ish))
barplot(out_coc4$loadings[,6], main = "")      # old, near water, low housing cost burden, american indian, impoverished







help4 <- lm(homeless_rate ~ out_coc4$scores[, 1:6], data = cocstuff4pca)

summary(help4)
supernova(help4)

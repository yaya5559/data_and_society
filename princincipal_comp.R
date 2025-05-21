library(tidyverse)
library(plotly)
library(supernova)
library(car)
library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)



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

cocstuff4pca <- cocstuff4pca |>
  select(-coc_num)







# select variables you want

coc_pca1 <- cocstuff4pca |>
  select(med_hh_income,
         housing_units,
         # capacity_housing,
         employment_access_index,
         overcrowded_housing,
         # housing_cost_burden,
         vacancy_rate,
         poverty_rate,
         percent_poc,
         # pop_hisp_latx,
         pop_white,
         pop_density,
         # incomplete_kitchen,
         # incomplete_plumbing,
         #homeless_rate,
         # chronic_hmls,
         # ppl_in_families_hmls,
         # disadvantaged_county,
         # persistent_poverty_county,
         median_age,
         # pop_under_18,
         pop_over_64)





# standardize/normalize everything

coc_pca1 <- scale(coc_pca1)







# actually do principal components analysis

out_coc <- princomp(coc_pca1)

summary(out_coc)

out_coc$loadings[, 1:4]


fviz_eig(out_coc, addlabels = TRUE)

fviz_pca_var(out_coc, col.var = "black")

fviz_cos2(out_coc, choice = "var", axes = 1:2)

fviz_pca_var(out_coc, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)


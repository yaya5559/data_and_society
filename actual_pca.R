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
  group_by(coc_num, 
           # coc_name
  ) |>
  summarise(total_population = sum(total_pop),
            total_homeless = mean(`Overall Homeless`),
            homeless_rate = mean(`Overall Homeless`)/sum(total_pop),
            fnd_per_person = sum(funding_tot)/sum(total_pop),
            # hmls_AIAN = mean(`Overall Homeless - American Indian, Alaska Native, or Indigenous`)/mean(`Overall Homeless`),
            # hmls_asian = mean(`Overall Homeless - Asian or Asian American`)/mean(`Overall Homeless`),
            # hmls_black = mean(`Overall Homeless - Black, African American, or African`)/mean(`Overall Homeless`),
            # hmls_multi = mean(`Overall Homeless - Multi-Racial`)/mean(`Overall Homeless`),
            # hmls_NHPI = mean(`Overall Homeless - Native Hawaiian or Other Pacific Islander`)/mean(`Overall Homeless`),
            # hmls_white = mean(`Overall Homeless - White` + `Overall Homeless - Middle Eastern or North African`)/mean(`Overall Homeless`),
            # hmls_under_18 = mean(`Overall Homeless - Under 18`)/mean(`Overall Homeless`),
            # hmls_18_to_24 = mean(`Overall Homeless - Age 18 to 24`)/mean(`Overall Homeless`),
            # hmls_25_to_34 = mean(`Overall Homeless - Age 25 to 34`)/mean(`Overall Homeless`),
            # hmls_35_to_44 = mean(`Overall Homeless - Age 35 to 44`)/mean(`Overall Homeless`),
            # hmls_45_to_54 = mean(`Overall Homeless - Age 45 to 54`)/mean(`Overall Homeless`),
            # hmls_55_to_64 = mean(`Overall Homeless - Age 55 to 64`)/mean(`Overall Homeless`),
            # hmls_over_64 = mean(`Overall Homeless - Over 64`)/mean(`Overall Homeless`),
            # hmls_female = mean(`Overall Homeless - Woman`)/mean(`Overall Homeless`),
            # hmls_male = mean(`Overall Homeless - Man`)/mean(`Overall Homeless`),
            # hmls_trans = mean(`Overall Homeless - Transgender`)/mean(`Overall Homeless`),
            # hmls_nonbi = mean(`Overall Homeless - Non Binary`)/mean(`Overall Homeless`),
            # hmls_cultural_id = mean(`Overall Homeless - Culturally Specific Identity`)/mean(`Overall Homeless`),
            # hmls_gender_questioning = mean(`Overall Homeless - Gender Questioning`)/mean(`Overall Homeless`),
            # hmls_more_than_1_gend = mean(`Overall Homeless - More Than One Gender`)/mean(`Overall Homeless`),
            # hmls_other_gend = mean(`Overall Homeless - Different Identity`)/mean(`Overall Homeless`),
            # hmls_hisp_latx = mean(`Overall Homeless - Hispanic/Latina/e/o`)/mean(`Overall Homeless`),
            # hmls_non_hisp_latx = mean(`Overall Homeless - Non-Hispanic/Latina/e/o`)/mean(`Overall Homeless`),
            # chronic_hmls = mean(`Overall Chronically Homeless`)/mean(`Overall Homeless`),
            # veteran_hmls = mean(`Overall Homeless Veterans`)/mean(`Overall Homeless`),
            # unaccompanied_yth_hmls = mean(`Overall Homeless Unaccompanied Youth (Under 25)`)/mean(`Overall Homeless`),
            # parenting_yth_hmls = mean(`Overall Homeless Parenting Youth (Under 25)`)/mean(`Overall Homeless`),
            # individual_hmls = mean(`Overall Homeless Individuals`)/mean(`Overall Homeless`),
            # ppl_in_families_hmls = mean(`Overall Homeless People in Families`)/mean(`Overall Homeless`),
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
            # AWATER = sum(AWATER),
            # ALAND = sum(ALAND),
            disadvantaged_county = mean(disadvantaged_county),
            persistent_poverty_county = mean(persistent_poverty_county),
            percent_rural = sum(percent_rural*ALAND)/sum(ALAND),
            percent_urban = sum(percent_urban*ALAND)/sum(ALAND),
            percent_poc = sum(percent_poc*total_pop)/sum(total_pop),
            # acs_popest = sum(popest),
            poverty_rate = sum(poverty_rate*total_pop)/sum(total_pop),
            pop_density = sum(total_pop)/sum(ALAND),
            med_hh_income = mean(sum(med_hh_income*total_pop)/sum(total_pop)),
            employment_access_index = mean(sum(employment_access_index*total_pop)/sum(total_pop)),
            housing_cost_burden = mean(sum(housing_cost_burden*total_pop)/sum(total_pop)),
            overcrowded_housing = sum(housing_units*total_pop*overcrowded_housing)/sum(housing_units*total_pop),
            vacancy_rate = sum(housing_units*total_pop*vacancy_rate)/sum(housing_units*total_pop),
            # incomplete_plumbing = sum(incomplete_plumbing*total_pop)/sum(total_pop),
            # incomplete_kitchen = sum(incomplete_kitchen*total_pop)/sum(total_pop),
            housing_units = sum(housing_units*total_pop)/sum(total_pop),
            # permits = sum(permits*total_pop)/sum(total_pop),
            # capacity_housing = sum(capacity_housing*total_pop)/sum(total_pop),
            # capacity_environment = sum(capacity_environment*total_pop)/sum(total_pop),
            # capacity_transport = sum(capacity_transport*total_pop)/sum(total_pop),
            # cnw_500 = sum(cnw_500*ALAND)/sum(ALAND),
            # contaminated_sites = sum(contaminated_sites*total_pop)/sum(total_pop),
            # stations_elevated = sum(stations_elevated*total_pop)/sum(total_pop),
            # stations_subway = sum(stations_subway*total_pop)/sum(total_pop),
            airports_primary = sum(airports_primary*total_pop)/sum(total_pop),
            # docks = sum(docks*total_pop)/sum(AWATER),
            # rail_transit_km = sum(rail_transit_km*total_pop)/sum(ALAND),
            # transport_trade_jobs = sum(transport_trade_jobs*total_pop)/sum(total_pop),
            highway_miles = sum(highway_miles*total_pop*ALAND)/sum(ALAND),
            # median_age = mean(sum(median_age*total_pop)/sum(total_pop)),
            # hcv = sum(hcv),
            # hud_esg = sum(hud_esg),
            # public_hsg = sum(public_hsg),
            funding_tot = sum(funding_tot),
            fnd_per_impov = sum(funding_tot)/sum(total_pop*poverty_rate),
            fnd_per_hmls = sum(funding_tot)/sum(`Overall Homeless`)
  )


# check for NAs

cocstuff4pca <- cocstuff4pca[-c(371), ]

colSums(is.na(cocstuff4pca))
temp <- filter(cocstuff4pca, rowSums(is.na(cocstuff4pca))>0)

cocstuff4pca <- drop_na(cocstuff4pca)





# make sure everything is numeric

sapply(cocstuff4pca, class)

too_many_cocs <- cocstuff4pca
ffff <- cocstuff4pca

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

coc_pca5 <- cocstuff4pca |>
  select(
    med_hh_income,
    percent_poc,
    pop_hisp_latx,
    pop_under_18,
    pop_over_64,
    pop_density,
    employment_access_index,
    poverty_rate,
    housing_cost_burden,
    overcrowded_housing,
    housing_units,
  )

# standardize/normalize everything
coc_pca5 <- scale(coc_pca5)

out_coc5 <- princomp(coc_pca5)

summary(out_coc5)

out_coc5$loadings[, 1:6]




fviz_eig(out_coc5, addlabels = TRUE)

fviz_pca_var(out_coc5, col.var = "black")

fviz_cos2(out_coc5, choice = "var", axes = 1:2)

fviz_pca_var(out_coc5, col.var = "cos2",
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)

fviz_pca_var(out_coc5, col.var = "cos2",
             axes = c(3,4),
             gradient.cols = c("black", "orange", "green"),
             repel = TRUE)



barplot(out_coc5$loadings[,1], main = "")    # High = low hsng units, not old, latinx, poc, high hcb, low = many hsng units, white, not latx, young          shorthand: urban/poc
barplot(out_coc5$loadings[,2], main = "")    # High = high hh income, children, low poverty, good jobs, low = fewer children, high poverty/hcb
barplot(out_coc5$loadings[,3], main = "")    # High = low pop density, lots of kids, poor jobs, low = few kids, old people, dense, accessible jobs, plentiful hsng
barplot(out_coc5$loadings[,4], main = "")    # High = high hh income, latx, not poverty, rural, old, low = dense pop, impoverished, high hcb
barplot(out_coc5$loadings[,5], main = "")    # High = High housing cost burden, not overcrowded, (suburb) low = High hcb, overcrowded
barplot(out_coc5$loadings[,6], main = "")    # High = not latinx, middle-aged, overcrowded, low = latinx, not middle aged, not overcrowded

barplot(out_coc5$loadings[,7], main = "")    # High = poc, lots of housing, not a lot of old people/children, low = not poc, few hsng units, old people/kids



help9 <- lm(fnd_per_person ~ out_coc5$scores[, 1:6], data = cocstuff4pca)
help8 <- lm(homeless_rate ~ out_coc5$scores[, 1:6], data = cocstuff4pca)

summary(help8)

summary(help9)
supernova(help9)


ggplot(cocstuff4pca) +
  geom_jitter(aes(x = out_coc5$scores[,1], y = fnd_per_person)) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format(prefix = "$"))

ggplot(cocstuff4pca) +
  geom_jitter(aes(x = out_coc5$scores[,2], y = fnd_per_person)) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format(prefix = "$"))

ggplot(cocstuff4pca) +
  geom_jitter(aes(x = out_coc5$scores[,3], y = fnd_per_person)) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format(prefix = "$"))

ggplot(cocstuff4pca) +
  geom_jitter(aes(x = out_coc5$scores[,4], y = fnd_per_person)) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format(prefix = "$"))

ggplot(cocstuff4pca) +
  geom_jitter(aes(x = out_coc5$scores[,5], y = fnd_per_person)) +
  scale_x_continuous(labels = scales::comma_format()) +
  scale_y_continuous(labels = scales::comma_format(prefix = "$"))


too_many_cocs$pc1 <- out_coc5$scores[,1]
too_many_cocs$pc2 <- out_coc5$scores[,2]
too_many_cocs$pc3 <- out_coc5$scores[,3]
too_many_cocs$pc4 <- out_coc5$scores[,4]
too_many_cocs$pc5 <- out_coc5$scores[,5]
too_many_cocs$pc6 <- out_coc5$scores[,6]
too_many_cocs$pc7 <- out_coc5$scores[,7]




vvv <- stupid_simple_GIS |>
  left_join(too_many_cocs, by = c("COCNUM" = "coc_num"))

vvv <- vvv |>
  left_join(coc_data, by = c("COCNUM" = "coc_num"))

vvv <- sf::st_cast(vvv, "MULTIPOLYGON")

write_csv(vvv, file = "~/Data and Society/data_and_society/vvv.csv")
write_csv(cocstuff4pca, file = "~/Data and Society/data_and_society/cocstuff4pca.csv")
write_csv(too_many_cocs, file = "~/Data and Society/data_and_society/too_many_cocs.csv")


#pc maps

y1 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc1,
              text = paste(coc_name,
                           '<br>Pc1 score: ', round(pc1, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f03b20", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc1 score")

ggplotly(y1, tooltip = "text") |>
  style(hoveron = "fills") 


y2 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc2,
              text = paste(coc_name,
                           '<br>Pc2 score: ', round(pc2, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f03b20", "#fd8d3c", "#feb24c", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc2 score",
                       # limits = c(-5,5)
  )

ggplotly(y2, tooltip = "text") |>
  style(hoveron = "fills") 


y3 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc3,
              text = paste(coc_name,
                           '<br>Pc3 score: ', round(pc3, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#ffffcc", "#feb24c", "#fd8d3c", "#f03b20"),
                       name = "Pc3 score")

ggplotly(y3, tooltip = "text") |>
  style(hoveron = "fills")


y4 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc4,
              text = paste(coc_name,
                           '<br>Pc4 score: ', round(pc4, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#7fcdbb", "#a1dab4", "#ffffcc", "#feb24c", "#f03b20"),
                       name = "Pc4 score")

ggplotly(y4, tooltip = "text") |>
  style(hoveron = "fills")


y5 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc5,
              text = paste(coc_name,
                           '<br>Pc5 score: ', round(pc5, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f3330c", "#fd8d3c", "#fecc5c", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc5 score")

ggplotly(y5, tooltip = "text") |>
  style(hoveron = "fills")



y6 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc6,
              text = paste(coc_name,
                           '<br>Pc6 score: ', round(pc6, digits = 3)))) +
  scale_fill_gradientn(colours = c("#225ea8", "#41b6c4", "#ffffcc", "#fecc5c", "#fd8d3c", "#f3330c"),
                       name = "Pc6 score")

ggplotly(y6, tooltip = "text") |>
  style(hoveron = "fills")


y7 <- vvv |>
  filter(ST_1 != "AK", ST_1 != "HI", ST_1 != "PR", ST_1 != "GU", ST_1 != "VI", ST_1 != "MP") |>
  ggplot() +
  geom_sf(aes(fill = pc7,
              text = paste(coc_name,
                           '<br>Pc7 score: ', round(pc7, digits = 3)))) +
  scale_fill_gradientn(colours = c("#f3330c", "#fecc5c", "#ffffcc", "#41b6c4", "#225ea8"),
                       name = "Pc7 score")

ggplotly(y7, tooltip = "text") |>
  style(hoveron = "fills")

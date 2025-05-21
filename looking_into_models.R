library(tidyverse)
library(supernova)
library(car)



race <- lm(funding_tot ~ pct_black + pct_asian + pct_white + pct_amer_indian_ak_native + pct_native_hi_other_pi + pct_multi_racial + total_population, data = hud_gis)

summary(race)

supernova(race)



race2 <- lm(fnd_per_person ~ pct_black + pct_asian + pct_white + pct_amer_indian_ak_native + pct_native_hi_other_pi + pct_multi_racial, data = hud_gis)

summary(race2)

supernova(race2)

vif(race2)





homelessrate <- lm(homeless_rate ~ fnd_per_hmls, data = hud_gis)

summary(homelessrate)

supernova(homelessrate)






income_impov <- lm(fnd_per_impov ~ med_hh_income + housing_units + capacity_housing + employment_access_index + overcrowded_housing + vacancy_rate + poverty_rate + percent_poc + pop_density + incomplete_plumbing + incomplete_kitchen, data = hud_gis)

summary(income_impov)

supernova(income_impov)






income_pop <- lm(fnd_per_person ~ med_hh_income + housing_units + housing_cost_burden + employment_access_index + overcrowded_housing + vacancy_rate + poverty_rate + percent_poc + pop_density + incomplete_kitchen + homeless_rate + incomplete_plumbing, data = hud_gis)

summary(income_pop)

supernova(income_pop)



# car::vif (variance inflation factor) suggests multicolinearity (anything over 5 is tipping into multi-co-lin)
# look into priciple components analysis

vif(income_impov)

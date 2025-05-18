library(tidyverse)
library(supernova)

race <- lm(funding_tot ~ pct_black + pct_asian + pct_white + pct_amer_indian_ak_native + pct_native_hi_other_pi + pct_multi_racial + total_population, data = hud_gis)

summary(race)

supernova(race)





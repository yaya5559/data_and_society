library(tidyverse)
library(supernova)

coc_data <- read_csv("coc_data.csv")

hud_gis |>
  ggplot() +
  geom_density(aes(x = pct_amer_indian_ak_native)) +
  geom_density(aes(x = `Overall Homeless - American Indian, Alaska Native, or Indigenous`/`Overall Homeless`), color = "red") +
  scale_x_continuous(labels = scales::percent_format())


hud_gis |>
  ggplot() +
  geom_density(aes(x = pct_white)) +
  geom_density(aes(x = (`Overall Homeless - White` + `Overall Homeless - Middle Eastern or North African`)/`Overall Homeless`), color = "red") +
  scale_x_continuous(labels = scales::percent_format())



hud_gis |>
  ggplot() +
  geom_density(aes(x = pct_black)) +
  geom_density(aes(x = `Overall Homeless - Black, African American, or African`/`Overall Homeless`), color = "red") +
  scale_x_continuous(labels = scales::percent_format())



### Are the log versions better or continuous?

hud_gis |>
  ggplot() +
  geom_histogram(aes(x = pct_black), fill = "navy") +
  geom_histogram(aes(x = `Overall Homeless - Black, African American, or African`/`Overall Homeless`), fill = "red3", alpha = 0.5) +
  scale_x_continuous(labels = scales::percent_format())

hud_gis |>
  ggplot() +
  geom_histogram(aes(x = pct_black), fill = "navy") +
  geom_histogram(aes(x = `Overall Homeless - Black, African American, or African`/`Overall Homeless`), fill = "red3", alpha = 0.5) +
  scale_x_log10(labels = scales::percent_format())



hud_gis |>
  ggplot() +
  geom_density(aes(x = pct_homeless)) +
  scale_x_log10(labels = scales::percent_format())





hud_gis <- hud_gis[-c(375, 376, 377, 378), ]
coc_data <- coc_data[-c(375, 376, 377, 378), ]

hud_gis <- hud_gis |>
  mutate(pct_homeless = `Overall Homeless`/total_population)


sd(hud_gis$pct_homeless)





shorterrrr <- coc_data |>
  summarise(total_population = sum(total_population), 
            total_homeless = sum(`Overall Homeless`), 
            amer_indian_ak_native = sum(`Overall Homeless - American Indian, Alaska Native, or Indigenous`)/sum(`Overall Homeless`),
            asian = sum(`Overall Homeless - Asian or Asian American`)/sum(`Overall Homeless`),
            black = sum(`Overall Homeless - Black, African American, or African`)/sum(`Overall Homeless`),
            multi_racial = sum(`Overall Homeless - Multi-Racial`)/sum(`Overall Homeless`),
            native_hi_other_pi = sum(`Overall Homeless - Native Hawaiian or Other Pacific Islander`)/sum(`Overall Homeless`),
            white = sum(`Overall Homeless - White` + `Overall Homeless - Middle Eastern or North African`)/sum(`Overall Homeless`)
  )

shorterrrr <- shorterrrr |>
  pivot_longer(cols = c(amer_indian_ak_native,asian,black,multi_racial,native_hi_other_pi,white), names_to = "race", values_to = "hmls_pct")

shugggg <- coc_data |>
  summarise( amer_indian_ak_native = sum(est_amer_indian_ak_native)/sum(acs_popest - est_other_race),
             asian = sum(est_asian)/sum(acs_popest - est_other_race),
             black = sum(est_black)/sum(acs_popest - est_other_race),
             multi_racial = sum(est_multi_racial)/sum(acs_popest - est_other_race),
             native_hi_other_pi = sum(est_native_hi_other_pi)/sum(acs_popest - est_other_race),
             white = sum(est_white)/sum(acs_popest - est_other_race))

shugggg <- shugggg |>
  pivot_longer(cols = c(amer_indian_ak_native,asian,black,multi_racial,native_hi_other_pi,white), names_to = "race", values_to = "pop_pct")

thing2 <- shugggg |>
  left_join(shorterrrr, by = c("race" = "race")) |>
  pivot_longer(cols = c(pop_pct,hmls_pct), names_to = "stat", values_to = "vals")

thing2$abc <- c("b","b","c","c","e","e","d","d","a","a","f","f")

thing2 <- thing2 |>
  arrange(abc)

race_labels <- c("Navive Hawaiian/Other Pacific Islander", 
  "American Indian/Alaska Native", 
  "Asian or Asian American", 
  "Multi-Racial", 
  "Black, African American, or African", 
  "White")

thing2 |>
  ggplot() +
  geom_col(aes(x = abc, y = vals, fill = stat), position = position_dodge2(preserve = "single")) +
  scale_fill_discrete(type = c("orange2", "blue4"),
                      labels = c("Homeless", "Overall"),
                      name = "Population") +
  scale_x_discrete(labels = race_labels) +
  theme(axis.text.x = element_text(angle = -45, hjust = 0)) +
  scale_y_continuous(labels = percent_format()) +
  labs(title = "Racial Breakdown of Overall Population and Homeless Subpopulation", 
       x = "Race (2020 census desgignations)", 
       y = "Proportion of Population")

  


# Trying to compare standard deviations
# Take the gap between outcome and expected outcome and find the avg of those for standard deviations
  
another <- cdat |>
  group_by(coc_num) |>
  summarise(total_population = sum(total_pop),
            total_homeless = mean(`Overall Homeless`),
            homeless_rate = mean(`Overall Homeless`)/sum(total_pop),
            hmls_null = (mean(`Overall Homeless`)/6)/sum(total_pop),
            hmls_AIAN = mean(`Overall Homeless - American Indian, Alaska Native, or Indigenous`)/sum(total_pop),
            hmls_asian = mean(`Overall Homeless - Asian or Asian American`)/sum(total_pop),
            hmls_black = mean(`Overall Homeless - Black, African American, or African`)/sum(total_pop),
            hmls_multi = mean(`Overall Homeless - Multi-Racial`)/sum(total_pop),
            hmls_NHPI = mean(`Overall Homeless - Native Hawaiian or Other Pacific Islander`)/sum(total_pop),
            hmls_white = mean(`Overall Homeless - White` + `Overall Homeless - Middle Eastern or North African`)/sum(total_pop),
            pop_AIAN = sum(amer_indian_ak_native)/sum(popest - other_race),
            pop_asian = sum(asian)/sum(popest - other_race),
            pop_black = sum(black)/sum(popest - other_race),
            pop_multi = sum(multi_racial)/sum(popest - other_race),
            pop_NHPI = sum(native_hi_other_pi)/sum(popest - other_race),
            pop_white = sum(white)/sum(popest - other_race),
            null_hmls_AIAN = pop_AIAN * homeless_rate,
            null_hmls_asian = pop_asian * homeless_rate,
            null_hmls_black = pop_black * homeless_rate,
            null_hmls_multi = pop_multi * homeless_rate,
            null_hmls_NHPI = pop_NHPI * homeless_rate,
            null_hmls_white = pop_white * homeless_rate
            )

another <- another[-c(371), ]
  
another |>
  ggplot() +
  geom_density(aes(x = hmls_white)) +
  geom_density(aes(x = null_hmls_white), color = "red") +
  scale_x_log10(labels = scales::percent_format())

# gap = avg outcome for racial group - scores for racial group if equitable

another <- another |>
  mutate(gap_AIAN = hmls_AIAN - null_hmls_AIAN,
         gap_asian = hmls_asian - null_hmls_asian,
         gap_black = hmls_black - null_hmls_black,
         gap_multi = hmls_multi - null_hmls_multi,
         gap_NHPI = hmls_NHPI - null_hmls_NHPI,
         gap_white = hmls_white - null_hmls_white
         )

again <- another |>
  summarise(sd_AIAN = sqrt(sum(gap_AIAN^2)/length(another)),
            sd_asian = sqrt(sum(gap_asian^2)/length(another)),
            sd_black = sqrt(sum(gap_black^2)/length(another)),
            sd_multi = sqrt(sum(gap_multi^2)/length(another)),
            sd_NHPI = sqrt(sum(gap_NHPI^2)/length(another)),
            sd_white = sqrt(sum(gap_white^2)/length(another)),
            )

another <- another |>
  mutate(sd_gap_AIAN = gap_AIAN/again$sd_AIAN[1],
         sd_gap_asian = gap_asian/again$sd_asian[1],
         sd_gap_black = gap_black/again$sd_black[1],
         sd_gap_multi = gap_multi/again$sd_multi[1],
         sd_gap_NHPI = gap_NHPI/again$sd_NHPI[1],
         sd_gap_white = gap_white/again$sd_white[1])

smthn_to_graph <- another |>
  summarise(aIAN = median(sd_gap_AIAN),
            asian = median(sd_gap_asian),
            black = median(sd_gap_black),
            multi = median(sd_gap_multi),
            nHPI = median(sd_gap_NHPI),
            white = median(sd_gap_white))

smthn_to_graph <- smthn_to_graph |> 
  pivot_longer(cols = c(aIAN, asian, black, multi, nHPI, white), names_to = "race", values_to = "median_sd")

sht <- another |>
  summarise(aIAN = mean(sd_gap_AIAN),
            asian = mean(sd_gap_asian),
            black = mean(sd_gap_black),
            multi = mean(sd_gap_multi),
            nHPI = mean(sd_gap_NHPI),
            white = mean(sd_gap_white))

sht <- sht |> 
  pivot_longer(cols = c(aIAN, asian, black, multi, nHPI, white), names_to = "race", values_to = "mean_sd")



another |>
  ggplot() +
  geom_density(aes(x = sd_gap_AIAN)) +
  geom_density(aes(x = sd_gap_asian), color = "red") +
  geom_density(aes(x = sd_gap_black), color = "blue") +
  geom_density(aes(x = sd_gap_multi), color = "darkgreen") +
  geom_density(aes(x = sd_gap_NHPI), color = "purple") +
  geom_density(aes(x = sd_gap_white), color = "orange") + 
  scale_x_log10(labels = scales::comma_format()) +
  labs(title = "Gap (Homeless% - Population%) in standard deviations", x = "Gap size in standard deviations")

another |>
  ggplot() +
  geom_density(aes(x = sd_gap_AIAN))

# Plot for gap between race homeless % of pop - expected race homeless % (sd)
smthn_to_graph |>
  ggplot() + 
  geom_col(aes(x = race, y = -median_sd, fill = race))


sht |>
  ggplot() + 
  geom_col(aes(x = race, y = -mean_sd, fill = race))






# Trying same thing as ooo but with entire fractions of population

againnnn <- another |>
  summarise(aIAN = median(gap_AIAN),
            asian = median(gap_asian),
            black = median(gap_black),
            multi = median(gap_multi),
            nHPI = median(gap_NHPI),
            white = median(gap_white))

againnnn <- againnnn |> 
  pivot_longer(cols = c(aIAN, asian, black, multi, nHPI, white), names_to = "race", values_to = "median_scale_of_diff")

aaaagain <- another |>
  summarise(aIAN = mean(gap_AIAN),
            asian = mean(gap_asian),
            black = mean(gap_black),
            multi = mean(gap_multi),
            nHPI = mean(gap_NHPI),
            white = mean(gap_white))

aaaagain <- aaaagain |> 
  pivot_longer(cols = c(aIAN, asian, black, multi, nHPI, white), names_to = "race", values_to = "mean_scale_of_diff")


againnnn |>
  ggplot() + 
  geom_col(aes(x = race, y = -median_scale_of_diff, fill = race))

aaaagain |>
  ggplot() + 
  geom_col(aes(x = race, y = -mean_scale_of_diff, fill = race))






# Same thing but with larger population percents

ccc <- cdat |>
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
            pop_AIAN = sum(amer_indian_ak_native)/sum(popest - other_race),
            pop_asian = sum(asian)/sum(popest - other_race),
            pop_black = sum(black)/sum(popest - other_race),
            pop_multi = sum(multi_racial)/sum(popest - other_race),
            pop_NHPI = sum(native_hi_other_pi)/sum(popest - other_race),
            pop_white = sum(white)/sum(popest - other_race)
  )

ccc <- ccc[-c(371), ]

ccc <- ccc |>
  mutate(gap_AIAN = hmls_AIAN - pop_AIAN,
         gap_asian = hmls_asian - pop_asian,
         gap_black = hmls_black - pop_black,
         gap_multi = hmls_multi - pop_multi,
         gap_NHPI = hmls_NHPI - pop_NHPI,
         gap_white = hmls_white - pop_white
  )
           
ddd <- ccc |>
  summarise(sd_AIAN = sqrt(sum(gap_AIAN^2)/length(ccc)),
            sd_asian = sqrt(sum(gap_asian^2)/length(ccc)),
            sd_black = sqrt(sum(gap_black^2)/length(ccc)),
            sd_multi = sqrt(sum(gap_multi^2)/length(ccc)),
            sd_NHPI = sqrt(sum(gap_NHPI^2)/length(ccc)),
            sd_white = sqrt(sum(gap_white^2)/length(ccc)),
  )

ccc <- ccc |>
  mutate(sd_gap_AIAN = gap_AIAN/ddd$sd_AIAN[1],
         sd_gap_asian = gap_asian/ddd$sd_asian[1],
         sd_gap_black = gap_black/ddd$sd_black[1],
         sd_gap_multi = gap_multi/ddd$sd_multi[1],
         sd_gap_NHPI = gap_NHPI/ddd$sd_NHPI[1],
         sd_gap_white = gap_white/ddd$sd_white[1])

xxx <- ccc |>
  summarise(aIAN = median(sd_gap_AIAN),
            asian = median(sd_gap_asian),
            black = median(sd_gap_black),
            multi = median(sd_gap_multi),
            nHPI = median(sd_gap_NHPI),
            white = median(sd_gap_white))

xxx <- xxx |> 
  pivot_longer(cols = c(aIAN, asian, black, multi, nHPI, white), names_to = "race", values_to = "median_sd")

yyy <- ccc |>
  summarise(aIAN = mean(sd_gap_AIAN),
            asian = mean(sd_gap_asian),
            black = mean(sd_gap_black),
            multi = mean(sd_gap_multi),
            nHPI = mean(sd_gap_NHPI),
            white = mean(sd_gap_white))

yyy <- yyy |> 
  pivot_longer(cols = c(aIAN, asian, black, multi, nHPI, white), names_to = "race", values_to = "mean_sd")



zzz <- ccc |> 
  select(coc_num, sd_gap_AIAN, sd_gap_asian, sd_gap_black, sd_gap_multi, sd_gap_NHPI, sd_gap_white) |>
  pivot_longer(cols = c(sd_gap_AIAN, sd_gap_asian, sd_gap_black, sd_gap_multi, sd_gap_NHPI, sd_gap_white), names_to = "race", values_to = "sd_gap")

# Plot for larger percents: homeless race % - expected race % (sd)
xxx |>
  ggplot() + 
  geom_col(aes(x = race, y = -median_sd, fill = race))

yyy |>
  ggplot() + 
  geom_col(aes(x = race, y = -mean_sd, fill = race))


zzz |>
  ggplot() +
  geom_density(aes(x = sd_gap, color = race)) + 
  scale_x_continuous(labels = scales::comma_format(),
                     limits = c(-1,1)) +
  labs(title = "Gap (Homeless% - Population%) in standard deviations", x = "Gap size in standard deviations")





ccc |>
  ggplot() +
  geom_density(aes(x = hmls_white)) +
  geom_density(aes(x = pop_white), color = "orange") +
  scale_x_continuous(labels = scales::percent_format())

ccc |>
  ggplot() +
  geom_density(aes(x = hmls_AIAN)) +
  geom_density(aes(x = pop_AIAN), color = "red") +
  scale_x_continuous(label = scales::percent_format())

ccc |>
  ggplot() +
  geom_density(aes(x = hmls_black)) +
  geom_density(aes(x = pop_black), color = "blue") +
  scale_x_continuous(label = scales::percent_format())

ccc |>
  ggplot() +
  geom_density(aes(x = hmls_asian)) +
  geom_density(aes(x = pop_asian), color = "turquoise") +
  scale_x_continuous(label = scales::percent_format())

ccc |>
  ggplot() +
  geom_density(aes(x = hmls_multi)) +
  geom_density(aes(x = pop_multi), color = "green3") +
  scale_x_continuous(label = scales::percent_format())

ccc |>
  ggplot() +
  geom_density(aes(x = hmls_NHPI)) +
  geom_density(aes(x = pop_NHPI), color = "skyblue1") +
  scale_x_continuous(label = scales::percent_format())












# Trying to make scaled gap with percent of homeless population and percent of overall population (I like this less than xxx and yyy)

ooo <- cdat |>
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
            pop_AIAN = sum(amer_indian_ak_native)/sum(popest - other_race),
            pop_asian = sum(asian)/sum(popest - other_race),
            pop_black = sum(black)/sum(popest - other_race),
            pop_multi = sum(multi_racial)/sum(popest - other_race),
            pop_NHPI = sum(native_hi_other_pi)/sum(popest - other_race),
            pop_white = sum(white)/sum(popest - other_race)
  )

ooo <- ooo[-c(371), ]

ooo <- ooo |>
  mutate(gap_AIAN = hmls_AIAN - pop_AIAN,
         gap_asian = hmls_asian - pop_asian,
         gap_black = hmls_black - pop_black,
         gap_multi = hmls_multi - pop_multi,
         gap_NHPI = hmls_NHPI - pop_NHPI,
         gap_white = hmls_white - pop_white
  )


# ooo$pop_NHPI[96] <- 0.00000000000000000000001
# ooo$pop_NHPI[143] <- 0.00000000000000000000001
# ooo$pop_NHPI[177] <- 0.00000000000000000000001

ooo <- ooo[-c(177,143,96), ]


ooo <- ooo |>
  mutate(gap_pct_AIAN = gap_AIAN/pop_AIAN,
         gap_pct_asian = gap_asian/pop_asian,
         gap_pct_black = gap_black/pop_black,
         gap_pct_multi = gap_multi/pop_multi,
         gap_pct_NHPI = gap_NHPI/pop_NHPI,
         gap_pct_white = gap_white/pop_white)

ppp <- ooo |>
  summarise(aIAN = median(gap_pct_AIAN),
            asian = median(gap_pct_asian),
            black = median(gap_pct_black),
            multi = median(gap_pct_multi),
            nHPI = median(gap_pct_NHPI),
            white = median(gap_pct_white))

ppp <- ppp |> 
  pivot_longer(cols = c(aIAN, asian, black, multi, nHPI, white), names_to = "race", values_to = "median_scale_of_diff")

ppp <- ppp |> 
  mutate(race_better = case_when(race == "aIAN" ~ "American Indian/Alaska Native",
                                 race == "asian" ~ "Asian",
                                 race == "black" ~ "Black, African American, or African",
                                 race == "multi" ~ "Multi-Racial",
                                 race == "nHPI" ~ "Native Hawaiian/Other Pacific Islander",
                                 race == "white" ~ "White"))

ppp$race_better <- factor(ppp$race_better, levels = c("Native Hawaiian/Other Pacific Islander", 
                                                      "Black, African American, or African", 
                                                      "American Indian/Alaska Native", 
                                                      "White", 
                                                      "Multi-Racial", 
                                                      "Asian"))

write_csv(ppp, file = "~/Data and Society/data_and_society/ppp.csv")




qqq <- ooo |>
  summarise(aIAN = mean(gap_pct_AIAN),
            asian = mean(gap_pct_asian),
            black = mean(gap_pct_black),
            multi = mean(gap_pct_multi),
            nHPI = mean(gap_pct_NHPI),
            white = mean(gap_pct_white))

qqq <- qqq |> 
  pivot_longer(cols = c(aIAN, asian, black, multi, nHPI, white), names_to = "race", values_to = "mean_scale_of_diff")

# plots (arrange by value)
ppp |>
  ggplot() + 
  geom_col(aes(x = race_better, y = median_scale_of_diff, fill = race)) +
  theme(axis.text.x = element_text(vjust = .5)) +
  scale_x_discrete(labels = c("Native Hawaiian/\nOther Pacific\nIslander",
                            "Black, African\nAmerican, or\nAfrican",
                            "American Indian/\nAlaska Native",
                            "White",
                            "Multi-Racial",
                            "Asian"),
                   name = "Race Group") +
  scale_y_continuous(name = "Median Outcome Gap") +
  labs(title = "Racial Disparities In Homelessness",
       subtitle = "Gap is how many times more likely a given group is to be homeless than average homelessness rate for whole population") +
  scale_fill_discrete(guide = FALSE)


qqq |>
  ggplot() + 
  geom_col(aes(x = race, y = mean_scale_of_diff, fill = race))




# Add legends, explain density, make things clear

ooo |>
  ggplot() +
  geom_density(aes(x = hmls_white)) +
  geom_density(aes(x = pop_white), color = "orange") +
  scale_x_continuous(labels = scales::percent_format())


# lower line at same point means there's more observations elsewhere

ooo |>
  ggplot() +
  geom_density(aes(x = hmls_AIAN)) +
  geom_density(aes(x = pop_AIAN), color = "red") +
  scale_x_continuous(label = scales::percent_format(),
                     limits = c(0,0.1))


?geom_density

ooo |>
  ggplot() +
  geom_density(aes(x = hmls_black)) +
  geom_density(aes(x = pop_black), color = "blue") +
  scale_x_continuous(label = scales::percent_format())

ooo |>
  ggplot() +
  geom_density(aes(x = hmls_asian)) +
  geom_density(aes(x = pop_asian), color = "turquoise") +
  scale_x_continuous(label = scales::percent_format())

ooo |>
  ggplot() +
  geom_density(aes(x = hmls_multi)) +
  geom_density(aes(x = pop_multi), color = "green3") +
  scale_x_continuous(label = scales::percent_format())

ooo |>
  ggplot() +
  geom_density(aes(x = hmls_NHPI)) +
  geom_density(aes(x = pop_NHPI), color = "skyblue1") +
  scale_x_continuous(label = scales::percent_format(),
                     limits = c(0,0.015))



ooo |>
  ggplot() +
  geom_density(aes(x = hmls_NHPI)) +
  geom_density(aes(x = pop_NHPI), color = "skyblue1") +
  scale_x_log10(label = scales::percent_format())



ooo |>
  ggplot() +
  geom_histogram(aes(x = gap_pct_NHPI)) +
  scale_x_continuous(labels = scales::percent_format())



hmmmm <- ooo |>
  select(coc_num, total_population, total_homeless, hmls_NHPI, pop_NHPI, gap_NHPI, gap_pct_NHPI)


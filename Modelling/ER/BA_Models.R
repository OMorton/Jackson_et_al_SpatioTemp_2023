## global options settings
options(scipen = 999) ## remove scientific notation, namely "e"  to show very small and large numbers in full.
options(na.action = "na.pass")

## Set working directory and package location.
## Change this in yours
.libPaths("C:/Packages") ## Set up for working from home.
#setwd("G:/Other computers/My computer/PhD/Alice/Project")

library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)
library(ggpubr)


#### Read in data ####
Country_dat_full_vol_aves <- data.table::fread("Models/ER/Fitting_data/Exp_Vol_Aves.csv", na.strings = "")
Country_dat_full_vol_amph <- data.table::fread("Models/ER/Fitting_data/Exp_Vol_Amph.csv", na.strings = "")
Country_dat_full_vol_mam <- data.table::fread("Models/ER/Fitting_data/Exp_Vol_Mam.csv", na.strings = "")
Country_dat_full_vol_rept <- data.table::fread("Models/ER/Fitting_data/Exp_Vol_Rept.csv", na.strings = "")

#### Process the data ####
## Per taxonomic class:
## 1) Create binary for presence of Cat1 legislation.
## 2) Make new time series centred on the change point from not Cat1 to Cat1.
## 3) But remove countries that go from not assessed (P) to 1 as they could be 1 just not officially checked.
## 4) Remove countries that trade less than 5 years.
## 5) Make notes on whether a series is all 0 before or after a change to Cat1.

Aves_BA <- Country_dat_full_vol_aves %>% group_by(Country) %>%
  filter(n_distinct(Category)>1, any(Category == 1)) %>%
  ## this removes countries that only go straint from P -> 1
  filter(any(Category %in% c(2, 3)))  %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No")) %>%
  group_by(Country, Category1) %>%
  mutate(Time = ifelse(Category1 == "Yes", seq(from = 0, to = n()), seq(from = -n(), -1))) %>%
  select(-SYear) %>%
  group_by(Country) %>%
  mutate(Pres = ifelse(vol > 0, 1, 0)) %>%
  filter(sum(Pres) > 5) %>%
  group_by(Country, Category1) %>%
  mutate(Zeroes = case_when(sum(vol) <1 & Category1 == "No" ~ "Absent prior to law",
                            sum(vol) <1 & Category1 == "Yes" ~ "Absent after law",
                            TRUE ~ NA_character_))  %>%
  group_by(Country) %>% fill(Zeroes, .direction = "updown") %>%
  group_by(Country) %>% filter(sum(Category1 == "Yes") > 2, sum(Category1 == "No") > 2) %>%
  ungroup() %>%
  mutate(SYear = (as.numeric(as.character(Year))- mean(as.numeric(as.character(Year))))/sd(as.numeric(as.character(Year))),
         STime = Time - mean(Time)/sd(Time)) %>%
  ungroup()

Amph_BA <- Country_dat_full_vol_amph %>% group_by(Country) %>%
  filter(n_distinct(Category)>1, any(Category == 1)) %>%
  ## this removes countries that only go straint from P -> 1
  filter(any(Category %in% c(2, 3)))  %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No")) %>%
  group_by(Country, Category1) %>%
  mutate(Time = ifelse(Category1 == "Yes", seq(from = 0, to = n()), seq(from = -n(), -1))) %>%
  select(-SYear) %>%
  group_by(Country) %>%
  mutate(Pres = ifelse(vol > 0, 1, 0)) %>%
  filter(sum(Pres) > 5) %>%
  group_by(Country, Category1) %>%
  mutate(Zeroes = case_when(sum(vol) <1 & Category1 == "No" ~ "Absent prior to law",
                            sum(vol) <1 & Category1 == "Yes" ~ "Absent after law",
                            TRUE ~ NA_character_))  %>%
  group_by(Country) %>% fill(Zeroes, .direction = "updown") %>%
  group_by(Country) %>% filter(sum(Category1 == "Yes") > 2, sum(Category1 == "No") > 2) %>%
  ungroup() %>%
  mutate(SYear = (as.numeric(as.character(Year))- mean(as.numeric(as.character(Year))))/sd(as.numeric(as.character(Year))),
         STime = Time - mean(Time)/sd(Time)) %>%
  ungroup()

Mam_BA <- Country_dat_full_vol_mam %>% group_by(Country) %>%
  filter(n_distinct(Category)>1, any(Category == 1)) %>%
  ## this removes countries that only go straint from P -> 1
  filter(any(Category %in% c(2, 3)))  %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No")) %>%
  group_by(Country, Category1) %>%
  mutate(Time = ifelse(Category1 == "Yes", seq(from = 0, to = n()), seq(from = -n(), -1))) %>%
  select(-SYear) %>%
  group_by(Country) %>%
  mutate(Pres = ifelse(vol > 0, 1, 0)) %>%
  filter(sum(Pres) > 5) %>%
  group_by(Country, Category1) %>%
  mutate(Zeroes = case_when(sum(vol) <1 & Category1 == "No" ~ "Absent prior to law",
                            sum(vol) <1 & Category1 == "Yes" ~ "Absent after law",
                            TRUE ~ NA_character_))  %>%
  group_by(Country) %>% fill(Zeroes, .direction = "updown") %>%
  group_by(Country) %>% filter(sum(Category1 == "Yes") > 2, sum(Category1 == "No") > 2) %>%
  ungroup() %>%
  mutate(SYear = (as.numeric(as.character(Year))- mean(as.numeric(as.character(Year))))/sd(as.numeric(as.character(Year))),
         STime = Time - mean(Time)/sd(Time)) %>%
  ungroup()

Rept_BA <- Country_dat_full_vol_rept %>% group_by(Country) %>%
  filter(n_distinct(Category)>1, any(Category == 1)) %>%
  ## this removes countries that only go straint from P -> 1
  filter(any(Category %in% c(2, 3)))  %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No")) %>%
  group_by(Country, Category1) %>%
  mutate(Time = ifelse(Category1 == "Yes", seq(from = 0, to = n()), seq(from = -n(), -1))) %>%
  select(-SYear) %>%
  group_by(Country) %>%
  mutate(Pres = ifelse(vol > 0, 1, 0)) %>%
  filter(sum(Pres) > 5) %>%
  group_by(Country, Category1) %>%
  mutate(Zeroes = case_when(sum(vol) <1 & Category1 == "No" ~ "Absent prior to law",
                            sum(vol) <1 & Category1 == "Yes" ~ "Absent after law",
                            TRUE ~ NA_character_))  %>%
  group_by(Country) %>% fill(Zeroes, .direction = "updown") %>%
  group_by(Country) %>% filter(sum(Category1 == "Yes") > 2, sum(Category1 == "No") > 2) %>%
  ungroup() %>%
  mutate(SYear = (as.numeric(as.character(Year))- mean(as.numeric(as.character(Year))))/sd(as.numeric(as.character(Year))),
         STime = Time - mean(Time)/sd(Time)) %>%
  ungroup()

## Tally the number of countries.
length(unique(Aves_BA$Country)) ## 16
length(unique(Amph_BA$Country)) ## 2
length(unique(Mam_BA$Country)) ## 13
length(unique(Rept_BA$Country)) ## 16


#### Fit the models ####

## Models are fit in a hierachial framework, allowing the mean volume and trend to vary pre and post
## effective legislation indixed by country (as this is the scale of interest). To incorporate the variation through
## time attributed to random temporal shocks and fluctuations this is also indexed by a Year factor on the real scale 
## (2000 - 2020, where as time is zero centred).

Aves_Mod_BA <- brm(bf(vol ~ 1 + Time*Category1 + (1 + Time*Category1|Country) + (1|Year)), 
               family = negbinomial(), 
               sample_prior = TRUE,
               prior = c(
                 prior(normal(0,5), "Intercept"),
                 prior(normal(0,.5), "b"),
                 prior(normal(0,2), "sd")),
               data = Aves_BA,
               control = list(adapt_delta = .97),
               file = "BA_Models/Exports_ER/Aves_Exp_BA.rds",
               chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

Mam_Mod_BA <- brm(bf(vol ~ 1 + Time*Category1 + (1 + Time*Category1|Country) + (1|Year)), 
               family = negbinomial(), 
               sample_prior = TRUE,
               prior = c(
                 prior(normal(0,10), "Intercept"),
                 prior(normal(0,.5), "b"),
                 prior(normal(0,4), "sd")),
               data = Mam_BA,
               control = list(adapt_delta = .99, max_treedepth = 15),
               file = "BA_Models/Exports_ER/Mam_Exp_BA.rds",
               chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

Rept_Mod_BA <- brm(bf(vol ~ 1 + Time*Category1 + (1 + Time*Category1|Country) + (1|Year)), 
                  family = negbinomial(), 
                  sample_prior = TRUE,
                  prior = c(
                    prior(normal(0,10), "Intercept"),
                    prior(normal(0,.5), "b"),
                    prior(normal(0,4), "sd")),
                  data = Rept_BA,
                  control = list(adapt_delta = .99),
                  file = "BA_Models/Exports_ER/Rept_Exp_BA.rds",
                  chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

## Note for Amphs there are only 2 suitable countries so we fit these seperately as single level
## models, without random effects for Year.
Amph_MG_Mod_BA <- brm(bf(vol ~ 1 + Time*Category1), 
                   family = negbinomial(), 
                   sample_prior = TRUE,
                   prior = c(
                     prior(normal(0,10), "Intercept"),
                     prior(normal(0,.5), "b")),
                   data = filter(Amph_BA, Country == "Madagascar"),
                   #control = list(adapt_delta = .99),
                   file = "BA_Models/Exports_ER/Amph_Madagascar_Exp_BA.rds",
                   chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

Amph_GY_Mod_BA <- brm(bf(vol ~ 1 + Time*Category1), 
                      family = negbinomial(), 
                      sample_prior = TRUE,
                      prior = c(
                        prior(normal(0,10), "Intercept"),
                        prior(normal(0,.5), "b")),
                      data = filter(Amph_BA, Country == "Guyana"),
                      #control = list(adapt_delta = .99),
                      file = "BA_Models/Exports_ER/Amph_Guyana_Exp_BA.rds",
                      chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

C_Fit <- add_epred_draws(Mam_Mod_BA, newdata =  Mam_BA, re_formula = NULL, ndraws = 200)  %>% 
  group_by(Country, Time, Category1) %>% median_hdci(.epred, .width = .9)

ggplot(C_Fit, aes(Time, .epred, colour = Category1)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = NA) +
  geom_point(data = Mam_BA, aes(Time, vol)) +
  facet_wrap(~Country, scales = "free") 


#### Country posteriors ####

## Two parameters of interest:
## 1) Binary variable of legislation presence. A postive value here would denote in year 0 (when legislation occured) that trade
## increased.
## 2) Legislation presences interaction with time.  A negative value here would mean post change volumes decreased/increased slower 
## than was previouly observed under no legislation.
## NOTE - It is also possible for flat trends of pure zeroes either pre or post change these must be interpretted seperately. 
## As a decreasing trend transitioning to a flat trend would be a postive coeficient but that cant be interpretted as increasing trade
## time relationship through time (except relative to the previous decrease).

#### Aves ####
## Aves Cat
Aves_Cat_Sum <- coef(Aves_Mod_BA, summary = FALSE)$Country[,,"Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Country, PD) %>%
  median_hdci(value, .width = .9)

Aves_Cat_Sum_Pos <- Aves_Cat_Sum %>% filter(value > 0)
Aves_Cat_Sum_Neg <- Aves_Cat_Sum %>% filter(value < 0)

Aves_Cat_draws <- coef(Aves_Mod_BA, summary = FALSE)$Country[,,"Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100)

Aves_Country_T <- ggplot(Aves_Cat_draws, aes(value, Country)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Aves_Cat_Sum, aes(value, Country)) +
  geom_errorbarh(data = Aves_Cat_Sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Aves_Cat_Sum_Pos, aes(x =  .upper + 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Aves_Cat_Sum_Neg, aes(x =  .lower - 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-7, 6)) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab(expression(beta~"[Category 1 legislation]")) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

## Aves Cat:Year
Aves_CatTime_Sum <- coef(Aves_Mod_BA, summary = FALSE)$Country[,,"Time:Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Country, PD) %>%
  median_hdci(value, .width = .9)

Aves_CatTime_Sum_Pos <- Aves_CatTime_Sum %>% filter(value > 0)
Aves_CatTime_Sum_Neg <- Aves_CatTime_Sum %>% filter(value < 0)

Aves_CatTime_draws <- coef(Aves_Mod_BA, summary = FALSE)$Country[,,"Time:Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100)

Aves_Country_CT <- ggplot(Aves_CatTime_draws, aes(value, Country)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Aves_CatTime_Sum, aes(value, Country)) +
  geom_errorbarh(data = Aves_CatTime_Sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Aves_CatTime_Sum_Pos, aes(x =  .upper + 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Aves_CatTime_Sum_Neg, aes(x =  .lower - 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-2, 2)) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab(expression(beta~"Time:[Category 1 legislation]")) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

## Aves fixed
Aves_fixef_draw <- fixef(Aves_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Aves_fixef_sum <- fixef(Aves_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Par, PD) %>%
  median_hdci(value, .width = .9) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Aves_fixef_sum_Pos <- Aves_fixef_sum %>% filter(value > 0)
Aves_fixef_sum_Neg <- Aves_fixef_sum %>% filter(value < 0)

Aves_fixef_plt <- ggplot(Aves_fixef_draw, aes(value, Par)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Aves_fixef_sum, aes(value, Par)) +
  geom_errorbarh(data = Aves_fixef_sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Aves_fixef_sum_Neg, aes(x =  .lower - 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Aves_fixef_sum_Pos, aes(x =  .lower + 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-2, 1.2)) +
  scale_y_discrete(labels = c(expression(beta~"[Category 1 legislation]"), 
                              expression(beta~"Time:[Category 1 legislation]"))) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab("Fixed effect") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())


#### Mam ####
## Mam Cat
Mam_Cat_Sum <- coef(Mam_Mod_BA, summary = FALSE)$Country[,,"Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Country, PD) %>%
  median_hdci(value, .width = .9)

Mam_Cat_Sum_Pos <- Mam_Cat_Sum %>% filter(value > 0)
Mam_Cat_Sum_Neg <- Mam_Cat_Sum %>% filter(value < 0)

Mam_Cat_draws <- coef(Mam_Mod_BA, summary = FALSE)$Country[,,"Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100)

Mam_Country_T <- ggplot(Mam_Cat_draws, aes(value, Country)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Mam_Cat_Sum, aes(value, Country)) +
  geom_errorbarh(data = Mam_Cat_Sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Mam_Cat_Sum_Pos, aes(x =  .upper + 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Mam_Cat_Sum_Neg, aes(x =  .lower - 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-8, 5)) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab(expression(beta~"[Category 1 legislation]")) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

## Mam Cat:Year
Mam_CatTime_Sum <- coef(Mam_Mod_BA, summary = FALSE)$Country[,,"Time:Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Country, PD) %>%
  median_hdci(value, .width = .9)

Mam_CatTime_Sum_Pos <- Mam_CatTime_Sum %>% filter(value > 0)
Mam_CatTime_Sum_Neg <- Mam_CatTime_Sum %>% filter(value < 0)

Mam_CatTime_draws <- coef(Mam_Mod_BA, summary = FALSE)$Country[,,"Time:Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100)

Mam_Country_CT <- ggplot(Mam_CatTime_draws, aes(value, Country)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Mam_CatTime_Sum, aes(value, Country)) +
  geom_errorbarh(data = Mam_CatTime_Sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Mam_CatTime_Sum_Pos, aes(x =  .upper + 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Mam_CatTime_Sum_Neg, aes(x =  .lower - 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-2, 1.5)) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab(expression(beta~"Time:[Category 1 legislation]")) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

## Mam fixed
Mam_fixef_draw <- fixef(Mam_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Mam_fixef_sum <- fixef(Mam_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Par, PD) %>%
  median_hdci(value, .width = .9) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Mam_fixef_sum_Pos <- Mam_fixef_sum %>% filter(value > 0)
Mam_fixef_sum_Neg <- Mam_fixef_sum %>% filter(value < 0)

Mam_fixef_plt <- ggplot(Mam_fixef_draw, aes(value, Par)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Mam_fixef_sum, aes(value, Par)) +
  geom_errorbarh(data = Mam_fixef_sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Mam_fixef_sum_Neg, aes(x =  .lower - 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Mam_fixef_sum_Pos, aes(x =  .lower + 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-2, 1.2)) +
  scale_y_discrete(labels = c(expression(beta~"[Category 1 legislation]"), 
                              expression(beta~"Time:[Category 1 legislation]"))) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab("Fixed effect") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())


#### Rept ####
## Rept Cat
Rept_Cat_Sum <- coef(Rept_Mod_BA, summary = FALSE)$Country[,,"Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Country, PD) %>%
  median_hdci(value, .width = .9)

Rept_Cat_Sum_Pos <- Rept_Cat_Sum %>% filter(value > 0)
Rept_Cat_Sum_Neg <- Rept_Cat_Sum %>% filter(value < 0)

Rept_Cat_draws <- coef(Rept_Mod_BA, summary = FALSE)$Country[,,"Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100)

Rept_Country_T <- ggplot(Rept_Cat_draws, aes(value, Country)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Rept_Cat_Sum, aes(value, Country)) +
  geom_errorbarh(data = Rept_Cat_Sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Rept_Cat_Sum_Pos, aes(x =  .upper + 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Rept_Cat_Sum_Neg, aes(x =  .lower - 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-10, 6)) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab(expression(beta~"[Category 1 legislation]")) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

## Rept Cat:Year
Rept_CatTime_Sum <- coef(Rept_Mod_BA, summary = FALSE)$Country[,,"Time:Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Country, PD) %>%
  median_hdci(value, .width = .9)

Rept_CatTime_draws <- coef(Rept_Mod_BA, summary = FALSE)$Country[,,"Time:Category1Yes"] %>% as.data.frame() %>%
  pivot_longer(everything(),names_to = "Country") %>% 
  group_by(Country) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100)

Rept_Country_CT <- ggplot(Rept_CatTime_draws, aes(value, Country)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Rept_CatTime_Sum, aes(value, Country)) +
  geom_errorbarh(data = Rept_CatTime_Sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Rept_CatTime_Sum, aes(x =  .lower - 0.5, y = Country, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-2.5, 1)) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab(expression(beta~"Time:[Category 1 legislation]")) +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

## Rept fixed
Rept_fixef_draw <- fixef(Rept_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Rept_fixef_sum <- fixef(Rept_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Par, PD) %>%
  median_hdci(value, .width = .9) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Rept_fixef_plt <- ggplot(Rept_fixef_draw, aes(value, Par)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Rept_fixef_sum, aes(value, Par)) +
  geom_errorbarh(data = Rept_fixef_sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Rept_fixef_sum, aes(x =  .lower - 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-2.5, 1)) +
  scale_y_discrete(labels = c(expression(beta~"[Category 1 legislation]"), 
                              expression(beta~"Time:[Category 1 legislation]"))) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab("Fixed effect") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

#### Amphs MG ####
Amph_MG_fixef_draw <- fixef(Amph_MG_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Amph_MG_fixef_sum <- fixef(Amph_MG_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Par, PD) %>%
  median_hdci(value, .width = .9) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Amph_MG_fixef_sum_Pos <- Amph_MG_fixef_sum %>% filter(value > 0)
Amph_MG_fixef_sum_Neg <- Amph_MG_fixef_sum %>% filter(value < 0)

Amph_MG_fixef_plt <- ggplot(Amph_MG_fixef_draw, aes(value, Par)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Amph_MG_fixef_sum, aes(value, Par)) +
  geom_errorbarh(data = Amph_MG_fixef_sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Amph_MG_fixef_sum_Neg, aes(x =  .lower - 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Amph_MG_fixef_sum_Pos, aes(x =  .upper + 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-1.5, 1.5)) +
  scale_y_discrete(labels = c(expression(beta~"[Category 1 legislation]"), 
                              expression(beta~"Time:[Category 1 legislation]"))) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab("Fixed effects (Madagascar)") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

#### Amphs GY ####
Amph_GY_fixef_draw <- fixef(Amph_GY_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Amph_GY_fixef_sum <- fixef(Amph_GY_Mod_BA, summary = FALSE) %>% 
  as.data.frame() %>% 
  pivot_longer(everything(), names_to = "Par") %>% 
  group_by(Par) %>%
  mutate(PD = (sum(sign(value) == sign(median(value)))/n())*100,
         PD = round(PD, digits = 2)) %>%
  group_by(Par, PD) %>%
  median_hdci(value, .width = .9) %>%
  filter(Par %in% c("Category1Yes", "Time:Category1Yes"))

Amph_GY_fixef_sum_Pos <- Amph_GY_fixef_sum %>% filter(value > 0)
Amph_GY_fixef_sum_Neg <- Amph_GY_fixef_sum %>% filter(value < 0)

Amph_GY_fixef_plt <- ggplot(Amph_GY_fixef_draw, aes(value, Par)) +
  stat_slab(aes(fill = PD)) +
  geom_point(data = Amph_GY_fixef_sum, aes(value, Par)) +
  geom_errorbarh(data = Amph_GY_fixef_sum, aes(xmin = .lower, xmax = .upper), height = 0) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 1) +
  geom_text(data = Amph_GY_fixef_sum_Neg, aes(x =  .lower - 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  geom_text(data = Amph_GY_fixef_sum_Pos, aes(x =  .upper + 0.5, y = Par, label = PD), 
            nudge_y = .25, fontface = "bold") +
  coord_cartesian(xlim = c(-2.5, 2.5)) +
  scale_y_discrete(labels = c(expression(beta~"[Category 1 legislation]"), 
                              expression(beta~"Time:[Category 1 legislation]"))) +
  scale_fill_gradient(low = "grey90", high = "blue", limits = c(50, 100)) +
  ylab("Fixed effects (Guyana)") +
  xlab("Possible parameter values") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "none", legend.title = element_blank())

#### Write out data ####

## Aves 
write.csv(Aves_fixef_sum, "Outputs/SOM/BA/Aves_fixef_sum.csv", na = "")
write.csv(Aves_CatTime_Sum, "Outputs/SOM/BA/Aves_CatTime_sum.csv", na = "")
write.csv(Aves_Cat_Sum, "Outputs/SOM/BA/Aves_Cat_sum.csv", na = "")

## Mam 
write.csv(Mam_fixef_sum, "Outputs/SOM/BA/Mam_fixef_sum.csv", na = "")
write.csv(Mam_CatTime_Sum, "Outputs/SOM/BA/Mam_CatTime_sum.csv", na = "")
write.csv(Mam_Cat_Sum, "Outputs/SOM/BA/Mam_Cat_sum.csv", na = "")

## rept 
write.csv(Rept_fixef_sum, "Outputs/SOM/BA/Rept_fixef_sum.csv", na = "")
write.csv(Rept_CatTime_Sum, "Outputs/SOM/BA/Rept_CatTime_sum.csv", na = "")
write.csv(Rept_Cat_Sum, "Outputs/SOM/BA/Rept_Cat_sum.csv", na = "")

## Amph 
write.csv(Amph_MG_fixef_sum, "Outputs/SOM/BA/Rept_fixef_sum.csv", na = "")
write.csv(Amph_GY_fixef_sum, "Outputs/SOM/BA/Rept_fixef_sum.csv", na = "")

#### Arrangement ####

## Get images
library(png)
library(grid)
library(ggpubr)
Rept <-  rasterGrob(readPNG("Data/Misc/Varanus2.png"), interpolate = TRUE)
Amph <- rasterGrob(readPNG("Data/Misc/Dendrobates2.png"), interpolate = TRUE)
Bird <- rasterGrob(readPNG("Data/Misc/Amazona2.png"), interpolate = TRUE)
Mam <- rasterGrob(readPNG("Data/Misc/Lycaloplex2.png"), interpolate = TRUE)

## Aves
Aves_combi <- ggarrange(ggarrange(Aves_Country_T, Aves_Country_CT, nrow = 1, labels = c("A.", "B.")), Aves_fixef_plt, nrow = 2,
                        heights = c(1, .3), labels = c("", "C."))
Aves_final <- Aves_combi + annotation_custom(Bird, xmin = 0.9, xmax = 0.99, ymin = 0.15, ymax = .21)

## Mam
Mam_combi <- ggarrange(ggarrange(Mam_Country_T, Mam_Country_CT, nrow = 1, labels = c("A.", "B.")), Mam_fixef_plt, nrow = 2,
                        heights = c(1, .3), labels = c("", "C."))
Mam_final <- Mam_combi + annotation_custom(Mam, xmin = 0.9, xmax = 0.99, ymin = 0.15, ymax = .21)

## Rept
Rept_combi <- ggarrange(ggarrange(Rept_Country_T, Rept_Country_CT, nrow = 1, labels = c("A.", "B.")), Rept_fixef_plt, nrow = 2,
                        heights = c(1, .3), labels = c("", "C."))
Rept_final <- Rept_combi + annotation_custom(Rept, xmin = 0.9, xmax = 0.99, ymin = 0.14, ymax = .24)

## Amph
Amph_combi <- ggarrange(Amph_MG_fixef_plt, Amph_GY_fixef_plt, nrow = 2, labels = c("A.", "B."), align = "hv")
Amph_final <- Amph_combi + annotation_custom(Amph, xmin = 0.9, xmax = 0.99, ymin = 0.88, ymax = .97)

## Save
ggsave(path = "Outputs/SOM/BA", Aves_final, filename = "Aves_BA_ER.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")

ggsave(path = "Outputs/SOM/BA", Mam_final, filename = "Mam_BA_ER.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")

ggsave(path = "Outputs/SOM/BA", Rept_final, filename = "Rept_BA_ER.png",  bg = "white",
       device = "png", width = 25, height = 25, units = "cm")

ggsave(path = "Outputs/SOM/BA", Amph_final, filename = "Amph_BA_ER.png",  bg = "white",
       device = "png", width = 25, height = 20, units = "cm")

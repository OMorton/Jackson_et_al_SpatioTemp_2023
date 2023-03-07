#######################
#### Model Fitting ####
#######################

## Purpose
## Build up basic to full models for the CITES trade data.

## global options settings
options(scipen = 999) ## remove scientific notation, namely "e"  to show very small and large numbers in full.
options(na.action = "na.pass")

## Set working directory and package location.
## Change this in yours
.libPaths("C:/Packages") ## Set up for working from home.


library(tidyverse)
library(brms)
library(bayesplot)
library(tidybayes)

#### Data ####
## read in the exporter reported volumes
CITES_Vol <- data.table::fread("Data/All_Vertebrates/Cleaned/ER/CITES_Vert_Exporter_dat_ER.csv", na.strings = "")

NLP_dat <- data.table::fread("Data/NLP/Party_NLP_Full.csv", na.strings = "") %>%
  select(ISO, Year, Category) %>% mutate(Year = Year - 2000)

## Double checked here that all NA values are from years 2000 -2020 when the country was not a party to CITES
Check <- left_join(CITES_Vol, NLP_dat, by = c("Exporter" = "ISO", "Year" = "Year")) %>%
  filter(is.na(Category))


## Birds
CITES_Aves_vol <- CITES_Vol %>% filter(Class == "Aves")
## Mam
CITES_Mam_vol <- CITES_Vol %>% filter(Class == "Mammalia")
## Amph
CITES_Amph_vol <- CITES_Vol %>% filter(Class == "Amphibia")
## Rept
CITES_Rept_vol <- CITES_Vol %>% filter(Class == "Reptilia")

#### Preparation ####
#### Aves ####
## Summarise the data to total yearly volumes per country.
Country_dat_Aves <- CITES_Aves_vol %>% group_by(Exporter, Year, name, region, subregion) %>%
  summarise(vol = sum(n)) %>%
  filter(Year>-1) %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(name) %>% filter(sum(vol) > 0)

## Series length - Eritrea has been a cites party our whole series but only traded one species that had
## a shorter series, so we do want to expand that eritrea series to the full length.
backbone <- NLP_dat %>% select(ISO, Year, Category) %>% filter(ISO %in% unique(Country_dat_Aves$Exporter)) %>%
  rename(Exporter = 1, Year = 2)


Country_dat_full_vol_aves <- left_join(backbone, Country_dat_Aves) %>% 
  group_by(Exporter) %>% fill(name, .direction = "down") %>%
  fill(region, .direction = "down") %>%
  fill(subregion, .direction = "down") %>%
  ungroup() %>%
  mutate(vol = ifelse(is.na(vol), 0, vol)) %>%
  ## standardise year (z-score standardistation)
  ## This is not essential but can help convergence of models
  mutate(SYear = (Year - mean(Year))/sd(Year),
         ## round up any decimal volumes as traded woes are count data. Cant have 2.5 animals must be 3
         vol = ceiling(vol),
         Year = as.factor(Year)) %>%
  rename(Country = name)

length(unique(Country_dat_full_vol_aves$Exporter)) ## 114
Country_dat_full_vol_aves  %>% filter(is.na(Country), is.na(Category))
Country_dat_full_vol_aves <- Country_dat_full_vol_aves %>% mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                                                                                        grepl("union", Country) ~ "Reunion",
                                                                                        grepl("Cura", Country) ~ "Curacao",
                                                                                        TRUE ~ Country)) %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No"))

#### Amphibia ####
## Summarise the data to total yearly volumes per country.
Country_dat_Amph <- CITES_Amph_vol %>% group_by(Exporter, Year, name, region, subregion) %>%
  summarise(vol = sum(n)) %>%
  filter(Year>-1) %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(name) %>% filter(sum(vol) > 0)

## Series length - Eritrea has been a cites party our whole series but only traded one species that had
## a shorter series, so we do want to expand that eritrea series to the full length.
backbone <- NLP_dat %>% select(ISO, Year, Category) %>% filter(ISO %in% unique(Country_dat_Amph$Exporter)) %>%
  rename(Exporter = 1, Year = 2)


Country_dat_full_vol_amph <- left_join(backbone, Country_dat_Amph) %>% 
  group_by(Exporter) %>% fill(name, .direction = "down") %>%
  fill(region, .direction = "down") %>%
  fill(subregion, .direction = "down") %>%
  ungroup() %>%
  mutate(vol = ifelse(is.na(vol), 0, vol)) %>%
  ## standardise year (z-score standardistation)
  ## This is not essential but can help convergence of models
  mutate(SYear = (Year - mean(Year))/sd(Year),
         ## round up any decimal volumes as traded woes are count data. Cant have 2.5 animals must be 3
         vol = ceiling(vol),
         Year = as.factor(Year)) %>%
  rename(Country = name)

length(unique(Country_dat_full_vol_amph$Exporter)) ## 9
Country_dat_full_vol_amph  %>% filter(is.na(Country), is.na(Category))
Country_dat_full_vol_amph <- Country_dat_full_vol_amph %>% mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                                                                                        grepl("union", Country) ~ "Reunion",
                                                                                        grepl("Cura", Country) ~ "Curacao",
                                                                                        TRUE ~ Country)) %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No"))

#### Mammalia ####
## Summarise the data to total yearly volumes per country.
Country_dat_Mam <- CITES_Mam_vol %>% group_by(Exporter, Year, name, region, subregion) %>%
  summarise(vol = sum(n)) %>%
  filter(Year>-1) %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(name) %>% filter(sum(vol) > 0)

## Series length - Eritrea has been a cites party our whole series but only traded one species that had
## a shorter series, so we do want to expand that eritrea series to the full length.
backbone <- NLP_dat %>% select(ISO, Year, Category) %>% filter(ISO %in% unique(Country_dat_Mam$Exporter)) %>%
  rename(Exporter = 1, Year = 2)


Country_dat_full_vol_mam <- left_join(backbone, Country_dat_Mam) %>% 
  group_by(Exporter) %>% fill(name, .direction = "down") %>%
  fill(region, .direction = "down") %>%
  fill(subregion, .direction = "down") %>%
  ungroup() %>%
  mutate(vol = ifelse(is.na(vol), 0, vol)) %>%
  ## standardise year (z-score standardistation)
  ## This is not essential but can help convergence of models
  mutate(SYear = (Year - mean(Year))/sd(Year),
         ## round up any decimal volumes as traded woes are count data. Cant have 2.5 animals must be 3
         vol = ceiling(vol),
         Year = as.factor(Year)) %>%
  rename(Country = name)

length(unique(Country_dat_full_vol_mam$Exporter)) ##  99
Country_dat_full_vol_mam  %>% filter(is.na(Country), is.na(Category))
Country_dat_full_vol_mam <- Country_dat_full_vol_mam %>% mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                                                                                        grepl("union", Country) ~ "Reunion",
                                                                                        grepl("Cura", Country) ~ "Curacao",
                                                                                        TRUE ~ Country)) %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No"))

#### Reptilia ####
## Summarise the data to total yearly volumes per country.
Country_dat_Rept <- CITES_Rept_vol %>% group_by(Exporter, Year, name, region, subregion) %>%
  summarise(vol = sum(n)) %>%
  filter(Year>-1) %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(name) %>% filter(sum(vol) > 0)

## Series length - Eritrea has been a cites party our whole series but only traded one species that had
## a shorter series, so we do want to expand that eritrea series to the full length.
backbone <- NLP_dat %>% select(ISO, Year, Category) %>% filter(ISO %in% unique(Country_dat_Rept$Exporter)) %>%
  rename(Exporter = 1, Year = 2)

Country_dat_full_vol_rept <- left_join(backbone, Country_dat_Rept) %>% 
  group_by(Exporter) %>% fill(name, .direction = "up") %>%
  fill(region, .direction = "up") %>%
  fill(subregion, .direction = "up") %>%
  ungroup() %>%
  mutate(vol = ifelse(is.na(vol), 0, vol)) %>%
  ## standardise year (z-score standardistation)
  ## This is not essential but can help convergence of models
  mutate(SYear = (Year - mean(Year))/sd(Year),
         ## round up any decimal volumes as traded woes are count data. Cant have 2.5 animals must be 3
         vol = ceiling(vol),
         Year = as.factor(Year)) %>%
  rename(Country = name)

length(unique(Country_dat_full_vol_rept$Exporter)) ## 97
Country_dat_full_vol_rept %>% filter(is.na(Country), is.na(Category))
Country_dat_full_vol_rept <- Country_dat_full_vol_rept %>% mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                                                                                        grepl("union", Country) ~ "Reunion",
                                                                                        grepl("Cura", Country) ~ "Curacao",
                                                                                        TRUE ~ Country)) %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No"))

#### Model Fitting ####
Mod_Exp_vol_Aves <- brm(bf(vol ~ 1 + SYear*Category1 + (1 + SYear |Country) + (1|Year),
                          hu ~ 1 + SYear*Category1 + (1 + SYear |Country) + (1|Year)), 
                       family = hurdle_negbinomial(), 
                       sample_prior = TRUE,
                       prior = c(
                         prior(normal(0,2), "Intercept"),
                         prior(normal(0,2), "Intercept", dpar = "hu"),
                         prior(normal(0,.5), "b"),
                         prior(normal(0,.5), "b", dpar = "hu"),
                         prior(normal(0,2), "sd"),
                         prior(normal(0,2), "sd", dpar = "hu")),
                       data = Country_dat_full_vol_aves,
                       file = "Models/ER/Exp_Vol_Aves_HNB.rds",
                       chains = 4, iter = 3000, thin = 1, cores = 4, warmup = 1500)


Mod_Exp_vol_Amph <- brm(bf(vol ~ 1 + SYear*Category1 + (1 + SYear |Country) + (1|Year),
                           hu ~ 1 + SYear*Category1 + (1 + SYear |Country) + (1|Year)), 
                         family = hurdle_negbinomial(), 
                         sample_prior = TRUE,
                        prior = c(
                          prior(normal(0,2), "Intercept"),
                          prior(normal(0,2), "Intercept", dpar = "hu"),
                          prior(normal(0,.5), "b"),
                          prior(normal(0,.5), "b", dpar = "hu"),
                          prior(normal(0,2), "sd"),
                          prior(normal(0,2), "sd", dpar = "hu")),
                        data = Country_dat_full_vol_amph,
                         control = list(adapt_delta = 0.99),
                        file = "Models/ER/Exp_Vol_Amph_HNB.rds",
                        chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)



Mod_Exp_vol_Mam <- brm(bf(vol ~ 1 + SYear*Category1 + (1 + SYear |Country) + (1|Year),
                          hu ~ 1 + SYear*Category1 + (1 + SYear |Country) + (1|Year)), 
                        family = hurdle_negbinomial(), 
                        sample_prior = TRUE,
                       prior = c(
                         prior(normal(0,2), "Intercept"),
                         prior(normal(0,2), "Intercept", dpar = "hu"),
                         prior(normal(0,.5), "b"),
                         prior(normal(0,.5), "b", dpar = "hu"),
                         prior(normal(0,2), "sd"),
                         prior(normal(0,2), "sd", dpar = "hu")),
                        data = Country_dat_full_vol_mam,
                        file = "Models/ER/Exp_Vol_Mam_HNB.rds",
                        chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)


Mod_Exp_vol_Rept <- brm(bf(vol ~ 1 + SYear*Category1 + (1 + SYear |Country) + (1|Year),
                           hu ~ 1 + SYear*Category1 + (1 + SYear |Country) + (1|Year)), 
                        family = hurdle_negbinomial(), 
                        sample_prior = TRUE,
                        prior = c(
                          prior(normal(0,2), "Intercept"),
                          prior(normal(0,2), "Intercept", dpar = "hu"),
                          prior(normal(0,.5), "b"),
                          prior(normal(0,.5), "b", dpar = "hu"),
                          prior(normal(0,2), "sd"),
                          prior(normal(0,2), "sd", dpar = "hu")),
                        data = Country_dat_full_vol_rept,
                        file = "Models/ER/Exp_Vol_Rept_HNB.rds",
                        chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)


write.csv(Country_dat_full_vol_aves, "Models/ER/Fitting_data/Exp_Vol_Aves.csv", na = "")
write.csv(Country_dat_full_vol_amph, "Models/ER/Fitting_data/Exp_Vol_Amph.csv", na = "")
write.csv(Country_dat_full_vol_mam, "Models/ER/Fitting_data/Exp_Vol_Mam.csv", na = "")
write.csv(Country_dat_full_vol_rept, "Models/ER/Fitting_data/Exp_Vol_Rept.csv", na = "")

#### Checking ####

pp_check(Mod_Exp_vol_Rept, type = "stat")

pp_check(Mod_Exp_vol_Aves)+ coord_cartesian(xlim=c(0,100000))

prop_zero <- function(x) {sum(x == 0)/length(x)}
ppc_stat(y = Country_dat_full_vol_aves$vol, yrep = posterior_predict(Mod_Exp_vol_Aves, draws = 500), stat="prop_zero")

dispersion<- function(x) {var(x)/mean(x)}
ppc_stat(y = Country_dat_full_vol_rept$vol, yrep = posterior_predict(Mod_Exp_vol_ReptL, draws = 500), stat="dispersion")


C_Fit <- add_epred_draws(Mod_Exp_vol_Aves, newdata =  Country_dat_full_vol_aves, re_formula = NULL, ndraws = 200)  %>% 
  group_by(Country, SYear) %>% median_hdci(.epred, .width = .9)

library(ggforce)
ggplot(C_Fit, aes(SYear, .epred)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = NA, colour = "black") +
  geom_point(data = Country_dat_full_vol_aves, aes(SYear, vol)) +
  #facet_wrap(~Country, scales = "free", col = 11, row = 11) 
  facet_wrap_paginate(~Country, scales = "free", nrow = 10, ncol = 10, page = 1)

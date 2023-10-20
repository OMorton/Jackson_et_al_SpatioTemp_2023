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
CITES_Vol <- data.table::fread("Data/All_Vertebrates/Cleaned/IR/CITES_Vert_Importer_dat_IR.csv", na.strings = "")
NLP_dat <- data.table::fread("Data/NLP/Party_NLP_Full.csv", na.strings = "") %>% select(ISO, Year, Category) %>% mutate(Year = Year - 2000) %>%
  ## Note guernsey appears in cites NLP docs as GX yet GG in the data for imports
  mutate(ISO = ifelse(ISO == "GX", "GG", ISO))

## Double checked here that all NA values are from years 2000 -2020 when the country was not a party to CITES.
## Now in the importer reported, import volumes we want to limit each countries series as to only when it was a Party.
## e.g. a party couldnt report its own imports before it became a party
Check <- left_join(CITES_Vol, NLP_dat, by = c("Importer" = "ISO", "Year" = "Year")) %>%
  filter(is.na(Category)) %>% group_by(Importer, Year) %>% tally()

## Birds
CITES_Aves_vol <- CITES_Vol %>% filter(Class == "Aves")
## Birds
CITES_Mam_vol <- CITES_Vol %>% filter(Class == "Mammalia")
## Birds
CITES_Amph_vol <- CITES_Vol %>% filter(Class == "Amphibia")
## Birds
CITES_Rept_vol <- CITES_Vol %>% filter(Class == "Reptilia")

#### Preparation ####
#### Aves ####
## Summarise the data to total yearly volumes per country.
Country_dat_Aves <- CITES_Aves_vol %>% group_by(Importer, Year, name, region, subregion) %>%
  summarise(vol = sum(n)) %>%
  filter(Year>-1) %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(name) %>% filter(sum(vol) > 0)

## Series length - Eritrea has been a cites party our whole series but only traded one species that had
## a shorter series, so we do want to expand that eritrea series to the full length.
backbone <- NLP_dat %>% select(ISO, Year, Category) %>% filter(ISO %in% unique(Country_dat_Aves$Importer)) %>%
  rename(Importer = 1, Year = 2)


Country_dat_full_vol_aves_IMP <- left_join(backbone, Country_dat_Aves) %>% 
  group_by(Importer) %>% fill(name, .direction = "up") %>%
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

length(unique(Country_dat_full_vol_aves_IMP$Importer)) ## 98
Country_dat_full_vol_aves_IMP %>% filter(is.na(Country))
Country_dat_full_vol_aves_IMP <- Country_dat_full_vol_aves_IMP %>% mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                                                                                               grepl("union", Country) ~ "Reunion",
                                                                                               grepl("Cura", Country) ~ "Curacao",
                                                                                               TRUE ~ Country)) %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No"))

#### Amphibia ####
## Summarise the data to total yearly volumes per country.
Country_dat_Amph <- CITES_Amph_vol %>% group_by(Importer, Year, name, region, subregion) %>%
  summarise(vol = sum(n)) %>%
  filter(Year>-1) %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(name) %>% filter(sum(vol) > 0)

## Series length - Eritrea has been a cites party our whole series but only traded one species that had
## a shorter series, so we do want to expand that eritrea series to the full length.
backbone <- NLP_dat %>% select(ISO, Year, Category) %>% filter(ISO %in% unique(Country_dat_Amph$Importer)) %>%
  rename(Importer = 1, Year = 2)


Country_dat_full_vol_amph_IMP <- left_join(backbone, Country_dat_Amph) %>% 
  group_by(Importer) %>% fill(name, .direction = "up") %>%
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

length(unique(Country_dat_full_vol_amph_IMP$Importer)) ## 19
Country_dat_full_vol_amph_IMP %>% filter(is.na(Country))
Country_dat_full_vol_amph_IMP <- Country_dat_full_vol_amph_IMP %>% mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                                                                                                TRUE ~ Country)) %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No"))

#### Mammalia ####
## Summarise the data to total yearly volumes per country.
Country_dat_Mam <- CITES_Mam_vol %>% group_by(Importer, Year, name, region, subregion) %>%
  summarise(vol = sum(n)) %>%
  filter(Year>-1) %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(name) %>% filter(sum(vol) > 0)

## Series length - Eritrea has been a cites party our whole series but only traded one species that had
## a shorter series, so we do want to expand that eritrea series to the full length.
backbone <- NLP_dat %>% select(ISO, Year, Category) %>% filter(ISO %in% unique(Country_dat_Mam$Importer)) %>%
  rename(Importer = 1, Year = 2)


Country_dat_full_vol_mam_IMP <- left_join(backbone, Country_dat_Mam) %>% 
  group_by(Importer) %>% fill(name, .direction = "down") %>%
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

length(unique(Country_dat_full_vol_mam_IMP$Importer)) ## 113
Country_dat_full_vol_mam_IMP %>% filter(is.na(Country))
Country_dat_full_vol_mam_IMP <- Country_dat_full_vol_mam_IMP %>% mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                                                                                            grepl("union", Country) ~ "Reunion",
                                                                                            TRUE ~ Country)) %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No"))


#### Reptilia ####
## Summarise the data to total yearly volumes per country.
Country_dat_Rept <- CITES_Rept_vol %>% group_by(Importer, Year, name, region, subregion) %>%
  summarise(vol = sum(n)) %>%
  filter(Year>-1) %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(name) %>% filter(sum(vol) > 0)

## Series length - Eritrea has been a cites party our whole series but only traded one species that had
## a shorter series, so we do want to expand that eritrea series to the full length.
backbone <- NLP_dat %>% select(ISO, Year, Category) %>% filter(ISO %in% unique(Country_dat_Rept$Importer)) %>%
  rename(Importer = 1, Year = 2)

Country_dat_full_vol_rept_IMP <- left_join(backbone, Country_dat_Rept) %>% 
  group_by(Importer) %>% fill(name, .direction = "up") %>%
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

length(unique(Country_dat_full_vol_rept_IMP$Importer)) ## 91
Country_dat_full_vol_rept_IMP %>% filter(is.na(Country))
Country_dat_full_vol_rept_IMP <- Country_dat_full_vol_rept_IMP %>% mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                                                                                              grepl("union", Country) ~ "Reunion",
                                                                                              TRUE ~ Country)) %>%
  mutate(Category1 = ifelse(Category == 1, "Yes", "No"))

#### Model Fitting ####
Mod_Imp_vol_Aves <- brm(bf(vol ~ 1 + SYear*Category1 + (1|Year) + (1 + SYear|Country),
                           hu ~ 1 + SYear*Category1 + (1|Year) + (1 + SYear|Country)), 
                        family = hurdle_negbinomial(), 
                        sample_prior = TRUE,
                        prior = c(
                          prior(normal(0,2), "Intercept"),
                          prior(normal(0,2), "Intercept", dpar = "hu"),
                          prior(normal(0,.5), "b"),
                          prior(normal(0,.5), "b", dpar = "hu"),
                          prior(normal(0,2), "sd"),
                          prior(normal(0,2), "sd", dpar = "hu")),
                        data = Country_dat_full_vol_aves_IMP,
                        file = "Models/IR/Imp_Vol_Aves_vH.rds",
                        chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

Mod_Imp_vol_Amph <- brm(bf(vol ~ 1 + SYear*Category1 + (1|Year) + (1 + SYear|Country),
                           hu ~ 1 + SYear*Category1 + (1|Year) + (1 + SYear|Country)), 
                        family = hurdle_negbinomial(), 
                        sample_prior = TRUE,
                        prior = c(
                          prior(normal(0,2), "Intercept"),
                          prior(normal(0,2), "Intercept", dpar = "hu"),
                          prior(normal(0,.5), "b"),
                          prior(normal(0,.5), "b", dpar = "hu"),
                          prior(normal(0,2), "sd"),
                          prior(normal(0,2), "sd", dpar = "hu")),
                        data = Country_dat_full_vol_amph_IMP,
                        control = list(adapt_delta = 0.99),
                        file = "Models/IR/Imp_Vol_Amph_vH.rds",
                        chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

Mod_Imp_vol_Mam <- brm(bf(vol ~ 1 + SYear*Category1 + (1|Year) + (1 + SYear|Country),
                          hu ~ 1 + SYear*Category1 + (1|Year) + (1 + SYear|Country)), 
                       family = hurdle_negbinomial(), 
                       sample_prior = TRUE,
                       prior = c(
                         prior(normal(0,2), "Intercept"),
                         prior(normal(0,2), "Intercept", dpar = "hu"),
                         prior(normal(0,.5), "b"),
                         prior(normal(0,.5), "b", dpar = "hu"),
                         prior(normal(0,2), "sd"),
                         prior(normal(0,2), "sd", dpar = "hu")),
                       data = Country_dat_full_vol_mam_IMP,
                       file = "Models/IR/Imp_Vol_Mam_vH.rds",
                       chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

Mod_Imp_vol_Rept <- brm(bf(vol ~ 1 + SYear*Category1 + (1|Year) + (1 + SYear|Country),
                           hu ~ 1 + SYear*Category1 + (1|Year) + (1 + SYear|Country)), 
                        family = hurdle_negbinomial(), 
                        sample_prior = TRUE,
                        prior = c(
                          prior(normal(0,2), "Intercept"),
                          prior(normal(0,2), "Intercept", dpar = "hu"),
                          prior(normal(0,.5), "b"),
                          prior(normal(0,.5), "b", dpar = "hu"),
                          prior(normal(0,2), "sd"),
                          prior(normal(0,2), "sd", dpar = "hu")),
                        data = Country_dat_full_vol_rept_IMP,
                        file = "Models/IR/Imp_Vol_Rept_vH.rds",
                        chains = 4, iter = 2000, thin = 1, cores = 4, warmup = 1000)

write.csv(Country_dat_full_vol_aves_IMP, "Models/IR/Fitting_data/Imp_Vol_Aves.csv", na = "")
write.csv(Country_dat_full_vol_amph_IMP, "Models/IR/Fitting_data/Imp_Vol_Amph.csv", na = "")
write.csv(Country_dat_full_vol_mam_IMP, "Models/IR/Fitting_data/Imp_Vol_Mam.csv", na = "")
write.csv(Country_dat_full_vol_rept_IMP, "Models/IR/Fitting_data/Imp_Vol_Rept.csv", na = "")

#### Checking ####

pp_check(Mod_Imp_vol_Aves, type = "stat")

pp_check(Mod_Imp_vol_Aves)+ coord_cartesian(xlim=c(0,1000000))

prop_zero <- function(x) {sum(x == 0)/length(x)}
ppc_stat(y = Country_dat_full_IMP$vol, yrep = posterior_predict(Mod_Imp_vol_Aves, draws = 500), stat="prop_zero")

dispersion<- function(x) {var(x)/mean(x)}
ppc_stat(y = Country_dat_full_IMP$vol, yrep = posterior_predict(Mod_Imp_vol_Aves, draws = 500), stat="dispersion")


C_Fit <- add_fitted_draws(Mod_Imp, newdata =  Country_dat_full_IMP, re_formula = NULL, ndraws = 200)  %>% 
  group_by(Country, SYear) %>% median_hdci(.value, .width = .9)

library(ggforce)
ggplot(C_Fit, aes(SYear, .value)) +
  geom_line() +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), fill = NA, colour = "black") +
  geom_point(data = Country_dat_full_IMP, aes(SYear, vol)) +
  facet_wrap_paginate(~Country, scales = "free", nrow = 10, ncol = 10, page = 2)


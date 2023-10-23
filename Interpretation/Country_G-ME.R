#########################
#### Country G-ME ####
#########################

## Purpose:  calculate the general temporal trend per country 2000 - 2020, in the form of group level marginal effect for the average year.

options(scipen = 999) ## remove scientific notation, namely "e"  to show very small and large numbers in full.
options(na.action = "na.pass")

## Set working directory and package location.
## Change this in yours
.libPaths("C:/Packages") ## Set up for working from home.

library(tidyverse)
library(marginaleffects)
library(bayestestR)
library(tidybayes)

#### Read in ####
## Model fitting data
Country_dat_full_vol_aves <- data.table::fread("Models/ER/Fitting_data/Exp_Vol_Aves.csv", na.strings = "")
Country_dat_full_vol_amph <- data.table::fread("Models/ER/Fitting_data/Exp_Vol_Amph.csv", na.strings = "")
Country_dat_full_vol_mam <- data.table::fread("Models/ER/Fitting_data/Exp_Vol_Mam.csv", na.strings = "")
Country_dat_full_vol_rept <- data.table::fread("Models/ER/Fitting_data/Exp_Vol_Rept.csv", na.strings = "")

Country_dat_full_vol_aves_IMP <- data.table::fread("Models/ER/Fitting_data/Imp_Vol_Aves.csv", na.strings = "")
Country_dat_full_vol_amph_IMP <- data.table::fread("Models/ER/Fitting_data/Imp_Vol_Amph.csv", na.strings = "")
Country_dat_full_vol_mam_IMP <- data.table::fread("Models/ER/Fitting_data/Imp_Vol_Mam.csv", na.strings = "")
Country_dat_full_vol_rept_IMP <- data.table::fread("Models/ER/Fitting_data/Imp_Vol_Rept.csv", na.strings = "")

## Models
Mod_Exp_vol_Aves <- readRDS("Models/ER/Exp_Vol_Aves_HNB_vH.rds")
Mod_Exp_vol_Amph <- readRDS("Models/ER/Exp_Vol_Amph_HNB_vH.rds")
Mod_Exp_vol_Mam <- readRDS("Models/ER/Exp_Vol_Mam_HNB_vH.rds")
Mod_Exp_vol_Rept <- readRDS("Models/ER/Exp_Vol_Rept_HNB_vH.rds")

Mod_Imp_vol_Aves <- readRDS("Models/ER/Imp_Vol_Aves_HNB_vH.rds")
Mod_Imp_vol_Amph <- readRDS("Models/ER/Imp_Vol_Amph_HNB_vH.rds")
Mod_Imp_vol_Mam <- readRDS("Models/ER/Imp_Vol_Mam_HNB_vH.rds")
Mod_Imp_vol_Rept <- readRDS("Models/ER/Imp_Vol_Rept_HNB_vH.rds")

#### Country-level marginal effects - EXPORTS ####
## Aves

Exp_Aves <- Country_dat_full_vol_aves %>% group_by(Country) %>% 
  mutate(Category1 = ifelse(sum(Category1 == "Yes")/n() > 0.5, "Yes", "No"))

G_ME_Aves_EXP <- marginaleffects(Mod_Exp_vol_Aves, variables = "SYear", type = "response",
                                 re_formula = ~(1 + SYear |Country), newdata = Exp_Aves)
G_ME_Sum_Aves_EXP <- posteriordraws(G_ME_Aves_EXP) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(Country_dat_full_vol_aves$Year))))  %>%
  group_by(Country) %>% 
  mutate(PD = (sum(sign(G_ME) == sign(median(G_ME)))/n())*100) %>%
  group_by(Country, PD) %>% 
  median_hdci(G_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(G_ME > 0, "Positive", "Negative")) %>%
  left_join(Country_dat_full_vol_aves %>% group_by(region, Country, Exporter) %>% tally(vol)) %>%
  mutate(Interpretation = case_when(PD > 95 & PD < 97.5 & G_ME < 0 ~ "PD > 95%, -ve trend",
                                    PD >= 97.5 & G_ME < 0 ~ "PD > 97.5%, -ve trend",
                                    PD > 95 & PD < 97.5 & G_ME > 0 ~ "PD > 95%, +ve trend",
                                    PD >= 97.5 & G_ME > 0 ~ "PD > 97.5%, +ve trend",
                                    PD < 95 ~ "Uncertain"), Total = sum(n), Prop = n/Total)

## Amph
Exp_Amph <- Country_dat_full_vol_amph %>% group_by(Country) %>% 
  mutate(Category1 = ifelse(sum(Category1 == "Yes")/n() > 0.5, "Yes", "No"))

G_ME_Amph_EXP <- marginaleffects(Mod_Exp_vol_Amph, variables = "SYear", type = "response",
                                 re_formula = ~(1 + SYear |Country), newdata = Exp_Amph)
G_ME_Sum_Amph_EXP <- posteriordraws(G_ME_Amph_EXP) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(Country_dat_full_vol_amph$Year))))  %>%
  group_by(Country) %>% 
  mutate(PD = (sum(sign(G_ME) == sign(median(G_ME)))/n())*100) %>%
  group_by(Country, PD) %>% 
  median_hdci(G_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(G_ME > 0, "Positive", "Negative")) %>%
  left_join(Country_dat_full_vol_amph %>% group_by(region, Country, Exporter) %>% tally(vol)) %>%
  mutate(Interpretation = case_when(PD > 95 & PD < 97.5 & G_ME < 0 ~ "PD > 95%, -ve trend",
                                    PD >= 97.5 & G_ME < 0 ~ "PD > 97.5%, -ve trend",
                                    PD > 95 & PD < 97.5 & G_ME > 0 ~ "PD > 95%, +ve trend",
                                    PD >= 97.5 & G_ME > 0 ~ "PD > 97.5%, +ve trend",
                                    PD < 95 ~ "Uncertain"), Total = sum(n), Prop = n/Total)

## Mam
Exp_Mam <- Country_dat_full_vol_mam %>% group_by(Country) %>% 
  mutate(Category1 = ifelse(sum(Category1 == "Yes")/n() > 0.5, "Yes", "No"))

G_ME_Mam_EXP <- marginaleffects(Mod_Exp_vol_Mam, variables = "SYear", type = "response",
                                re_formula = ~(1 + SYear |Country), newdata = Exp_Mam)
G_ME_Sum_Mam_EXP <- posteriordraws(G_ME_Mam_EXP) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(Country_dat_full_vol_mam$Year))))  %>%
  group_by(Country) %>% 
  mutate(PD = (sum(sign(G_ME) == sign(median(G_ME)))/n())*100) %>%
  group_by(Country, PD) %>% 
  median_hdci(G_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(G_ME > 0, "Positive", "Negative")) %>%
  left_join(Country_dat_full_vol_mam %>% group_by(region, Country, Exporter) %>% tally(vol)) %>%
  mutate(Interpretation = case_when(PD > 95 & PD < 97.5 & G_ME < 0 ~ "PD > 95%, -ve trend",
                                    PD >= 97.5 & G_ME < 0 ~ "PD > 97.5%, -ve trend",
                                    PD > 95 & PD < 97.5 & G_ME > 0 ~ "PD > 95%, +ve trend",
                                    PD >= 97.5 & G_ME > 0 ~ "PD > 97.5%, +ve trend",
                                    PD < 95 ~ "Uncertain"), Total = sum(n), Prop = n/Total)

## Rept
Exp_Rept <- Country_dat_full_vol_rept %>% group_by(Country) %>% 
  mutate(Category1 = ifelse(sum(Category1 == "Yes")/n() > 0.5, "Yes", "No"))

G_ME_Rept_EXP <- marginaleffects(Mod_Exp_vol_Rept, variables = "SYear", type = "response",
                                 re_formula = ~(1 + SYear |Country), newdata = Exp_Rept)
G_ME_Sum_Rept_EXP <- posteriordraws(G_ME_Rept_EXP) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(Country_dat_full_vol_rept$Year))))  %>%
  group_by(Country) %>% 
  mutate(PD = (sum(sign(G_ME) == sign(median(G_ME)))/n())*100) %>%
  group_by(Country, PD) %>% 
  median_hdci(G_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(G_ME > 0, "Positive", "Negative")) %>%
  left_join(Country_dat_full_vol_rept %>% group_by(region, Country, Exporter) %>% tally(vol)) %>%
  mutate(Interpretation = case_when(PD > 95 & PD < 97.5 & G_ME < 0 ~ "PD > 95%, -ve trend",
                                    PD >= 97.5 & G_ME < 0 ~ "PD > 97.5%, -ve trend",
                                    PD > 95 & PD < 97.5 & G_ME > 0 ~ "PD > 95%, +ve trend",
                                    PD >= 97.5 & G_ME > 0 ~ "PD > 97.5%, +ve trend",
                                    PD < 95 ~ "Uncertain"), Total = sum(n), Prop = n/Total)


#### Country-level marginal effects - IMPORTS ####
## Aves
Imp_Aves <- Country_dat_full_vol_aves_IMP %>% group_by(Country) %>% 
  mutate(Category1 = ifelse(sum(Category1 == "Yes")/n() > 0.5, "Yes", "No"))

G_ME_Aves_IMP <- marginaleffects(Mod_Imp_vol_Aves, variables = "SYear", type = "response",
                                 re_formula = ~(1 + SYear |Country), newdata = Imp_Aves)
G_ME_Sum_Aves_IMP <- posteriordraws(G_ME_Aves_IMP) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(Country_dat_full_vol_aves_IMP$Year))))  %>%
  group_by(Country) %>% 
  mutate(PD = (sum(sign(G_ME) == sign(median(G_ME)))/n())*100) %>%
  group_by(Country, PD) %>% 
  median_hdci(G_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(G_ME > 0, "Positive", "Negative")) %>%
  left_join(Country_dat_full_vol_aves_IMP %>% group_by(region, Country, Importer) %>% tally(vol)) %>%
  mutate(Interpretation = case_when(PD > 95 & PD < 97.5 & G_ME < 0 ~ "PD > 95%, -ve trend",
                                    PD >= 97.5 & G_ME < 0 ~ "PD > 97.5%, -ve trend",
                                    PD > 95 & PD < 97.5 & G_ME > 0 ~ "PD > 95%, +ve trend",
                                    PD >= 97.5 & G_ME > 0 ~ "PD > 97.5%, +ve trend",
                                    PD < 95 ~ "Uncertain"), Total = sum(n), Prop = n/Total)

## Amph
Imp_Amph <- Country_dat_full_vol_amph_IMP %>% group_by(Country) %>% 
  mutate(Category1 = ifelse(sum(Category1 == "Yes")/n() > 0.5, "Yes", "No"))

G_ME_Amph_IMP <- marginaleffects(Mod_Imp_vol_Amph, variables = "SYear", type = "response",
                                 re_formula = ~(1 + SYear |Country), newdata = Imp_Amph)
G_ME_Sum_Amph_IMP <- posteriordraws(G_ME_Amph_IMP) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(Country_dat_full_vol_amph_IMP$Year))))  %>%
  group_by(Country) %>% 
  mutate(PD = (sum(sign(G_ME) == sign(median(G_ME)))/n())*100) %>%
  group_by(Country, PD) %>% 
  median_hdci(G_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(G_ME > 0, "Positive", "Negative")) %>%
  left_join(Country_dat_full_vol_amph_IMP %>% group_by(region, Country, Importer) %>% tally(vol)) %>%
  mutate(Interpretation = case_when(PD > 95 & PD < 97.5 & G_ME < 0 ~ "PD > 95%, -ve trend",
                                    PD >= 97.5 & G_ME < 0 ~ "PD > 97.5%, -ve trend",
                                    PD > 95 & PD < 97.5 & G_ME > 0 ~ "PD > 95%, +ve trend",
                                    PD >= 97.5 & G_ME > 0 ~ "PD > 97.5%, +ve trend",
                                    PD < 95 ~ "Uncertain"), Total = sum(n), Prop = n/Total)

## Mam
Imp_Mam <- Country_dat_full_vol_mam_IMP %>% group_by(Country) %>% 
  mutate(Category1 = ifelse(sum(Category1 == "Yes")/n() > 0.5, "Yes", "No"))

G_ME_Mam_IMP <- marginaleffects(Mod_Imp_vol_Mam, variables = "SYear", type = "response",
                                re_formula = ~(1 + SYear |Country), newdata = Imp_Mam)
G_ME_Sum_Mam_IMP <- posteriordraws(G_ME_Mam_IMP) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(Country_dat_full_vol_mam_IMP$Year))))  %>%
  group_by(Country) %>% 
  mutate(PD = (sum(sign(G_ME) == sign(median(G_ME)))/n())*100) %>%
  group_by(Country, PD) %>% 
  median_hdci(G_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(G_ME > 0, "Positive", "Negative")) %>%
  left_join(Country_dat_full_vol_mam_IMP %>% group_by(region, Country, Importer) %>% tally(vol)) %>%
  mutate(Interpretation = case_when(PD > 95 & PD < 97.5 & G_ME < 0 ~ "PD > 95%, -ve trend",
                                    PD >= 97.5 & G_ME < 0 ~ "PD > 97.5%, -ve trend",
                                    PD > 95 & PD < 97.5 & G_ME > 0 ~ "PD > 95%, +ve trend",
                                    PD >= 97.5 & G_ME > 0 ~ "PD > 97.5%, +ve trend",
                                    PD < 95 ~ "Uncertain"), Total = sum(n), Prop = n/Total)

## Rept
Imp_Rept <- Country_dat_full_vol_rept_IMP %>% group_by(Country) %>% 
  mutate(Category1 = ifelse(sum(Category1 == "Yes")/n() > 0.5, "Yes", "No"))

G_ME_Rept_IMP <- marginaleffects(Mod_Imp_vol_Rept, variables = "SYear", type = "response",
                                 re_formula = ~(1 + SYear |Country), newdata = Imp_Rept)
G_ME_Sum_Rept_IMP <- posteriordraws(G_ME_Rept_IMP) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(Country_dat_full_vol_rept_IMP$Year))))  %>%
  group_by(Country) %>% 
  mutate(PD = (sum(sign(G_ME) == sign(median(G_ME)))/n())*100) %>%
  group_by(Country, PD) %>% 
  median_hdci(G_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(G_ME > 0, "Positive", "Negative")) %>%
  left_join(Country_dat_full_vol_rept_IMP %>% group_by(region, Country, Importer) %>% tally(vol)) %>%
  mutate(Interpretation = case_when(PD > 95 & PD < 97.5 & G_ME < 0 ~ "PD > 95%, -ve trend",
                                    PD >= 97.5 & G_ME < 0 ~ "PD > 97.5%, -ve trend",
                                    PD > 95 & PD < 97.5 & G_ME > 0 ~ "PD > 95%, +ve trend",
                                    PD >= 97.5 & G_ME > 0 ~ "PD > 97.5%, +ve trend",
                                    PD < 95 ~ "Uncertain"), Total = sum(n), Prop = n/Total)

#### Write out ####

write.csv(G_ME_Sum_Aves_EXP, "Outputs/G-ME/G_ME_Sum_Aves_EXP.csv", na = "")
write.csv(G_ME_Sum_Amph_EXP, "Outputs/G-ME/G_ME_Sum_Amph_EXP.csv", na = "")
write.csv(G_ME_Sum_Mam_EXP, "Outputs/G-ME/G_ME_Sum_Mam_EXP.csv", na = "")
write.csv(G_ME_Sum_Rept_EXP, "Outputs/G-ME/G_ME_Sum_Rept_EXP.csv", na = "")

write.csv(G_ME_Sum_Aves_IMP, "Outputs/G-ME/G_ME_Sum_Aves_IMP.csv", na = "")
write.csv(G_ME_Sum_Amph_IMP, "Outputs/G-ME/G_ME_Sum_Amph_IMP.csv", na = "")
write.csv(G_ME_Sum_Mam_IMP, "Outputs/G-ME/G_ME_Sum_Mam_IMP.csv", na = "")
write.csv(G_ME_Sum_Rept_IMP, "Outputs/G-ME/G_ME_Sum_Rept_IMP.csv", na = "")


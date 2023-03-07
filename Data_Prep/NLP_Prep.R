#### Packages and presets ####

## global options settings
options(scipen = 999) ## remove scientific notation, namely "e"  to show very small and large numbers in full.
options(na.action = "na.pass")
## Set working directory and package location.
## These need changing to your file paths.
setwd("D:/PhD/Alice/Project")
.libPaths("C:/Packages") ## Set up for working from home.

## for all plotting and data handling
library(tidyverse)

## Read in CITES Parties
Parties <- data.table::fread("Data/CITES/Parties_to_Convention.csv")

## Make specific sequences for each Party
Parties_Sum <- Parties %>% mutate(Start = as.numeric(as.character(stringi::stri_sub(`Entry into force`, -2, -1)))) %>%
  mutate(Start = ifelse(Start < 20, Start + 2000, Start + 1900),
                   Finish = 2020) %>%
  select(1,2,3, 7,8) %>%
  rename("Party" = 1, "ISO" = 2) %>%
  group_by(Party, ISO, Region) %>%
  summarise(Year = seq(from = Start, to = Finish, by = 1)) %>%
  filter(Year > 1999)

write.csv(Parties_Sum, "Data/CITES/Parties_Series.csv")

#### NLP Data ####
## The Party time series were then appended to NLP category data manually extracted 
## from SC and CoP meeting Documents.
NLP_raw <- data.table::fread("Data/NLP/Party_NLP_raw.csv", na.strings = c(NA_character_, ""))
ggplot(filter(NLP_raw, !is.na(Dependent)), aes(Year, Category)) + geom_point() + facet_wrap(~Party)
ggplot(filter(NLP_raw, is.na(Dependent)), aes(Year, Category)) + geom_point() + facet_wrap(~Party)

## There are blanks for 4 years (2000, 2001, 2003 and 2020).
## 2000 and 2001 - Phase 1, 2 and 3 are still being reported and the full data appears unavailable only summaries of the proportion of countries,
## and a few obs for specific countries <50, which were included.
## 2003 - there is no indication that categories have been updated since 2002.
## 2020 - no standing committee or CoP met.

## for 2000 and 2001 we wwant to fill forwards to account for those countries with obs in 2000 then no obs in 2001 for example but then after 
## filling forward for the other years we want to fill backwards from 2003.
## for 2003 we will fill forwards from 2002 as documents state no changes were made apart from those specifically mentioned which are included.
## for 2020 we will fill forwards from 2019


NLP_full <- NLP_raw %>%
  arrange(Party, Year)  %>% group_by(Party) %>%
  ## down first
  fill(Category, .direction = "down") %>%
  fill(Category, .direction = "up")

write.csv(NLP_full, "Data/NLP/Party_NLP_Full.csv")


#### Previous method ####
NLP_raw %>% mutate(Category = case_when(Year %in% c(2003, 2020) & is.na(Dependent) ~ "FILL_DOWN",
                                                    TRUE ~ Category)) %>%
  arrange(Party, Year)  %>% group_by(Party) %>%
  fill(Category, .direction = "up") %>%
  mutate(Category = ifelse(Category == "FILL_DOWN", NA, Category)) %>%
  fill(Category, .direction = "down") %>%
  ## catch parties whose series only began in 2003 so need to fill up from 2004
  mutate(Category = ifelse(is.na(Category), "P", Category))

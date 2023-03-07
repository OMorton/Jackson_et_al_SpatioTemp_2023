#################################
##---CITES raw data curation---##
#################################

## Author - O Morton 
## Date - 27/01/22
## Notes - Processing methods derived from Harfoot et al (2018) and code from Morton et al (2022)

## Purpose
## To read in the raw cites data product and process it to include WOES and focus only on commercial and wild sourced trades between 2000 and 2018.
## Runtime ~ 5 minutes

#### Packages and presets ####

## global options settings
options(scipen = 999) ## remove scientific notation, namely "e"  to show very small and large numbers in full.
options(na.action = "na.pass")
## Set working directory and package location.
## These need changing to your file paths.
.libPaths("C:/Packages") ## Set up for working from home.

## for all plotting and data handling
library(tidyverse)


#### Functions ####


## function to create t/f vector, function courtesy of Simon 
unroll <- function(col) {
  ind <- !is.na(col)
  headers <- col[ind]
  headers[cumsum(ind)]
}

# function to get count in year prior (checks that year interval ==1)
count_prior <- function(count, diff_priorYr, interval=1) {
  out <- rep(NA, length(count))
  # out vector with an observation in year prior (when interval == 1) gets the 
  # count from the year prior. When there is no observation in the year prior, 
  # it returns NA
  out[which(diff_priorYr==interval)] <- count[(which(diff_priorYr==interval)-interval)]
  out
}

#### Bulk data download and tidying ####

## Read in the 46 seperate .csv's from CITES latest Bulk release 2021 v1.
## https://trade.cites.org/ database is availble here needs downloading and saving.
## Then you supply the code below with the path to the entire file of csv's not to an individual one, the function then reads in all 46 csvs and 
## appends them.
## Takes a few minutes to run ~ results in a database with 22616522 records. Updated 2022 is 23680557
CITES_MASTER <- list.files(path="D:/Data/CITES/CITES_all_records_2022.1", full.names = TRUE, pattern="*.csv") %>% 
  lapply(read_csv, na = "", col_types = cols(Unit = col_character(), Import.permit.RandomID = col_character(),
                                    Export.permit.RandomID = col_character(), Origin.permit.RandomID = col_character(),
                                    Purpose = col_character())) %>% 
  bind_rows


## Remove all re-exports as per published CITES guidelines and Pers Comm with UNEP-WMC and CITES
## remove all reports where the origin is stated and is not the same as the exporter.
## This avoids double counting i.e. where a trade passes through multiple countries. 
CITES_TRUE <- CITES_MASTER %>% filter(Origin == Exporter  | is.na(Origin)) # this leaves 12.8 out 22.6 million shipments

## Focus on only the target taxa, you may want to broaden this to other taxa.
## this leaves 3.85 million records
CITES_TRUE <- CITES_TRUE %>% filter(Class %in% c("Aves", "Mammalia", "Amphibia", "Reptilia"))


#### Subset into wild source and commercial purpose ####

## Wild source as defined in Morton et al (2022), a full list of codes are available at
## https://trade.cites.org/cites_trade_guidelines/en-CITES_Trade_Database_Guide.pdf
## If we want to use captive as well just need to add some more conditions to this to class trades as wild, captive or unknown.
CITES_TRUE$WildSource <- if_else(CITES_TRUE$Source %in% c("W", "X", "R"), "Yes", "No")

## Commercial purpose as defined by ourselves. We justify the inclusion of personal under commercial to capture aspects of the pet trade,
## as done in previous studies see Bush et al.
CITES_TRUE$PurposeC <- if_else(CITES_TRUE$Purpose %in% c("P", "T"), "Commercial", "Not Commercial")

CITES_Vert <- CITES_TRUE


#### Convert to WOE's ####

## import the conversion table used by Harfoot et al. A WOE is a whole organism equivalent and standardises all the strange terms cites uses.
## Change file path to yours.
WOE_Factors <- data.table::fread("D:/Data/CITES/Conversion/Harfoot_Clean.csv")
WOE_Factors_all <- WOE_Factors %>% filter(Species_specific == 0) %>% select(Class, Term, Factor)
WOE_Factors_sp <- WOE_Factors %>% filter(Species_specific == 1) %>% select(Class, Term, Taxa_applicable, Factor)

## Match the trade terms with the WOE conversion terms and add the factor for conversion.
CITES_Vert <- left_join(CITES_Vert, WOE_Factors_all, by = c("Class", "Term")) %>% 
  left_join(WOE_Factors_sp, by = c("Class", "Term", "Taxon" = "Taxa_applicable")) %>%
  mutate(Factor = ifelse(is.na(Factor.y), Factor.x, Factor.y)) %>%
  select(-Factor.x, -Factor.y)

## Calculate WOEs - you take the quanity traded multipled by conversion term.
CITES_Vert <- CITES_Vert %>% mutate(WOE = Factor*Quantity)

## Note only consider records where the unit is NA.This is important nuance as well as terms CITES trade is in various forms of unit
## including grams, boxes etc - these are ambiguous and cant be converted.
## Only the NA unit records can be - na is used when the term is the unit so 1 tusk = 1 tusk rather than 1 kg of tusk
left_join(
  CITES_Vert %>% filter(is.na(Unit)|Unit == "Number of specimens", !is.na(WOE)) %>%group_by(Class) %>% summarise(Converted = n()), 
  CITES_Vert %>% group_by(Class) %>% tally()) %>% 
  mutate(Prop = Converted/n)

## only keep the NA unit and the woes with a value
## This reduces the data from 3.7m to 3.0m
CITES_Vert <- CITES_Vert %>% filter(is.na(Unit)|Unit == "Number of specimens", !is.na(WOE))

## In total 434,296,150 vert WOEs moved since the stard of CITES (all sources codes and purposes)
sum(na.omit(CITES_Vert$WOE))

## Figure shows the values for 2021 and 2022 are clearly incomplete.
## it is uncertain how many reports are still pending for 2018 or earlier.
CITES_Vertplot <- CITES_Vert %>% filter(Year %in% c(2000:2022)) %>% group_by(Year) %>% tally()
CITES_Vert %>% group_by(Year) %>% tally() %>% 
  ggplot(aes(Year, n)) + geom_bar(stat = 'identity', width = 1, alpha = 0.5) +
  geom_bar(data = CITES_Vertplot, stat = 'identity', width = 1, fill = "dodgerblue", alpha = 0.5) +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0)) +
  #annotate(x = 1980, y=110000, "text", label = "b.", size = 8) +
  xlab("Year") + ylab("Records") +
  theme_classic(base_size = 14) + 
  theme(axis.text = element_text(angle = 45, hjust = 1, vjust = -1))

## Trim the data to our time frame - change if different time frame is needed.
CITES_Vert <- CITES_Vert %>% filter(Year %in% c(1999:2020))

#### Importer or Exporter #####
## CITES trade can be reported by both the importing and exporting country. So the data set essentially "should" count most things twice.
## These never match up though and for a number of reasons cant really be expected to.
## Best practice is to focus on one reporting source - ultimately we will analyse both but separately.
CITES_Vert %>% group_by(Reporter.type) %>% tally()
#CITES_Vert <- CITES_Vert %>% filter(Reporter.type == "E")

#### Original listing ####

## extract species in trade taxonomic information
## 1227 species in both Imp and Exp reported data sets
(CITES_Species <- CITES_Vert %>% filter(Year %in% c(2000:2020), !grepl("spp", Taxon), !grepl("hybrid", Taxon),
                                        !Appendix == "N",
                                      WildSource == "Yes", PurposeC == "Commercial", WOE >0) %>%
                                      group_by(Taxon) %>% slice(1) %>% select(Class, Order, Family, Genus, Taxon) %>% ungroup()) 

CITES_Vert <- CITES_Vert %>% filter(Year %in% c(2000:2020), !grepl("spp", Taxon), !grepl("hybrid", Taxon),
                                    !Appendix == "N",
                      WildSource == "Yes", PurposeC == "Commercial", WOE >0) 

## This may or may not be relevant to you - I needed this to make individual species time series e.g. if a species was traded yearly 2000 - 2010
## but not after 2010 I needed to add these years and fill the volumes with 0 (not traded - cites doesnt do this its only a record of trade that 
## happened). But the issue is what if the species was only cites listed 2000 - 2012, as cites only records trade in listed species so after 2012 
## trade wouldnt be reported to cites so those years shouldnt have zeros there should be no values.
## Thus this code uses the cites listings database to extract the time periods the species were listed for and add the actual trade and 0's to
## that.

## Read in the cites historic listings data
Historic_CITES <- data.table::fread("Data/CITES/History_of_CITES_Listings_2021.csv") %>% 
  mutate(Year = format(as.Date(EffectiveAt, format="%d/%m/%Y"),"%Y"))

## Get the unique listings from the listing data/
## This tidies and gets the first year a species is CITES listed (the start of its possible time series)
First_listing <- Historic_CITES %>% group_by(Order, Family, Genus, FullName, Year) %>% tally() %>%
  mutate(FullName = ifelse(FullName == Order, NA, FullName),
         FullName = ifelse(FullName == Family, NA, FullName),
         FullName = ifelse(FullName == Genus, NA, FullName)) %>%
  group_by(Order, Family, Genus, FullName) %>% slice_min(Year) %>% ungroup() %>% rename(Taxon = FullName) %>%
  mutate(across(everything(), ~ifelse(.=="", NA, as.character(.))))

## First match all species level CITES listings
FL_SP <- First_listing %>% filter(!is.na(Taxon)) %>% select(Taxon, Year)

Sp_join <- left_join(CITES_Species, FL_SP, by = "Taxon")

sp_done <- Sp_join %>% filter(!is.na(Year)) ## 526 perfect matches

## Second match at genus level
genus_to_match <- Sp_join %>% filter(is.na(Year)) %>% select(-Year)
## get all the genus level appendix listings
FL_Genus <- First_listing %>% filter(is.na(Taxon), !is.na(Genus)) %>% select(Genus, Year) ## 217 listings
Genus_join <- left_join(genus_to_match, FL_Genus, by = "Genus")
Genus_done <- Genus_join %>% filter(!is.na(Year)) ## 308 perfect matches


## third match at family level
## get all the family level listings 53 listings
FL_Family <- First_listing %>% filter(is.na(Taxon), is.na(Genus), !is.na(Family)) %>% select(Family, Year)
Fam_to_match <- Genus_join %>% filter(is.na(Year)) %>% select(-Year)
Fam_join <- left_join(Fam_to_match, FL_Family, by = "Family")
Fam_done <- Fam_join %>% filter(!is.na(Year)) ## 302 perfect matches

## Fourth match at order level
FL_Order <- First_listing %>% filter(is.na(Taxon), is.na(Genus), is.na(Family), !is.na(Order)) %>% select(Order, Year)
Order_to_match <- Fam_join %>% filter(is.na(Year)) %>% select(-Year) ## 56 to still match
Order_join <- left_join(Order_to_match, FL_Order, by = "Order")
Order_done <- Order_join %>% filter(!is.na(Year)) ## 81 perfect matches

All_sp_fl <- rbind(sp_done,Genus_done,Fam_done, Order_done)

All_sp_fl %>% filter(Year >1999)

#### Attach species FL to CITES db ####
FL_CITES_Species <- All_sp_fl %>% rename(FL_year = Year) %>% select(Taxon, FL_year) ## 1217 sp

CITES_Vert <- left_join(CITES_Vert, FL_CITES_Species, by = "Taxon")
check <- CITES_Vert %>% filter(is.na(FL_year))
unique(check$Taxon)

CITES_Vert <- CITES_Vert %>% mutate(FL_year = case_when(Taxon == "Lontra canadensis" ~ 1977, 
                                          Taxon == "Nasua nasua" ~ 1977,
                                          Taxon == "Vulpes vulpes" ~ 1989,
                                          Taxon == "Damaliscus pygargus" ~ 1975,
                                          Taxon == "Hydrictis maculicollis" ~ 1977,
                                          Taxon == "Equus zebra" ~ 1975,
                                          Taxon == "Ovis cycloceros arkal" ~ 2000,
                                          Taxon == "Mustela erminea" ~ 1989,
                                          Taxon == "Hippotragus niger" ~ 1975,
                                          Taxon == "Enhydra lutris" ~ 1975,
                                          TRUE ~ as.numeric(FL_year)))

#### Remove CITES Deletion ####

## extract the true cites deletions
## We considered removing these, but that would bias the data against "success stories". Therefore we
## include them for the period in 2000 - 2018 that they were listed. Therefore a species could have a time series 2000 - 2007.

CITES_Deletions <- Historic_CITES %>% filter(ChangeType == "DELETION" & IsCurrent == "TRUE") %>%
  select(Year, FullName, ChangeType) %>% 
  rename(Taxon = FullName, Year_DEL = Year) %>%
  group_by(Taxon) %>% arrange(Taxon, Year_DEL) %>% slice_max(Year_DEL)

Del_sp <- CITES_Deletions$Taxon
CITES_Vert %>% filter(Taxon %in% Del_sp) %>% summarise(n = (unique(Taxon))) ## 118 speices

## Add the deletions to the trade database as a separate year of deletion collumn.
## Later we will use this as an end point for all these series.
## All other species that had not been deleted will have full time series to 2018, therefore we can add the
## final year (2018) to the species without a deletion year.
CITES_Vert <- left_join(CITES_Vert, CITES_Deletions, by = "Taxon") %>%
  mutate(Year_DEL = ifelse(is.na(Year_DEL), 2020, Year_DEL))



## totals of woes for different classes
(CITES_Vert %>% filter( Year %in% c(2000:2020), !grepl("spp", Taxon), 
                      WildSource == "Yes", PurposeC == "Commercial", WOE >0) %>%
  group_by(Class) %>% tally(WOE))

write.csv(CITES_Vert, "Data/All_Vertebrates/CITES_Vert_Raw.csv", na = "")






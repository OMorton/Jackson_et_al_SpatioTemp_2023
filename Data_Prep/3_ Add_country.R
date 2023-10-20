
CITES_IUCN_data <- data.table::fread("Data/All_Vertebrates/CITES_Vert_Exp_Reported.csv", na.strings = "")
CITES_IUCN_data_IR <- data.table::fread("Data/All_Vertebrates/CITES_Vert_Imp_Reported.csv", na.strings = "")



## https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
Alpha_codes <- data.table::fread("Data/Alpha_codes.csv", na.strings = "")

#### Exporter reported ####

## Exporters
CITES_exp <- CITES_IUCN_data %>% group_by(Class, Name_for_CITESdb, Name_for_rl_history, Exporter, IUCN_code, Threat_code, Year) %>%
  tally(n)

n_distinct(CITES_exp$Name_for_CITESdb) ## 1015

CITES_exp <- CITES_exp %>% ungroup() %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(Exporter, Name_for_CITESdb) %>% filter(sum(n) > 0)

Alpha_sum <- Alpha_codes %>% select(1,2, 6,7) %>% rename(Exporter = 2)

CITES_IUCN_Trade_Data_Match_EXP <- left_join(CITES_exp, Alpha_sum, by = "Exporter")

## This removes former yugoslavia and its one record in 2002 for 7000 turtle doves.
filter(CITES_IUCN_Trade_Data_Match_EXP, is.na(name))
CITES_IUCN_Trade_Data_Match_EXP <- filter(CITES_IUCN_Trade_Data_Match_EXP, !is.na(name))
length(unique(CITES_IUCN_Trade_Data_Match_EXP$Name_for_CITESdb)) # 1015
length(unique(CITES_IUCN_Trade_Data_Match_EXP$Exporter)) # 148

write.csv(CITES_IUCN_Trade_Data_Match_EXP, "Data/All_Vertebrates/Cleaned/ER/CITES_Vert_Exporter_dat_ER.csv", na = "")


## Importers
CITES_imp <- CITES_IUCN_data %>% group_by(Class, Name_for_CITESdb, Name_for_rl_history, Importer, IUCN_code, Threat_code, Year) %>%
  tally(n)

n_distinct(CITES_imp$Name_for_CITESdb) ## 1017

CITES_imp <- CITES_imp %>% ungroup() %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(Importer, Name_for_CITESdb) %>% filter(sum(n) > 0)

Alpha_sum <- Alpha_codes %>% select(1,2, 6,7) %>% rename(Importer = 2)

CITES_IUCN_Trade_Data_Match_IMP <- left_join(CITES_imp, Alpha_sum, by = "Importer")

## This removes former yugoslavia, Kosovo, Soviet Union, Various, Unknown, Former Serbia and its one record in 2002 for 7000 turtle doves.
filter(CITES_IUCN_Trade_Data_Match_IMP, is.na(name)) %>% ungroup() %>% summarise(unique(Importer))
filter(CITES_IUCN_Trade_Data_Match_IMP, is.na(name)) %>% ungroup() %>% summarise(sum(n))
CITES_IUCN_Trade_Data_Match_IMP <- filter(CITES_IUCN_Trade_Data_Match_IMP, !is.na(name))
length(unique(CITES_IUCN_Trade_Data_Match_IMP$Name_for_CITESdb)) # 1014
length(unique(CITES_IUCN_Trade_Data_Match_IMP$Importer)) # 208

write.csv(CITES_IUCN_Trade_Data_Match_IMP, "Data/All_Vertebrates/Cleaned/ER/CITES_Vert_Importer_dat_ER.csv", na = "")


#### Importer reported ####

## Exporters
CITES_exp <- CITES_IUCN_data_IR %>% group_by(Class, Name_for_CITESdb, Name_for_rl_history, Exporter, IUCN_code, Threat_code, Year) %>%
  tally(n)

n_distinct(CITES_exp$Name_for_CITESdb) ## 1085

CITES_exp <- CITES_exp %>% ungroup() %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(Exporter, Name_for_CITESdb) %>% filter(sum(n) > 0)
n_distinct(CITES_exp$Name_for_CITESdb) ## 1083

Alpha_sum <- Alpha_codes %>% select(1,2, 6,7) %>% rename(Exporter = 2)

CITES_IUCN_Trade_Data_Match_EXP <- left_join(CITES_exp, Alpha_sum, by = "Exporter")
## This removes former yugoslavia, various, unknown and the holy see (only 1 trade of 2 Stenella longirostris in 2000).
filter(CITES_IUCN_Trade_Data_Match_EXP, is.na(name)) %>% ungroup() %>% summarise(unique(Exporter))
filter(CITES_IUCN_Trade_Data_Match_EXP, is.na(name)) %>% ungroup() %>% summarise(sum(n))

CITES_IUCN_Trade_Data_Match_EXP <- filter(CITES_IUCN_Trade_Data_Match_EXP, !is.na(name))
length(unique(CITES_IUCN_Trade_Data_Match_EXP$Name_for_CITESdb)) # 1081
length(unique(CITES_IUCN_Trade_Data_Match_EXP$Exporter)) # 171

write.csv(CITES_IUCN_Trade_Data_Match_EXP, "Data/All_Vertebrates/Cleaned/IR/CITES_Vert_Exporter_dat_IR.csv", na = "")


## Importers
CITES_imp <- CITES_IUCN_data_IR %>% group_by(Class, Name_for_CITESdb, Name_for_rl_history, Importer, IUCN_code, Threat_code, Year) %>%
  tally(n)

n_distinct(CITES_imp$Name_for_CITESdb) ## 1085

CITES_imp <- CITES_imp %>% ungroup() %>%
  ## remove countries with all 0's after removing the year 1999
  group_by(Importer, Name_for_CITESdb) %>% filter(sum(n) > 0)
n_distinct(CITES_imp$Name_for_CITESdb) ## 1083

Alpha_sum <- Alpha_codes %>% select(1,2, 6,7) %>% rename(Importer = 2)

CITES_IUCN_Trade_Data_Match_IMP <- left_join(CITES_imp, Alpha_sum, by = "Importer")

## This removes former yugoslavia and former serbia and montrenegro
filter(CITES_IUCN_Trade_Data_Match_IMP, is.na(name)) %>% ungroup() %>% summarise(unique(Importer))
filter(CITES_IUCN_Trade_Data_Match_IMP, is.na(name)) %>% ungroup() %>% summarise(sum(n))

CITES_IUCN_Trade_Data_Match_IMP <- filter(CITES_IUCN_Trade_Data_Match_IMP, !is.na(name))
length(unique(CITES_IUCN_Trade_Data_Match_IMP$Name_for_CITESdb)) # 1083
length(unique(CITES_IUCN_Trade_Data_Match_IMP$Importer)) # 135

write.csv(CITES_IUCN_Trade_Data_Match_IMP, "Data/All_Vertebrates/Cleaned/IR/CITES_Vert_Importer_dat_IR.csv", na = "")





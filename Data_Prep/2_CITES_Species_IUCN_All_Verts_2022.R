
library(rredlist)

CITES_Vert <- data.table::fread("Data/All_Vertebrates/CITES_Vert_RawH.csv", na.strings = "")

## 1240 species in both Importer and Exported Data
Sp_List <- CITES_Vert %>% group_by(Taxon) %>% tally() %>% select(Taxon) %>% as.data.frame()
CITES_Vert %>% filter(Year > 1999) %>% group_by(WildSource) %>% summarise(n_distinct(Taxon))


##  1 domestic species.
## 6 sub species specific statuses to manually add.
## 9 never evaluated species.
NA_update <- Sp_List %>% mutate(IUCNName = case_when(Taxon == "Aceros cassidix" ~ "Rhyticeros cassidix",
                                                   Taxon == "Aceros corrugatus" ~"Rhabdotorrhinus corrugatus",
                                                   Taxon == "Aceros leucocephalus" ~ "Rhabdotorrhinus leucocephalus",
                                                   Taxon == "Aglaiocercus kingi" ~ "Aglaiocercus kingii",
                                                   Taxon == "Alisterus chloropterus mozskowskii" ~ "Alisterus chloropterus",
                                                   Taxon == "Amazilia amazilia" ~ "Amazilis amazilia",
                                                   Taxon == "Amazilia cyanocephala" ~ "Saucerottia cyanocephala",
                                                   Taxon == "Amazilia julie" ~ "Chlorestes julie",
                                                   Taxon == "Lonchura oryzivora" ~ "Padda oryzivora",
                                                   Taxon == "Amazona festiva festiva" ~ "Amazona festiva",
                                                   Taxon == "Amazona mercenaria" ~ "Amazona mercenarius",
                                                   Taxon == "Amazona xanthops" ~ "Alipiopsitta xanthops",
                                                   Taxon == "Anas clypeata" ~ "Spatula clypeata",
                                                   Taxon == "Anas formosa" ~ "Sibirionetta formosa",
                                                   Taxon == "Anas penelope" ~ "Mareca penelope",
                                                   Taxon == "Anas querquedula" ~ "Spatula querquedula",
                                                   Taxon == "Aquila clanga" ~ "Clanga clanga",
                                                   Taxon == "Aquila pomarina" ~ "Clanga pomarina",
                                                   Taxon == "Aratinga acuticaudata" ~ "Psittacara acuticaudatus",
                                                   Taxon == "Aratinga aurea" ~ "Eupsittula aurea",
                                                   Taxon == "Aratinga cactorum" ~ "Eupsittula cactorum",
                                                   Taxon == "Aratinga canicularis" ~ "Eupsittula canicularis",
                                                   Taxon == "Aratinga chloroptera" ~ "Psittacara chloropterus",
                                                   Taxon == "Aratinga erythrogenys" ~ "Psittacara erythrogenys",
                                                   Taxon == "Aratinga euops" ~ "Psittacara euops",
                                                   Taxon == "Aratinga finschi" ~ "Psittacara finschi",
                                                   Taxon == "Aratinga holochlora" ~ "Psittacara holochlorus",
                                                   Taxon == "Aratinga leucophthalma" ~ "Psittacara leucophthalmus",
                                                   Taxon == "Aratinga mitrata" ~ "Psittacara mitratus",
                                                   Taxon == "Aratinga nana" ~ "Eupsittula nana",
                                                   Taxon == "Aratinga pertinax" ~ "Eupsittula pertinax",
                                                   Taxon == "Aratinga wagleri" ~ "Psittacara wagleri",
                                                   Taxon == "Baillonius bailloni" ~ "Pteroglossus bailloni",
                                                   Taxon == "Barnardius barnardi" ~ "Barnardius zonarius",
                                                   Taxon == "Branta canadensis leucopareia" ~ "Branta canadensis",
                                                   Taxon == "Bubo bubo bengalensis" ~ "Bubo bubo",
                                                   Taxon == "Buceros hydrocorax hydrocorax" ~ "Buceros hydrocorax",
                                                   Taxon == "Buteo magnirostris" ~ "Rupornis magnirostris",
                                                   Taxon == "Buteo poecilochrous" ~ "Geranoaetus polyosoma",
                                                   Taxon == "Buteo polyosoma" ~ "Geranoaetus polyosoma",
                                                   Taxon == "Calyptorhynchus baudinii" ~ "Zanda baudinii",
                                                   Taxon == "Calyptorhynchus funereus" ~ "Zanda funerea",
                                                   Taxon == "Calyptorhynchus latirostris" ~ "Zanda latirostris",
                                                   Taxon == "Carduelis cucullata" ~ "Spinus cucullatus",
                                                   Taxon == "Carduelis yarrellii" ~ "Spinus yarrellii", 
                                                   Taxon == "Chalcopsitta sintillata" ~ "Chalcopsitta scintillata",
                                                   Taxon == "Damophila julie" ~ "Amazilia julie",
                                                   Taxon == "Eos rubra" ~ "Eos bornea",
                                                   Taxon == "Eos squamata riciniata" ~ "Eos squamata",
                                                   Taxon == "Estrilda caerulescens" ~ "Estrilda coerulescens",
                                                   Taxon == "Eupodotis vigorsii" ~ "Heterotetrax vigorsii",
                                                   Taxon == "Falco pelegrinoides" ~ "Falco peregrinus",
                                                   Taxon == "Falco pelegrinoides babylonicus" ~ "Falco peregrinus",
                                                   Taxon == "Falco peregrinus anatum" ~ "Falco peregrinus",
                                                   Taxon == "Falco peregrinus peregrinus" ~ "Falco peregrinus",
                                                   Taxon == "Grus canadensis pratensis" ~ "Grus canadensis",
                                                   Taxon == "Grus leucogeranus" ~ "Leucogeranus leucogeranus",
                                                   Taxon == "Guarouba guarouba" ~ "Guaruba guarouba",
                                                   Taxon == "Gyps rueppellii" ~ "Gyps rueppelli",
                                                   Taxon == "Hieraaetus fasciatus" ~ "Aquila fasciata",
                                                   Taxon == "Hieraaetus spilogaster" ~ "Aquila spilogaster",
                                                   Taxon == "Leptoptilos crumeniferus" ~ "Leptoptilos crumenifer",
                                                   Taxon == "Leucopternis albicollis" ~ "Pseudastur albicollis",
                                                   Taxon == "Lonchura bicolor" ~ "Spermestes bicolor",
                                                   Taxon == "Lonchura cantans" ~ "Euodice cantans",
                                                   Taxon == "Lonchura cucullata" ~ "Spermestes cucullata",
                                                   Taxon == "Lonchura fringilloides" ~ "Spermestes fringilloides",
                                                   Taxon == "Lophura imperialis" ~ "Lophura imperialis", ## not in this
                                                   Taxon == "Morelia boeleni" ~ "Simalia boeleni",
                                                   Taxon == "Nandayus nenday" ~ "Aratinga nenday",
                                                   Taxon == "Nigrita canicapilla" ~ "Nigrita canicapillus",
                                                   Taxon == "Nyctea scandiaca" ~ "Bubo scandiacus",
                                                   Taxon == "Orthopsittaca manilata" ~ "Orthopsittaca manilatus",
                                                   Taxon == "Otus asio" ~ "Megascops asio",
                                                   Taxon == "Otus choliba" ~ "Megascops choliba",
                                                   Taxon == "Otus kennicottii" ~ "Megascops kennicottii",
                                                   Taxon == "Otus roboratus" ~ "Megascops roboratus",
                                                   Taxon == "Otus watsonii" ~ "Megascops watsonii",
                                                   Taxon == "Paradisaea rudolphi" ~ "Paradisornis rudolphi",
                                                   Taxon == "Phoenicopterus ruber ruber" ~ "Phoenicopterus ruber",
                                                   Taxon == "Pionopsitta barrabandi" ~ "Pyrilia barrabandi",
                                                   Taxon == "Pitta guajana" ~ "Hydrornis guajanus",
                                                   Taxon == "Platycercus barnardi" ~ "Barnardius zonarius",
                                                   Taxon == "Platycercus zonarius" ~ "Barnardius zonarius",
                                                   Taxon == "Poephila cincta cincta" ~ "Poephila cincta",
                                                   Taxon == "Psephotus chrysopterygius" ~ "Psephotellus chrysopterygius",
                                                   Taxon == "Psephotus dissimilis" ~ "Psephotellus dissimilis",
                                                   Taxon == "Psephotus pulcherrimus" ~ "Psephotellus pulcherrimus",
                                                   Taxon == "Psephotus varius" ~ "Psephotellus varius",
                                                   Taxon == "Pseudoscops clamator" ~ "Asio clamator",
                                                   Taxon == "Psittacula calthorpae" ~ "Nicopsitta calthrapae",
                                                   Taxon == "Psittacula columboides" ~ "Nicopsitta columboides",
                                                   Taxon == "Psittacula cyanocephala" ~ "Himalayapsitta cyanocephala",
                                                   Taxon == "Psittacula echo" ~ "Alexandrinus eques",
                                                   Taxon == "Psittacula eupatria" ~ "Palaeornis eupatria",
                                                   Taxon == "Psittacula finschii" ~ "Himalayapsitta finschii",
                                                   Taxon == "Psittacula himalayana" ~ "Himalayapsitta himalayana",
                                                   Taxon == "Psittacula krameri" ~ "Alexandrinus krameri",
                                                   Taxon == "Psittacula longicauda" ~ "Belocercus longicaudus",
                                                   Taxon == "Psittacula roseata" ~ "Himalayapsitta roseata",
                                                   Taxon == "Psittacus erithacus timneh" ~ "Psittacus timneh",
                                                   Taxon == "Psitteuteles iris" ~ "Trichoglossus iris",
                                                   Taxon == "Pterocnemia pennata" ~ "Rhea pennata",
                                                   Taxon == "Pterocnemia pennata pennata" ~ "Rhea pennata",
                                                   Taxon == "Ptiloris magnificus" ~ "Lophorina magnifica",
                                                   Taxon == "Rhea americana albescens" ~ "Rhea americana",
                                                   Taxon == "Serinus leucopygius" ~ "Crithagra leucopygia",
                                                   Taxon == "Serinus mozambicus" ~ "Crithagra mozambica",
                                                   Taxon == "Spizaetus africanus" ~ "Aquila africana",
                                                   Taxon == "Spizaetus nipalensis" ~ "Nisaetus nipalensis",
                                                   Taxon == "Spizastur melanoleucus" ~ "Spizaetus melanoleucus",
                                                   Taxon == "Streptopelia senegalensis" ~ "Spilopelia senegalensis",
                                                   Taxon == "Strix virgata" ~ "Ciccaba virgata",
                                                   Taxon == "Tauraco porphyreolophus" ~ "Gallirex porphyreolophus",
                                                   Taxon == "Torgos tracheliotus" ~ "Torgos tracheliotos",
                                                   Taxon == "Aldabrachelys gigantea" ~ "Geochelone gigantea",
                                                   Taxon == "Anaplectes melanotis" ~ "Anaplectes rubriceps",
                                                   Taxon == "Antilocapra americana mexicana" ~ "Antilocapra americana",
                                                   Taxon == "Bison bison athabascae" ~ "Bison bison",
                                                   Taxon == "Buteo albicaudatus" ~ "Geranoaetus albicaudatus",
                                                   Taxon == "Caiman crocodilus crocodilus" ~ "Caiman crocodilus",
                                                   Taxon == "Caiman crocodilus fuscus" ~ "Caiman crocodilus",
                                                   Taxon == "Caiman crocodilus yacare" ~ "Caiman yacare",
                                                   Taxon == "Callithrix argentata" ~ "Mico argentatus",
                                                   Taxon == "Callithrix pygmaea" ~ "Cebuella pygmaea",
                                                   Taxon == "Canis lupus crassodon" ~ "Canis lupus",
                                                   Taxon == "Canis lupus monstrabilis" ~ "Canis lupus",
                                                   Taxon == "Cebus apella" ~ "Sapajus apella",
                                                   Taxon == "Ceratotherium simum simum" ~ "Ceratotherium simum ssp. simum", ## Add manually
                                                   Taxon == "Cercocebus albigena" ~ "Lophocebus albigena",
                                                   Taxon == "Cercopithecus dryas" ~ "Chlorocebus dryas",
                                                   Taxon == "Cercopithecus kandti" ~ "Cercopithecus mitis ssp. kandti", ## Add manually
                                                   Taxon == "Cercopithecus lhoesti" ~ "Allochrocebus lhoesti",
                                                   Taxon == "Chelonoidis carbonarius" ~ "Chelonoidis carbonarius", ## NE
                                                   Taxon == "Chelonoidis denticulatus" ~ "Chelonoidis denticulata",
                                                   Taxon == "Crocodylus cataphractus" ~ "Mecistops cataphractus",
                                                   Taxon == "Cyclagras gigas" ~ "Hydrodynastes gigas",
                                                   Taxon == "Damaliscus pygargus pygargus" ~ "Damaliscus pygargus ssp. pygargus", ## Add manually
                                                   Taxon == "Diphyllodes magnificus" ~ "Cicinnurus magnificus",
                                                   Taxon == "Epicrates angulifer" ~ "Chilabothrus angulifer",
                                                   Taxon == "Epicrates cenchria cenchria" ~ "Epicrates cenchria",
                                                   Taxon == "Epicrates fordii" ~ "Chilabothrus fordii",
                                                   Taxon == "Epicrates gracilis" ~ "Chilabothrus gracilis",
                                                   Taxon == "Epicrates striatus" ~ "Chilabothrus striatus",
                                                   Taxon == "Epimachus fastuosus" ~ "Epimachus fastosus",
                                                   Taxon == "Equus zebra hartmannae" ~ "Equus zebra ssp. hartmannae", ## Add manually
                                                   Taxon == "Equus zebra zebra" ~ "Equus zebra ssp. zebra", ## Add manually
                                                   Taxon == "Galago alleni" ~ "Sciurocheirus alleni",
                                                   Taxon == "Galago demidoff" ~ "Galagoides demidoff",
                                                   Taxon == "Gongylophis colubrinus" ~ "Eryx colubrinus",
                                                   Taxon == "Gongylophis muelleri" ~ "Eryx muelleri",
                                                   Taxon == "Kinixys belliana" ~ "Kinixys belliana", ## NE
                                                   Taxon == "Kinixys nogueyi" ~ "Kinixys nogueyi", ## NE
                                                   Taxon == "Kinixys spekii" ~ "Kinixys spekii", ## NE
                                                   Taxon == "Lagothrix lagotricha" ~ "Lagothrix lagothricha",
                                                   Taxon == "Leiopython hoserae" ~ "Leiopython meridionalis",
                                                   Taxon == "Manis gigantea" ~ "Smutsia gigantea",
                                                   Taxon == "Manis temminckii" ~ "Smutsia temminckii",
                                                   Taxon == "Manis tetradactyla" ~ "Phataginus tetradactyla",
                                                   Taxon == "Manis tricuspis" ~ "Phataginus tricuspis",
                                                   Taxon == "Mazama temama cerasina" ~ "Mazama temama",
                                                   Taxon == "Morelia clastolepis" ~ "Simalia clastolepis",
                                                   Taxon == "Morelia nauta" ~ "Simalia nauta",
                                                   Taxon == "Morelia tracyae" ~ "Simalia tracyae",
                                                   Taxon == "Nasua nasua solitaria" ~ "Nasua nasua",
                                                   Taxon == "Nigrita fusconota" ~ "Nigrita fusconotus",
                                                   Taxon == "Odocoileus virginianus mayensis" ~ "Odocoileus virginianus",
                                                   Taxon == "Ovis aries" ~ "Ovis aries", ## Domestic Sheep REMOVE
                                                   Taxon == "Ovis bochariensis" ~ "Ovis vignei",
                                                   Taxon == "Ovis cycloceros arkal" ~ "Ovis vignei",
                                                   Taxon == "Ovis darwini" ~ "Ovis ammon",
                                                   Taxon == "Ovis polii" ~ "Ovis ammon",
                                                   Taxon == "Pelodiscus axenaria" ~ "Pelodiscus axenaria", ## NE
                                                   Taxon == "Pelomedusa subrufa" ~ "Pelomedusa galeata",
                                                   Taxon == "Pelusios adansonii" ~ "Pelusios adansonii", ## NE
                                                   Taxon == "Pelusios castaneus" ~ "Pelusios castaneus", ## NE
                                                   Taxon == "Pelusios gabonensis" ~ "Pelusios gabonensis", ## NE
                                                   Taxon == "Prionailurus bengalensis bengalensis" ~ "Prionailurus bengalensis",
                                                   Taxon == "Psammobates oculifer" ~ "Psammobates oculifer", ## NE
                                                   Taxon == "Ptyas mucosus" ~ "Ptyas mucosa",
                                                   Taxon == "Puma concolor couguar" ~ "Puma concolor",
                                                   Taxon == "Python reticulatus" ~ "Malayopython reticulatus",
                                                   Taxon == "Rupicapra pyrenaica ornata" ~ "Rupicapra pyrenaica",
                                                   Taxon == "Saguinus fuscicollis" ~ "Leontocebus fuscicollis",
                                                   Taxon == "Saguinus nigricollis" ~ "Leontocebus nigricollis",
                                                   Taxon == "Serinus gularis" ~ "Crithagra gularis",
                                                   Taxon == "Trachypithecus villosus" ~ "Trachypithecus cristatus",
                                                   Taxon == "Ursus americanus emmonsii" ~ "Ursus americanus",
                                                   Taxon == "Varanus ornatus" ~ "Varanus niloticus",
                                                   Taxon == "Vulpes vulpes griffithi" ~ "Vulpes vulpes",
                                                   Taxon == "Vulpes vulpes montana" ~ "Vulpes vulpes",
                                                   Taxon == "Xenochrophis piscator" ~ "Fowlea piscator",
                                                   Taxon == "Zaglossus bruijni" ~ "Zaglossus bruijnii",
                                                   Taxon == "Cervus elaphus bactrianus" ~ "Cervus elaphus",
                                                   Taxon == "Hippotragus niger variani" ~ "Hippotragus niger ssp. variani", ## ssp added manually
                                                   Taxon == "Hyaena brunnea" ~ "Parahyaena brunnea",
                                                   Taxon == "Ovis nigrimontana" ~ "Ovis ammon",
                                                   TRUE ~ Taxon))


#### Add the IUCN Statuses ####

## get key
## token can be generated here
## https://apiv3.iucnredlist.org/api/v3/token
apikey <- "a3fc116c122aefc621329055aeae8f67483e575c518acc14fcb77709bd94f6a2"

length(unique(NA_update$Taxon))
length(unique(NA_update$IUCNName))

## Dummy data frame for the loops
df <- data.frame(IUCNName = character(),
                 Year = character(),
                 IUCN_code = character(),
                 IUCN_cat = character())

## Get species historical statuses.
## Needs an API key and because of the delay needed between calls takes ~1 hour to run.
## Do not remove the delay see rredlist guide for details.
## https://cran.r-project.org/web/packages/rredlist/rredlist.pdf

for(i in 1:nrow(NA_update)){ # would have used for(sp in speciesList) but need i for progress bar?
  ## incorporate 2s delay between each query
  Sys.sleep(2)
  ## Progress update
  cat('Species=',i, '\n')
  ## get historical data from website
  sp <- NA_update$IUCNName[i]
  iucnHistory <- rl_history(name=sp, key=apikey)
  # IF species cannot be found
  if (length(iucnHistory$result) == 0){ 
    spDf <- data.frame(IUCNName = sp,
                       Year = NA,
                       IUCN_code = NA,
                       IUCN_cat = NA)
    df <- rbind(df, spDf)
    # cat('Check ', sp, '\n')
  } else { 
    spdf <- data.frame(IUCNName = sp,
                       Year = iucnHistory$result$year,
                       IUCN_code = iucnHistory$result$code,
                       IUCN_cat = iucnHistory$result$category)
    df <- rbind(df, spdf)
  }
}

rl_history(name="Trachypithecus cristatus", key = apikey)

## write out the the data for speed.
#write.csv(df, "Data/All_Vertebrates/IUCN_API/IUCN_API_NA_ASSESSMENTS.csv")  
df <- read.csv("Data/All_Vertebrates/IUCN_API/IUCN_API_NA_ASSESSMENTS.csv") %>% select(-X)

## 16 NA species (this is as expected as per the totals above)
df %>% filter(is.na(Year))

Ssp_code <- rbind(data.frame(Taxon = "Equus zebra zebra", IUCNName = "Equus zebra ssp. zebra", Year = c(1996, 2008, 2019), 
                             IUCN_code = c("EN", "VU", "LC")),
                  data.frame(Taxon = "Hippotragus niger variani", IUCNName = "Hippotragus niger ssp. variani", Year = c(1996, 2008, 2016), 
                             IUCN_code = c("CR", "CR", "CR")),
                  data.frame(Taxon = "Equus zebra hartmannae", IUCNName = "Equus zebra ssp. hartmannae", Year = c(1996, 2008, 2019), 
                             IUCN_code = c("EN", "VU", "VU")),
                  data.frame(Taxon = "Damaliscus pygargus pygargus", IUCNName = "Damaliscus pygargus ssp. pygargus", Year = c(1996, 2003, 2008, 2017), 
                             IUCN_code = c("VU", "VU", "NT", "VU")),
                  data.frame(Taxon = "Cercopithecus kandti", IUCNName = "Cercopithecus mitis ssp. kandti", Year = c(2000, 2008, 2020), 
                             IUCN_code = c("EN", "EN", "EN")),
                  data.frame(Taxon = "Ceratotherium simum simum", IUCNName = "Ceratotherium simum ssp. simum", Year = c(2000, 2020), 
                             IUCN_code = c("NT", "NT"))) %>% mutate(IUCN_cat = NA) %>% select(- Taxon)

df <- rbind(filter(df, !IUCNName %in% c("Equus zebra ssp. zebra", "Hippotragus niger ssp. variani",
                                        "Equus zebra ssp. hartmannae", "Damaliscus pygargus ssp. pygargus", 
                                     "Cercopithecus mitis ssp. kandti", "Ceratotherium simum ssp. simum")),
            Ssp_code)

## IUCN names will always be less than Taxon as some CITES sp/ssp resolve only to a single IUCN recognized taxon. 
length(unique(df$IUCNName)) ## 1216
df_all <- left_join(NA_update, df, by = "IUCNName")
length(unique(df_all$Taxon)) ## 1224


#### Cleaning pre 2000 IUCN codes ####
## Check the early version of the IUCN are updated prior to 2000
## all instance where the following codes are given species are updated again before 2000
check <- df_all %>% group_by(Taxon) %>% filter(any(IUCN_code %in% c("CT", "NR", "K", "R", "T", "V", "E", "I", "nt", NA)))
length(unique(df_all$Taxon)) ## 1224


## Therefore remove these
Historic_IUCN <- df_all %>% 
  ## Set Not assessed species as not assessed in 2000 otherwise they are also NA for Year and get removed later when they shouldnt
  mutate(Year = if_else(is.na(Year), 2000, as.numeric(Year))) %>%
  filter(!IUCN_code %in% c("CT", "NR", "K", "R", "T", "V", "E", "I", "nt"))

length(unique(Historic_IUCN$Taxon)) ## 1240

## Only EX and EW species are a single Chelonoidis niger (in 2000) and 2 Oryx from wild sources in 2017 and 19.
## Remove these further on.
Historic_IUCN %>% filter(IUCN_code %in% c("EX", "EW"))

## Convert the 1994 system to post-2000
Historic_IUCN_up <- Historic_IUCN %>%
  mutate(IUCN_code = replace(IUCN_code, IUCN_code == "LR/lc", "LC"),
         IUCN_code = replace(IUCN_code, IUCN_code == "LR/nt", "NT"),
         IUCN_code = replace(IUCN_code, IUCN_code == "LR/cd", "NT"))

## Check removal and conversion left only the post 2001 framework
unique(Historic_IUCN_up$IUCN_code)
unique(Historic_IUCN_up$Taxon) ## 1240

## Backbone of values 2000 - 2020
backbone <- expand.grid(Year = as.integer(1975:2020), Taxon = unique(Historic_IUCN_up$Taxon))

## left join this and create your unrolled status
## Some species in trade before being IUCN assessed these are the NA values
Historic_IUCN_up$Year <- as.integer(Historic_IUCN_up$Year)
backbone$Year <- as.integer(backbone$Year)

## Here we add the backbone of species and dates to the IUCN data
df_new <- left_join(backbone, Historic_IUCN_up) %>%
  arrange(Taxon, Year) %>% group_by(Taxon) %>% 
  fill(IUCN_code , .direction = "down") %>% 
  fill(IUCNName, .direction = "down") %>% 
  fill(IUCNName, .direction = "up") %>% 
  fill(Taxon, .direction = "down") %>% 
  fill(Taxon, .direction = "up") %>% 
  filter(Year %in% c(1999:2020)) %>% ungroup() %>%
  mutate(Year = as.numeric(Year) - 2000) %>% 
  select(Year, Taxon, IUCN_code, IUCNName, Taxon) %>% 
  mutate(IUCN_code = replace_na(IUCN_code, "Not assessed")) %>% distinct()

## Now we have a clean time series of all statuses for all species 1999 - 2020 (-1 - 20)
length(unique(df_new$Taxon)) ## 1227 species
## Tidy up and remove the 2 ex and ew sp, the hybrid and the domestic sheep.
df_new <- df_new %>% filter(!Taxon %in% c("Chelonoidis niger", "Oryx dammah", "Ovis aries"))
length(unique(df_new$Taxon)) ## 1237 species

## Check no species were assessed twice in one year (2 records for 1 year interfere with the joins)
df_new %>% group_by(Year, Taxon) %>% tally() %>% filter(n != 1)
df_new <- df_new %>% mutate(filt = paste(Taxon, Year, IUCN_code)) %>% filter(filt !="Macaca nemestrina 20 VU")

#### Custom Species TimeFrames #####

## Tidy up and remove the 2 ex and ew sp, the hybrid and the domestic sheep.
CITES_Vert_Clean <- CITES_Vert %>% filter(!Taxon %in% c("Chelonoidis niger", "Oryx dammah", "Ovis aries"))
length(unique(CITES_Vert_Clean$Taxon)) ## 1237 species


#### Exporter Series ####
## Get all trades to volumes per species per year
CITES_Vert_pro <- CITES_Vert %>% filter(Reporter.type == "E") %>%
  ## Group and tally per species/year
  group_by(Year, Taxon, Class, Exporter, Importer, FL_year, Year_DEL) %>% 
  tally(WOE) %>% group_by(Taxon, Class, Exporter) %>% filter(n() > 0) %>%
  ## here we remove record for species that are not cites listed and as such lack a year they were listed
  filter(!is.na(FL_year)) %>% mutate(FL_year = as.numeric(FL_year), Year_DEL = as.numeric(Year_DEL))

## Create custom length time series for each species potential presence in trade
## the series begins when the species was listed, this prevent us marking species as absent from trade in years when 
## that species wasnt listed on the cites appendices
## We use the FL_year to start the series (when the species was first listed) and the Year_DEL column to end it.
## Thus creating a series corrected for each species first listing and/or its deletion from the appendices.
## Except for the complex cases which potentially move in and out and in (and out) again of the appendices.
Species_timeframe <- CITES_Vert_pro %>% group_by(Taxon, Class, Exporter, Importer) %>% 
  summarise(Year = seq(from = min(FL_year), to = max(Year_DEL), length.out = max(Year_DEL) - min(FL_year) + 1))

## 1018
length(unique(Species_timeframe$Taxon))

## We automate the process but at this point we checked all species that our code produced as being listed then
## removed. We manually read the the historic cites listings for each species.
Manual_check <- Species_timeframe %>% group_by(Taxon) %>% filter(Year == max(Year)) %>% filter(Year != 2020)

## Get the complex cases
Historic_CITES <- data.table::fread("Data/CITES/History_of_CITES_Listings_2021.csv") %>% 
  mutate(Year = format(as.Date(EffectiveAt, format="%d/%m/%Y"),"%Y"))
Complex_series <- rbind((filter(Historic_CITES, ChangeType == "DELETION" & IsCurrent == "TRUE") %>%
                           select(Year, FullName, ChangeType)), (filter(Historic_CITES, IsCurrent == "TRUE" & ChangeType == "ADDITION") %>%
                                                                   select(Year, FullName, ChangeType))) %>%
  group_by(FullName) %>% filter(n() > 1) %>% filter(n_distinct(ChangeType) > 1)

Complex_series_SP <- unique(Complex_series$FullName)
CXSP <- Species_timeframe %>% filter(Taxon %in% Complex_series_SP) %>% summarise(Taxon = unique(Taxon))
unique(CXSP$Taxon)

## Complex cases - 2 species with "TRUE" Additions and Deletions in our final dataset
## Dendrocygna autumnalis - CITES listed since 1975 in App III briefly moved out in 85, added back in 87. 
## Dendrocygna bicolor - CITES listed since 1975 in App III briefly moved out in 85, added back in 87.

## Actions on complex cases - all species are still cites listed throughout our study period except trionyx which was briefly not. 
Complex_Species_timeframe <- rbind(expand.grid(Taxon = "Dendrocygna autumnalis", Year = c(1975:2020)),
                           expand.grid(Taxon = "Dendrocygna bicolor", Year = c(1975:2020)),
                           expand.grid(Taxon = "Batagur borneoensis", Year = c(1997:2020)),
                           expand.grid(Taxon = "Mellivora capensis", Year = c(1976:2020)),
                           expand.grid(Taxon = "Trionyx triunguis", Year = c(1976:2007, 2017:2020)))

Complex_fix <- full_join(Complex_Species_timeframe, CXSP)
Species_timeframe_full <- rbind(filter(Species_timeframe, !Taxon %in% c("Batagur borneoensis", "Dendrocygna autumnalis", "Dendrocygna bicolor",
                                              "Mellivora capensis", "Trionyx triunguis")),Complex_fix) %>% filter(Year > 1999)

## 1017 sp
length(unique(Species_timeframe_full$Taxon))
## 1017 lose one species Hippotragus equinus as it was traded when it wasnt listed.
length(unique(CITES_Vert_pro$Taxon))


## Expand the data set to fill missing years between 2000 - 2018
CITES_Wild_Com_Exp <- left_join(Species_timeframe_full, CITES_Vert_pro, by = c("Taxon", "Exporter", "Importer", "Year", "Class"))

## 1017
length(unique(CITES_Wild_Com_Exp$Taxon))

## For 2000 - 2018 when the species are listed and present in the appendices, by our species specific time series,
## if the species is absent its a zero volume. We go back to 1999 for the lagged volume.
CITES_Com <- CITES_Wild_Com_Exp %>% 
  group_by(Taxon) %>% 
  mutate(n = replace_na(n, 0)) %>%
  select(-FL_year, -Year_DEL) %>% 
  mutate(Year = Year -2000)


CITES_Vert_Series <- left_join(CITES_Com, df_new, by = c("Year", "Taxon")) %>%
  mutate(Name_for_CITESdb = Taxon,
         Name_for_rl_history = if_else(is.na(IUCNName), Taxon, IUCNName))

length(unique(CITES_Vert_Series$Name_for_CITESdb)) ## now 1017
length(unique(CITES_Vert_Series$Name_for_rl_history)) ## now 1006
length(unique(CITES_Vert_Series$Taxon)) ## now 1017


## This code prepares the two versions of IUCN assessments.
## IUCN_code is the "true" state of assessments. Species are marked as Not assessed prior to having an assessment.
## So a species would go from Not assessed to LC to NT.
CITES_IUCN_data <- CITES_Vert_Series %>% 
  mutate(IUCN_code = case_when(is.na(IUCN_code) ~ "Not assessed", 
                               TRUE ~ IUCN_code)) %>%
  mutate(Threat_code = case_when(IUCN_code == "LC" ~ "Non-threatened",
                                 IUCN_code == "NT" ~ "Non-threatened",
                                 IUCN_code == "VU" ~ "Threatened",
                                 IUCN_code == "EN" ~ "Threatened",
                                 IUCN_code == "CR" ~ "Threatened",
                                 IUCN_code == "EW" ~ "Threatened",
                                 IUCN_code == "Not assessed" ~ "Not assessed",
                                 IUCN_code == "DD" ~ "Not assessed",
                                 TRUE ~ "ERROR"))

CITES_IUCN_data %>% group_by(IUCN_code) %>% tally()
CITES_IUCN_data %>% group_by(Threat_code) %>% tally()

length(unique(CITES_IUCN_data$Name_for_rl_history)) ## 1006
length(unique(CITES_IUCN_data$Name_for_CITESdb)) ## 1017

## Write this data into a csv to be read into subsequent analysis to save time.
write.csv(CITES_IUCN_data, "Data/All_Vertebrates/CITES_Vert_Exp_Reported.csv", na = "")


#### Importer Series ####
## Get all trades to volumes per species per year
CITES_Vert_pro_I <- CITES_Vert %>% filter(Reporter.type == "I") %>%
  ## Group and tally per species/year
  group_by(Year, Taxon, Class, Exporter, Importer, FL_year, Year_DEL) %>% 
  tally(WOE) %>% group_by(Taxon, Class, Exporter) %>% filter(n() > 0) %>%
  ## here we remove record for species that are not cites listed and as such lack a year they were listed
  filter(!is.na(FL_year)) %>% mutate(FL_year = as.numeric(FL_year), Year_DEL = as.numeric(Year_DEL))

## Create custom length time series for each species potential presence in trade
## the series begins when the species was listed, this prevent us marking species as absent from trade in years when 
## that species wasnt listed on the cites appendices
## We use the FL_year to start the series (when the species was first listed) and the Year_DEL column to end it.
## Thus creating a series corrected for each species first listing and/or its deletion from the appendices.
## Except for the complex cases which potentially move in and out and in (and out) again of the appendices.
Species_timeframe_I <- CITES_Vert_pro_I %>% group_by(Taxon, Class, Exporter, Importer) %>% 
  summarise(Year = seq(from = min(FL_year), to = max(Year_DEL), length.out = max(Year_DEL) - min(FL_year) + 1))

## 1085
length(unique(Species_timeframe_I$Taxon))

## We automate the process but at this point we checked all species that our code produced as being listed then
## removed. We manually read the the historic cites listings for each species.
Manual_check_I <- Species_timeframe_I %>% group_by(Taxon) %>% filter(Year == max(Year)) %>% filter(Year != 2020)

## Get the complex cases
Historic_CITES <- data.table::fread("Data/CITES/History_of_CITES_Listings_2021.csv") %>% 
  mutate(Year = format(as.Date(EffectiveAt, format="%d/%m/%Y"),"%Y"))
Complex_series <- rbind((filter(Historic_CITES, ChangeType == "DELETION" & IsCurrent == "TRUE") %>%
                           select(Year, FullName, ChangeType)), (filter(Historic_CITES, IsCurrent == "TRUE" & ChangeType == "ADDITION") %>%
                                                                   select(Year, FullName, ChangeType))) %>%
  group_by(FullName) %>% filter(n() > 1) %>% filter(n_distinct(ChangeType) > 1)

Complex_series_SP <- unique(Complex_series$FullName)
CXSP_I <- Species_timeframe_I %>% filter(Taxon %in% Complex_series_SP) %>% summarise(Taxon = unique(Taxon))
unique(CXSP_I$Taxon)

## Complex cases - 2 species with "TRUE" Additions and Deletions in our final dataset
## Dendrocygna autumnalis - CITES listed since 1975 in App III briefly moved out in 85, added back in 87. 
## Dendrocygna bicolor - CITES listed since 1975 in App III briefly moved out in 85, added back in 87.

## Actions on complex cases - all species are still cites listed throughout our study period except trionyx which was briefly not. 
Complex_Species_timeframe_I <- rbind(expand.grid(Taxon = "Dendrocygna autumnalis", Year = c(1975:2020)),
                                   expand.grid(Taxon = "Dendrocygna bicolor", Year = c(1975:2020)),
                                   expand.grid(Taxon = "Batagur borneoensis", Year = c(1997:2020)),
                                   expand.grid(Taxon = "Mellivora capensis", Year = c(1976:2020)),
                                   expand.grid(Taxon = "Trionyx triunguis", Year = c(1976:2007, 2017:2020)))

Complex_fix_I <- full_join(Complex_Species_timeframe_I, CXSP_I)
Species_timeframe_full_I <- rbind(filter(Species_timeframe_I, !Taxon %in% c("Batagur borneoensis", "Dendrocygna autumnalis", "Dendrocygna bicolor",
                                                                        "Mellivora capensis", "Trionyx triunguis")),
                                Complex_fix_I) %>% filter(Year > 1999)

## 1085 sp
length(unique(Species_timeframe_full_I$Taxon))
## 1085
length(unique(CITES_Vert_pro_I$Taxon))


## Expand the data set to fill missing years between 2000 - 2018
CITES_Wild_Com_Imp <- left_join(Species_timeframe_full_I, CITES_Vert_pro_I, by = c("Taxon", "Exporter", "Importer", "Year", "Class"))

## 1085
length(unique(CITES_Wild_Com_Imp$Taxon))

## For 2000 - 2018 when the species are listed and present in the appendices, by our species specific time series,
## if the species is absent its a zero volume. We go back to 1999 for the lagged volume.
CITES_Com_I <- CITES_Wild_Com_Imp %>% 
  group_by(Taxon) %>% 
  mutate(n = replace_na(n, 0)) %>%
  select(-FL_year, -Year_DEL) %>% 
  mutate(Year = Year -2000)


CITES_Vert_Series_I <- left_join(CITES_Com_I, df_new, by = c("Year", "Taxon")) %>%
  mutate(Name_for_CITESdb = Taxon,
         Name_for_rl_history = if_else(is.na(IUCNName), Taxon, IUCNName))

length(unique(CITES_Vert_Series_I$Name_for_CITESdb)) ## now 1085
length(unique(CITES_Vert_Series_I$Name_for_rl_history)) ## now 1068
length(unique(CITES_Vert_Series_I$Taxon)) ## now 1085


## This code prepares the two versions of IUCN assessments.
## IUCN_code is the "true" state of assessments. Species are marked as Not assessed prior to having an assessment.
## So a species would go from Not assessed to LC to NT.
CITES_IUCN_data_I <- CITES_Vert_Series_I %>% 
  mutate(IUCN_code = case_when(is.na(IUCN_code) ~ "Not assessed", 
                               TRUE ~ IUCN_code)) %>%
  mutate(Threat_code = case_when(IUCN_code == "LC" ~ "Non-threatened",
                                 IUCN_code == "NT" ~ "Non-threatened",
                                 IUCN_code == "VU" ~ "Threatened",
                                 IUCN_code == "EN" ~ "Threatened",
                                 IUCN_code == "CR" ~ "Threatened",
                                 IUCN_code == "EW" ~ "Threatened",
                                 IUCN_code == "Not assessed" ~ "Not assessed",
                                 IUCN_code == "DD" ~ "Not assessed",
                                 TRUE ~ "ERROR"))

CITES_IUCN_data_I %>% group_by(IUCN_code) %>% tally()
CITES_IUCN_data_I %>% group_by(Threat_code) %>% tally()

length(unique(CITES_IUCN_data_I$Name_for_rl_history)) ## 1068
length(unique(CITES_IUCN_data_I$Name_for_CITESdb)) ## 1085

## Write this data into a csv to be read into subsequent analysis to save time.
write.csv(CITES_IUCN_data_I, "Data/All_Vertebrates/CITES_Vert_Imp_Reported.csv", na = "")

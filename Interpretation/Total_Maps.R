###########################
#### Map visualisation ####
###########################

## Purpose: visualise total trade volumes for 2000 - 2020

## global options settings
options(scipen = 999) ## remove scientific notation, namely "e"  to show very small and large numbers in full.
options(na.action = "na.pass")

## Set working directory and package location.
## Change this in yours
.libPaths("C:/Packages") ## Set up for working from home.


library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sp)
library(ggpubr)
library(sf)
library(viridis)
library(ggpubr)

#### Read in data ####

## Raw model fitting data (pre-sumarizing to country total) so we can extract species richness.
CITES_Exp_dat <- data.table::fread("Data/All_Vertebrates/Cleaned/ER/CITES_Vert_Exporter_dat_ER.csv", na.strings = "") %>% 
  rename(Country = name, iso = Exporter)

CITES_Imp_dat <- data.table::fread("Data/All_Vertebrates/Cleaned/ER/CITES_Vert_Importer_dat_ER.csv", na.strings = "") %>% 
  rename(Country = name, iso = Importer)



## Split out the classes
CITES_Aves_Exp <- CITES_Exp_dat %>% filter(Class == "Aves")
CITES_Mam_Exp <- CITES_Exp_dat %>% filter(Class == "Mammalia")
CITES_Amph_Exp <- CITES_Exp_dat %>% filter(Class == "Amphibia")
CITES_Rept_Exp <- CITES_Exp_dat %>% filter(Class == "Reptilia")

CITES_Aves_Imp <- CITES_Imp_dat %>% filter(Class == "Aves")
CITES_Mam_Imp <- CITES_Imp_dat %>% filter(Class == "Mammalia")
CITES_Amph_Imp <- CITES_Imp_dat %>% filter(Class == "Amphibia")
CITES_Rept_Imp <- CITES_Imp_dat %>% filter(Class == "Reptilia")

## Get country coords
world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") 
World_short <-  world %>% select(iso_a2, admin)

#### Functions ####

## Convenience for repitition
summarise_and_match <- function(taxa_raw_data) {
  
  Country_Sum <- taxa_raw_data %>%
    filter(Year>-1) %>%
    ## remove countries with all 0's after removing the year 1999
    filter(n > 0) %>%
    group_by(Class,iso, Country, region, subregion) %>%
    ## Sum total volumes and richness per country (2000 - 2020)
    summarise(Richness = n_distinct(Name_for_CITESdb),
              vol = sum(n)) %>% 
    mutate(Country = case_when(grepl("d'Ivoire", Country) ~ "Cote d'Ivoire",
                               grepl("union", Country) ~ "Reunion",
                               grepl("Cura", Country) ~ "Curacao",
                               TRUE ~ Country))
  
    Check <- left_join(Country_Sum, World_short, by = c("iso" = "iso_a2"))

    ## Check if one iso code ever matches more than 1 country
    Multiple_matches <- Check %>% group_by(iso) %>% filter(n()>1)
    ## Check which CITES parties have no matches
    No_match <- Check %>% filter(is.na(admin))

    ## Match countries to geometries
    Country_Match <- left_join(world, Country_Sum, by = c("iso_a2" = "iso"))
    
    return(list("Multiple_matches" = Multiple_matches, "No_match" = No_match, "Country_vol_totals" = Country_Match, "Country_Sum" = Country_Sum))
}

## Convenience for repitition, metric takes "SR" or "vol" and type takes "Exporter" or "Importer"
plot_map <- function(data, metric = "SR", type = "Exporter") {
  ## push map aes to the tidy function
  if (metric == "vol" & type == "Exporter") {
  Map <- ggplot() + geom_sf(data = data$Country_vol_totals, aes(fill = vol), colour = NA) +
    scale_fill_viridis(name = "Volume", trans = "log10", na.value="grey90",
                       option = "viridis", 
                       breaks = c(1, 100, 100000), labels = c(1, 100, 100000), limits = c(1, 18000000)) +
    coord_sf(ylim = c(-50, 90), datum = NA) +
    theme_classic(base_size = 12) +
    theme(panel.grid = element_blank(), legend.position = "none")
  }
  if (metric == "SR" & type == "Exporter") {
    Map <- ggplot() + geom_sf(data = data$Country_vol_totals, aes(fill = Richness), colour = NA) +
      scale_fill_viridis(name = "Volume", trans = "log10", na.value="grey90",
                         option = "viridis", 
                         breaks = c(1, 10, 100), labels = c(1, 10, 100), limits = c(1, 260)) +
      coord_sf(ylim = c(-50, 90), datum = NA) +
      theme_classic(base_size = 12) +
      theme(panel.grid = element_blank(), legend.position = "none")
  }
  if (metric == "vol" & type == "Importer") {
    Map <- ggplot() + geom_sf(data = data$Country_vol_totals, aes(fill = vol), colour = NA) +
      scale_fill_viridis(name = "Volume", trans = "log10", na.value="grey90",
                         option = "viridis", 
                         breaks = c(1, 100, 100000), labels = c(1, 100, 100000), limits = c(1, 18000000)) +
      coord_sf(ylim = c(-50, 90), datum = NA) +
      theme_classic(base_size = 12) +
      theme(panel.grid = element_blank(), legend.position = "none")
  }
  if (metric == "SR" & type == "Importer") {
    Map <- ggplot() + geom_sf(data = data$Country_vol_totals, aes(fill = Richness), colour = NA) +
      scale_fill_viridis(name = "Volume", trans = "log10", na.value="grey90",
                         option = "viridis", 
                         breaks = c(1, 10, 100), labels = c(1, 10, 100), limits = c(1, 260)) +
      coord_sf(ylim = c(-50, 90), datum = NA) +
      theme_classic(base_size = 12) +
      theme(panel.grid = element_blank(), legend.position = "none")
  }
  
  return(Map)
}

## Get pearson correlation in nice format, takes the output of the summarise and match function
tidy_correlation <- function(SM_output) {
  Cor_Sum <- SM_output$Country_Sum %>% ungroup() %>%
    summarise(Cor.coef = round(cor.test(vol, Richness)$estimate, 3),
              Lower.CI = round(cor.test(vol, Richness)$conf.int[1], 3),
              Upper.CI = round(cor.test(vol, Richness)$conf.int[2], 3), 
              p.value = round(cor.test(vol, Richness)$p.value, 8), 
              t.statistic = round(cor.test(vol, Richness)$statistic, 3),
              n = n())
  return(Cor_Sum)
}

## Convenience for tidy XY plots, takes the output of the summarise and match function
plot_XY <- function(SM_output) {
  ggplot(filter(SM_output$Country_Sum, iso != "AQ"), aes(Richness, vol, colour = region)) + 
    geom_point(size = 2) +
    scale_y_log10() +
    scale_x_log10() +
    scale_color_manual(labels = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                       values = c("#000000", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
                       drop = FALSE) +
    xlab("Traded richness") + ylab("Traded volumes") +
    theme_minimal(base_size = 14) +
    theme(legend.position = "none")
}

#### Summaries ####
CITES_Aves_Exp_Sum <- summarise_and_match(CITES_Aves_Exp) ## 114
CITES_Mam_Exp_Sum <- summarise_and_match(CITES_Mam_Exp) ## 99
CITES_Amph_Exp_Sum <- summarise_and_match(CITES_Amph_Exp) ## 9
CITES_Rept_Exp_Sum <- summarise_and_match(CITES_Rept_Exp) ## 97

CITES_Aves_Imp_Sum <- summarise_and_match(CITES_Aves_Imp) ## 173
CITES_Mam_Imp_Sum <- summarise_and_match(CITES_Mam_Imp) ## 173
CITES_Amph_Imp_Sum <- summarise_and_match(CITES_Amph_Imp) ## 36
CITES_Rept_Imp_Sum <- summarise_and_match(CITES_Rept_Imp) ## 146

## For common legends
Exp_min_max <- rbind(CITES_Aves_Exp_Sum$Country_vol_totals, CITES_Mam_Exp_Sum$Country_vol_totals, 
                     CITES_Amph_Exp_Sum$Country_vol_totals, CITES_Rept_Exp_Sum$Country_vol_totals) %>%
  ungroup() %>%
  summarise(min_SR = min(Richness), max_SR = max(Richness),
            min_vol = min(vol), max_vol = max(vol))

Imp_min_max <- rbind(CITES_Aves_Imp_Sum$Country_vol_totals, CITES_Mam_Imp_Sum$Country_vol_totals, 
                     CITES_Amph_Imp_Sum$Country_vol_totals, CITES_Rept_Imp_Sum$Country_vol_totals) %>%
  ungroup() %>%
  summarise(min_SR = min(Richness), max_SR = max(Richness),
            min_vol = min(vol), max_vol = max(vol))            

#### Plotting ####

## Vol (for the main text)
Aves_Exp_vol_plt <- plot_map(data = CITES_Aves_Exp_Sum, metric = "vol", type = "Exporter")
Mam_Exp_vol_plt <- plot_map(data = CITES_Mam_Exp_Sum, metric = "vol", type = "Exporter")
Amph_Exp_vol_plt <- plot_map(data = CITES_Amph_Exp_Sum, metric = "vol", type = "Exporter")
Rept_Exp_vol_plt <- plot_map(data = CITES_Rept_Exp_Sum, metric = "vol", type = "Exporter")

Aves_Imp_vol_plt <- plot_map(data = CITES_Aves_Imp_Sum, metric = "vol", type = "Importer")
Mam_Imp_vol_plt <- plot_map(data = CITES_Mam_Imp_Sum, metric = "vol", type = "Importer")
Amph_Imp_vol_plt <- plot_map(data = CITES_Amph_Imp_Sum, metric = "vol", type = "Importer")
Rept_Imp_vol_plt <- plot_map(data = CITES_Rept_Imp_Sum, metric = "vol", type = "Importer")

## SR (for the SOM)
Aves_Exp_SR_plt <- plot_map(data = CITES_Aves_Exp_Sum, metric = "SR", type = "Exporter")
Mam_Exp_SR_plt <- plot_map(data = CITES_Mam_Exp_Sum, metric = "SR", type = "Exporter")
Amph_Exp_SR_plt <- plot_map(data = CITES_Amph_Exp_Sum, metric = "SR", type = "Exporter")
Rept_Exp_SR_plt <- plot_map(data = CITES_Rept_Exp_Sum, metric = "SR", type = "Exporter")

Aves_Imp_SR_plt <- plot_map(data = CITES_Aves_Imp_Sum, metric = "SR", type = "Importer")
Mam_Imp_SR_plt <- plot_map(data = CITES_Mam_Imp_Sum, metric = "SR", type = "Importer")
Amph_Imp_SR_plt <- plot_map(data = CITES_Amph_Imp_Sum, metric = "SR", type = "Importer")
Rept_Imp_SR_plt <- plot_map(data = CITES_Rept_Imp_Sum, metric = "SR", type = "Importer")

#### Arrangement ####

## Get legends
Legend_vol <- get_legend(ggplot() + geom_sf(data = CITES_Aves_Exp_Sum$Country_vol_totals, aes(fill = vol), colour = NA) +
  scale_fill_viridis(name = "Volume", trans = "log10", na.value="grey90",
                     option = "viridis", 
                     breaks = c(1, 100, 10000, 1000000), 
                     labels = c(1, 100, 10000, 1000000), limits = c(1, 18000000)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 16) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        legend.key.width = unit(2, "cm")))

Legend_SR <- get_legend(ggplot() + geom_sf(data = CITES_Aves_Exp_Sum$Country_vol_totals, aes(fill = Richness), colour = NA) +
  scale_fill_viridis(name = "Volume", trans = "log10", na.value="grey90",
                     option = "viridis", 
                     breaks = c(1, 10, 100), labels = c(1, 10, 100), limits = c(1, 260)) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  theme_classic(base_size = 12) +
  theme(panel.grid = element_blank(), legend.position = "bottom", legend.title = element_blank(),
        legend.key.width = unit(2, "cm")))


## Vol and SR Maps
Map_Vol_arrange <- ggarrange(Aves_Exp_vol_plt, Aves_Imp_vol_plt, Mam_Exp_vol_plt, Mam_Imp_vol_plt, 
                         Amph_Exp_vol_plt, Amph_Imp_vol_plt, Rept_Exp_vol_plt, Rept_Imp_vol_plt,
                         nrow = 4, ncol = 2, 
                         labels = c("A.", "B.", "C.", "D.", "E.", "F.", "G.", "H."))

Map_SR_arrange <- ggarrange(Aves_Exp_SR_plt, Aves_Imp_SR_plt, Mam_Exp_SR_plt, Mam_Imp_SR_plt, 
                             Amph_Exp_SR_plt, Amph_Imp_SR_plt, Rept_Exp_SR_plt, Rept_Imp_SR_plt,
                             nrow = 4, ncol = 2, 
                             labels = c("A.", "B.", "C.", "D.", "E.", "F.", "G.", "H."))

## Arranging and annotating
library(png)
library(grid)
Rept <-  rasterGrob(readPNG("Data/Misc/Varanus2.png"), interpolate = TRUE)
Amph <- rasterGrob(readPNG("Data/Misc/Dendrobates2.png"), interpolate = TRUE)
Bird <- rasterGrob(readPNG("Data/Misc/Amazona2.png"), interpolate = TRUE)
Mam <- rasterGrob(readPNG("Data/Misc/Lycaloplex2.png"), interpolate = TRUE)



Map_Vol_arrange2 <- Map_Vol_arrange + annotation_custom(Bird, xmin = 0.95, xmax = 0.99, ymin = 0.9, ymax = 1) +
  annotation_custom(Mam, xmin = 0.93, xmax = 0.99, ymin = 0.68, ymax = .78) +
  annotation_custom(Amph, xmin = 0.95, xmax = 0.99, ymin = 0.43, ymax = .53) +
  annotation_custom(Rept, xmin = 0.93, xmax = 0.99, ymin = 0.18, ymax = .28) +
  annotation_custom(text_grob("Exported (2000 - 2020)", face = "bold", size= 14), xmin = 0.25, xmax = 0.25, ymin = 0.97, ymax = 1.0) +
  annotation_custom(text_grob("Imported (2000 - 2020)", face = "bold", size = 14), xmin = 0.75, xmax = 0.75, ymin = 0.97, ymax = 1.0) +
  annotation_custom(text_grob("n = 114", face = "italic", size = 11), xmin = 0.25, xmax = 0.25, ymin = 0.77, ymax = 0.77) +
  annotation_custom(text_grob("n = 99", face = "italic", size = 11), xmin = 0.25, xmax = 0.25, ymin = 0.52, ymax = 0.52) +
  annotation_custom(text_grob("n = 9", face = "italic", size = 11), xmin = 0.25, xmax = 0.25, ymin = 0.27, ymax = 0.27) +
  annotation_custom(text_grob("n = 97", face = "italic", size = 11), xmin = 0.25, xmax = 0.25, ymin = 0.02, ymax = 0.02) +
  annotation_custom(text_grob("n = 173", face = "italic", size = 11), xmin = 0.75, xmax = 0.75, ymin = 0.77, ymax = 0.77) +
  annotation_custom(text_grob("n = 173", face = "italic", size = 11), xmin = 0.75, xmax = 0.75, ymin = 0.52, ymax = 0.52) +
  annotation_custom(text_grob("n = 36", face = "italic", size = 11), xmin = 0.75, xmax = 0.75, ymin = 0.27, ymax = 0.27) +
  annotation_custom(text_grob("n = 146", face = "italic", size = 11), xmin = 0.75, xmax = 0.75, ymin = 0.02, ymax = 0.02)

Map_SR_arrange2 <- Map_SR_arrange + annotation_custom(Bird, xmin = 0.95, xmax = 0.99, ymin = 0.9, ymax = 1) +
  annotation_custom(Mam, xmin = 0.93, xmax = 0.99, ymin = 0.68, ymax = .78) +
  annotation_custom(Amph, xmin = 0.95, xmax = 0.99, ymin = 0.43, ymax = .53) +
  annotation_custom(Rept, xmin = 0.93, xmax = 0.99, ymin = 0.18, ymax = .28) +
  annotation_custom(text_grob("Exported (2000 - 2020)", face = "bold", size= 14), xmin = 0.25, xmax = 0.25, ymin = 0.97, ymax = 1.0) +
  annotation_custom(text_grob("Imported (2000 - 2020)", face = "bold", size = 14), xmin = 0.75, xmax = 0.75, ymin = 0.97, ymax = 1.0) +
  annotation_custom(text_grob("n = 114", face = "italic", size = 11), xmin = 0.25, xmax = 0.25, ymin = 0.77, ymax = 0.77) +
  annotation_custom(text_grob("n = 99", face = "italic", size = 11), xmin = 0.25, xmax = 0.25, ymin = 0.52, ymax = 0.52) +
  annotation_custom(text_grob("n = 9", face = "italic", size = 11), xmin = 0.25, xmax = 0.25, ymin = 0.27, ymax = 0.27) +
  annotation_custom(text_grob("n = 97", face = "italic", size = 11), xmin = 0.25, xmax = 0.25, ymin = 0.02, ymax = 0.02) +
  annotation_custom(text_grob("n = 173", face = "italic", size = 11), xmin = 0.75, xmax = 0.75, ymin = 0.77, ymax = 0.77) +
  annotation_custom(text_grob("n = 173", face = "italic", size = 11), xmin = 0.75, xmax = 0.75, ymin = 0.52, ymax = 0.52) +
  annotation_custom(text_grob("n = 36", face = "italic", size = 11), xmin = 0.75, xmax = 0.75, ymin = 0.27, ymax = 0.27) +
  annotation_custom(text_grob("n = 146", face = "italic", size = 11), xmin = 0.75, xmax = 0.75, ymin = 0.02, ymax = 0.02)

Map_Vol_arrange3 <- ggarrange(Map_Vol_arrange2, legend = "bottom", legend.grob = Legend_vol)
Map_SR_arrange3 <- ggarrange(Map_SR_arrange2, legend = "bottom", legend.grob = Legend_SR)

ggsave(path = "Outputs/Figure_1", Map_Vol_arrange3, filename = "Figure1.png",  bg = "white",
       device = "png", width = 30, height = 30, units = "cm")

ggsave(path = "Outputs/SOM", Map_SR_arrange3, filename = "Figure1_SR.png",  bg = "white",
       device = "png", width = 30, height = 30, units = "cm")

#### Supplementary XY Vol - SR analysis ####

## All tidy correlations
all_correlations <- rbind(tidy_correlation(SM_output = CITES_Aves_Exp_Sum) %>% mutate(Class = "Aves", Type = "Exported"),
                          tidy_correlation(SM_output = CITES_Mam_Exp_Sum) %>% mutate(Class = "Mammalia", Type = "Exported"),
                          tidy_correlation(SM_output = CITES_Amph_Exp_Sum) %>% mutate(Class = "Amphibia", Type = "Exported"),
                          tidy_correlation(SM_output = CITES_Rept_Exp_Sum) %>% mutate(Class = "Reptilia", Type = "Exported"),
                          tidy_correlation(SM_output = CITES_Aves_Imp_Sum) %>% mutate(Class = "Aves", Type = "Imported"),
                          tidy_correlation(SM_output = CITES_Mam_Imp_Sum) %>% mutate(Class = "Mammalia", Type = "Imported"),
                          tidy_correlation(SM_output = CITES_Amph_Imp_Sum) %>% mutate(Class = "Amphibia", Type = "Imported"),
                          tidy_correlation(SM_output = CITES_Rept_Imp_Sum) %>% mutate(Class = "Reptilia", Type = "Imported"))

write.csv(all_correlations, "Outputs/SOM/SR_Vol_Correlations.csv")

## Make XY plots
Aves_XY_Exp_vol_SR <- plot_XY(SM_output = CITES_Aves_Exp_Sum)
Mam_XY_Exp_vol_SR <- plot_XY(SM_output = CITES_Mam_Exp_Sum)
Amph_XY_Exp_vol_SR <- plot_XY(SM_output = CITES_Amph_Exp_Sum)
Rept_XY_Exp_vol_SR <- plot_XY(SM_output = CITES_Rept_Exp_Sum)

Aves_XY_Imp_vol_SR <- plot_XY(SM_output = CITES_Aves_Imp_Sum)
Mam_XY_Imp_vol_SR <- plot_XY(SM_output = CITES_Mam_Imp_Sum)
Amph_XY_Imp_vol_SR <- plot_XY(SM_output = CITES_Amph_Imp_Sum)
Rept_XY_Imp_vol_SR <- plot_XY(SM_output = CITES_Rept_Imp_Sum)


XY_Legend <- get_legend(ggplot(CITES_Aves_Exp_Sum$Country_Sum, aes(Richness, vol, colour = region)) + 
  geom_point(size = 2) +
  scale_y_log10() +
  scale_x_log10() +
  scale_color_manual(labels = c("Africa", "Americas", "Asia", "Europe", "Oceania"),
                     values = c("#000000", "#009E73", "#F0E442", "#0072B2", "#D55E00"),
                     drop = FALSE) +
  xlab("Traded richness") + ylab("Traded volumes") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.title = element_blank()))

XY_arrangement <- ggarrange(Aves_XY_Exp_vol_SR, Aves_XY_Imp_vol_SR, Mam_XY_Exp_vol_SR, Mam_XY_Imp_vol_SR, 
          Amph_XY_Exp_vol_SR, Amph_XY_Imp_vol_SR, Rept_XY_Exp_vol_SR, Rept_XY_Imp_vol_SR, 
          legend = "bottom", legend.grob = XY_Legend, ncol = 2, nrow = 4,
          labels = c("A.", "B.", "C.", "D.", "E.", "F.", "G.", "H."))

ggsave(path = "Outputs/SOM", XY_arrangement, filename = "Figure1_SR_Vol_XY.png",  bg = "white",
       device = "png", width = 30, height = 30, units = "cm")

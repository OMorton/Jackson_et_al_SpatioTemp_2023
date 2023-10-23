###########################
#### Country G-ME Maps ####
###########################


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

#### Read in ####
G_ME_Sum_Aves_EXP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Aves_EXP.csv", na.strings = "") %>%
  rename("iso" = "Exporter")
G_ME_Sum_Amph_EXP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Amph_EXP.csv", na.strings = "") %>%
  rename("iso" = "Exporter")
G_ME_Sum_Mam_EXP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Mam_EXP.csv", na.strings = "") %>%
  rename("iso" = "Exporter")
G_ME_Sum_Rept_EXP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Rept_EXP.csv", na.strings = "") %>%
  rename("iso" = "Exporter")

G_ME_Sum_Aves_IMP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Aves_IMP.csv", na.strings = "") %>%
  rename("iso" = "Importer")
G_ME_Sum_Amph_IMP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Amph_IMP.csv", na.strings = "") %>%
  rename("iso" = "Importer") 
G_ME_Sum_Mam_IMP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Mam_IMP.csv", na.strings = "") %>%
  rename("iso" = "Importer")
G_ME_Sum_Rept_IMP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Rept_IMP.csv", na.strings = "") %>%
  rename("iso" = "Importer") 

#### Test for matching ####
## join the data
world <- ne_countries(scale = "medium", returnclass = "sf", type = "countries") 

## No NA
test <-  world %>% select(iso_a2, admin)

## Exp
Check <- left_join(G_ME_Sum_Aves_EXP, test, by = c("iso" = "iso_a2"))
Check %>% group_by(iso) %>% filter(n()>1)
Check %>% filter(is.na(admin))

Check <- left_join(G_ME_Sum_Amph_EXP, test, by = c("iso" = "iso_a2"))
Check %>% group_by(iso) %>% filter(n()>1)
Check %>% filter(is.na(admin))

Check <- left_join(G_ME_Sum_Mam_EXP, test, by = c("iso" = "iso_a2"))
Check %>% group_by(iso) %>% filter(n()>1)
Check %>% filter(is.na(admin))

Check <- left_join(G_ME_Sum_Rept_EXP, test, by = c("iso" = "iso_a2"))
Check %>% group_by(iso) %>% filter(n()>1)
Check %>% filter(is.na(admin))

## Imp
Check <- left_join(G_ME_Sum_Aves_IMP, test, by = c("iso" = "iso_a2"))
Check %>% group_by(iso) %>% filter(n()>1)
Check %>% filter(is.na(admin)) #5 small island nationds inc dissolved Netherlands Antilles

Check <- left_join(G_ME_Sum_Amph_IMP, test, by = c("iso" = "iso_a2"))
Check %>% group_by(iso) %>% filter(n()>1)
Check %>% filter(is.na(admin))

Check <- left_join(G_ME_Sum_Mam_IMP, test, by = c("iso" = "iso_a2"))
Check %>% group_by(iso) %>% filter(n()>1)
Check %>% filter(is.na(admin)) #5 small island nations inc dissolved Netherlands Antilles

Check <- left_join(G_ME_Sum_Rept_IMP, test, by = c("iso" = "iso_a2"))
Check %>% group_by(iso) %>% filter(n()>1)
Check %>% filter(is.na(admin)) #4 small island nations inc dissolved Netherlands Antilles

#check all codes matched

#### Matching ####

## Exp
Aves_Exp <- left_join(test, G_ME_Sum_Aves_EXP, by = c("iso_a2" = "iso")) %>%
  mutate(Interpretation = factor(Interpretation, levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain",
                                                            "PD > 95%, -ve trend", "PD > 97.5%, -ve trend"), ordered = TRUE))

Mam_Exp <- left_join(test, G_ME_Sum_Mam_EXP, by = c("iso_a2" = "iso")) %>%
  mutate(Interpretation = factor(Interpretation, levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain",
                                                            "PD > 95%, -ve trend", "PD > 97.5%, -ve trend"), ordered = TRUE))

Amph_Exp <- left_join(test, G_ME_Sum_Amph_EXP, by = c("iso_a2" = "iso")) %>%
  mutate(Interpretation = factor(Interpretation, levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain",
                                                            "PD > 95%, -ve trend", "PD > 97.5%, -ve trend"), ordered = TRUE))

Rept_Exp <- left_join(test, G_ME_Sum_Rept_EXP, by = c("iso_a2" = "iso")) %>%
  mutate(Interpretation = factor(Interpretation, levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain",
                                                            "PD > 95%, -ve trend", "PD > 97.5%, -ve trend"), ordered = TRUE))

## Imp
Aves_Imp <- left_join(test, G_ME_Sum_Aves_IMP, by = c("iso_a2" = "iso")) %>%
  mutate(Interpretation = factor(Interpretation, levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain",
                                                            "PD > 95%, -ve trend", "PD > 97.5%, -ve trend"), ordered = TRUE))

Mam_Imp <- left_join(test, G_ME_Sum_Mam_IMP, by = c("iso_a2" = "iso")) %>%
  mutate(Interpretation = factor(Interpretation, levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain",
                                                            "PD > 95%, -ve trend", "PD > 97.5%, -ve trend"), ordered = TRUE))

Amph_Imp <- left_join(test, G_ME_Sum_Amph_IMP, by = c("iso_a2" = "iso")) %>%
  mutate(Interpretation = factor(Interpretation, levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain",
                                                            "PD > 95%, -ve trend", "PD > 97.5%, -ve trend"), ordered = TRUE))

Rept_Imp <- left_join(test, G_ME_Sum_Rept_IMP, by = c("iso_a2" = "iso")) %>%
  mutate(Interpretation = factor(Interpretation, levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain",
                                                            "PD > 95%, -ve trend", "PD > 97.5%, -ve trend"), ordered = TRUE))


#### Plotting ####

## Aves exp
Aves_Exp_map <- ggplot() + 
  geom_sf(data = Aves_Exp, aes(fill = Interpretation), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), 
                    drop = FALSE, na.value="grey90", breaks = levels(Aves_Exp$Interpretation)) +
  theme_classic(base_size = 18) +
  theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_blank())

## Mam exp
Mam_Exp_map <- ggplot() + 
  geom_sf(data = Mam_Exp, aes(fill = Interpretation), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), 
                    drop = FALSE, na.value="grey90", breaks = levels(Mam_Exp$Interpretation)) +
  theme_classic(base_size = 18) +
  theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_blank())

## Amph exp
Amph_Exp_map <- ggplot() + 
  geom_sf(data = Amph_Exp, aes(fill = Interpretation), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), 
                    drop = FALSE, na.value="grey90", breaks = levels(Amph_Exp$Interpretation)) +
  theme_classic(base_size = 18) +
  theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_blank())

## Rept exp
Rept_Exp_map <- ggplot() + 
  geom_sf(data = Rept_Exp, aes(fill = Interpretation), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), 
                    drop = FALSE, na.value="grey90", breaks = levels(Rept_Exp$Interpretation)) +
  theme_classic(base_size = 18) +
  theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_blank())

## Aves imp
Aves_Imp_map <- ggplot() + 
  geom_sf(data = Aves_Imp, aes(fill = Interpretation), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), 
                    drop = FALSE, na.value="grey90", breaks = levels(Aves_Imp$Interpretation)) +
  theme_classic(base_size = 18) +
  theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_blank())

## Mam imp
Mam_Imp_map <- ggplot() + 
  geom_sf(data = Mam_Imp, aes(fill = Interpretation), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), 
                    drop = FALSE, na.value="grey90", breaks = levels(Mam_Imp$Interpretation)) +
  theme_classic(base_size = 18) +
  theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_blank())

## Amph imp
Amph_Imp_map <- ggplot() + 
  geom_sf(data = Amph_Imp, aes(fill = Interpretation), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), 
                    drop = FALSE, na.value="grey90", breaks = levels(Amph_Imp$Interpretation)) +
  theme_classic(base_size = 18) +
  theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_blank())

## Rept imp
Rept_Imp_map <- ggplot() + 
  geom_sf(data = Rept_Imp, aes(fill = Interpretation), colour = NA, size = .01) +
  coord_sf(ylim = c(-50, 90), datum = NA) +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), 
                    drop = FALSE, na.value="grey90", breaks = levels(Rept_Imp$Interpretation)) +
  theme_classic(base_size = 18) +
  theme(panel.grid = element_blank(), legend.position = "none", legend.title = element_blank())

#### Arrangement ####

library(ggpubr)

Map_G_ME <- ggarrange(Aves_Exp_map, Aves_Imp_map, Mam_Exp_map, Mam_Imp_map, Amph_Exp_map, Amph_Imp_map, Rept_Exp_map, Rept_Imp_map,
                      ncol = 2, nrow = 4, labels = c("A.", "B.", "C.", "D.", "E.", "F.", "G.", "H."))

## Get images
library(png)
library(grid)
Rept <-  rasterGrob(readPNG("Data/Misc/Varanus2.png"), interpolate = TRUE)
Amph <- rasterGrob(readPNG("Data/Misc/Dendrobates2.png"), interpolate = TRUE)
Bird <- rasterGrob(readPNG("Data/Misc/Amazona2.png"), interpolate = TRUE)
Mam <- rasterGrob(readPNG("Data/Misc/Lycaloplex2.png"), interpolate = TRUE)



Map_G_ME2 <- Map_G_ME + annotation_custom(Bird, xmin = 0.95, xmax = 0.99, ymin = 0.9, ymax = 1) +
  annotation_custom(Mam, xmin = 0.93, xmax = 0.99, ymin = 0.68, ymax = .78) +
  annotation_custom(Amph, xmin = 0.95, xmax = 0.99, ymin = 0.43, ymax = .53) +
  annotation_custom(Rept, xmin = 0.93, xmax = 0.99, ymin = 0.18, ymax = .28) +
  annotation_custom(text_grob("Exported trend", face = "bold", size= 14), xmin = 0.25, xmax = 0.25, ymin = 0.97, ymax = 1.0) +
  annotation_custom(text_grob("Imported trend", face = "bold", size = 14), xmin = 0.75, xmax = 0.75, ymin = 0.97, ymax = 1.0)

dat <- data.frame(x = c(1:5), y = c(1:5))
(Legend <- ggplot(dat) +
  geom_rect(xmin = 0, xmax = 1, ymin = 0, ymax = 1, fill = "tomato") +
  geom_rect(xmin = 1, xmax = 2, ymin = 0, ymax = 1, fill = "darkgoldenrod") +
  geom_rect(xmin = 2, xmax = 3, ymin = 0, ymax = 1, fill = "grey40") +
  geom_rect(xmin = 3, xmax = 4, ymin = 0, ymax = 1, fill = "steelblue2") +
  geom_rect(xmin = 4, xmax = 5, ymin = 0, ymax = 1, fill = "royalblue3") +
  geom_rect(xmin = .45, xmax = .55, ymin = 1, ymax = 1.1, fill = "tomato") +
  geom_rect(xmin = 1.45, xmax = 1.55, ymin = 0, ymax = -.1, fill = "darkgoldenrod") +
  geom_rect(xmin = 2.45, xmax = 2.55, ymin = 1, ymax = 1.1, fill = "grey40") +
  geom_rect(xmin = 3.45, xmax = 3.55, ymin = 0, ymax = -.1, fill = "steelblue2") +
  geom_rect(xmin = 4.45, xmax = 4.55, ymin = 1, ymax = 1.1, fill = "royalblue3") +
  annotate("text", label = "PD > 97.5%, +ve ME",x = .5, y = 1.8, size = 4, colour = "black") +
  annotate("text", label = "Uncertain",x = 2.5, y = 1.8, size = 4, colour = "black") +
  annotate("text", label = "PD > 97.5%, -ve ME",x = 4.5, y = 1.8, size = 4, colour = "black") +
  annotate("text", label = "PD > 95%, -ve ME",x = 3.5, y = -.8, size = 4, colour = "black") +
  annotate("text", label = "PD > 95%, +ve ME",x = 1.5, y = -.8, size = 4, colour = "black") +
  annotate("text", label = "Country-level marginal effect\n direction (2000 - 2021)",
           x = -2, y = .5, size = 4.5, colour = "black", fontface = "bold") +
  coord_cartesian(xlim = c(-5.5, 8), ylim = c(-1, 2)) +
  theme_void())

Map_G_ME3 <- ggarrange(Map_G_ME2, Legend, heights = c(1,.09), nrow =2 )


ggsave(path = "Outputs/Figure_2", Map_G_ME3, filename = "Figure2.png",  bg = "white",
       device = "png", width = 30, height = 30, units = "cm")






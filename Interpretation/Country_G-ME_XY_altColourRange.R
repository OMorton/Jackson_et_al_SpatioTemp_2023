###############################
#### Country G-ME XY plots ####
###############################

options(scipen = 999) ## remove scientific notation, namely "e"  to show very small and large numbers in full.
options(na.action = "na.pass")

## Set working directory and package location.
## Change this in yours
.libPaths("C:/Packages") ## Set up for working from home.

library(tidyverse)
library(marginaleffects)
library(bayestestR)
library(ggpubr)

#### Read in ####
G_ME_Sum_Aves_EXP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Aves_EXP.csv") %>%
  rename("iso" = "Exporter") %>% mutate(iso = ifelse(is.na(iso), "NA", iso))
G_ME_Sum_Amph_EXP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Amph_EXP.csv") %>%
  rename("iso" = "Exporter") %>% mutate(iso = ifelse(is.na(iso), "NA", iso))
G_ME_Sum_Mam_EXP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Mam_EXP.csv") %>%
  rename("iso" = "Exporter") %>% mutate(iso = ifelse(is.na(iso), "NA", iso))
G_ME_Sum_Rept_EXP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Rept_EXP.csv") %>%
  rename("iso" = "Exporter") %>% mutate(iso = ifelse(is.na(iso), "NA", iso))

G_ME_Sum_Aves_IMP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Aves_IMP.csv") %>%
  rename("iso" = "Importer") %>% mutate(iso = ifelse(is.na(iso), "NA", iso))
G_ME_Sum_Amph_IMP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Amph_IMP.csv") %>%
  rename("iso" = "Importer") %>% mutate(iso = ifelse(is.na(iso), "NA", iso))
G_ME_Sum_Mam_IMP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Mam_IMP.csv") %>%
  rename("iso" = "Importer") %>% mutate(iso = ifelse(is.na(iso), "NA", iso))
G_ME_Sum_Rept_IMP <- data.table::fread("Outputs/G-ME/G_ME_Sum_Rept_IMP.csv") %>%
  rename("iso" = "Importer") %>% mutate(iso = ifelse(is.na(iso), "NA", iso))

### Add interpretation ####

## Exporter
G_ME_Sum_Aves_EXP <- G_ME_Sum_Aves_EXP %>% 
  mutate(PD_bi = ifelse(Median_Trend == "Positive", 100 - PD, PD),
         Substantial = ifelse(PD >= 97.5, "Yes", "No")) %>%
  unite(Short_int, c(`Median_Trend`, `Substantial`), remove = FALSE)%>%
  mutate(Interpretation = factor(Interpretation, 
                                 levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                                            "Uncertain", "PD > 95%, -ve trend", 
                                            "PD > 97.5%, -ve trend"), ordered = TRUE))

G_ME_Sum_Mam_EXP <- G_ME_Sum_Mam_EXP %>% 
  mutate(PD_bi = ifelse(Median_Trend == "Positive", 100 - PD, PD),
         Substantial = ifelse(PD >= 97.5, "Yes", "No"))%>%
  mutate(Interpretation = factor(Interpretation, 
                                 levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                            "Uncertain", "PD > 95%, -ve trend", 
                                            "PD > 97.5%, -ve trend"), ordered = TRUE))

G_ME_Sum_Amph_EXP <- G_ME_Sum_Amph_EXP %>% 
  mutate(PD_bi = ifelse(Median_Trend == "Positive", 100 - PD, PD),
         Substantial = ifelse(PD >= 97.5, "Yes", "No"))%>%
  mutate(Interpretation = factor(Interpretation, 
                                 levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                            "Uncertain", "PD > 95%, -ve trend", 
                                            "PD > 97.5%, -ve trend"), ordered = TRUE))

G_ME_Sum_Rept_EXP <- G_ME_Sum_Rept_EXP %>% 
  mutate(PD_bi = ifelse(Median_Trend == "Positive", 100 - PD, PD),
         Substantial = ifelse(PD >= 97.5, "Yes", "No"))%>%
  mutate(Interpretation = factor(Interpretation, 
                                 levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                            "Uncertain", "PD > 95%, -ve trend", 
                                            "PD > 97.5%, -ve trend"), ordered = TRUE))

## Importer
G_ME_Sum_Aves_IMP <- G_ME_Sum_Aves_IMP %>% 
  mutate(PD_bi = ifelse(Median_Trend == "Positive", 100 - PD, PD),
         Substantial = ifelse(PD >= 97.5, "Yes", "No"))%>%
  mutate(Interpretation = factor(Interpretation, 
                                 levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                            "Uncertain", "PD > 95%, -ve trend", 
                                            "PD > 97.5%, -ve trend"), ordered = TRUE))

G_ME_Sum_Mam_IMP <- G_ME_Sum_Mam_IMP %>% 
  mutate(PD_bi = ifelse(Median_Trend == "Positive", 100 - PD, PD),
         Substantial = ifelse(PD >= 97.5, "Yes", "No"))%>%
  mutate(Interpretation = factor(Interpretation, 
                                 levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                            "Uncertain", "PD > 95%, -ve trend", 
                                            "PD > 97.5%, -ve trend"), ordered = TRUE))

G_ME_Sum_Amph_IMP <- G_ME_Sum_Amph_IMP %>% 
  mutate(PD_bi = ifelse(Median_Trend == "Positive", 100 - PD, PD),
         Substantial = ifelse(PD >= 97.5, "Yes", "No"))%>%
  mutate(Interpretation = factor(Interpretation, 
                                 levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                            "Uncertain", "PD > 95%, -ve trend", 
                                            "PD > 97.5%, -ve trend"), ordered = TRUE))

G_ME_Sum_Rept_IMP <- G_ME_Sum_Rept_IMP %>% 
  mutate(PD_bi = ifelse(Median_Trend == "Positive", 100 - PD, PD),
         Substantial = ifelse(PD >= 97.5, "Yes", "No"))%>%
  mutate(Interpretation = factor(Interpretation, 
                                 levels = c("PD > 97.5%, +ve trend","PD > 95%, +ve trend",
                                            "Uncertain", "PD > 95%, -ve trend", 
                                            "PD > 97.5%, -ve trend"), ordered = TRUE))
#### Exporter ####
## Aves
Aves_Exp_top <- ggplot(filter(G_ME_Sum_Aves_EXP, n >= 10000), aes(G_ME, n, colour = Interpretation)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, alpha = .35, size = 1) +
  geom_point(shape = 16, size = 2) + 
  scale_colour_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  scale_y_log10(breaks = c(1000000, 100000, 10000)) +
  coord_cartesian(xlim = c(-85000, 85000/4), expand = TRUE) + 
  annotate("text", label = "Senegal", x = -80000, y = 2631014, colour = "royalblue3") +
  annotate("text", label = "Guinea", x = -60000, y = 825214, colour = "royalblue3") +
  annotate("text", label = "Mali", x = -15000, y = 440080, colour = "royalblue3") +
  annotate("text", label = "Uruguay", x = 20000, y = 485800, colour = "grey40") +
  ylab("Total volume") +
  xlab("Country marginal effect of year (2000 - 2020)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

Aves_bar_sum <- G_ME_Sum_Aves_EXP %>% filter(n >= 10000) %>% group_by(Interpretation) %>% tally() %>% mutate(Col = "A")

Aves_exp_bar <- ggplot(Aves_bar_sum, aes(x = n, y = Col, fill = Interpretation)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  annotate("text", label = "0", x = 0, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "18", x = 18, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "23", x = 23, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  coord_cartesian(ylim = c(0.5, 2.2)) +
  theme_void() +
  theme(legend.position = "none")
  
Dual_Exp_Aves2 <- ggarrange(Aves_Exp_top, Aves_exp_bar, ncol = 1, heights = c(1, .3))

## Mam  
Mam_Exp_top <- ggplot(filter(G_ME_Sum_Mam_EXP, n >= 10000), aes(G_ME, n, colour = Interpretation)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, alpha = .35, size = 1) +
  geom_point(shape = 16, size = 2) + 
  scale_colour_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  scale_y_log10(breaks = c(1000000, 100000, 10000)) +
  coord_cartesian(xlim = c(-30000, 30000/4), expand = TRUE) + 
  annotate("text", label = "Argentina", x = 3000, y =  1714849, colour = "royalblue3") +
  annotate("text", label = "USA", x = 7000, y = 1135016, colour = "grey40") +
  annotate("text", label = "Peru", x = 5000, y = 800322, colour = "grey40") +
  annotate("text", label = "Namibia", x = 5000, y = 555355, colour = "grey40") +
  annotate("text", label = "Canada", x = -10000, y = 550824, colour = "grey40") +
  annotate("text", label = "China", x = -7000, y = 211149, colour = "royalblue3") +
  ylab("Total volume") +
  xlab("Country marginal effect of year (2000 - 2020)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

Mam_bar_sum <- G_ME_Sum_Mam_EXP %>% filter(n >= 10000) %>% group_by(Interpretation) %>% tally() %>% mutate(Col = "A")

Mam_exp_bar <- ggplot(Mam_bar_sum, aes(x = n, y = Col, fill = Interpretation)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  annotate("text", label = "0", x = 0, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "4", x = 4, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "12", x = 12, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  coord_cartesian(ylim = c(0.5, 2.2)) +
  theme_void() +
  theme(legend.position = "none")

Dual_Exp_Mam2 <- ggarrange(Mam_Exp_top, Mam_exp_bar, ncol = 1, heights = c(1, .3))

## Amph
Amph_Exp_top <- ggplot(filter(G_ME_Sum_Amph_EXP, n >= 10000), aes(G_ME, n, colour = Interpretation)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, alpha = .35, size = 1) +
  geom_point(shape = 16, size = 2) + 
  scale_colour_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  scale_y_log10(breaks = c(1000000, 100000, 10000), limits = c(10000, 1000000)) +
  coord_cartesian(xlim = c(-3000, 3000/2.5), expand = TRUE) + 
  annotate("text", label = "Madagascar", x = 500, y =  295448, colour = "royalblue3") +
  annotate("text", label = "Suriname", x = 500, y = 27116, colour = "grey40") +
  ylab("Total volume") +
  xlab("Country marginal effect of year (2000 - 2020)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

Amph_bar_sum <- G_ME_Sum_Amph_EXP %>% filter(n >= 10000) %>% group_by(Interpretation) %>% tally() %>% mutate(Col = "A")

Amph_exp_bar <- ggplot(Amph_bar_sum, aes(x = n, y = Col, fill = Interpretation)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  annotate("text", label = "0", x = 0, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "1", x = 1, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "2", x = 2, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  coord_cartesian(ylim = c(0.5, 2.2)) +
  theme_void() +
  theme(legend.position = "none")

Dual_Exp_Amph2 <- ggarrange(Amph_Exp_top, Amph_exp_bar, ncol = 1, heights = c(1, .3))

## Rept
Rept_Exp_top <- ggplot(filter(G_ME_Sum_Rept_EXP, n >= 10000), aes(G_ME, n, colour = Interpretation)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, alpha = .35, size = 1) +
  geom_point(shape = 16, size = 2) + 
  scale_colour_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  scale_y_log10(breaks = c(10000000, 1000000, 100000, 10000)) +
  coord_cartesian(xlim = c(-120000, 120000), expand = TRUE) + 
  annotate("text", label = "Indonesia", x = -100000, y =  17433325, colour = "grey40") +
  annotate("text", label = "USA", x = 100000, y = 8021399, colour = "grey40") +
  annotate("text", label = "Malaysia", x = -75000, y = 6152901, colour = "grey40") +
  annotate("text", label = "Argentina", x = -75000, y = 3085065, colour = "steelblue2") +
  annotate("text", label = "Peru", x = 100000, y = 1850809, colour = "tomato") +
  ylab("Total volume") +
  xlab("Country marginal effect of year (2000 - 2020)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

Rept_bar_sum <- G_ME_Sum_Rept_EXP %>% filter(n >= 10000) %>% group_by(Interpretation) %>% tally() %>% mutate(Col = "A")

Rept_exp_bar <- ggplot(Rept_bar_sum, aes(x = n, y = Col, fill = Interpretation)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  annotate("text", label = "0", x = 0, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "9", x = 9, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "17", x = 17, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "33", x = 32, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "34", x = 35, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "42", x = 42, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  coord_cartesian(ylim = c(0.5, 2.2)) +
  theme_void() +
  theme(legend.position = "none")

Dual_Exp_Rept2 <- ggarrange(Rept_Exp_top, Rept_exp_bar, ncol = 1, heights = c(1, .3))


#### Importer ####
## Aves
Aves_Imp_top <- ggplot(filter(G_ME_Sum_Aves_IMP, n >= 10000), aes(G_ME, n, colour = Interpretation)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, alpha = .35, size = 1) +
  geom_point(shape = 16, size = 2) + 
  scale_colour_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  scale_y_log10(breaks = c(1000000, 100000, 10000), limits = c(10000, 1000000)) +
  coord_cartesian(xlim = c(-95000, 10000), expand = TRUE) + 
  annotate("text", label = "Italy, Spain", x = -85000, y =  944296, colour = "royalblue3") +
  annotate("text", label = "Portugal", x = -75000, y = 650866, colour = "royalblue3") +
  ylab("Total volume") +
  xlab("Country marginal effect of year (2000 - 2020)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

Aves_bar_sum_imp <- G_ME_Sum_Aves_IMP %>% filter(n >= 10000) %>% group_by(Interpretation) %>% tally() %>% mutate(Col = "A")

Aves_imp_bar <- ggplot(Aves_bar_sum_imp, aes(x = n, y = Col, fill = Interpretation)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  annotate("text", label = "0", x = 0, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "22", x = 21, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "23", x = 24, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "31", x = 30, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "33", x = 33, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  coord_cartesian(ylim = c(0.5, 2.2)) +
  theme_void() +
  theme(legend.position = "none")

Dual_Imp_Aves2 <- ggarrange(Aves_Imp_top, Aves_imp_bar, ncol = 1, heights = c(1, .3))

## Mam  
Mam_Imp_top <- ggplot(filter(G_ME_Sum_Mam_IMP, n >= 10000), aes(G_ME, n, colour = Interpretation)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, alpha = .35, size = 1) +
  geom_point(shape = 16, size = 2) + 
  scale_colour_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  scale_y_log10(breaks = c(1000000, 100000, 10000)) +
  coord_cartesian(xlim = c(-32000, 25000/2.5), expand = TRUE) + 
  annotate("text", label = "Italy", x = -27000, y =  971233, colour = "royalblue3") +
  annotate("text", label = "Turkey", x = 6000, y = 956557, colour = "steelblue2") +
  annotate("text", label = "Canada", x = 6000, y =  687265, colour = "grey40") +
  annotate("text", label = "Germany", x = -22000, y = 618537, colour = "royalblue3") +
  ylab("Total volume") +
  xlab("Country marginal effect of year (2000 - 2020)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

Mam_bar_sum_imp <- G_ME_Sum_Mam_IMP %>% filter(n >= 10000) %>% group_by(Interpretation) %>% tally() %>% mutate(Col = "A")

Mam_imp_bar <- ggplot(Mam_bar_sum_imp, aes(x = n, y = Col, fill = Interpretation)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  annotate("text", label = "0", x = 0, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "16", x = 16, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "19", x = 19, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "24", x = 23, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "25", x = 25, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  coord_cartesian(ylim = c(0.5, 2.2)) +
  theme_void() +
  theme(legend.position = "none")

Dual_Imp_Mam2 <- ggarrange(Mam_Imp_top, Mam_imp_bar, ncol = 1, heights = c(1, .3))

## Amph
Amph_Imp_top <- ggplot(filter(G_ME_Sum_Amph_IMP, n >= 10000), aes(G_ME, n, colour = Interpretation)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, alpha = .35, size = 1) +
  geom_point(shape = 16, size = 2) + 
  scale_colour_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  scale_y_log10(breaks = c(1000000, 100000, 10000), limits = c(10000, 1000000)) +
  coord_cartesian(xlim = c(-2700, 2000/2.5), expand = TRUE) + 
  annotate("text", label = "USA", x = -2300, y =  224632, colour = "royalblue3") +
  annotate("text", label = "Japan", x = 300, y = 27281, colour = "grey40") +
  annotate("text", label = "Canada", x = -500, y =  24129, colour = "royalblue3") +
  ylab("Total volume") +
  xlab("Country marginal effect of year (2000 - 2020)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

Amph_bar_sum_imp <- G_ME_Sum_Amph_IMP %>% filter(n >= 10000) %>% group_by(Interpretation) %>% tally() %>% mutate(Col = "A")

Amph_imp_bar <-  ggplot(Amph_bar_sum_imp, aes(x = n, y = Col, fill = Interpretation)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  annotate("text", label = "0", x = 0, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "2", x = 2, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "3", x = 3, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "5", x = 5, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  coord_cartesian(ylim = c(0.5, 2.2)) +
  theme_void() +
  theme(legend.position = "none")

Dual_Imp_Amph2 <- ggarrange(Amph_Imp_top, Amph_imp_bar, ncol = 1, heights = c(1, .3))

## Rept
Rept_Imp_top <- ggplot(filter(G_ME_Sum_Rept_IMP, n >= 10000), aes(G_ME, n, colour = Interpretation)) + 
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper), height = 0, alpha = .35, size = 1) +
  geom_point(shape = 16, size = 2) + 
  scale_colour_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  scale_y_log10(breaks = c(1000000, 100000, 10000)) +
  coord_cartesian(xlim = c(-160000, 75000), expand = TRUE) + 
  annotate("text", label = "Singapore", x = -145000, y =  12682462, colour = "steelblue2") +
  annotate("text", label = "China", x = 70000, y = 5470365, colour = "tomato") +
  annotate("text", label = "Italy", x = -30000, y =  8002235, colour = "grey40") +
  annotate("text", label = "USA", x = -50000, y =  4977257, colour = "grey40") +
  ylab("Total volume") +
  xlab("Country marginal effect of year (2000 - 2020)") +
  theme_minimal() +
  theme(legend.position = "none", panel.grid.minor = element_blank())

Rept_bar_sum_imp <- G_ME_Sum_Rept_IMP %>% filter(n >= 10000) %>% group_by(Interpretation) %>% tally() %>% mutate(Col = "A")

Rept_imp_bar <- ggplot(Rept_bar_sum_imp, aes(x = n, y = Col, fill = Interpretation)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c("tomato", "darkgoldenrod", "grey40",  "steelblue2", "royalblue3"), drop=FALSE) +
  annotate("text", label = "0", x = 0, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "11", x = 11, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "14", x = 14, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "41", x = 40, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "42", x = 43, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  annotate("text", label = "48", x = 48, y = 1.9, colour = "black", fontface = 'bold', size = 4) +
  coord_cartesian(ylim = c(0.5, 2.2)) +
  theme_void() +
  theme(legend.position = "none")

Dual_Imp_Rept2 <- ggarrange(Rept_Imp_top, Rept_imp_bar, ncol = 1, heights = c(1, .3))

#### Arrangement ####

(Dual_all2 <- ggarrange(Dual_Exp_Aves2, Dual_Imp_Aves2, Dual_Exp_Mam2, Dual_Imp_Mam2, 
                        Dual_Exp_Amph2, Dual_Imp_Amph2, Dual_Exp_Rept2, Dual_Imp_Rept2,
                        ncol = 2, nrow = 4, labels = c("A.", "B.","C.", "D.","E.", "F.","G.", "H.")))

empty <- ggplot() + theme_void()
XY_Arrange <- ggarrange(empty, empty, Dual_Exp_Aves2, Dual_Imp_Aves2, empty, empty,
                        Dual_Exp_Mam2, Dual_Imp_Mam2, empty, empty,
                        Dual_Exp_Amph2, Dual_Imp_Amph2, empty, empty,
                        Dual_Exp_Rept2, Dual_Imp_Rept2, nrow = 8, ncol = 2,
                        heights = c(0.12, 1, 0.07, 1, 0.07, 1, 0.07, 1),
                        labels = c("", "", "A.", "B.","", "", "C.", "D.","", "", "E.", "F.","", "", "G.", "H."))
library(png)
library(grid)

## Get images
Rept <-  rasterGrob(readPNG("Data/Misc/Varanus2.png"), interpolate = TRUE)
Amph <- rasterGrob(readPNG("Data/Misc/Dendrobates2.png"), interpolate = TRUE)
Bird <- rasterGrob(readPNG("Data/Misc/Amazona2.png"), interpolate = TRUE)
Mam <- rasterGrob(readPNG("Data/Misc/Lycaloplex2.png"), interpolate = TRUE)

XY_Arrange2 <- XY_Arrange + annotation_custom(Bird, xmin = 0.95, xmax = 0.99, ymin = 0.95, ymax = 1) +
  annotation_custom(Mam, xmin = 0.93, xmax = 0.99, ymin = 0.68, ymax = .78) +
  annotation_custom(Amph, xmin = 0.95, xmax = 0.99, ymin = 0.42, ymax = .52) +
  annotation_custom(Rept, xmin = 0.93, xmax = 0.99, ymin = 0.18, ymax = .28) +
  annotation_custom(text_grob("Exported", face = "bold", size= 14), xmin = 0.25, xmax = 0.25, ymin = 0.97, ymax = 1.0) +
  annotation_custom(text_grob("Imported", face = "bold", size = 14), xmin = 0.75, xmax = 0.75, ymin = 0.97, ymax = 1.0)


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
    annotate("text", label = "Country-level marginal effect\n direction (2000 - 2020)",
             x = -2, y = .5, size = 4.5, colour = "black", fontface = "bold") +
    coord_cartesian(xlim = c(-5.5, 8), ylim = c(-1, 2)) +
    theme_void())

XY_Arrange3 <- ggarrange(XY_Arrange2, Legend, nrow = 2, heights = c(1, .09))


ggsave(path = "Outputs/Figure_3", XY_Arrange3, filename = "Figure3_altColRange.png",  bg = "white",
       device = "png", width = 30, height = 30, units = "cm")



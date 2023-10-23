#########################
#### NLP Category ME ####
#########################


options(scipen = 999) ## remove scientific notation, namely "e"  to show very small and large numbers in full.
options(na.action = "na.pass")

## Set working directory and package location.
## Change this in yours
.libPaths("C:/Packages") ## Set up for working from home.

## Packages
library(tidyverse)
library(marginaleffects)
library(bayestestR)
library(tidybayes)
library(ggridges)
library(cowplot)
library(ggpubr)

#### Read in ####

## Model fitting data
Country_dat_full_vol_aves <- data.table::fread("Models/IR/Fitting_data/Exp_Vol_Aves.csv", na.strings = "")
Country_dat_full_vol_amph <- data.table::fread("Models/IR/Fitting_data/Exp_Vol_Amph.csv", na.strings = "")
Country_dat_full_vol_mam <- data.table::fread("Models/IR/Fitting_data/Exp_Vol_Mam.csv", na.strings = "")
Country_dat_full_vol_rept <- data.table::fread("Models/IR/Fitting_data/Exp_Vol_Rept.csv", na.strings = "")

Country_dat_full_vol_aves_IMP <- data.table::fread("Models/IR/Fitting_data/Imp_Vol_Aves.csv", na.strings = "")
Country_dat_full_vol_amph_IMP <- data.table::fread("Models/IR/Fitting_data/Imp_Vol_Amph.csv", na.strings = "")
Country_dat_full_vol_mam_IMP <- data.table::fread("Models/IR/Fitting_data/Imp_Vol_Mam.csv", na.strings = "")
Country_dat_full_vol_rept_IMP <- data.table::fread("Models/IR/Fitting_data/Imp_Vol_Rept.csv", na.strings = "")

## Models
Mod_Exp_vol_Aves <- readRDS("Models/IR/Exp_Vol_Aves_vH.rds")
Mod_Exp_vol_Amph <- readRDS("Models/IR/Exp_Vol_Amph_vH.rds")
Mod_Exp_vol_Mam <- readRDS("Models/IR/Exp_Vol_Mam_vH.rds")
Mod_Exp_vol_Rept <- readRDS("Models/IR/Exp_Vol_Rept_vH.rds")

Mod_Imp_vol_Aves <- readRDS("Models/IR/Imp_Vol_Aves_vH.rds")
Mod_Imp_vol_Amph <- readRDS("Models/IR/Imp_Vol_Amph_vH.rds")
Mod_Imp_vol_Mam <- readRDS("Models/IR/Imp_Vol_Mam_vH.rds")
Mod_Imp_vol_Rept <- readRDS("Models/IR/Imp_Vol_Rept_vH.rds")



## Convenience function for repetitively fetching and tidying the expectation of a number of models
mod_pred <- function(fitting_data, model) {
  ## Make new data
  ND_FYear <- fitting_data %>% group_by(Category1, SYear, Year) %>% tally()
  
  ## Fit with Yearly fluctuation for the average country
  Fit_FYear <- add_epred_draws(model, newdata = ND_FYear, summary = FALSE, re_formula = ~(1|Year)) %>%
    group_by(Category1, SYear, Year) %>% median_hdci(.epred, .width = .9) %>% 
    mutate(Year = as.numeric(as.character(Year)))
  
  ## Fit with average fluctuations for the average country
  Fit_Average <- add_epred_draws(model, newdata = ND_FYear, summary = FALSE, re_formula = NA) %>%
    group_by(Category1, SYear, Year) %>% median_hdci(.epred, .width = .9) %>% 
    mutate(Year = as.numeric(as.character(Year)))
 
  ## Outputs 
  return(list("Fit_FYear" = Fit_FYear, "Fit_Average" = Fit_Average))
}


## Convenience function for fetching marginal effects - Average country. And N - Y contrasts.
marginal_effect_delta <- function(fitting_data, model, eps = 0.0001) {
  ## Make reference data frame
  ND_FYear <- fitting_data %>% group_by(Category1, SYear, Year) %>% tally()
  
  ## Get the marginal effects of year for the average country with and without Category 1 legislation
  Cat_ME_Y <- marginaleffects(model, variables = "SYear", type = "response", eps = eps,
                                      newdata = filter(ND_FYear, Category1 == "Yes"), re_formula = NA)
  Cat_ME_N <- marginaleffects(model, variables = "SYear", type = "response", eps = eps,
                                      newdata = filter(ND_FYear, Category1 == "No"), re_formula = NA)
  
  ## convert Y (with laws) back to the non z-scored scale
  Cat_ME_Y_Sum <- posteriordraws(Cat_ME_Y) %>% 
    mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))
  
  ## convert N (without laws) back to the non z-scored scale and find the posterior contrast.
  ## Summarise as the average marginal effect per category each year (per year is key as the non linear model
  ## results in varying slopes (marginal effects) per year).
  ## Then summarise to the median difference in marginal effect.
  Cat_ME_Sum <- posteriordraws(Cat_ME_N) %>% 
    mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))  %>%
    ## No - Yes
    mutate(delta_ME = G_ME - Cat_ME_Y_Sum$G_ME) %>%
    group_by(Year) %>%
    mutate(PD = round((sum(sign(delta_ME) == sign(median(delta_ME)))/n())*100, digits = 2)) %>%
    group_by(Year, PD) %>% 
    median_hdci(delta_ME, .width = .9) %>%
    mutate(Median_Trend = ifelse(delta_ME > 0, "Positive", "Negative"))
  
  ## get the raw difference draws.
  Cat_ME_Sum_Raw <- posteriordraws(Cat_ME_N) %>% 
    mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))  %>%
    ## No - Yes
    mutate(delta_ME = G_ME - Cat_ME_Y_Sum$G_ME)
  
  ## provide shorter 3 year summary
  Cat_ME_Sum_3yr <- Cat_ME_Sum %>% filter(Year %in% c("0", "10", "20"))
  
  ## Outputs 
  return(list("Cat_ME_Sum_Raw" = Cat_ME_Sum_Raw, "Cat_ME_Sum" = Cat_ME_Sum, "Cat_ME_Sum_3yr" = Cat_ME_Sum_3yr))
}

## Convenience function for fetching marginal effects - new country. And N - Y contrasts.
marginal_effect_delta_Pred <- function(fitting_data, model, eps = 0.0001) {
  ## Make reference data frame
  ND_FYear <- fitting_data %>% group_by(Category1, SYear, Year) %>% tally() %>% mutate(Country = "Hypothetical")
  
  ## Get the marginal effects of year for the average country with and without Category 1 legislation
  Cat_ME_Y <- marginaleffects(model, variables = "SYear", type = "response", eps = eps,
                              newdata = filter(ND_FYear, Category1 == "Yes"), re_formula = ~(1 + SYear |Country),
                              allow_new_levels = TRUE, sample_new_levels = "uncertainty")
  Cat_ME_N <- marginaleffects(model, variables = "SYear", type = "response", eps = eps,
                              newdata = filter(ND_FYear, Category1 == "No"), re_formula = ~(1 + SYear |Country),
                              allow_new_levels = TRUE, sample_new_levels = "uncertainty")
  
  ## convert Y (with laws) back to the non z-scored scale
  Cat_ME_Y_Sum <- posteriordraws(Cat_ME_Y) %>% 
    mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))
  
  ## convert N (without laws) back to the non z-scored scale and find the posterior contrast.
  ## Summarise as the average marginal effect per category each year (per year is key as the non linear model
  ## results in varying slopes (marginal effects) per year).
  ## Then summarise to the median difference in marginal effect.
  Cat_ME_Sum <- posteriordraws(Cat_ME_N) %>% 
    mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))  %>%
    ## No - Yes
    mutate(delta_ME = G_ME - Cat_ME_Y_Sum$G_ME) %>%
    group_by(Year) %>%
  mutate(PD = (sum(sign(delta_ME) == sign(median(delta_ME)))/n())*100) %>%
    group_by(Year, PD) %>% 
    median_hdci(delta_ME, .width = .9) %>%
    mutate(Median_Trend = ifelse(delta_ME > 0, "Positive", "Negative"))
  
  ## get the raw difference draws.
  Cat_ME_Sum_Raw <- posteriordraws(Cat_ME_N) %>% 
    mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))  %>%
    ## No - Yes
    mutate(delta_ME = G_ME - Cat_ME_Y_Sum$G_ME)
  
  ## Outputs 
  return(list("Cat_ME_Sum_Raw" = Cat_ME_Sum_Raw, "Cat_ME_Sum" = Cat_ME_Sum))
}

## Plotting function to create expecation plots and then the marginal effect contrast inset plots.
inset_ME_plot <- function(mod_pred_output, marginal_effect_delta_output, y_axis_buffer = FALSE) {
  ## Input is the list output of mod_pred and the list output of marginal_effect_delta
  
  if (y_axis_buffer == FALSE) {
  ## Plot the expectation both with and with yearly fluctuations
  Expectation_plot <- ggplot(mod_pred_output$Fit_Average, aes(Year+2000, .epred, colour = Category1, fill = Category1)) +
     geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .2, colour = NA) +
     geom_line(size = 1) +
     geom_line(data = mod_pred_output$Fit_FYear, size = 1, linetype = "dashed") +
     scale_colour_manual(values = c("black", "chartreuse4")) +
     scale_fill_manual(values = c("black", "chartreuse4")) +
     xlab("Year") + ylab("Volume (WOEs)") +
     scale_x_continuous(breaks = c(2000, 2010, 2020)) +
     theme_minimal(base_size = 14) +
     theme(legend.position = "none")
  }
  
  if (y_axis_buffer == TRUE) {
    ## Plot the expectation both with and with yearly fluctuations
    Expectation_plot <- ggplot(mod_pred_output$Fit_Average, aes(Year+2000, .epred, colour = Category1, fill = Category1)) +
      geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .2, colour = NA) +
      geom_line(size = 1) +
      geom_line(data = mod_pred_output$Fit_FYear, size = 1, linetype = "dashed") +
      scale_colour_manual(values = c("black", "chartreuse4")) +
      scale_fill_manual(values = c("black", "chartreuse4")) +
      xlab("Year") + ylab("Volume (WOEs)") +
      scale_x_continuous(breaks = c(2000, 2010, 2020)) +
      coord_cartesian(ylim = c(min(mod_pred_output$Fit_Average$.lower),
                               1.5*max(mod_pred_output$Fit_Average$.upper))) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
  }
  
  ## Plot the delta ME between Y and N in 2000, 2010, and 2020
  delta_ME_plot <- ggplot(marginal_effect_delta_output$Cat_ME_Sum_3yr, aes(delta_ME, Year)) +
      geom_point(size = 2) +
      geom_errorbarh(aes(xmin = .lower, xmax = .upper, height = 0), size = 1) +
      geom_vline(xintercept = 0, linetype = "dashed", size = .5) +
      xlab(expression(Delta*AME)) +
    scale_y_continuous(labels = c(2000, 2010, 2020), breaks = c(0, 10, 20)) +
    geom_text(aes(label = PD, x = .upper + 0.2*(max(.upper) - min(.lower)), y = Year)) +
      coord_cartesian(xlim = c(min(marginal_effect_delta_output$Cat_ME_Sum_3yr$.lower) - 
                               0.15*(max(marginal_effect_delta_output$Cat_ME_Sum_3yr$.upper) -
                                       min(marginal_effect_delta_output$Cat_ME_Sum_3yr$.lower)),
                             max(marginal_effect_delta_output$Cat_ME_Sum_3yr$.upper) + 
                               0.25*(max(marginal_effect_delta_output$Cat_ME_Sum_3yr$.upper) - 
                                       min(marginal_effect_delta_output$Cat_ME_Sum_3yr$.lower))),
                      ylim = c(-5, 25)) +
      theme_minimal(base_size = 12) +
      theme(axis.line = element_line(colour = "black"), panel.grid = element_blank(), axis.title.y = element_blank(),
            axis.line.y = element_blank(), axis.text.y = element_text(colour = "black"))
  
  ## Format the delta_me plot inside the expecation plot
  Inset_plot <- ggdraw() + draw_plot(Expectation_plot) + draw_plot(delta_ME_plot, x = 0.45, y = .55, width = .5, height = .4)
  
  ## Outputs 
  return(list("Expectation_plot" = Expectation_plot, "delta_ME_plot" = delta_ME_plot, "Inset_plot" = Inset_plot))
}

#### Expectation values - Exporter ####

Aves_Exp_Fits <- mod_pred(fitting_data = Country_dat_full_vol_aves, model = Mod_Exp_vol_Aves)
Mam_Exp_Fits <- mod_pred(fitting_data = Country_dat_full_vol_mam, model = Mod_Exp_vol_Mam)
Amph_Exp_Fits <- mod_pred(fitting_data = Country_dat_full_vol_amph, model = Mod_Exp_vol_Amph)
Rept_Exp_Fits <- mod_pred(fitting_data = Country_dat_full_vol_rept, model = Mod_Exp_vol_Rept)

#### Expectation values - Importer ####
Aves_Imp_Fits <- mod_pred(fitting_data = Country_dat_full_vol_aves_IMP, model = Mod_Imp_vol_Aves)
Mam_Imp_Fits <- mod_pred(fitting_data = Country_dat_full_vol_mam_IMP, model = Mod_Imp_vol_Mam)
Amph_Imp_Fits <- mod_pred(fitting_data = Country_dat_full_vol_amph_IMP, model = Mod_Imp_vol_Amph)
Rept_Imp_Fits <- mod_pred(fitting_data = Country_dat_full_vol_rept_IMP, model = Mod_Imp_vol_Rept)

#### Marginal effects - Exporter ####
Aves_Exp_ME <- marginal_effect_delta(fitting_data = Country_dat_full_vol_aves, model = Mod_Exp_vol_Aves)
Mam_Exp_ME <- marginal_effect_delta(fitting_data = Country_dat_full_vol_mam, model = Mod_Exp_vol_Mam)
Amph_Exp_ME <- marginal_effect_delta(fitting_data = Country_dat_full_vol_amph, model = Mod_Exp_vol_Amph)
Rept_Exp_ME <- marginal_effect_delta(fitting_data = Country_dat_full_vol_rept, model = Mod_Exp_vol_Rept)

#### Marginal effects - Importer ####
Aves_Imp_ME <- marginal_effect_delta(fitting_data = Country_dat_full_vol_aves_IMP, model = Mod_Imp_vol_Aves)
Mam_Imp_ME <- marginal_effect_delta(fitting_data = Country_dat_full_vol_mam_IMP, model = Mod_Imp_vol_Mam)
## Note for amphibians we dont have obs yes/no counries for the full time frame so we do it maunally below
#Amph_Imp_ME <- marginal_effect_delta(fitting_data = Country_dat_full_vol_amph_IMP, model = Mod_Imp_vol_Amph)
Rept_Imp_ME <- marginal_effect_delta(fitting_data = Country_dat_full_vol_rept_IMP, model = Mod_Imp_vol_Rept)

## Amph
fitting_data = Country_dat_full_vol_amph_IMP
model = Mod_Imp_vol_Amph
eps = 0.0001
## Make reference data frame
ND_FYear <- fitting_data %>% group_by(Category1, SYear, Year) %>% tally() %>% 
  filter(Year %in% c("0", "1", "2", "3", "4", "5", "6"))

## Get the marginal effects of year for the average country with and without Category 1 legislation
Cat_ME_Y <- marginaleffects(model, variables = "SYear", type = "response", eps = eps,
                            newdata = filter(ND_FYear, Category1 == "Yes"), re_formula = NA)
Cat_ME_N <- marginaleffects(model, variables = "SYear", type = "response", eps = eps,
                            newdata = filter(ND_FYear, Category1 == "No"), re_formula = NA)

## convert Y (with laws) back to the non z-scored scale
Cat_ME_Y_Sum <- posteriordraws(Cat_ME_Y) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))

## convert N (without laws) back to the non z-scored scale and find the posterior contrast.
## Summarise as the average marginal effect per category each year (per year is key as the non linear model
## results in varying slopes (marginal effects) per year).
## Then summarise to the median difference in marginal effect.
Cat_ME_Sum <- posteriordraws(Cat_ME_N) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))  %>%
  ## No - Yes
  mutate(delta_ME = G_ME - Cat_ME_Y_Sum$G_ME) %>%
  group_by(Year) %>%
  mutate(PD = round((sum(sign(delta_ME) == sign(median(delta_ME)))/n())*100, digits = 2)) %>%
  group_by(Year, PD) %>% 
  median_hdci(delta_ME, .width = .9) %>%
  mutate(Median_Trend = ifelse(delta_ME > 0, "Positive", "Negative"))

## get the raw difference draws.
Cat_ME_Sum_Raw <- posteriordraws(Cat_ME_N) %>% 
  mutate(G_ME = draw/sd(as.numeric(as.character(fitting_data$Year))))  %>%
  ## No - Yes
  mutate(delta_ME = G_ME - Cat_ME_Y_Sum$G_ME)

## provide shorter 3 year summary
Amph_Cat_ME_Sum_Raw <- Cat_ME_Sum %>% filter(Year %in% c("0"))

#### Plotting ####
Aves_Exp_plots <- inset_ME_plot(mod_pred_output = Aves_Exp_Fits, marginal_effect_delta_output = Aves_Exp_ME)
Mam_Exp_plots <- inset_ME_plot(mod_pred_output = Mam_Exp_Fits, marginal_effect_delta_output = Mam_Exp_ME)
Amph_Exp_plots <- inset_ME_plot(mod_pred_output = Amph_Exp_Fits, marginal_effect_delta_output = Amph_Exp_ME)
Rept_Exp_plots <- inset_ME_plot(mod_pred_output = Rept_Exp_Fits, marginal_effect_delta_output = Rept_Exp_ME)

Aves_Imp_plots<- inset_ME_plot(mod_pred_output = Aves_Imp_Fits, marginal_effect_delta_output = Aves_Imp_ME)
Mam_Imp_plots <- inset_ME_plot(mod_pred_output = Mam_Imp_Fits, marginal_effect_delta_output = Mam_Imp_ME)
#Amph_Imp_plots <- inset_ME_plot(mod_pred_output = Amph_Imp_Fits, marginal_effect_delta_output = Amph_Imp_ME, y_axis_buffer = TRUE)
Rept_Imp_plots <- inset_ME_plot(mod_pred_output = Rept_Imp_Fits, marginal_effect_delta_output = Rept_Imp_ME)

## AMph
Expectation_plot_Amph <-ggplot(Amph_Imp_Fits$Fit_Average, aes(Year+2000, .epred, colour = Category1, fill = Category1)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .2, colour = NA) +
  geom_line(size = 1) +
  geom_line(data = Amph_Imp_Fits$Fit_FYear, size = 1, linetype = "dashed") +
  scale_colour_manual(values = c("black", "chartreuse4")) +
  scale_fill_manual(values = c("black", "chartreuse4")) +
  xlab("Year") + ylab("Volume (WOEs)") +
  scale_x_continuous(breaks = c(2000, 2010, 2020)) +
  coord_cartesian(ylim = c(min(Amph_Imp_Fits$Fit_Average$.lower),
                           1.5*max(Amph_Imp_Fits$Fit_Average$.upper))) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

delta_ME_plot_amph <- ggplot(Amph_Cat_ME_Sum_Raw, aes(delta_ME, Year)) +
  geom_point(size = 2) +
  geom_errorbarh(aes(xmin = .lower, xmax = .upper, height = 0), size = 1) +
  geom_vline(xintercept = 0, linetype = "dashed", size = .5) +
  xlab(expression(Delta*AME)) +
  scale_y_continuous(labels = c(2000), breaks = c(0)) +
  geom_text(aes(label = PD, x = .upper + 0.2*(max(.upper) - max(.lower)), y = Year)) +
  coord_cartesian(xlim = c(-12, 20),
                  ylim = c(-5, 5)) +
  theme_minimal(base_size = 12) +
  theme(axis.line = element_line(colour = "black"), panel.grid = element_blank(), axis.title.y = element_blank(),
        axis.line.y = element_blank(), axis.text.y = element_text(colour = "black"))

## Format the delta_me plot inside the expecation plot
Inset_plot_amph <- ggdraw() + draw_plot(Expectation_plot_Amph) + draw_plot(delta_ME_plot_amph, x = 0.45, y = .55, width = .5, height = .4)

#### Arrangement ####

empty <- ggplot() + theme_void()
Category_plt <- ggarrange(empty, empty, Aves_Exp_plots$Inset_plot, Aves_Imp_plots$Inset_plot, 
                          empty, empty, Mam_Exp_plots$Inset_plot, Mam_Imp_plots$Inset_plot,
                          empty, empty, Amph_Exp_plots$Inset_plot, Inset_plot_amph, 
                          empty, empty, Rept_Exp_plots$Inset_plot, Rept_Imp_plots$Inset_plot,
                          ncol = 2, nrow = 8, heights = c(.15, 1, .05, 1, .05, 1, .05, 1),
                          labels = c("", "", "A.", "B.","", "", "C.", "D.","", "", "E.", "F.","", "", "G.", "H."))

library(png)
library(grid)
## Get images
Rept <-  rasterGrob(readPNG("Data/Misc/Varanus2.png"), interpolate = TRUE)
Amph <- rasterGrob(readPNG("Data/Misc/Dendrobates2.png"), interpolate = TRUE)
Bird <- rasterGrob(readPNG("Data/Misc/Amazona2.png"), interpolate = TRUE)
Mam <- rasterGrob(readPNG("Data/Misc/Lycaloplex2.png"), interpolate = TRUE)

## Arrange images and titles
Category_plt2 <- Category_plt + annotation_custom(Bird, xmin = 0.95, xmax = 0.99, ymin = 0.93, ymax = .98) +
  annotation_custom(Mam, xmin = 0.93, xmax = 0.99, ymin = 0.67, ymax = .77) +
  annotation_custom(Amph, xmin = 0.95, xmax = 0.99, ymin = 0.43, ymax = .53) +
  annotation_custom(Rept, xmin = 0.93, xmax = 0.99, ymin = 0.18, ymax = .28) +
  annotation_custom(text_grob("Exported", face = "bold", size= 14), xmin = 0.25, xmax = 0.25, ymin = 0.97, ymax = 1.0) +
  annotation_custom(text_grob("Imported", face = "bold", size = 14), xmin = 0.75, xmax = 0.75, ymin = 0.97, ymax = 1.0)

## get legend
Legend <- get_legend(ggplot(Aves_Exp_Fits$Fit_Average, aes(Year+2000, .epred, colour = Category1, fill = Category1)) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper), alpha = .2, colour = NA) +
  geom_line(size = 1) +
  scale_colour_manual(values = c("black", "chartreuse4"), labels = c("Category 1 legislation absent", "Category 1 legislation present")) +
  scale_fill_manual(values = c("black", "chartreuse4"), labels = c("Category 1 legislation absent", "Category 1 legislation present")) +
  theme_minimal(base_size = 16) +
  theme(legend.position = "bottom", legend.title = element_blank()))

## Add legend
Category_plt3 <- ggarrange(Category_plt2, legend.grob = Legend, legend = "bottom")

ggsave(path = "Outputs/SOM/IR", Category_plt3, filename = "Figure_4_IR.png",  bg = "white",
       device = "png", width = 30, height = 30, units = "cm")

#### Write out ####
write.csv(Aves_Exp_ME$Cat_ME_Sum, "Outputs/SOM/IR/Aves_Exp_deltaME.csv", na = "")
write.csv(Mam_Exp_ME$Cat_ME_Sum, "Outputs/SOM/IR/Mam_Exp_deltaME.csv", na = "")
write.csv(Amph_Exp_ME$Cat_ME_Sum, "Outputs/SOM/IR/Amph_Exp_deltaME.csv", na = "")
write.csv(Rept_Exp_ME$Cat_ME_Sum, "Outputs/SOM/IR/Rept_Exp_deltaME.csv", na = "")

write.csv(Aves_Imp_ME$Cat_ME_Sum, "Outputs/SOM/IR/Aves_Imp_deltaME.csv", na = "")
write.csv(Mam_Imp_ME$Cat_ME_Sum, "Outputs/SOM/IR/Mam_Imp_deltaME.csv", na = "")
write.csv(Amph_Imp_ME$Cat_ME_Sum, "Outputs/SOM/IR/Amph_Imp_deltaME.csv", na = "")
write.csv(Rept_Imp_ME$Cat_ME_Sum, "Outputs/SOM/IR/Rept_Imp_deltaME.csv", na = "")

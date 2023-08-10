################################################################
# Small Area Estimation Models for Total Population Estimation #
# Reading and Preparation of Databases                         #
# Author: Stalyn Guerrero & Andrés Gutiérrez                   #
# Description: Consolidation of CENSUS housing databases and   #
# exploration (point estimation and standard error)            #
################################################################

### Cleaning R environment ###
# Clear the workspace by removing all variables
rm(list = ls())

#################
### Libraries ###
#################
# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(data.table)  # For efficient data manipulation
library(openxlsx)  # For reading Excel files
library(magrittr)  # For data manipulation using pipe operators
library(lme4)  # For fitting linear mixed-effects models
library(posterior)  # For Bayesian data analysis
library(rstanarm)  # For fitting Bayesian regression models
library(rstan)  # For Bayesian data analysis using Stan
cat("\f")  # Clear the console

## Reading census data.
# Read census data from the 'censo_viviendas.rds' file
censo_vivienda <- readRDS("Recursos/04_Model_binomial/Data/censo_viviendas.rds")
# Read UGMS covariates from the 'Base_ugms_estandarizada.rds' file
Base_ugms <- readRDS("Recursos/04_Model_binomial/Data/Base_ugms_estandarizada.rds")
# Read binomial Bayesian model for unoccupied housing from the 'Binomial_bayes_vivienda_desocupadas.rds' file
modelo_binomial <- readRDS("Recursos/04_Model_binomial/Data/Binomial_bayes_vivienda_desocupadas.rds")

## Defining occupied and unoccupied houses.
censo_vivienda %<>% mutate(Desocupada = case_when(
  greenpoint2 == "Censado con informacion n=0" ~ 1,
  greenpoint2 == "Censado con informacion n>0" ~ 0,
  greenpoint2 == "Sin informacion pero  n>0" ~ 0, 
  Filtros == "Censado en papel" & H01A_TOTAL_PERSONAS > 0 ~ 0,
  Filtros == "Censado en papel" & H01A_TOTAL_PERSONAS == 0 ~ 1,
  greenpoint2 == "Sin informacion pero n>=0" ~ NA_real_
) )

# Waiting time of 5 to 10 minutes 
pred_linear <- posterior_linpred(modelo_binomial,
                                 newdata = Base_ugms,
                                 draws = 1000) 
pred_unoccupied <- plogis(pred_linear)

saveRDS(pred_unoccupied, "Recursos/04_Model_binomial/Data/pred_unoccupied.rds")
pred_unoccupied <- readRDS("Recursos/04_Model_binomial/Data/pred_unoccupied.rds")

# Check dimensions of the predicted values and the base
dim(pred_unoccupied)
dim(Base_ugms)

# Count the number of predicted values below 0 and above 1
sum(colMeans(pred_unoccupied) < 0)
sum(colMeans(pred_unoccupied) > 1)

# Summarize the means of predicted values
summary(colMeans(pred_unoccupied))

# Calculate the 5th and 95th percentiles of predicted values
q0.05 <- apply(pred_unoccupied, MARGIN = 2,
                function(x) quantile(x, 0.05))
q0.95 <- apply(pred_unoccupied, MARGIN = 2,
                function(x) quantile(x, 0.95))

# Calculate the standard deviation of predicted values
sd_pred <- apply(pred_unoccupied, MARGIN = 2, sd)
summary(sd_pred)
# Create a data frame with prediction intervals
intervalos <- data.frame(UGM_ID = Base_ugms$UGM_ID,
                         Pred_unoccupied = colMeans(pred_unoccupied),
                         UpperLim_unoccupied = colMeans(pred_unoccupied) + 3 * sd_pred * q0.975,
                         LowerLim_unoccupied = colMeans(pred_unoccupied) - 3 * sd_pred * q0.025
)

# Inner join between censo_vivienda and intervalos based on UGM_ID
censo_vivienda %<>% inner_join(intervalos, by = "UGM_ID")

# Calculate new values for Desocupada and prediction intervals
censo_vivienda %<>%
  mutate(
    Desocupada2  = case_when(is.na(Desocupada) ~ Pred_unoccupied,
                             TRUE ~ Desocupada),
    LimInf_desocupadas  = case_when(is.na(Desocupada) ~ LowerLim_unoccupied,
                                    TRUE ~ Desocupada),
    LimSup_desocupadas  = case_when(is.na(Desocupada) ~ UpperLim_unoccupied,
                                    TRUE ~ Desocupada)
  )

# Calculate means and sums of different variables
mean_original_desocupada <- mean(censo_vivienda$Desocupada, na.rm = TRUE)
mean_updated_desocupada <- mean(censo_vivienda$Desocupada2)
mean_predicted_desocupada <- mean(censo_vivienda$Pred_unoccupied)

sum_original_desocupada <- sum(censo_vivienda$Desocupada, na.rm = TRUE)
sum_updated_desocupada <- sum(censo_vivienda$Desocupada2)
sum_predicted_desocupada <- sum(censo_vivienda$Pred_unoccupied)

# Calculate mean estimation, confidence intervals, and percentages
result_summary <- censo_vivienda %>%
  mutate(MEInf_desocupadas = Desocupada2 - LimInf_desocupadas,
         MESup_desocupadas = LimSup_desocupadas - Desocupada2) %>% 
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2) ,
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas)/sqrt(n()))) * 100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas)/sqrt(n()))) * 100,
    Porcen = Porcen * 100
  )

saveRDS(result_summary,
        "Recursos/04_Model_binomial/RecurseBooks/result_summary.rds")


# Group by PROV_ID and calculate MEInf_desocupadas and MESup_desocupadas
prov_summary <- censo_vivienda %>% group_by(PROV_ID) %>%   
  mutate(
    MEInf_desocupadas = Desocupada2 - LimInf_desocupadas,
    MESup_desocupadas = LimSup_desocupadas - Desocupada2
  ) %>%
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2),
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) / sqrt(n()))) * 100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas) / sqrt(n()))) * 100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  )

saveRDS(prov_summary,
        "Recursos/04_Model_binomial/RecurseBooks/prov_summary.rds")


# Group by CANT_ID and calculate MEInf_desocupadas and MESup_desocupadas
cant_summary <- censo_vivienda %>% group_by(CANT_ID) %>%   
  mutate(
    MEInf_desocupadas = Desocupada2 - LimInf_desocupadas,
    MESup_desocupadas = LimSup_desocupadas - Desocupada2
  ) %>%
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2),
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) / sqrt(n()))) * 100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas) / sqrt(n()))) * 100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  )

saveRDS(cant_summary, "Recursos/04_Model_binomial/RecurseBooks/cant_summary.rds")

# Group by DIST_ID and calculate MEInf_desocupadas and MESup_desocupadas
dist_summary <- censo_vivienda %>% group_by(DIST_ID) %>%   
  mutate(
    MEInf_desocupadas = Desocupada2 - LimInf_desocupadas,
    MESup_desocupadas = LimSup_desocupadas - Desocupada2
  ) %>%
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2),
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) / sqrt(n()))) * 100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas) / sqrt(n()))) * 100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  )

saveRDS(dist_summary, "Recursos/04_Model_binomial/RecurseBooks/dist_summary.rds")

# Save modified censo_vivienda without Pred_unoccupied column
censo_vivienda_modified <- censo_vivienda %>% dplyr::select(-Pred_unoccupied)
saveRDS(censo_vivienda_modified,
        file = "Recursos/04_Model_binomial/Data/01_censo_vivienda_desocupadas.rds")


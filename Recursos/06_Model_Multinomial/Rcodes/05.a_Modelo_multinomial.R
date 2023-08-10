###############################################################
# Small Area Estimation Models for Population Estimation      #
# Reading and Data Preparation                                #
# Author: Stalyn Guerrero & Andrés Gutiérrez                  #
# Multinomial model for estimating people by age range and sex#
###############################################################

### Cleaning R environment ###
# Clearing the R environment by removing all variables from memory.
rm(list = ls())

#################
### Libraries ###
#################

# Loading necessary libraries for data analysis.
library(tidyverse)   # Data manipulation and visualization
library(data.table)  # Fast data manipulation
library(openxlsx)    # Reading Excel files
library(magrittr)    # Pipe operator
library(lme4)        # For fitting linear mixed-effects models
library(nnet)        # For fitting neural networks
cat("\f")  # Clears console output

## Read census data. 
# Reading the preprocessed census data file containing information about households.
censo_vivienda <- readRDS("Recursos/06_Model_Multinomial/Data/02_censo_vivienda_personas.rds") 

# Filtering the census data to retain relevant columns for age and gender analysis.
censo_vivienda_age_gender <- censo_vivienda %>% 
  filter( !greenpoint2 %in% c("Sin informacion pero  n>0", 
                              "Sin informacion pero n>=0")) %>% 
  dplyr::select(PROV_ID, HOMBRES_GRUPO1_sum:MUJERES_GRUPO20_sum) 

# Summarizing the age and gender data by grouping it based on province (PROV_ID).
censo_personas <- censo_vivienda_age_gender %>% 
  group_by(PROV_ID) %>% 
  summarise_if(is.numeric, sum)

# Fitting a multinomial model to estimate the distribution of age and gender within households,
# using province as the predictor.
model <- multinom( censo_personas[,-1] %>% as.matrix() ~ censo_personas$PROV_ID)

# Predicting the distribution probabilities using the fitted model.
predict(model,type = "probs")

# Saving the fitted multinomial model to a file.
saveRDS(model, "Recursos/06_Model_Multinomial/Data/Multinomial_model.rds")

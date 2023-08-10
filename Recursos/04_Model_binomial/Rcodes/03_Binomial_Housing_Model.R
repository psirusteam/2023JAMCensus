################################################################
# Small Area Estimation Models for Total Population Estimation #
# Reading and Preparation of Databases                         #
# Author: Stalyn Guerrero & Andrés Gutiérrez                   #
# Description: A binomial model for occupied and unoccupied    #
#              households                                      #
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
library(rstan)  # For Bayesian data analysis using Stan
library(rstanarm)  # For fitting Bayesian regression models
select <-
  dplyr::select  # Define the 'select' function from dplyr package
cat("\f")  # Clear the console

## Reading census data.
# Read census data from the 'censo_viviendas.rds' file and select specific columns
censo_vivienda <- readRDS("Recursos/04_Model_binomial/Data/censo_viviendas.rds") %>%
  select(un_ID:Filtros)

Covariables_UGM <- readRDS("Recursos/04_Model_binomial/Data/Base_ugms_estandarizada.rds")
# Read UGM covariates from the 'Base_ugms_estandarizada.rds' file

## Defining occupied and unoccupied households.
# Define the 'Desocupada' column based on specific conditions

censo_vivienda %<>% mutate(
  Desocupada = case_when(
    greenpoint2 == "Censado con informacion n=0" ~ 1,
    greenpoint2 == "Censado con informacion n>0" ~ 0,
    greenpoint2 == "Sin informacion pero  n>0" ~ 0,
    Filtros == "Censado en papel" & H01A_TOTAL_PERSONAS > 0 ~ 0,
    Filtros == "Censado en papel" & H01A_TOTAL_PERSONAS == 0 ~ 1,
    greenpoint2 == "Sin informacion pero n>=0" ~ NA_real_
  )
)

# Counting combinations of occupancy status and greenpoint values 

# Grouping and summarizing to get counts for different combinations

conteos <- censo_vivienda %>% 
  group_by(greenpoint2, Desocupada) %>% 
  summarise(total = n(), .groups = "drop")

# Save the summary of counts to a file
saveRDS(conteos, 
        "Recursos/04_Model_binomial/RecurseBooks/conteos.rds")

####### Binomial model base dataset #########
# Grouping and summarizing data to create a binomial model base dataset
base_conteo_viviendas <- censo_vivienda %>% group_by(UGM_ID) %>%
  summarise(
    Desocupadas = sum(Desocupada, na.rm = TRUE),
    Ocupadas = sum(1 - Desocupada, na.rm = TRUE),
    n_vivienda = n()
  )

# Save the binomial model base dataset
saveRDS(base_conteo_viviendas, 
        "Recursos/04_Model_binomial/RecurseBooks/base_conteo_viviendas.rds")

# Summarize the numeric columns in 'base_conteo_viviendas' and save the results
base_conteo_viviendas %>% summarise_if(is.numeric, sum) %>% 
  saveRDS("Recursos/04_Model_binomial/RecurseBooks/base_conteo2.rds")

# Inner join the binomial model base dataset with standardized covariates
base_conteo_viviendas <- inner_join(base_conteo_viviendas,
                                    Covariables_UGM, by = "UGM_ID")

saveRDS(base_conteo_viviendas, 
        "Recursos/04_Model_binomial/Data/base_conteo_viviendas.rds")

# The provided code includes the following options:
  
# `options(mc.cores = parallel::detectCores())`:
# This option sets the number of cores used for parallel computation in Stan 
# models. The `parallel::detectCores()` function automatically detects the 
# number of available CPU cores on your machine. By setting the number of cores,
# you can leverage parallel processing to speed up the estimation process of the 
# Stan model.

# `rstan::rstan_options(auto_write = TRUE)`: This option is related to writing 
# compiled Stan models to disk for caching purposes. When `auto_write` is set 
# to `TRUE`, it indicates that compiled Stan models should be automatically 
# saved to disk to speed up the compilation process in future runs. This can 
# improve the running time of the model, especially if you run the same model 
# multiple times.

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) # Speed up running time 

modelo_binomial <- stan_glmer(
  cbind(Desocupadas, Ocupadas) ~ 1 +
    (1 | PROV_ID) +
    (1 | CANT_ID) +
    (1 | DIST_ID) +
    dist_codigo_urbanidad +
    ugm_peligrosidad +                        
    ugm_problema_de_acceso +                  
    ugm_riesgos_amenazas +                    
    ugm_cobertura_telecomunicaciones +        
    dist_permisos_de_construccion_2011_2022 + 
    dist_poblacion_proyeccion_ajustada_2022 + 
    dist_poblacion_ccss_abril_2023 +          
    dist_matricula_educacion_primaria_2021 +  
    dist_codigo_urbanidad +                   
    GHS_BUILT_S_E2020_GLOBE_R2023A_5367_CRI + 
    urban_coverfraction +                     
    crops_coverfraction +                     
    asent +                                   
    ppp_CRI_v2 +                              
    elev +                                    
    indig +                                   
    aprot +  
    ebais_tt +                                
    escu_tt +                                 
    igl_tt +                                  
    dist_nl_mean 
  ,
  data = base_conteo_viviendas,
  family = binomial(link = "logit"),
  iter = 1000,            # total number of iterations per chain
  cores = 4, 
)

saveRDS(object = modelo_binomial, 
        file = "Recursos/04_Model_binomial/Data/Binomial_bayes_vivienda_desocupadas.rds")

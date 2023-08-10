################################################################################
# Título del Script: Modelo de unidad para el censo de Costa Rica 
# Autor: Stalyn Guerrero, Andrés Gutiérrez 
# Fecha: 23/05/2023
################################################################################

### Lectura de librerías 
rm(list = ls())
library(tidyverse)
library(data.table)
library(openxlsx)
library(DataExplorer)
library(magrittr)
library(RColorBrewer)
library(lme4)
library(rstan)
library(rstanarm)

cat("\f")
## Lectura de la base censo 
censo_vivienda <- readRDS("Modelo_Unidad/Data/02_censo_vivienda_personas.rds") %>% 
  select(un_ID:Filtros) %>% as.data.frame()
Covariables_UGM <- readRDS("Modelo_Unidad/Data/Base_ugms_22062023.rds")

####### Base modelo total de personas 
censo_vivienda_estima_todas <- censo_vivienda %>% 
  filter( !greenpoint2 %in% c("Sin informacion pero  n>0", 
                              "Sin informacion pero n>=0")) %>% 
  group_by(UGM_ID) %>% 
  summarise(tot_personas = sum(H01A_TOTAL_PERSONAS),
            tot_viviendas = n())

nrow(censo_vivienda_estima_todas)
sum(is.na(censo_vivienda_estima_todas$tot_personas))

censo_vivienda_estima_todas <-
  inner_join(censo_vivienda_estima_todas, Covariables_UGM) 

censo_vivienda_estima_ocupadas <- censo_vivienda %>% 
  filter( greenpoint2 %in% c("Censado con informacion n>0",
                             "Papel n>0")) %>% 
  group_by(UGM_ID) %>% 
  summarise(tot_personas = sum(H01A_TOTAL_PERSONAS),
            tot_viviendas = n())


nrow(censo_vivienda_estima_ocupadas)
sum(is.na(censo_vivienda_estima_ocupadas$tot_personas))

censo_vivienda_estima_ocupadas <-
  inner_join(censo_vivienda_estima_ocupadas, Covariables_UGM)


############## Datos para stan #################

Y_obs <- censo_vivienda_estima_todas$tot_personas
V_obs <- censo_vivienda_estima_todas$tot_viviendas


X_obs <- model.matrix( UGM_ID~ dist_codigo_urbanidad +
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
                         dist_nl_mean,
                       data = censo_vivienda_estima_todas
) %>% as.matrix()

Z_obs <- model.matrix(UGM_ID ~
                        -1 +
                        (PROV_ID) +
                        (CANT_ID) +
                        (DIST_ID)  ,
                      data = censo_vivienda_estima_todas)
dim(Z_obs)

sample_data <- list(
  D = nrow(X_obs) ,
  K = ncol(X_obs),
  Kz = ncol(Z_obs),
  Y_obs = Y_obs,
  V_obs = V_obs,
  X_obs = X_obs %>% as.matrix(),
  Z_obs = Z_obs %>% as.matrix()
)

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) # speed up running time 

## Dado el numero de parametos es necesario tener muchas iteraciones

fit_poisson_todas <- stan(
  file = "Modelo_unidad/0funciones/Modelo_worldpop.stan",  # Stan program
  data = sample_data,    # named list of data
  verbose = TRUE,
  warmup = 10000,          # number of warmup iterations per chain
  iter = 15000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
)

saveRDS(fit_poisson_todas,
        "Modelo_unidad/Data/Modelos/fit_poisson_todas_worldpop,rds")


############## Datos para stan #################

Y_obs <- censo_vivienda_estima_ocupadas$tot_personas
V_obs <- censo_vivienda_estima_ocupadas$tot_viviendas


X_obs <- model.matrix( UGM_ID~ dist_codigo_urbanidad +
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
                         dist_nl_mean,
                       data = censo_vivienda_estima_ocupadas
) %>% as.matrix()

Z_obs <- model.matrix(UGM_ID ~
                        -1 +
                        (PROV_ID) +
                        (CANT_ID) +
                        (DIST_ID)  ,
                      data = censo_vivienda_estima_ocupadas)


sample_data <- list(
  D = nrow(X_obs) ,
  K = ncol(X_obs),
  Kz = ncol(Z_obs),
  Y_obs = Y_obs,
  V_obs = V_obs,
  X_obs = X_obs %>% as.matrix(),
  Z_obs = Z_obs %>% as.matrix()
)

options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) # speed up running time 

## Dado el numero de parametos es necesario tener muchas iteraciones

fit_poisson <- stan(
  file = "Modelo_unidad/0funciones/Modelo_worldpop.stan",  # Stan program
  data = sample_data,    # named list of data
  verbose = TRUE,
  warmup = 10000,          # number of warmup iterations per chain
  iter = 15000,            # total number of iterations per chain
  cores = 4,              # number of cores (could use one per chain)
)

saveRDS(fit_poisson, 
        "Modelo_unidad/Data/Costa_Rica/Base_prueba2/fit_poisson_ocupadas_worldpop,rds")


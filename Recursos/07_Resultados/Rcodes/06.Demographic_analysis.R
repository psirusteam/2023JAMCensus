###############################################################
# Small Area Estimation Models for Population Total Estimation #
# Reading and Data Preparation                                 #
# Author: Stalyn Guerrero & Andrés Gutiérrez                   #
###############################################################

### Cleaning R environment ###
rm(list = ls())

#################
### Libraries ###
#################
library(tidyverse)
library(data.table)
library(openxlsx)
library(magrittr)
library(lme4)
library(rstan)
library(rstanarm)
source("Recursos/07_Resultados/Rcodes/01_Agregados.R")
cat("\f")

## Reading the census data.
censo_vivienda <- readRDS("Resources/07_Results/Data/05_censo_vivienda_personas_grupo_edad.rds") 

#########################################################
## Prediction of the unoccupied household rate 
#########################################################

Pred_desocupado(censo_vivienda,
                agrega = NULL,
                Plot = TRUE,
                filtro = NULL)

Pred_desocupado(censo_vivienda, agrega = "PROV_ID")
Pred_desocupado(censo_vivienda, agrega = "CANT_ID")
Pred_desocupado(censo_vivienda, agrega = "DIST_ID")

Pred_desocupado(censo_vivienda,
                agrega = "DIST_ID",
                Plot = TRUE,
                filtro = "10101")

#########################################################
## Total Population Prediction
#########################################################

## National level result

Pred_totPob(censo_vivienda,
                     agrega = NULL,
                     Plot = TRUE)

## Provincial level result

Pred_totPob(censo_vivienda,
                     agrega = "PROV_ID",
                     Plot = FALSE)

Pred_totPob(censo_vivienda,
                     agrega = "PROV_ID",
                     filtro = "1",
                     Plot = TRUE)

## Canton level result

Pred_totPob(censo_vivienda,
                     agrega = "CANT_ID",
                     Plot = TRUE,
                     filtro = "101")

Pred_totPob(censo_vivienda,
                     agrega = "CANT_ID",
                     Plot = FALSE,
                     filtro = NULL)

## District level result

Pred_totPob(censo_vivienda,
                     agrega = "DIST_ID",
                     Plot = FALSE)

Pred_totPob(censo_vivienda,
                     agrega = "DIST_ID",
                     Plot = TRUE,
                     filtro = "10110")

#########################################################
# Prediction of Total Population by Age and Gender
#########################################################
piramide_pob(
  censo = censo_vivienda,
  Plot = FALSE,
  agrega = NULL,
  filtro = NULL
)

piramide_pob(
  censo = censo_vivienda,
  Plot = TRUE,
  agrega = NULL,
  filtro = NULL
)

piramide_pob(
  censo = censo_vivienda,
  Plot = FALSE,
  agrega = "PROV_ID",
  filtro = NULL
)

piramide_pob(
  censo = censo_vivienda,
  Plot = TRUE,
  agrega = "PROV_ID",
  filtro = "1"
)

piramide_pob(
  censo = censo_vivienda,
  Plot = TRUE,
  agrega = "PROV_ID",
  filtro = "2"
)

paso <- piramide_pob(
  censo = censo_vivienda,
  Plot = FALSE,
  agrega = "CANT_ID",
  filtro = NULL
)


piramide_pob(
  censo = censo_vivienda,
  Plot = TRUE,
  agrega = "CANT_ID",
  filtro = "101"
)

piramide_pob(
  censo = censo_vivienda,
  Plot = TRUE,
  agrega = "CANT_ID",
  filtro = "102"
)

###############################################################
# Small Area Estimation Models for Total Population Estimation #
# Reading and Preparation of Database                          #
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
library(DataExplorer)
library(magrittr)
library(RColorBrewer)
select<- dplyr::select
cat("\f")

## Lectura de la base censo 
censo <- readRDS("Modelo_unidad/Data/b_hh_filled_V12023-06-12.rds") %>% 
  select(-geometry) %>% as.data.frame()


resumen <- data.frame(Nombre_Columna = names(censo))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x)class(censo[[x]])))

## Asignación de Nas a la variable H01A_TOTAL_PERSONAS
# - las viviendas que no fueron visitadas (categoría 9), 
# - las viviendas que rechazaron o pendientes (clasificación 2)
# - las viviendas que tienen otro motivo (categoría 8)

## Conteo de caso antes y despues de la asignación 

censo %>% mutate(
  H01A_TOTAL_PERSONAS_temp = case_when(
    V02_OCUPACION_VIVIENDA == "9" ~ NA_real_,
    V02_OCUPACION_VIVIENDA == "2" ~ NA_real_,
    V02_OCUPACION_VIVIENDA == "8" ~ NA_real_,
    TRUE ~ H01A_TOTAL_PERSONAS
  )
) %>% group_by(V02_OCUPACION_VIVIENDA) %>%
  summarise(nas_antes = sum(is.na(H01A_TOTAL_PERSONAS)),
            nas_despues = sum(is.na(H01A_TOTAL_PERSONAS_temp))) %>%
  mutate(total_nas_despues = sum(nas_despues))

## Asignación de los NAs

censo %<>% mutate(
  H01A_TOTAL_PERSONAS = case_when(
    V02_OCUPACION_VIVIENDA == "9" ~ NA_real_,
    V02_OCUPACION_VIVIENDA == "2" ~ NA_real_,
    V02_OCUPACION_VIVIENDA == "8" ~ NA_real_,
    TRUE ~ H01A_TOTAL_PERSONAS
  )
) 

## Valores descriptivos del censo 

## Variable Númericas 
max_values <- censo %>%
  summarise(across(where(is.numeric) | where(is.integer), max)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Maximo")

min_values <- censo %>%
  summarise(across(where(is.numeric) | where(is.integer), min)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Minimo")

media_values <- censo %>%
  summarise(across(where(is.numeric) | where(is.integer), mean)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Media")

mediana_values <- censo %>%
  summarise(across(where(is.numeric) | where(is.integer), median)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Mediana")

SD_values <- censo %>%
  summarise(across(where(is.numeric) | where(is.integer), sd)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_sd")

nas_values <- censo %>%
  summarise(across(where(is.numeric) | where(is.integer), function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas")

# Variables carácter
max_char <- censo %>%
  summarise(across(where(is.character), function(x)max(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_max")

min_char <- censo %>%
  summarise(across(where(is.character), function(x)min(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_min")

nas_values_char <- censo %>%
  summarise(across(where(is.character) , function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas_char")

## Organizando en una base de datos resultados. 
resumen2 <- reduce(
  list(
    nas_values_char,
    min_char,
    max_char,
    nas_values,
    SD_values,
    mediana_values,
    media_values,
    min_values,
    max_values
  ),
  full_join,
  by = join_by(Nombre_Columna)
) %>%
  full_join(x = resumen,
            y = .,
            by = join_by(Nombre_Columna))

# Guardando reporte  
openxlsx::write.xlsx(resumen2,
                     file = "Modelo_unidad/Data/Estado_base22062023.xlsx")

## Realizando cambios según los resultados obtenidos en el reporte. 
Nombre_Columna <-  c(
  "un_ID" , 
  "PROV_ID" , 
  "CANT_ID" , 
  "DIST_ID" , 
  "UGM_ID" , 
  "LLAVEV" , 
  "RESUL_ENTREVISTA_VIV" , 
  "TIPO_VIVIENDA_PRECENSO" , 
  "V01_TIPO_VIVIENDA" , 
  "V02_OCUPACION_VIVIENDA" , 
  "H01A_TOTAL_PERSONAS" , 
  "greenpoint" , 
  "ugm_viviendas_totales_censo" , 
  "ugm_viviendas_ocupadas_censo" , 
  "ugm_viviendas_desocupadas_censo" , 
  "ugm_peligrosidad" , 
  "ugm_problema_de_acceso" , 
  "ugm_riesgos_amenazas" , 
  "ugm_cobertura_telecomunicaciones" , 
  "asent" , 
  "ppp_CRI_v2" , 
  "elev" , 
  "indig" , 
  "aprot" , 
  "dist_permisos_de_construccion_2011_2022" , 
  "dist_poblacion_proyeccion_ajustada_2022" , 
  "dist_poblacion_ccss_abril_2023" , 
  "dist_matricula_educacion_primaria_2021" , 
  "dist_codigo_urbanidad" , 
  "GHS_BUILT_S_E2020_GLOBE_R2023A_5367_CRI" , 
  "urban_coverfraction" , 
  "crops_coverfraction" , 
  "ebais_tt" , 
  "escu_tt" , 
  "igl_tt" , 
  "prov_nl_mean" , 
  "cant_nl_mean" , 
  "dist_nl_mean" , 
  "wpop_sum" ) 

Tipo_actualizar <- c(
  as.character,
  as.character,
  as.character,
  as.character,
  as.character,
  as.character,
  as.character,
  as.character,
  as.character,
  as.character,
  as.numeric,
  as.character,
  as.numeric,
  as.numeric,
  as.numeric,
  as.character,
  as.character,
  as.character,
  as.character,
  as.character,
  as.numeric,
  as.numeric,
  as.character,
  as.character,
  as.numeric,
  as.numeric,
  as.numeric,
  as.numeric,
  as.character,
  as.numeric,
  as.numeric,
  as.numeric,
  as.numeric,
  as.numeric,
  as.numeric,
  as.numeric,
  as.numeric,
  as.numeric,
  as.numeric)


map2(Nombre_Columna, Tipo_actualizar, function(nom,tipo){
  censo[[nom]] <<- tipo(censo[[nom]])
})

censo2 <- censo %>% select(all_of(Nombre_Columna))

## Resumen de variables
censo2 %>% distinct(UGM_ID, wpop_sum) %>% 
  summarise(n = sum(wpop_sum))

## conteo de ugm_viviendas_totales_censo == 0 

ugm_cero_viviendas <- censo2 %>%
  distinct(UGM_ID, ugm_viviendas_totales_censo) %>% 
  filter(ugm_viviendas_totales_censo == 0)
dim(ugm_cero_viviendas)
## Comparando con el numero de registros por ugm

cont_registros_ugm <- censo2 %>% group_by(UGM_ID) %>% 
  tally(name = "Total_vivienda_ugm")

inner_join(ugm_cero_viviendas, cont_registros_ugm) %>% 
  summarise(n_ugm = n(),
            min = min(Total_vivienda_ugm), 
            max = max(Total_vivienda_ugm),
            mediana = median(Total_vivienda_ugm))


censo2 %>% filter(V02_OCUPACION_VIVIENDA == "8") %>% 
  summarise(n_viviendas = n(),
            min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            mediana = median(H01A_TOTAL_PERSONAS))





resumen <- data.frame(Nombre_Columna = names(censo2))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x)class(censo2[[x]])))
tipo_char <- resumen$Nombre_Columna[resumen$tipo == "character"]

for(ii in tipo_char) {
  max_char <- max(nchar(censo2[[ii]]))
  censo2[[ii]] <-
    str_pad(string = censo2[[ii]],
            width = max_char,
            pad = "0")
}

max_char <- censo2 %>%
  summarise(across(where(is.character), function(x)max(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_max")

min_char <- censo2 %>%
  summarise(across(where(is.character), function(x)min(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_min")

nas_values_char <- censo2 %>%
  summarise(across(where(is.character) , function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas_char")

max_values <- censo2 %>%
  summarise(across(where(is.numeric) | where(is.integer), max)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Maximo")

min_values <- censo2 %>%
  summarise(across(where(is.numeric) | where(is.integer), min)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Minimo")

media_values <- censo2 %>%
  summarise(across(where(is.numeric) | where(is.integer), mean)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Media")

mediana_values <- censo2 %>%
  summarise(across(where(is.numeric) | where(is.integer), median)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Mediana")

SD_values <- censo2 %>%
  summarise(across(where(is.numeric) | where(is.integer), sd)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_sd")

nas_values <- censo2 %>%
  summarise(across(where(is.numeric) | where(is.integer), function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas")



resumen2 <- reduce(
  list(nas_values_char, min_char,max_char,
       nas_values, SD_values, mediana_values, media_values,min_values, max_values),
  full_join, by = join_by(Nombre_Columna)) %>% 
  full_join(x = resumen, y = ., by = join_by(Nombre_Columna))

openxlsx::write.xlsx(resumen2,
                     file = "Modelo_unidad/Data/Estado_base_despues_22062023.xlsx")

saveRDS(censo2, file = "Modelo_unidad/Data/censo_estandarizado_22062023.rds")


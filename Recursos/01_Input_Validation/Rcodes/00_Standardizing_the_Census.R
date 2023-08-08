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

## Reading the Census Data ##
# Reading the census data from the specified RDS file
# and excluding the 'geometry' column. Converting it to a data frame.

censo <- readRDS("Recursos/01_Input_Validation/Data/Data_census_V2023-06-12.rds") %>%
  select(-geometry) %>% as.data.frame()

# Creating a summary data frame for column names and their respective data types.

resumen <- data.frame(Nombre_Columna = names(censo))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x) class(censo[[x]])))
saveRDS(resumen, "Recursos/01_Input_Validation/RecurseBooks/resumen1.rds")
## Assigning Missing Values to the 'H01A_TOTAL_PERSONAS' Variable

# In this section, actions related to the 'H01A_TOTAL_PERSONAS' variable are performed.
# Missing values (NA) are assigned to this variable based on specific conditions,
# which include:
#   
# - Dwellings that were not visited (category 9).
# - Dwellings that rejected the visit or are pending (classification 2).
# - Dwellings with other reasons (category 8).
# 
# Next, a count of cases before and after the assignment of missing values is 
# conducted. The 'mutate' function is used to create a new temporary column 
# 'H01A_TOTAL_PERSONAS_temp' in which NA values are assigned according to the 
# specified conditions. The information is then grouped by the 'V02_OCUPACION_VIVIENDA'
# variable, and the number of missing values ('nas') before and after the 
# assignment is calculated, along with the total count of missing values after
# the assignment.
# 
# Subsequently, another assignment of missing values is performed directly to 
# the 'H01A_TOTAL_PERSONAS' variable within the 'censo' dataset. This is done 
# following the same conditions mentioned earlier.

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

## Assignment of Missing Values

censo %<>% mutate(
  H01A_TOTAL_PERSONAS = case_when(
    V02_OCUPACION_VIVIENDA == "9" ~ NA_real_,
    V02_OCUPACION_VIVIENDA == "2" ~ NA_real_,
    V02_OCUPACION_VIVIENDA == "8" ~ NA_real_,
    TRUE ~ H01A_TOTAL_PERSONAS
  )
) 

## Descriptive Values of the Census Data

### Numeric Variables

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

## Organizing Results in a Database

# In this section, the collected descriptive statistics are organized and combined
# into a comprehensive summary database named 'resumen2'.

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
                     file = "Recursos/01_Input_Validation/Data/Estado_base.xlsx")

saveRDS(resumen2,
        file = "Recursos/01_Input_Validation/RecurseBooks/Estado_base.xlsx")


## Updating the Dataset Based on Report Results

# In this part of the code, the 'censo' dataset is updated based on the results
# obtained from the report.

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
# Saving the updated dataset to the specified directory
saveRDS(censo2, 
        file = "Recursos/01_Input_Validation/RecurseBooks/censo_updated.rds")

## Resumen de variables
censo2 %>% distinct(UGM_ID, wpop_sum) %>% 
  summarise(n = sum(wpop_sum))

## Count of ugm_viviendas_totales_censo == 0

censo2 %>%
  distinct(UGM_ID, ugm_viviendas_totales_censo) %>%
  mutate(categoria = cut(ugm_viviendas_totales_censo,
                         breaks = c(
                           -1:5, 10, 20, 50,
                           max(ugm_viviendas_totales_censo)
                         ))) %>% group_by(categoria) %>% tally()
ugm_cero_viviendas

## Comparing with the number of records per UGM

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

# Creating a summary of the column names and their data types
resumen <- data.frame(Nombre_Columna = names(censo2))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x)class(censo2[[x]])))

# Checking for character variables and ensuring consistent character length
tipo_char <- resumen$Nombre_Columna[resumen$tipo == "character"]

for(ii in tipo_char) {
  max_char <- max(nchar(censo2[[ii]]))
  censo2[[ii]] <-
    str_pad(string = censo2[[ii]],
            width = max_char,
            pad = "0")
}

# Summarizing character variables
max_char <- censo2 %>%
  summarise(across(where(is.character), function(x)max(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_max")

min_char <- censo2 %>%
  summarise(across(where(is.character), function(x)min(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_min")

nas_values_char <- censo2 %>%
  summarise(across(where(is.character) , function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas_char")

# Summarizing numeric variables
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

# Combining all the summary information
resumen2 <- reduce(
  list(nas_values_char, min_char, max_char,
       nas_values, SD_values, mediana_values, media_values, min_values, max_values),
  full_join, by = join_by(Nombre_Columna)) %>% 
  full_join(x = resumen, y = ., by = join_by(Nombre_Columna))


# Saving the summary results to an Excel file
openxlsx::write.xlsx(resumen2,
                     file = "Recursos/01_Input_Validation/Data/Estado_base_despues.xlsx")

# Saving the standardized dataset
saveRDS(censo2, file = "Recursos/01_Input_Validation/Data/censo_estandarizado.rds")


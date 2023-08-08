###############################################################
# Small Area Estimation Models for Total Population Estimation #
# Author: Stalyn Guerrero & Andrés Gutiérrez                   #
# Description: Reading the standardized census data and defining filters 
# on the census dataset.                                      #
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
select <- dplyr::select
cat("\f")
## Lectura de la base censo 
## Seleccion de columnas de interes en el censo. 
Nombre_Columna <-  c(
  "un_ID" , 
  "PROV_ID" , 
  "CANT_ID" , 
  "DIST_ID" , 
  "UGM_ID" , 
  "LLAVEV" , 
  "V01_TIPO_VIVIENDA" , 
  "V02_OCUPACION_VIVIENDA" , 
  "H01A_TOTAL_PERSONAS" , 
  "greenpoint2",
  "Filtros"
   ) 

## Lectura de bases de viviendas sin coordenadas. 
Viviendas_sin_coordenadas <-
  read_csv2("Modelo_unidad/Data/Viviendas sin coordenadas.csv")

Viviendas_sin_coordenadas %<>% 
  transmute(
    LLAVEV,
    PROV_ID = str_sub(CODIGO_PCD, 1,1),
    CANT_ID = str_sub(CODIGO_PCD, 1,3),
    DIST_ID    = as.character(CODIGO_PCD),
    UGM_ID = paste0(CODIGO_PCD , ID_UGM), 
    H01A_TOTAL_PERSONAS = H01A_TOTAL_RESIDENTES_HAB)

## Realizado cambios de códigos de upms en la base de Viviendas_sin_coordenadas. 
Viviendas_sin_coordenadas %<>%
mutate(UGM_ID = 
  case_when(
    UGM_ID == "10108228" ~ "10108158",
    UGM_ID == "10805037" ~ "10807037",
    UGM_ID == "11803124" ~ "11803024",
    UGM_ID == "11803150" ~ "11803050",
    UGM_ID == "11803151" ~ "11803051",
    UGM_ID == "20302131" ~ "20302031",
    UGM_ID == "21305106" ~ "21305006",
    UGM_ID == "30101232" ~ "30101132",
    UGM_ID == "30201354" ~ "30201254",
    UGM_ID == "30302158" ~ "30302133",
    UGM_ID == "30305186" ~ "30305086",
  TRUE ~UGM_ID
))

## Lectura del censo. 
censo1 <- readRDS("Modelo_unidad/Data/censo_estandarizado_22062023.rds") %>% 
  mutate(UGM_ID =
    case_when(
      UGM_ID == "10108228" ~ "10108158",
      UGM_ID == "10805037" ~ "10807037",
      UGM_ID == "11803124" ~ "11803024",
      UGM_ID == "11803150" ~ "11803050",
      UGM_ID == "11803151" ~ "11803051",
      UGM_ID == "20302131" ~ "20302031",
      UGM_ID == "21305106" ~ "21305006",
      UGM_ID == "30101232" ~ "30101132",
      UGM_ID == "30201354" ~ "30201254",
      UGM_ID == "30302158" ~ "30302133",
      UGM_ID == "30305186" ~ "30305086",
      TRUE ~UGM_ID
    ))


###########################################################################
## Agregando la base de sexo-edad
censo_sexo_edad <-
  readRDS("Modelo_unidad/Data/Censo con grupos por sexo.rds") %>% 
  select(-H01A_TOTAL_PERSONAS ) %>% 
  mutate(UGM_ID =
           case_when(
             UGM_ID == "10108228" ~ "10108158",
             UGM_ID == "10805037" ~ "10807037",
             UGM_ID == "11803124" ~ "11803024",
             UGM_ID == "11803150" ~ "11803050",
             UGM_ID == "11803151" ~ "11803051",
             UGM_ID == "20302131" ~ "20302031",
             UGM_ID == "21305106" ~ "21305006",
             UGM_ID == "30101232" ~ "30101132",
             UGM_ID == "30201354" ~ "30201254",
             UGM_ID == "30302158" ~ "30302133",
             UGM_ID == "30305186" ~ "30305086",
             TRUE ~UGM_ID
           ))

censo1 <- inner_join(censo1, censo_sexo_edad)

## Definición del primer filtro 
censo1 %<>% mutate(personas = if_else(H01A_TOTAL_PERSONAS>0,"si","no"))
censo1 %<>% mutate(greenpoint = if_else(greenpoint == "0" & personas == "si","1",
                                        greenpoint))

censo1 %>% 
  group_by(greenpoint) %>% tally() %>% 
  mutate(n = 100 *n/sum(n))

censo1 %>% 
  group_by(greenpoint) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())

table(censo1$V02_OCUPACION_VIVIENDA, censo1$greenpoint, useNA = "a")

censo2 <- censo1 %>% mutate(greenpoint2 = case_when(
  H01A_TOTAL_PERSONAS > 0 ~ "Censado con informacion n>0",
  RESUL_ENTREVISTA_VIV %in% c(1) & H01A_TOTAL_PERSONAS == 0 ~ "Censado con informacion n=0",
  RESUL_ENTREVISTA_VIV %in% c(3,4) ~ "Sin informacion pero  n>0", 
  is.na(greenpoint) & is.na(personas) ~ "Sin informacion pero n>=0",
  V02_OCUPACION_VIVIENDA == "8" ~ "Sin informacion pero n>=0", 
  TRUE ~ "Resto"
)) 


table(censo2$greenpoint2, useNA = "a")

## Definición del segundo filtro

# 2. Partiendo de 1, además sea agregan todas las viviendas con covariable 
# WP que están a 1 desviaciones estándar de su media, pero que tienen un cero
# en la variable de interés. En ese caso la variable de interés se hace NA

censo2 %>% distinct(UGM_ID,wpop_sum) %>%
  summarise(media = mean(wpop_sum),
            sd = sd(wpop_sum),
            min = min(wpop_sum),
            max = max(wpop_sum))
li <- 96.96515 - 143.1986*1
ls <- 96.96515 + 143.1986*1

censo2 %>% filter(H01A_TOTAL_PERSONAS == 0,
                  wpop_sum > ls | wpop_sum < li) %>%
  group_by(V02_OCUPACION_VIVIENDA) %>% summarise(n = n() )


censo3 <- censo2 %>% mutate(
  greenpoint2 = case_when(
    H01A_TOTAL_PERSONAS == 0 & (wpop_sum > ls | wpop_sum < li)  ~ "Sin informacion pero n>=0",
    TRUE ~ greenpoint2
  ),
  Filtros = case_when(
    H01A_TOTAL_PERSONAS == 0 & (wpop_sum > ls | wpop_sum < li)  ~ "Criterio WorldPop",
    TRUE ~ NA_character_
  )
)

censo3 %>% 
  group_by(greenpoint2) %>% tally() %>% 
  mutate(n = 100 *n/sum(n))

censo3 %>% 
  group_by(greenpoint2,Filtros) %>% tally() %>% 
  mutate(n = 100 *n/sum(n))

censo3 %>% 
  group_by(greenpoint2) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())

## Definición del tercer filtro 

# 3. Partiendo de 1 y de 2, Todas las viviendas en las UGM que fueron visitadas 
# en un periodo mayor a 20 días (periodo más frecuente) que se  
# clasificaron como desocupadas fueron recategorizadas como con estatus 
# desconocido porque no se tiene la seguridad de que los ooperativos
# posteriores hubiesen sido efectivos.

upms_reporte <-
  openxlsx::read.xlsx("Modelo_unidad/Data/Desocupadas fuera periodo.xlsx") %>%
  select(UGM_ID = ID_UGM)

censo4 <- censo3 %>% mutate(
  greenpoint2 = case_when(
    UGM_ID %in% upms_reporte$UGM_ID &  H01A_TOTAL_PERSONAS == 0  ~ "Sin informacion pero n>=0",
    TRUE ~ greenpoint2
  ),
  Filtros = case_when(
    UGM_ID %in% upms_reporte$UGM_ID &  H01A_TOTAL_PERSONAS == 0  ~ "Fuera de periodo(20 días)",
    TRUE ~ Filtros
  )
)

censo4 %<>% mutate(Filtros = case_when(
  is.na(Filtros) & H01A_TOTAL_PERSONAS > 0 ~ "Número de personas mayor a 0",
  is.na(Filtros) & RESUL_ENTREVISTA_VIV %in% c(1) & H01A_TOTAL_PERSONAS == 0 ~ "Entrevista igual a 1 y Número de personas igual a 0",
  is.na(Filtros) & RESUL_ENTREVISTA_VIV %in% c(3,4) ~ "Entrevista  es 3 o 4", 
  is.na(Filtros) & is.na(greenpoint) & is.na(personas) ~ "Sin conteo de personas",
  is.na(Filtros) & V02_OCUPACION_VIVIENDA == "8" ~ "Ocupación de la vivienda es 8", 
  TRUE ~ Filtros
)) 

censo4 %>% 
  group_by(greenpoint2,Filtros) %>% tally() %>% 
  ungroup() %>% 
  mutate(n = 100 *n/sum(n))



censo4 %>% 
  group_by(greenpoint2) %>% tally() %>% 
  mutate(n = 100 *n/sum(n))


censo4 %>% 
  group_by(greenpoint2,Filtros) %>% tally() %>% 
  mutate(n = 100 *n/sum(n))

##### uniendo los no coordenadas 
Viviendas_sin_coordenadas2 <-
  censo_sexo_edad %>% filter(is.na(un_ID)) %>%
  inner_join(Viviendas_sin_coordenadas) %>% 
  mutate(un_ID = paste0("A",1:n()))

censo4 <- bind_rows(censo4, Viviendas_sin_coordenadas2 )

censo4 %<>% mutate(Filtros = ifelse(is.na(greenpoint2),
                                        "Censado en papel", greenpoint2))

censo4 %<>% mutate(greenpoint2 = case_when(
  Filtros == "Censado en papel" &  H01A_TOTAL_PERSONAS == 0 ~ "Papel n=0",
  Filtros == "Censado en papel" &  H01A_TOTAL_PERSONAS > 0 ~ "Papel n>0",
  TRUE ~greenpoint2
) )


censo4 %>% 
  group_by(greenpoint2) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())

censo4 %>% 
  group_by(greenpoint2, Filtros) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())


censo4 %>% 
  group_by(greenpoint2, Filtros) %>% 
  summarise(total = n(),
            nas = sum(is.na(H01A_TOTAL_PERSONAS)))


censo4 %>% group_by(un_ID) %>% 
  tally() %>% filter(n>1)

paso <- censo4 %>% select(
  all_of(Nombre_Columna),
  matches("GRUPO")
)

saveRDS(paso, 
        file = "Modelo_unidad/Data/censo_viviendas_22062023.rds")

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

# Load necessary libraries
library(tidyverse)      # For data manipulation and visualization
library(data.table)     # For efficient data handling
library(openxlsx)       # For reading/writing Excel files
library(magrittr)       # For pipe operators
select <- dplyr::select # Alias for dplyr's select function
cat("\f")               # Clear console output

## Census data reading
## Selection of columns of interest in the census.

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
  "Filters"
) 

## Reading housing data without coordinates.

Viviendas_sin_coordenadas <- read_csv2("Recursos/02_Census_Filters/Data/Viviendas sin coordenadas.csv")

# Transmute data to required format
Viviendas_sin_coordenadas %<>% 
  transmute(
    LLAVEV,
    PROV_ID = str_sub(CODIGO_PCD, 1,1),
    CANT_ID = str_sub(CODIGO_PCD, 1,3),
    DIST_ID    = as.character(CODIGO_PCD),
    UGM_ID = paste0(CODIGO_PCD , ID_UGM), 
    H01A_TOTAL_PERSONAS = H01A_TOTAL_RESIDENTES_HAB)

## Changing UGM codes in the housing data.

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

# Read the standardized census data
censo1 <- readRDS("Recursos/02_Census_Filters/Data/censo_estandarizado.rds") %>% 
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
## Adding the Age-Sex Base

censo_sexo_edad <-
  readRDS("Recursos/02_Census_Filters/Data/Censo con grupos por sexo.rds") %>% 
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

## The difference between the bases corresponds to paper-censused households
## that are included later
nrow(censo_sexo_edad) - nrow(censo1) 
nrow(Viviendas_sin_coordenadas)

## Inner join to add the sex-age base

censo1 <- inner_join(
  censo1,
  censo_sexo_edad,
  join_by(
    un_ID,
    PROV_ID,
    CANT_ID,
    DIST_ID,
    UGM_ID,
    LLAVEV,
    V01_TIPO_VIVIENDA,
    V02_OCUPACION_VIVIENDA
  )
)

## Applying filters and analyzing the data

## Applying the first filter: categorizing households with residents and determining greenpoint status

# Create 'personas' column to categorize households with or without residents
censo1 %<>% 
  mutate(personas = if_else(H01A_TOTAL_PERSONAS > 0, "si", "no")) 

# Assign greenpoint status based on conditions

censo1 %<>% mutate(greenpoint = if_else(greenpoint == "0" & personas == "si", "1", greenpoint))  

## Analyzing the greenpoint distribution
greenpoint_distribution <- censo1 %>%
  group_by(greenpoint) %>% tally() %>%  # Counting households with and without greenpoint
  mutate(percentage = 100 * n / sum(n))  # Calculating the percentage of each category

saveRDS(greenpoint_distribution,
        "Recursos/02_Census_Filters/RecurseBooks/greenpoint_distribution.rds")

## Summarizing household characteristics by greenpoint status
household_summary <- censo1 %>% 
  group_by(greenpoint) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS),  # Minimum number of residents in households
            max = max(H01A_TOTAL_PERSONAS),  # Maximum number of residents in households
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),  # Number of missing values for the total number of residents
            total = n())  # Total number of households

saveRDS(household_summary,
        "Recursos/02_Census_Filters/RecurseBooks/household_summary.rds")
## Creating a contingency table for occupancy and greenpoint status

occupancy_greenpoint_table <- table(censo1$V02_OCUPACION_VIVIENDA, censo1$greenpoint, useNA = "a")
occupancy_greenpoint_table

saveRDS(occupancy_greenpoint_table,
        "Recursos/02_Census_Filters/RecurseBooks/occupancy_greenpoint_table.rds")

censo2 <- censo1 %>% mutate(greenpoint2 = case_when(
  H01A_TOTAL_PERSONAS > 0 ~ "Censado con informacion n>0",
  RESUL_ENTREVISTA_VIV %in% c(1) & H01A_TOTAL_PERSONAS == 0 ~ "Censado con informacion n=0",
  RESUL_ENTREVISTA_VIV %in% c(3,4) ~ "Sin informacion pero  n>0", 
  is.na(greenpoint) & is.na(personas) ~ "Sin informacion pero n>=0",
  V02_OCUPACION_VIVIENDA == "8" ~ "Sin informacion pero n>=0", 
  TRUE ~ "Resto"
)) 

saveRDS(head(censo2, 100),
        "Recursos/02_Census_Filters/RecurseBooks/census2.rds")


censo2 %>% group_by(greenpoint2) %>% tally(name = "Freq")


## Applying the second filter: WorldPop criterion

# Starting from step 1, we also include all households with the 
# WorldPop variable (WP) that are within 1 standard deviation from its
# average value. However, if these households have zero residents in the 
# variable of interest, we mark that variable as "Not Available" (NA).


# Calculate summary statistics for the 'wpop_sum' variable
wpop_summary <- censo2 %>% distinct(UGM_ID,wpop_sum) %>%
  summarise(media = mean(wpop_sum),  # Mean value of 'wpop_sum'
            sd = sd(wpop_sum),  # Standard deviation of 'wpop_sum'
            min = min(wpop_sum),  # Minimum value of 'wpop_sum'
            max = max(wpop_sum))  # Maximum value of 'wpop_sum'

saveRDS(wpop_summary,
        "Recursos/02_Census_Filters/RecurseBooks/wpop_summary.rds")

# Calculate the lower and upper thresholds based on one standard deviation from the mean
li <- 96.96515 - 143.1986 * 1  # Lower threshold
ls <- 96.96515 + 143.1986 * 1  # Upper threshold

# Identify and count households that meet the criteria for the second filter
filter_2_counts <- censo2 %>% filter(H01A_TOTAL_PERSONAS == 0,
                                     wpop_sum > ls | wpop_sum < li) %>%
  group_by(V02_OCUPACION_VIVIENDA) %>% summarise(n = n())

filter_2_counts

saveRDS(filter_2_counts,
        "Recursos/02_Census_Filters/RecurseBooks/filter_2_counts.rds")


# Apply the second filter and update 'greenpoint2' and 'Filtros' columns

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

saveRDS(head(censo3, 100),
        "Recursos/02_Census_Filters/RecurseBooks/census3.rds")

# Summarizing the data based on the 'greenpoint2' variable
summary_greenpoint2 <- censo3 %>%
  group_by(greenpoint2) %>%
  tally() %>%
  mutate(percentage = 100 * n / sum(n))

saveRDS(summary_greenpoint2, 
        file = "Recursos/02_Census_Filters/RecurseBooks/summary_greenpoint2.rds")


# Summarizing the data based on the combination of 'greenpoint2' and 'Filtros' variables
summary_greenpoint2_filtros <- censo3 %>%
  group_by(greenpoint2, Filtros) %>%
  tally() %>%
  mutate(percentage = 100 * n / sum(n))

saveRDS(summary_greenpoint2_filtros, 
        file = "Recursos/02_Census_Filters/RecurseBooks/summary_greenpoint2_filtros.rds")

# Summarizing the data for 'greenpoint2' variable
summary_greenpoint2_stats <- censo3 %>%
  group_by(greenpoint2) %>%
  summarise(min = min(H01A_TOTAL_PERSONAS),
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())

saveRDS(summary_greenpoint2_stats, 
        file = "Recursos/02_Census_Filters/RecurseBooks/summary_greenpoint2_stats.rds")

## Defining the Third Filter

# Building upon Filters 1 and 2, all households within UGMs that were visited 
# after an interval greater than 20 days (most frequent interval) and were 
# classified as unoccupied have been reclassified as having an unknown status.
# This is due to uncertainty regarding the effectiveness of subsequent operations.

# Reading the 'Desocupadas fuera periodo.xlsx' file and selecting the UGM_ID column

upms_reporte <- openxlsx::read.xlsx(
  xlsxFile = "Recursos/02_Census_Filters/Data/Desocupadas fuera periodo.xlsx") %>%
  select(UGM_ID = ID_UGM)

# Creating 'censo4' by applying filters based on 'upms_reporte' and specific conditions
censo4 <- censo3 %>% mutate(
  greenpoint2 = case_when(
    UGM_ID %in% upms_reporte$UGM_ID & H01A_TOTAL_PERSONAS == 0  ~ "Sin informacion pero n>=0",
    TRUE ~ greenpoint2
  ),
  Filtros = case_when(
    UGM_ID %in% upms_reporte$UGM_ID & H01A_TOTAL_PERSONAS == 0  ~ "Fuera de periodo(20 días)",
    TRUE ~ Filtros
  )
)

# Applying additional filters and creating 'Filtros' values
censo4 %<>% mutate(Filtros = case_when(
  is.na(Filtros) & H01A_TOTAL_PERSONAS > 0 ~ "Número de personas mayor a 0",
  is.na(Filtros) & RESUL_ENTREVISTA_VIV %in% c(1) & H01A_TOTAL_PERSONAS == 0 ~ "Entrevista igual a 1 y Número de personas igual a 0",
  is.na(Filtros) & RESUL_ENTREVISTA_VIV %in% c(3,4) ~ "Entrevista  es 3 o 4", 
  is.na(Filtros) & is.na(greenpoint) & is.na(personas) ~ "Sin conteo de personas",
  is.na(Filtros) & V02_OCUPACION_VIVIENDA == "8" ~ "Ocupación de la vivienda es 8", 
  TRUE ~ Filtros
)) 

# Summarizing data based on 'greenpoint2' and 'Filtros' variables
summary_greenpoint2_filtros <- censo4 %>% 
  group_by(greenpoint2, Filtros) %>% tally() %>% 
  ungroup() %>% 
  mutate(percentage = 100 * n / sum(n))

saveRDS(summary_greenpoint2_filtros, 
        file = "Recursos/02_Census_Filters/RecurseBooks/summary_greenpoint2_filtros.rds")

# Summarizing data based on 'greenpoint2' variable
summary_greenpoint2 <- censo4 %>% 
  group_by(greenpoint2) %>% tally() %>% 
  mutate(percentage = 100 * n / sum(n))

saveRDS(summary_greenpoint2,
        file = "Recursos/02_Census_Filters/RecurseBooks/summary_greenpoint2.rds")

##### Combining non-coordinated houses #####
# Selecting non-coordinated houses from the 'censo_sexo_edad' dataset
# and inner joining with 'Viviendas_sin_coordenadas'

Viviendas_sin_coordenadas2 <- censo_sexo_edad %>%
  filter(is.na(un_ID)) %>%
  inner_join(Viviendas_sin_coordenadas) %>%
  # Adding a unique identifier 'un_ID' to the newly joined houses
  mutate(un_ID = paste0("A", 1:n()))

# Adding the newly joined houses to the 'censo4' dataset
censo4 <- bind_rows(censo4, Viviendas_sin_coordenadas2)

# Modifying 'Filtros' column based on 'greenpoint2' values
censo4 %<>% mutate(Filtros = ifelse(is.na(greenpoint2),
                                    "Censado en papel", greenpoint2))

# Modifying 'greenpoint2' column based on conditions
censo4 %<>% mutate(greenpoint2 = case_when(
  Filtros == "Censado en papel" &  H01A_TOTAL_PERSONAS == 0 ~ "Papel n=0",
  Filtros == "Censado en papel" &  H01A_TOTAL_PERSONAS > 0 ~ "Papel n>0",
  TRUE ~greenpoint2
))

##### Aggregating statistics and summaries #####
# Summarizing statistics for the 'censo4' dataset based on 'greenpoint2' column
summary1 <- censo4 %>% 
  group_by(greenpoint2) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())
saveRDS(summary1, 
        file = "Recursos/02_Census_Filters/RecurseBooks/summary1.rds")

# Summarizing statistics for the 'censo4' dataset based on 'greenpoint2' and 'Filtros' columns
summary2 <- censo4 %>% 
  group_by(greenpoint2, Filtros) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())
saveRDS(summary2, 
        file = "Recursos/02_Census_Filters/RecurseBooks/summary2.rds")

# Summarizing statistics for the 'censo4' dataset based on 'greenpoint2' and 'Filtros' columns
summary3 <- censo4 %>% 
  group_by(greenpoint2, Filtros) %>% 
  summarise(total = n(),
            nas = sum(is.na(H01A_TOTAL_PERSONAS)))
saveRDS(summary3,
        file = "Recursos/02_Census_Filters/RecurseBooks/summary3.rds")

# Counting occurrences of 'un_ID' and filtering for duplicates
duplicated_un_ID <- censo4 %>% 
  group_by(un_ID) %>% 
  tally() %>% 
  filter(n > 1)

saveRDS(duplicated_un_ID, 
        file = "Recursos/02_Census_Filters/RecurseBooks/duplicated_un_ID.rds")

##### Extracting and Saving Subset #####
# Selecting columns from 'censo4' that match 'Nombre_Columna' and contain 'GRUPO'
paso <- censo4 %>% select(
  all_of(Nombre_Columna),
  matches("GRUPO")
)

# Saving the 'paso' dataset as an RDS file in the specified directory
saveRDS(paso, 
        file = "Recursos/02_Census_Filters/RecurseBooks/censo_viviendas.rds")




# Standardization and validation of covariates                    

Similarly, just as the census variables underwent a validation process, the covariates dataset is subject to a similar procedure. This involves ensuring uniformity in the length of identifiers such as UGM, Cantos, regions, etc. Additionally, a validation is conducted to identify any missing values (NAs) in the dataset. Following this, a descriptive analysis is performed on the data.


## Environment Preparation and Library Loading

This code is responsible for reviewing and improving the data we have. First, it clears anything we have in memory. Then, it loads some special tools that we are going to use. After that, it reads information about the census and geographic areas.


```r
# Clear the workspace by removing all variables
rm(list = ls())

#################
### Libraries ###
#################
# Load required libraries
library(tidyverse)      # For data manipulation and visualization
library(data.table)     # For efficient data manipulation
library(openxlsx)       # For reading Excel files
library(magrittr)       # For data handling operations

# Clear the console
cat("\f")
```



```r
## Reading census data.
# Load the 'censo_viviendas.rds' file containing census data
censo1 <- readRDS("Recursos/03_Input_Validation_Cov/Data/censo_viviendas.rds") 

## Reading UGMS bases.
# Load the 'ugm_merged.rds' file containing UGMS base data
Base_ugms <- readRDS("Recursos/03_Input_Validation_Cov/Data/ugm_merged.rds") 

# Count distinct UGM_ID values in census data
n_distinct(censo1$UGM_ID)
```

```
## [1] 48060
```

```r
#48060
# Count distinct UGM_ID values in UGMS base data
n_distinct(Base_ugms$UGM_ID) # Not all UGMs have houses
```

```
## [1] 50760
```

```r
#50760
```


## Descriptive values of the UGMS base

We begin by creating a summary dataframe that includes the column names and their corresponding data types. Subsequently, we enhance this summary by adding a column indicating the data type of each column, achieved through the use of the map_chr function on the column names and their corresponding data.


```r
# Create a summary dataframe with column names and their data types
resumen <- data.frame(Nombre_Columna = names(Base_ugms))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x)class(Base_ugms[[x]])))
```

-   Numeric Variables 


```r
# Calculate maximum values for numeric and integer columns
max_values <- Base_ugms %>%
  summarise(across(where(is.numeric) | where(is.integer), max)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Maximo")

# Calculate minimum values for numeric and integer columns
min_values <- Base_ugms %>%
  summarise(across(where(is.numeric) | where(is.integer), min)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Minimo")

# Calculate mean values for numeric and integer columns
media_values <- Base_ugms %>%
  summarise(across(where(is.numeric) | where(is.integer), mean)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Media")

# Calculate median values for numeric and integer columns
mediana_values <- Base_ugms %>%
  summarise(across(where(is.numeric) | where(is.integer), median)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_Mediana")

# Calculate standard deviation values for numeric and integer columns
SD_values <- Base_ugms %>%
  summarise(across(where(is.numeric) | where(is.integer), sd)) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Valor_sd")

# Calculate the number of missing values for numeric and integer columns
nas_values <- Base_ugms %>%
  summarise(across(where(is.numeric) | where(is.integer), function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas")
```

-   Character Variables


```r
# Calculate maximum lengths of characters for character columns
max_char <- Base_ugms %>%
  summarise(across(where(is.character), function(x)max(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_max")

# Calculate minimum lengths of characters for character columns
min_char <- Base_ugms %>%
  summarise(across(where(is.character), function(x)min(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_min")

# Calculate the number of missing values for character columns
nas_values_char <- Base_ugms %>%
  summarise(across(where(is.character) , function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas_char")
```

-   Organizing results in a database.


```r
# Combine all results into a single dataframe
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

resumen2 %>% head(10) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Nombre_Columna </th>
   <th style="text-align:left;"> tipo </th>
   <th style="text-align:right;"> Num_nas_char </th>
   <th style="text-align:right;"> leng_min </th>
   <th style="text-align:right;"> leng_max </th>
   <th style="text-align:right;"> Num_nas </th>
   <th style="text-align:right;"> Valor_sd </th>
   <th style="text-align:right;"> Valor_Mediana </th>
   <th style="text-align:right;"> Valor_Media </th>
   <th style="text-align:right;"> Valor_Minimo </th>
   <th style="text-align:right;"> Valor_Maximo </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> un_id </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 14653.29 </td>
   <td style="text-align:right;"> 25380.5 </td>
   <td style="text-align:right;"> 25380.5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 50760 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PROV_ID </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CANT_ID </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DIST_ID </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UGM_ID </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ugm_viviendas_totales_censo </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ugm_viviendas_ocupadas_censo </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ugm_viviendas_desocupadas_censo </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ugm_peligrosidad </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ugm_problema_de_acceso </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
</tbody>
</table>

-   Variables of interest


```r
Nombre_Columna <- c(
  "un_id",
  "PROV_ID",
  "CANT_ID",
  "DIST_ID",
  "UGM_ID",
  "ugm_viviendas_totales_censo",
  "ugm_viviendas_ocupadas_censo",
  "ugm_viviendas_desocupadas_censo",
  "ugm_peligrosidad",
  "ugm_problema_de_acceso",
  "ugm_riesgos_amenazas",
  "ugm_cobertura_telecomunicaciones",
  "ugm_area_m2",
  "asent",
  "ppp_CRI_v2",
  "elev",
  "indig",
  "aprot",
  "dist_permisos_de_construccion_2011_2022",
  "dist_poblacion_proyeccion_ajustada_2022",
  "dist_poblacion_rup",
  "dist_poblacion_ccss_abril_2023",
  "dist_matricula_educacion_primaria_2021",
  "dist_matricula_educacion_secundaria_2021",
  "dist_codigo_urbanidad",
  "GHS_BUILT_S_E2020_GLOBE_R2023A_5367_CRI",
  "urban_coverfraction",
  "crops_coverfraction",
  "ebais_tt",
  "escu_tt",
  "igl_tt",
  "prov_nl_mean",
  "cant_nl_mean",
  "dist_nl_mean",
  "wpop_sum",
  "ugm_sin_info")
```

-   Changing the type of variables


```r
Tipo_actualizar <- c(
  as.character,
  as.character,
  as.character,
  as.character,
  as.character,
  as.numeric,
  as.numeric,
  as.numeric,
  as.character,
  as.character,
  as.character,
  as.character,
  as.numeric,
  as.character,
  as.numeric,
  as.numeric,
  as.character,
  as.character,
  as.numeric,
  as.numeric,
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
  as.numeric,
  as.character)
```

-   Update column types based on Nombre_Columna and Tipo_actualizar


```r
paso <- map2(Nombre_Columna, Tipo_actualizar, function(nom, tipo) {
  Base_ugms[[nom]] <<- tipo(Base_ugms[[nom]])
  cat(nom, "\n")
})
```

```
## un_id 
## PROV_ID 
## CANT_ID 
## DIST_ID 
## UGM_ID 
## ugm_viviendas_totales_censo 
## ugm_viviendas_ocupadas_censo 
## ugm_viviendas_desocupadas_censo 
## ugm_peligrosidad 
## ugm_problema_de_acceso 
## ugm_riesgos_amenazas 
## ugm_cobertura_telecomunicaciones 
## ugm_area_m2 
## asent 
## ppp_CRI_v2 
## elev 
## indig 
## aprot 
## dist_permisos_de_construccion_2011_2022 
## dist_poblacion_proyeccion_ajustada_2022 
## dist_poblacion_rup 
## dist_poblacion_ccss_abril_2023 
## dist_matricula_educacion_primaria_2021 
## dist_matricula_educacion_secundaria_2021 
## dist_codigo_urbanidad 
## GHS_BUILT_S_E2020_GLOBE_R2023A_5367_CRI 
## urban_coverfraction 
## crops_coverfraction 
## ebais_tt 
## escu_tt 
## igl_tt 
## prov_nl_mean 
## cant_nl_mean 
## dist_nl_mean 
## wpop_sum 
## ugm_sin_info
```

-   Create a summary dataframe with column names and their data types


```r
resumen <- data.frame(Nombre_Columna = names(Base_ugms))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x) class(Base_ugms[[x]])))

# Extract character columns
tipo_char <- resumen$Nombre_Columna[resumen$tipo == "character"]

# Select and display character columns from Base_ugms
Base_ugms[, tipo_char] %>% 
  head(10) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> un_id </th>
   <th style="text-align:left;"> PROV_ID </th>
   <th style="text-align:left;"> CANT_ID </th>
   <th style="text-align:left;"> DIST_ID </th>
   <th style="text-align:left;"> UGM_ID </th>
   <th style="text-align:left;"> ugm_peligrosidad </th>
   <th style="text-align:left;"> ugm_problema_de_acceso </th>
   <th style="text-align:left;"> ugm_riesgos_amenazas </th>
   <th style="text-align:left;"> ugm_cobertura_telecomunicaciones </th>
   <th style="text-align:left;"> asent </th>
   <th style="text-align:left;"> indig </th>
   <th style="text-align:left;"> aprot </th>
   <th style="text-align:left;"> dist_codigo_urbanidad </th>
   <th style="text-align:left;"> ugm_sin_info </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101001 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101002 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101004 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101005 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101006 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101007 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101008 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101009 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101010 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
</tbody>
</table>

### Standardizing Variables and Joining Datasets


```r
# Loop through character variables
for (ii in tipo_char) {
  max_char <- max(nchar(Base_ugms[[ii]]), na.rm = TRUE)
  Base_ugms[[ii]] <- str_pad(string = Base_ugms[[ii]],
                             width = max_char,
                             pad = "0")
}
UGM_censo <- censo1 %>% distinct(UGM_ID)
# Join the UGM_censo and Base_ugms datasets
Base_ugms_censo <- inner_join(UGM_censo, Base_ugms)
Base_ugms_censo[, tipo_char] %>% 
  head(10) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> un_id </th>
   <th style="text-align:left;"> PROV_ID </th>
   <th style="text-align:left;"> CANT_ID </th>
   <th style="text-align:left;"> DIST_ID </th>
   <th style="text-align:left;"> UGM_ID </th>
   <th style="text-align:left;"> ugm_peligrosidad </th>
   <th style="text-align:left;"> ugm_problema_de_acceso </th>
   <th style="text-align:left;"> ugm_riesgos_amenazas </th>
   <th style="text-align:left;"> ugm_cobertura_telecomunicaciones </th>
   <th style="text-align:left;"> asent </th>
   <th style="text-align:left;"> indig </th>
   <th style="text-align:left;"> aprot </th>
   <th style="text-align:left;"> dist_codigo_urbanidad </th>
   <th style="text-align:left;"> ugm_sin_info </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 00001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101001 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00002 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101002 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00003 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00004 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101004 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00006 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101006 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00007 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101007 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00008 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101008 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00009 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101009 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00010 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101010 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 00011 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101011 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
  </tr>
</tbody>
</table>

-   Calculate the counts of missing 


```r
# Calculate the counts of missing values for numeric variables
nas_values <- Base_ugms_censo %>%
  summarise(across(where(is.numeric) | where(is.integer), function(x) sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas")

# Calculate the counts of missing values for character variables
nas_values2 <- Base_ugms_censo %>%
  summarise(across(where(is.character), function(x) sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas")
```

-   Remove specific columns from the dataset


```r
Base_ugms_censo$dist_poblacion_rup <- NULL
Base_ugms_censo$dist_matricula_educacion_secundaria_2021 <- NULL
```

### Standardize numeric variables using z-score scaling


```r
Base_ugms_censo <- Base_ugms_censo %>% 
  mutate_if(is.numeric, function(x) as.numeric(scale(x)))
```

-   Save the standardized dataset  


```r
saveRDS(Base_ugms_censo, "Recursos/03_Input_Validation_Cov/Data/Base_ugms_estandarizada.rds")
```

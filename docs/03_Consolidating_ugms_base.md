


# Estandarización y validación de covariables

De manera similar a cómo las variables del censo se sometieron a un proceso de validación, el conjunto de covariables se somete a un procedimiento similar. Esto implica asegurar la uniformidad en la longitud de los identificadores, como UGM, cantones, regiones, etc. Además, se realiza una validación para identificar valores faltantes (NAs) en el conjunto de datos. A continuación, se realiza un análisis descriptivo de los datos.

## Preparación del entorno y carga de bibliotecas

Este código es responsable de revisar y mejorar los datos que tenemos. Primero, borra cualquier cosa que tengamos en la memoria. Luego, carga algunas herramientas especiales que vamos a utilizar. Después, lee información sobre el censo y las áreas geográficas.



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

## Valores descriptivos de la base de UGMs

Comenzamos creando un marco de datos resumen que incluye los nombres de las columnas y sus tipos de datos correspondientes. Posteriormente, mejoramos este resumen agregando una columna que indica el tipo de dato de cada columna, lo que se logra mediante el uso de la función map_chr en los nombres de las columnas y sus datos correspondientes.


```r
# Create a summary dataframe with column names and their data types
resumen <- data.frame(Nombre_Columna = names(Base_ugms))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x)class(Base_ugms[[x]])))
```

-   Variables numéricas 


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

-   Variables tipo carácter


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

-  Organizando resultados en una base de datos


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

-  Selección de variables de intrés


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

-   Cambiando la naturaleza de las variables 


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

-   Actualizar los tipos de columna basados en `Nombre_Columna` y `Tipo_actualizar`.


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

-   Crear un `data.frame` resumen con los nombres de las columnas y sus tipos de datos.


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

### Estandarización de variables y unión de conjuntos de datos.


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

-   Calcular el conteo de valores faltantes.


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

-   Removiendo columnas adicionales de la base. 


```r
Base_ugms_censo$dist_poblacion_rup <- NULL
Base_ugms_censo$dist_matricula_educacion_secundaria_2021 <- NULL
```

### Estandarizar las variables numéricas utilizando la escala de puntuación z (z-score).


```r
Base_ugms_censo <- Base_ugms_censo %>% 
  mutate_if(is.numeric, function(x) as.numeric(scale(x)))
```

-   Guardando la base estandarizada. 


```r
saveRDS(Base_ugms_censo, "Recursos/03_Input_Validation_Cov/Data/Base_ugms_estandarizada.rds")
```

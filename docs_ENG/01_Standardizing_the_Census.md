


# Standardization and Validation of Available Census Data Variables

In the following code set, a series of processes are carried out for data cleaning and preparation. These steps include removing objects from the workspace, loading necessary libraries, reading census data, assigning missing values based on certain conditions, and calculating descriptive statistics for numeric and character variables. Additionally, adjustments are made to character variables to ensure consistent length. The final results are summarized in a data structure and saved in files for further analysis and reference.


```r
library(tidyverse)
library(data.table)
library(openxlsx)
library(DataExplorer)
library(magrittr)
library(RColorBrewer)
select<- dplyr::select
cat("\f")

censo <- readRDS("Recursos/01_Input_Validation/Data/Data_census_V2023-06-12.rds") %>%
  select(-geometry) %>% as.data.frame()

# Creating a summary data frame for column names and their respective data types.

resumen <- data.frame(Nombre_Columna = names(censo))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x) class(censo[[x]])))
```



```r
resumen <- readRDS("Recursos/01_Input_Validation/RecurseBooks/resumen1.rds")
tba(head(resumen,10))
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> Nombre_Columna </th>
   <th style="text-align:left;"> tipo </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> un_ID </td>
   <td style="text-align:left;"> integer </td>
  </tr>
  <tr>
   <td style="text-align:left;"> PROV_ID </td>
   <td style="text-align:left;"> character </td>
  </tr>
  <tr>
   <td style="text-align:left;"> CANT_ID </td>
   <td style="text-align:left;"> character </td>
  </tr>
  <tr>
   <td style="text-align:left;"> DIST_ID </td>
   <td style="text-align:left;"> character </td>
  </tr>
  <tr>
   <td style="text-align:left;"> UGM_ID </td>
   <td style="text-align:left;"> character </td>
  </tr>
  <tr>
   <td style="text-align:left;"> LLAVEV </td>
   <td style="text-align:left;"> character </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RESUL_ENTREVISTA_VIV </td>
   <td style="text-align:left;"> integer </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TIPO_VIVIENDA_PRECENSO </td>
   <td style="text-align:left;"> character </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V01_TIPO_VIVIENDA </td>
   <td style="text-align:left;"> integer </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V02_OCUPACION_VIVIENDA </td>
   <td style="text-align:left;"> integer </td>
  </tr>
</tbody>
</table>


## Assigning Missing Values to the 'H01A_TOTAL_PERSONAS' Variable

In this section, actions related to the 'H01A_TOTAL_PERSONAS' variable are performed. Missing values (NA) are assigned to this variable based on specific conditions, which include:

- Dwellings that were not visited (category 9).
- Dwellings that rejected the visit or are pending (classification 2).
- Dwellings with other reasons (category 8).

Next, a count of cases before and after the assignment of missing values is conducted. The 'mutate' function is used to create a new temporary column 'H01A_TOTAL_PERSONAS_temp' in which NA values are assigned according to the specified conditions. The information is then grouped by the 'V02_OCUPACION_VIVIENDA' variable, and the number of missing values ('nas') before and after the assignment is calculated, along with the total count of missing values after the assignment.

Subsequently, another assignment of missing values is performed directly to the 'H01A_TOTAL_PERSONAS' variable within the 'censo' dataset. This is done following the same conditions mentioned earlier.


```r
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
```


## Descriptive Values of the Census Data

### Numeric Variables {-}

In this section, various descriptive statistics are calculated for the numeric variables within the 'censo' dataset. These statistics provide insights into the distribution and characteristics of the numeric data.

- 'max_values': The maximum values of numeric and integer variables are calculated using the 'summarise' and 'pivot_longer' functions. The result is a table that lists the maximum values for each variable.

- 'min_values': Similarly, the minimum values of numeric and integer variables are computed and organized into a table format.

- 'media_values': The mean (average) values of numeric and integer variables are calculated and presented in tabular form.

- 'mediana_values': The median values of numeric and integer variables are determined and displayed as a table.

- 'SD_values': Standard deviations (SD) of numeric and integer variables are computed and organized into a table structure.

- 'nas_values': The number of missing values (NAs) for each numeric and integer variable is counted and presented in tabular format.


```r
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
```


### Character Variables {-}

For character variables within the 'censo' dataset, specific descriptive statistics are generated:

- 'max_char': This table contains the maximum lengths of character variables. It calculates the maximum number of characters within each character variable.

- 'min_char': Similar to 'max_char', this table provides the minimum lengths of character variables.

- 'nas_values_char': This table displays the counts of missing values (NAs) for each character variable.


```r
max_char <- censo %>%
  summarise(across(where(is.character), function(x)max(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_max")

min_char <- censo %>%
  summarise(across(where(is.character), function(x)min(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_min")

nas_values_char <- censo %>%
  summarise(across(where(is.character) , function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas_char")
```

## Organizing Results in a Database

In this section, the collected descriptive statistics are organized and combined into a comprehensive summary database named 'resumen2'.

To achieve this, the 'reduce' function is used with a list of tables containing the various statistics. The tables include statistics related to character variables ('nas_values_char', 'min_char', 'max_char'), numeric variables ('nas_values', 'SD_values', 'mediana_values', 'media_values', 'min_values', 'max_values'), and a table that holds information about variable names and types ('resumen').

The 'full_join' function is applied iteratively using 'reduce' to combine all these tables together. The 'by' parameter specifies that the join should be performed based on the 'Nombre_Columna' (Column Name) variable, ensuring that the statistics for each variable are correctly matched and aligned.

The final result is the 'resumen2' database, which provides a consolidated view of descriptive statistics for each variable in the 'censo' dataset, incorporating information about NAs, minimums, maximums, standard deviations, medians, means, and more.



```r
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

# save data   

openxlsx::write.xlsx(resumen2,
                     file = "Recursos/01_Input_Validation/Data/Estado_base.xlsx")
openxlsx::openXL("Recursos/01_Input_Validation/Data/Estado_base.xlsx")
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
   <td style="text-align:left;"> un_ID </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 503230.4746 </td>
   <td style="text-align:right;"> 871621 </td>
   <td style="text-align:right;"> 871621.0000 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1743241 </td>
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
   <td style="text-align:left;"> LLAVEV </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 14 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RESUL_ENTREVISTA_VIV </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.5899 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2.2408 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> TIPO_VIVIENDA_PRECENSO </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 22 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V01_TIPO_VIVIENDA </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 4.6571 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2.9378 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 15 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> V02_OCUPACION_VIVIENDA </td>
   <td style="text-align:left;"> integer </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2.8114 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2.9311 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
</tbody>
</table>

## Updating the Dataset Based on Report Results

In this part of the code, the 'censo' dataset is updated based on the results obtained from the report.

First, the 'Nombre_Columna' vector is defined, which contains the names of the columns to be updated. Next, the 'Tipo_actualizar' vector is defined, which contains the type conversion functions to be applied to each corresponding column.

Using the 'map2' function from the 'purrr' package, each pair of elements in 'Nombre_Columna' and 'Tipo_actualizar' is iterated over, applying the respective type conversion function to each column in the 'censo' dataset. This is achieved using the '<<-' function to update values in the original dataset.

Finally, a new dataset 'censo2' is created that only contains the columns specified in 'Nombre_Columna'. This ensures that the dataset is updated according to the data types and modifications made based on the report results.


```r
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

# Selecting columns specified in 'Nombre_Columna'
censo2 <- censo %>% select(all_of(Nombre_Columna))
```

## Dataset Refinement and Analysis

In this section, the 'censo' dataset is refined further, and some descriptive analyses are conducted on the updated dataset. Additionally, the results of these analyses are saved in the specified directory.



- Summary of variables


```r
censo2 %>% distinct(UGM_ID, wpop_sum) %>% 
  summarise(n = sum(wpop_sum)) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 4653649 </td>
  </tr>
</tbody>
</table>

- Count of ugm_viviendas_totales_censo == 0


```r
censo2 %>%
  distinct(UGM_ID, ugm_viviendas_totales_censo) %>%
  mutate(categoria = cut(ugm_viviendas_totales_censo,
                         breaks = c(-1:5, 10, 20, 50, 
    max(ugm_viviendas_totales_censo)
  ))) %>% group_by(categoria) %>% tally() %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> categoria </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> (-1,0] </td>
   <td style="text-align:right;"> 3744 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (0,1] </td>
   <td style="text-align:right;"> 1760 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (1,2] </td>
   <td style="text-align:right;"> 1441 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (2,3] </td>
   <td style="text-align:right;"> 1446 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (3,4] </td>
   <td style="text-align:right;"> 1412 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (4,5] </td>
   <td style="text-align:right;"> 1527 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (5,10] </td>
   <td style="text-align:right;"> 7317 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (10,20] </td>
   <td style="text-align:right;"> 11660 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (20,50] </td>
   <td style="text-align:right;"> 12516 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> (50,285] </td>
   <td style="text-align:right;"> 5170 </td>
  </tr>
</tbody>
</table>

- Comparing with the number of records per UGM


```r
ugm_cero_viviendas <- censo2 %>%
  distinct(UGM_ID, ugm_viviendas_totales_censo) %>% 
  filter(ugm_viviendas_totales_censo == 0)

cont_registros_ugm <- censo2 %>% group_by(UGM_ID) %>% 
  tally(name = "Total_vivienda_ugm")

inner_join(ugm_cero_viviendas, cont_registros_ugm) %>% 
  summarise(n_ugm = n(),
            min = min(Total_vivienda_ugm), 
            max = max(Total_vivienda_ugm),
            mediana = median(Total_vivienda_ugm)) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> n_ugm </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> mediana </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 3744 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 207 </td>
   <td style="text-align:right;"> 12 </td>
  </tr>
</tbody>
</table>

- Summary of variables for specific condition


```r
censo2 %>% filter(V02_OCUPACION_VIVIENDA == "8") %>% 
  summarise(n_viviendas = n(),
            min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            mediana = median(H01A_TOTAL_PERSONAS)) %>% tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> n_viviendas </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> mediana </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 74871 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>



## Summary and Analysis of Data

In this section, we are summarizing and analyzing the data in order to gain insights. We will calculate different measures to understand the characteristics of the variables.

- Creating a summary of the column names and their data types

```r
resumen <- data.frame(Nombre_Columna = names(censo2))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x)class(censo2[[x]])))
```

- Checking for character variables and ensuring consistent character length


```r
tipo_char <- resumen$Nombre_Columna[resumen$tipo == "character"]

for(ii in tipo_char) {
  max_char <- max(nchar(censo2[[ii]]))
  censo2[[ii]] <-
    str_pad(string = censo2[[ii]],
            width = max_char,
            pad = "0")
}
```

- Summarizing character variables


```r
max_char <- censo2 %>%
  summarise(across(where(is.character), function(x)max(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_max")

min_char <- censo2 %>%
  summarise(across(where(is.character), function(x)min(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_min")

nas_values_char <- censo2 %>%
  summarise(across(where(is.character) , function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas_char")
```

- Summarizing numeric variables


```r
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
```

- Combining all the summary information


```r
resumen2 <- reduce(
  list(nas_values_char, min_char, max_char,
       nas_values, SD_values, mediana_values, media_values, min_values, max_values),
  full_join, by = join_by(Nombre_Columna)) %>% 
  full_join(x = resumen, y = ., by = join_by(Nombre_Columna))
```

- Saving the summary results to an Excel file


```r
openxlsx::write.xlsx(resumen2,
                     file = "Recursos/01_Input_Validation/Data/Estado_base_despues.xlsx")
```

- Saving the standardized dataset


```r
saveRDS(censo2, file = "Recursos/01_Input_Validation/Data/censo_estandarizado.rds")
```

In this code block, we are creating summaries of the dataset variables to understand their characteristics. We are calculating different measures for both character and numeric variables, such as maximum, minimum, mean, median, and standard deviation. Additionally, we are counting missing values for character variables.


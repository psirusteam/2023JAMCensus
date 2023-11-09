

# Estandarización y validación de variables de datos censales disponibles

En el siguiente conjunto de códigos, se llevan a cabo una serie de procesos para la limpieza y preparación de datos. Estos pasos incluyen eliminar objetos del espacio de trabajo, cargar las librerías necesarias, leer datos del censo, asignar valores faltantes según ciertas condiciones y calcular estadísticas descriptivas para variables numéricas y de caracteres. Además, se realizan ajustes a las variables de caracteres para garantizar una longitud constante. Los resultados finales se resumen en una estructura de datos y se guardan en archivos para su posterior análisis y referencia.


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


## Asignación de valores faltantes a la variable `H01A_TOTAL_PERSONAS`

En esta sección se realizan acciones relacionadas con la variable `H01A_TOTAL_PERSONAS`. Los valores faltantes (NA) se asignan a esta variable según condiciones específicas, que incluyen:

- Viviendas que no fueron visitadas (categoría 9).
- Viviendas que rechazaron la visita o se encuentran pendientes (clasificación 2).
- Viviendas con otros motivos (categoría 8).

A continuación se realiza un recuento de casos antes y después de la asignación de valores faltantes. La función `mutar` se utiliza para crear una nueva columna temporal `H01A_TOTAL_PERSONAS_temp` en la que se asignan valores NA de acuerdo con las condiciones especificadas. Luego, la información se agrupa por la variable `V02_OCUPACION_VIVIENDA` y se calcula el número de valores faltantes (`NAs`) antes y después de la asignación, junto con el recuento total de valores faltantes después de la asignación.

Posteriormente, se realiza otra asignación de valores faltantes directamente a la variable `H01A_TOTAL_PERSONAS` dentro del conjunto de datos `censo`. Esto se hace siguiendo las mismas condiciones mencionadas anteriormente.


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


## Valores Descriptivos de los Datos Censos

### Variables numéricas {-}

En esta sección, se calculan varias estadísticas descriptivas para las variables numéricas dentro del conjunto de datos del `censo`. Estas estadísticas proporcionan información sobre la distribución y las características de los datos numéricos.

- `max_values`: Los valores máximos de variables numéricas y enteras se calculan utilizando las funciones `summarise` y `pivot_longer`. El resultado es una tabla que enumera los valores máximos para cada variable.

- `min_values`: De manera similar, los valores mínimos de variables numéricas y enteras se calculan y organizan en un formato de tabla.

- `media_values`: los valores medios (promedio) de variables numéricas y enteras se calculan y presentan en forma de tabla.

- `mediana_values`: Los valores medianos de variables numéricas y enteras se determinan y muestran como una tabla.

- `SD_values`: las desviaciones estándar (SD) de variables numéricas y enteras se calculan y organizan en una estructura de tabla.

- `nas_values`: el número de valores faltantes (NA) para cada variable numérica y entera se cuenta y se presenta en formato tabular.



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


### Variables de caracteres {-}

Para las variables de caracteres dentro del conjunto de datos del `censo`, se generan estadísticas descriptivas específicas:

- `max_char`: esta tabla contiene las longitudes máximas de las variables de caracteres. Calcula el número máximo de caracteres dentro de cada variable de carácter.

- `min_char`: similar a `max_char`, esta tabla proporciona las longitudes mínimas de las variables de caracteres.

- `nas_values_char`: esta tabla muestra los recuentos de valores faltantes (NA) para cada variable de carácter.


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
## Organización de resultados en una base de datos

En esta sección, las estadísticas descriptivas recopiladas se organizan y combinan en una base de datos resumida integral denominada `resumen2`.

Para lograr esto, se utiliza la función `reducir` con una lista de tablas que contienen las distintas estadísticas. Las tablas incluyen estadísticas relacionadas con variables de caracteres (`nas_values_char`, `min_char`, `max_char`), variables numéricas (`nas_values`, `SD_values`, `mediana_values`, `media_values`, `min_values`, `max_values`), y una tabla que contiene información sobre nombres y tipos de variables ("resumen").

La función `full_join` se aplica de forma iterativa utilizando `reduce` para combinar todas estas tablas. El parámetro `by` especifica que la unión debe realizarse en función de la variable `Nombre_Columna`, asegurando que las estadísticas de cada variable coincidan y alineen correctamente.

El resultado final es la base de datos `resumen2`, que proporciona una vista consolidada de estadísticas descriptivas para cada variable en el conjunto de datos del `censo`, incorporando información sobre NA, mínimos, máximos, desviaciones estándar, medianas, medias y más.



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

## Actualización del conjunto de datos basada en los resultados del informe

En esta parte del código, el conjunto de datos `censo` se actualiza en función de los resultados obtenidos del informe.

Primero, se define el vector `Nombre_Columna`, que contiene los nombres de las columnas que se van a actualizar. A continuación, se define el vector `Tipo_actualizar`, que contiene las funciones de conversión de tipo que se aplicarán a cada columna correspondiente.

Utilizando la función `map2` del paquete `purrr`, se itera sobre cada par de elementos en `Nombre_Columna` y `Tipo_actualizar`, aplicando la respectiva función de conversión de tipo a cada columna en el conjunto de datos `censo`. Esto se logra utilizando la función `<<-` para actualizar los valores en el conjunto de datos original.

Finalmente, se crea un nuevo conjunto de datos `censo2` que solo contiene las columnas especificadas en `Nombre_Columna`. Esto garantiza que el conjunto de datos se actualice de acuerdo con los tipos de datos y las modificaciones realizadas en función de los resultados del informe.



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

# Selecting columns specified in `Nombre_Columna`
censo2 <- censo %>% select(all_of(Nombre_Columna))
```

## Refinamiento y Análisis del Conjunto de Datos

En esta sección, el conjunto de datos `censo` se refina aún más, y se realizan algunos análisis descriptivos en el conjunto de datos actualizado. Además, los resultados de estos análisis se guardan en el directorio especificado.




- Resumen de variables


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

- Conteo de `ugm_viviendas_totales_censo` == 0


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

- Comparación con el número de registros por UGM


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

- Resumen de variables para una condición específica


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

## Resumen y Análisis de Datos

En esta sección, estamos resumiendo y analizando los datos para obtener información clave. Calcularemos diferentes medidas para comprender las características de las variables.

- Creación de un resumen de los nombres de las columnas y sus tipos de datos

```r
resumen <- data.frame(Nombre_Columna = names(censo2))
resumen %<>% mutate(tipo = map_chr(Nombre_Columna, function(x)class(censo2[[x]])))
```

- Comprobación de variables de tipo carácter y aseguramiento de una longitud de carácter consistente


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

- Resumen de variables de tipo carácter


```r
max_char <- censo %>%
  summarise(across(where(is.character), function(x)
    max(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_max")

min_char <- censo %>%
  summarise(across(where(is.character), function(x)
    min(nchar(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "leng_min")

nas_values_char <- censo %>%
  summarise(across(where(is.character) , function(x)
    sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas_char")
```

- Resumen de variables numéricas


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

nas_values <- censo %>%
  summarise(across(where(is.numeric) | where(is.integer), function(x)sum(is.na(x)))) %>%
  pivot_longer(everything(), names_to = "Nombre_Columna", values_to = "Num_nas")
```

- Combinación de toda la información de resumen


```r
resumen2 <- reduce(
  list(nas_values_char, min_char, max_char,
       nas_values, SD_values, mediana_values, media_values, min_values, max_values),
  full_join, by = join_by(Nombre_Columna)) %>% 
  full_join(x = resumen, y = ., by = join_by(Nombre_Columna))
```

- Guardando los resultados del resumen en un archivo de Excel


```r
openxlsx::write.xlsx(resumen2,
                     file = "Recursos/01_Input_Validation/Data/Estado_base_despues.xlsx")
```

- Guardando el dataset estandarizado


```r
saveRDS(censo2, file = "Recursos/01_Input_Validation/Data/censo_estandarizado.rds")
```

En este bloque de código, estamos creando resúmenes de las variables del conjunto de datos para comprender sus características. Calculamos diferentes medidas tanto para variables de tipo carácter como numéricas, como el máximo, mínimo, media, mediana y desviación estándar. Además, contamos los valores faltantes para variables de tipo carácter.




# Filtrado y Refinamiento de Datos del Censo

En el proceso de mejorar y depurar una base de datos del censo, es imperativo establecer reglas coherentes y replicables. En este contexto, el procedimiento de "Filtrado y Refinamiento de Datos del Censo" se vuelve esencial para mejorar la calidad de los datos y eliminar información irrelevante. A lo largo de los siguientes bloques de código, exploraremos cómo se aplicaron diversos filtros y técnicas de refinamiento a los datos del censo. Estos códigos nos guiarán a través de un proceso crucial para asegurar que los datos sean confiables y adecuados para un análisis posterior. Colectivamente, estos pasos nos permitirán obtener información más precisa y valiosa a partir de los datos del censo.

## Lectura de Bibliotecas, Bases de Datos y Otros Insumos

En esta sección, comenzamos cargando las bibliotecas necesarias utilizadas en todo el procesamiento. Además, definimos las columnas que se conservarán después de aplicar los filtros. También realizamos ajustes necesarios en las bases de datos, considerando actualizaciones en los códigos de las Unidades Geográficas Mínimas (UGM).



```r
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
  "Filtros"
) 
```

### Lectura de Datos de Vivienda sin Coordenadas. {-}

En esta sección, leemos los datos de vivienda desde un archivo CSV que no incluye coordenadas. Luego transformamos los datos al formato requerido, incluyendo variables como el ID de provincia, el ID de cantón y el ID de distrito basados en los códigos proporcionados. El conjunto de datos resultante se utilizará para análisis y procesamiento adicionales.



```r
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
```

### Cambio de Códigos UGM en los Datos de Vivienda. {-}

En esta sección, modificamos los códigos UGM en los datos de vivienda para garantizar la consistencia y precisión. Algunos códigos UGM se actualizan de acuerdo con asignaciones predefinidas. Este paso es crucial para mantener la uniformidad de los datos para análisis posteriores.



```r
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
```

### Leer los Datos del Censo Estandarizados. {-}

En esta sección, leemos los datos del censo estandarizados desde un archivo RDS (R Data Serialization) almacenado. Al igual que en el paso anterior, ajustamos los códigos UGM para mantener la consistencia de los datos. Los datos del censo estandarizados servirán como base para los procesos posteriores de filtrado y refinamiento.


```r
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
```

### Agregando la Base de Edad y Sexo. {-}

En esta sección, incorporamos la base de edad y sexo al análisis. La base de edad y sexo se lee desde un archivo RDS almacenado. Al igual que en los pasos anteriores, garantizamos la consistencia ajustando los códigos UGM. La base de edad y sexo proporciona información demográfica valiosa y se utilizará en los procedimientos posteriores de filtrado y refinamiento.



```r
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
```


### Inner Join  para Agregar la Base de Edad y Sexo

En esta sección, se realiza una operación de unión interna para integrar la base de edad y sexo con los datos censales. La diferencia en el recuento de filas entre las dos bases corresponde a los hogares censados en papel que se incluyen más adelante en el proceso. Se compara el número de filas en la base de edad y sexo con los datos del censo y los datos de viviendas sin coordenadas para verificar la coincidencia.



```r
## The difference between the bases corresponds to paper-censused households
## that are included later
nrow(censo_sexo_edad) - nrow(censo1) 
nrow(Viviendas_sin_coordenadas)

## Inner join to add the age-sex base

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
```

El código comienza calculando la diferencia en el recuento de filas entre la base de edad y sexo (`censo_sexo_edad`) y los datos censales existentes (`censo1`). Esta diferencia representa el número de hogares censados en papel que aún no se han incluido en los datos del censo. Además, se determina el número de filas en los datos de viviendas sin coordenadas (`Viviendas_sin_coordenadas`) como referencia.

A continuación, se aplica la operación `inner_join` para combinar la base de edad y sexo con los datos censales. La función `join_by` especifica las columnas utilizadas para la operación de unión, asegurando una integración completa de los datos de ambas fuentes. Este proceso mejora el conjunto de datos al incorporar información demográfica importante para un análisis posterior.

## Aplicación de filtros y análisis de datos

En esta sección, vamos a guiarlo a través del proceso de aplicar varios filtros y realizar análisis de datos en los datos censales refinados.

### Aplicación del primer filtro: categorización de hogares con residentes y determinación del estado de greenpoint

En este bloque de código, introducimos el primer filtro al categorizar los hogares como con residentes ('si') o vacíos ('no') basándonos en el número total de residentes en cada hogar. Además, determinamos el estado de greenpoint de cada hogar, considerando si el valor de greenpoint es '0' y el valor de 'personas' es 'si'. La columna 'greenpoint' se actualiza en consecuencia.


```r
# Create 'personas' column to categorize households with or without residents
censo1 %<>% 
  mutate(personas = if_else(H01A_TOTAL_PERSONAS > 0, "si", "no")) 

# Assign greenpoint status based on conditions

censo1 %<>% mutate(greenpoint = if_else(greenpoint == "0" & personas == "si", "1", greenpoint))  
```

*greenpoint*: La vivienda se reporta como censada en el mapa de puntos.

- Analizando la distribución de greenpoint

    En este código se calcula la distribución del estado de greenpoint entre los hogares. Agrupa los datos según el estado de 'greenpoint', cuenta el número de hogares en cada categoría y calcula la distribución porcentual. Los resultados ofrecen información sobre la prevalencia del estado de greenpoint entre los datos censales.



```r
greenpoint_distribution <- censo1 %>%
  group_by(greenpoint) %>% tally() %>%  # Counting households with and without greenpoint
  mutate(percentage = 100 * n / sum(n))  # Calculating the percentage of each category
greenpoint_distribution
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 471456 </td>
   <td style="text-align:right;"> 27.0448 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 1053477 </td>
   <td style="text-align:right;"> 60.4321 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 12.5231 </td>
  </tr>
</tbody>
</table>

- Resumiendo las características de los hogares por estado de greenpoint

    Este bloque de código resume las características de los hogares según su estado de greenpoint. Calcula los números mínimos y máximos de residentes en los hogares, cuenta los valores faltantes para el número total de residentes y proporciona el recuento total de hogares para cada categoría de greenpoint.



```r
household_summary <- censo1 %>% 
  group_by(greenpoint) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS),  # Minimum number of residents in households
            max = max(H01A_TOTAL_PERSONAS),  # Maximum number of residents in households
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),  # Number of missing values for the total number of residents
            total = n())  # Total number of households
household_summary
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> num_na </th>
   <th style="text-align:right;"> total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 471456 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 261 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1053477 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 218308 </td>
  </tr>
</tbody>
</table>

- Creación de una tabla de contingencia para la ocupación y el estado de greenpoint

En esta sección, se genera una tabla de contingencia para explorar la relación entre la ocupación y el estado de greenpoint. La tabla cruza la columna 'V02_OCUPACION_VIVIENDA' (ocupación) con la columna 'greenpoint', teniendo en cuenta los valores faltantes. Esto proporciona una representación visual de cómo se distribuyen estas dos variables entre los hogares.



```r
occupancy_greenpoint_table <- table(censo1$V02_OCUPACION_VIVIENDA, censo1$greenpoint, useNA = "a")
occupancy_greenpoint_table
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;">   </th>
   <th style="text-align:right;"> 0 </th>
   <th style="text-align:right;"> 1 </th>
   <th style="text-align:right;"> NA </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 772625 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 471456 </td>
   <td style="text-align:right;"> 3853 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 92465 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 31187 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 52463 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 23845 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2168 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 74871 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 9 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 218308 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
</tbody>
</table>
- Aplicación del segundo filtro y refinamiento

Continuando con el proceso de refinamiento, se aplica el siguiente filtro para categorizar aún más los hogares en función de criterios adicionales.


```r
censo2 <- censo1 %>% mutate(
  greenpoint2 = case_when(
    H01A_TOTAL_PERSONAS > 0 ~ "Censado con informacion n>0",
    RESUL_ENTREVISTA_VIV %in% c(1) &
      H01A_TOTAL_PERSONAS == 0 ~ "Censado con informacion n=0",
    RESUL_ENTREVISTA_VIV %in% c(3, 4) ~ "Sin informacion pero  n>0",
    is.na(greenpoint) & is.na(personas) ~ "Sin informacion pero n>=0",
    V02_OCUPACION_VIVIENDA == "8" ~ "Sin informacion pero n>=0",
    TRUE ~ "Resto"
  )
)
```

Introducimos el estado "greenpoint2" para categorizar aún más los hogares en función de diversas condiciones. Esto se basa en una combinación de factores como el número de residentes, los resultados de las entrevistas y la ocupación de la vivienda. Cada hogar se asigna a una categoría específica, como "Censado con información n>0", "Censado con información n=0" y "Sin información pero n>0", entre otros. Esto proporciona una forma más detallada de describir el estado de los hogares en función de diferentes criterios.



```r
readRDS("Recursos/02_Census_Filters/RecurseBooks/census2.rds") %>% 
  head(10) %>% tba()
```

### Aplicación del segundo filtro: Criterio WorldPop

A partir del paso 1, también incluimos todos los hogares con la variable WorldPop (WP) que se encuentran dentro de 1 desviación estándar del valor promedio. Sin embargo, si estos hogares tienen cero residentes en la variable de interés, marcamos esa variable como "No Disponible" (NA).

- Calcular estadísticas resumen para la variable 'wpop_sum'

En primer lugar, calculamos estadísticas resumen para la variable 'wpop_sum', que es una covariable relacionada con WorldPop. Las estadísticas resumen incluyen el promedio, la desviación estándar, el valor mínimo y máximo de 'wpop_sum'. Estas estadísticas nos ayudan a establecer los umbrales para el filtro y se guardan en un archivo resumen.



```r
wpop_summary <- censo2 %>% distinct(UGM_ID,wpop_sum) %>%
  summarise(media = mean(wpop_sum),  # Mean value of 'wpop_sum'
            sd = sd(wpop_sum),  # Standard deviation of 'wpop_sum'
            min = min(wpop_sum),  # Minimum value of 'wpop_sum'
            max = max(wpop_sum))  # Maximum value of 'wpop_sum'
wpop_summary
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> media </th>
   <th style="text-align:right;"> sd </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 96.9652 </td>
   <td style="text-align:right;"> 143.1986 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 6214.269 </td>
  </tr>
</tbody>
</table>
- Cálculo de umbrales inferior y superior

Utilizamos las estadísticas resumen para calcular los umbrales inferior y superior basados en una desviación estándar del promedio. Estos umbrales nos ayudarán a identificar los hogares que cumplen con los criterios del segundo filtro.


```r
# Calculate the lower and upper thresholds based on one standard deviation from the mean
li <- 96.96515 - 143.1986 * 1  # Lower threshold
ls <- 96.96515 + 143.1986 * 1  # Upper threshold
```

Identificamos y contamos los hogares que cumplen con los criterios del segundo filtro. Nos centramos en los hogares con cero residentes ('H01A_TOTAL_PERSONAS') pero que tienen valores de 'wpop_sum' fuera del umbral calculado. Realizamos esta cuenta y la agrupamos según la variable 'V02_OCUPACION_VIVIENDA'.


```r
# Identify and count households that meet the criteria for the second filter
filter_2_counts <- censo2 %>% filter(H01A_TOTAL_PERSONAS == 0,
                                     wpop_sum > ls | wpop_sum < li) %>%
  group_by(V02_OCUPACION_VIVIENDA) %>% summarise(n = n())

filter_2_counts
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> V02_OCUPACION_VIVIENDA </th>
   <th style="text-align:right;"> n </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 129652 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 22968 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 8210 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 10514 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 4635 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 532 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 8 </td>
   <td style="text-align:right;"> 17160 </td>
  </tr>
</tbody>
</table>

- Aplicación del Segundo Filtro y Actualizaciones de Columnas

Aplicamos el segundo filtro a los hogares y actualizamos las columnas 'greenpoint2' y 'Filtros' en consecuencia. La columna 'greenpoint2' se actualiza para reflejar la nueva clasificación basada en el Criterio WorldPop, mientras que la columna 'Filtros' indica la aplicación del Criterio WorldPop o se establece como NA según corresponda.



```r
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
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> un_ID </th>
   <th style="text-align:left;"> PROV_ID </th>
   <th style="text-align:left;"> CANT_ID </th>
   <th style="text-align:left;"> DIST_ID </th>
   <th style="text-align:left;"> UGM_ID </th>
   <th style="text-align:left;"> LLAVEV </th>
   <th style="text-align:left;"> RESUL_ENTREVISTA_VIV </th>
   <th style="text-align:left;"> TIPO_VIVIENDA_PRECENSO </th>
   <th style="text-align:left;"> V01_TIPO_VIVIENDA </th>
   <th style="text-align:left;"> V02_OCUPACION_VIVIENDA </th>
   <th style="text-align:right;"> H01A_TOTAL_PERSONAS </th>
   <th style="text-align:left;"> greenpoint </th>
   <th style="text-align:right;"> ugm_viviendas_totales_censo </th>
   <th style="text-align:right;"> ugm_viviendas_ocupadas_censo </th>
   <th style="text-align:right;"> ugm_viviendas_desocupadas_censo </th>
   <th style="text-align:left;"> ugm_peligrosidad </th>
   <th style="text-align:left;"> ugm_problema_de_acceso </th>
   <th style="text-align:left;"> ugm_riesgos_amenazas </th>
   <th style="text-align:left;"> ugm_cobertura_telecomunicaciones </th>
   <th style="text-align:left;"> asent </th>
   <th style="text-align:right;"> ppp_CRI_v2 </th>
   <th style="text-align:right;"> elev </th>
   <th style="text-align:left;"> indig </th>
   <th style="text-align:left;"> aprot </th>
   <th style="text-align:right;"> dist_permisos_de_construccion_2011_2022 </th>
   <th style="text-align:right;"> dist_poblacion_proyeccion_ajustada_2022 </th>
   <th style="text-align:right;"> dist_poblacion_ccss_abril_2023 </th>
   <th style="text-align:right;"> dist_matricula_educacion_primaria_2021 </th>
   <th style="text-align:left;"> dist_codigo_urbanidad </th>
   <th style="text-align:right;"> GHS_BUILT_S_E2020_GLOBE_R2023A_5367_CRI </th>
   <th style="text-align:right;"> urban_coverfraction </th>
   <th style="text-align:right;"> crops_coverfraction </th>
   <th style="text-align:right;"> ebais_tt </th>
   <th style="text-align:right;"> escu_tt </th>
   <th style="text-align:right;"> igl_tt </th>
   <th style="text-align:right;"> prov_nl_mean </th>
   <th style="text-align:right;"> cant_nl_mean </th>
   <th style="text-align:right;"> dist_nl_mean </th>
   <th style="text-align:right;"> wpop_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO1_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO2_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO3_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO4_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO5_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO6_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO7_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO8_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO9_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO10_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO11_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO12_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO13_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO14_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO15_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO16_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO17_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO18_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO19_sum </th>
   <th style="text-align:right;"> HOMBRES_GRUPO20_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO1_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO2_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO3_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO4_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO5_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO6_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO7_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO8_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO9_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO10_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO11_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO12_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO13_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO14_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO15_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO16_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO17_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO18_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO19_sum </th>
   <th style="text-align:right;"> MUJERES_GRUPO20_sum </th>
   <th style="text-align:left;"> personas </th>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:left;"> Filtros </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 0218491 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101001 </td>
   <td style="text-align:left;"> 10101001001001 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> VIVIENDA EN APARTAMENT </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 17.4872 </td>
   <td style="text-align:right;"> 1157.488 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 3773 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.0027 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.9933 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 34.5930 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0141265 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101002 </td>
   <td style="text-align:left;"> 10101001002001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> VIVIENDA EN APARTAMENT </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 17.5313 </td>
   <td style="text-align:right;"> 1156.738 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 4907 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.0000 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 9.4928 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> si </td>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0151378 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101002 </td>
   <td style="text-align:left;"> 10101001002002 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> VIVIENDA INDEPENDIENTE </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 17.5313 </td>
   <td style="text-align:right;"> 1158.038 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 4907 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.9932 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 9.4928 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> si </td>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0218056 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:left;"> 10101001003006 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> 00VIVIENDA EN EDIFICIO </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 15.9573 </td>
   <td style="text-align:right;"> 1152.401 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 4794 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.9932 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0000 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 13.1767 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0217145 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:left;"> 10101001003005 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> VIVIENDA INDEPENDIENTE </td>
   <td style="text-align:left;"> 09 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 15.9573 </td>
   <td style="text-align:right;"> 1156.873 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 4794 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 45.0000 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.9960 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 13.1767 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0216224 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:left;"> 10101001003003 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> VIVIENDA INDEPENDIENTE </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 15.8496 </td>
   <td style="text-align:right;"> 1157.138 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 3604 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.0027 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.9933 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 13.1767 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0215255 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:left;"> 10101001003001 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> VIVIENDA EN APARTAMENT </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 15.8496 </td>
   <td style="text-align:right;"> 1157.801 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 4794 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.0027 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1.0000 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 13.1767 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0215972 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:left;"> 10101001003002 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> VIVIENDA INDEPENDIENTE </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 15.8496 </td>
   <td style="text-align:right;"> 1157.138 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 3604 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.0027 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.9933 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 13.1767 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0216229 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:left;"> 10101001003004 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> VIVIENDA INDEPENDIENTE </td>
   <td style="text-align:left;"> 01 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 15.8496 </td>
   <td style="text-align:right;"> 1157.138 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 3604 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.0027 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.9933 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 13.1767 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 0925743 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:left;"> 10101004 </td>
   <td style="text-align:left;"> 10101001004002 </td>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:left;"> VIVIENDA INDEPENDIENTE </td>
   <td style="text-align:left;"> 04 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 17.4872 </td>
   <td style="text-align:right;"> 1158.147 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> 413 </td>
   <td style="text-align:right;"> 3233 </td>
   <td style="text-align:right;"> 14597 </td>
   <td style="text-align:right;"> 1374 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 3773 </td>
   <td style="text-align:right;"> 100 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 44.0000 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0.0027 </td>
   <td style="text-align:right;"> 10.802 </td>
   <td style="text-align:right;"> 62.9502 </td>
   <td style="text-align:right;"> 63 </td>
   <td style="text-align:right;"> 13.6008 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:left;"> no </td>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

- Resumen de datos basado en 'greenpoint2'

Resumimos los datos en función de la variable 'greenpoint2' actualizada. Calculamos la distribución y los porcentajes de hogares en cada categoría de 'punto verde2'. Estos resúmenes nos ayudan a comprender el impacto del filtro en la clasificación de los hogares.



```r
# Summarizing the data based on the 'greenpoint2' variable
summary_greenpoint2 <- censo3 %>%
  group_by(greenpoint2) %>%
  tally() %>%
  mutate(percentage = 100 * n / sum(n))
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:right;"> 212980 </td>
   <td style="text-align:right;"> 12.2175 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:right;"> 776478 </td>
   <td style="text-align:right;"> 44.5422 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:right;"> 341804 </td>
   <td style="text-align:right;"> 19.6074 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:right;"> 411979 </td>
   <td style="text-align:right;"> 23.6329 </td>
  </tr>
</tbody>
</table>

- Resumen de Datos Basado en 'greenpoint2' y 'Filtros'

Generamos un resumen adicional que considera la combinación de las variables 'greenpoint2' y 'Filtros'. Esto proporciona información más detallada sobre cómo el Criterio WorldPop afecta las categorías existentes.


```r
# Summarizing the data based on the combination of 'greenpoint2' and 'Filtros' variables
summary_greenpoint2_filtros <- censo3 %>%
  group_by(greenpoint2, Filtros) %>%
  tally() %>%
  mutate(percentage = 100 * n / sum(n))

summary_greenpoint2_filtros
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:left;"> Filtros </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 212980 </td>
   <td style="text-align:right;"> 100.0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 776478 </td>
   <td style="text-align:right;"> 100.0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 341804 </td>
   <td style="text-align:right;"> 100.0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Criterio WorldPop </td>
   <td style="text-align:right;"> 193671 </td>
   <td style="text-align:right;"> 47.0099 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 52.9901 </td>
  </tr>
</tbody>
</table>

### Resumen de estadísticas basado en 'greenpoint2'

Calculamos estadísticas adicionales para las categorías 'greenpoint2'. Estas estadísticas incluyen el número mínimo, máximo, de valores faltantes y el número total de hogares en cada categoría. Estos datos son esenciales para comprender la distribución de los residentes en los hogares filtrados.

Cada una de estas etapas contribuye al proceso de aplicar el segundo filtro y refinar los datos del censo con base en el Criterio WorldPop. Los resúmenes y datos generados son útiles para análisis posteriores y se guardan para referencia futura.



```r
# Summarizing the data for 'greenpoint2' variable
summary_greenpoint2_stats <- censo3 %>%
  group_by(greenpoint2) %>%
  summarise(min = min(H01A_TOTAL_PERSONAS),
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())
summary_greenpoint2_stats
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> num_na </th>
   <th style="text-align:right;"> total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 212980 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 261 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 776478 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 341804 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 411979 </td>
  </tr>
</tbody>
</table>

### Definición del tercer filtro

En esta sección, presentamos la implementación del tercer filtro, basándose en los cimientos establecidos por los Filtros 1 y 2. El tercer filtro se dirige a hogares dentro de las UGM que fueron encuestados después de un intervalo mayor a 20 días y, a pesar de estar clasificados como desocupados, allí existe una falta de certeza sobre su estado de ocupación. Estos hogares se reclasifican como de estatus desconocido.

- Lectura del Archivo 'Desocupadas fuera periodo.xlsx'

Comenzamos leyendo el archivo 'Desocupadas fuera periodo.xlsx' para recopilar información sobre hogares que estaban desocupados pero visitados fuera del intervalo estándar. Extraemos específicamente la columna 'UGM_ID' para un análisis más detallado.



```r
# Reading the 'Desocupadas fuera periodo.xlsx' file and selecting the UGM_ID column

upms_reporte <- openxlsx::read.xlsx(
  xlsxFile = "Recursos/02_Census_Filters/Data/Desocupadas fuera periodo.xlsx") %>%
  select(UGM_ID = ID_UGM)
```

- Aplicación de filtros basados en 'upms_reporte' y condiciones específicas

Utilizando la información recopilada de 'upms_reporte' y considerando ciertas condiciones, aplicamos filtros adicionales a los datos existentes. Actualizamos las columnas 'greenpoint2' y 'Filtros' según los criterios especificados.



```r
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
```

- Aplicar filtros adicionales y crear valores de 'Filtros'

Procedemos a refinar aún más los datos aplicando filtros adicionales. Los valores de 'Filtros' se actualizan en función de diversas condiciones como el número de residentes, el resultado de la entrevista ('RESUL_ENTREVISTA_VIV') y la ocupación de la vivienda ('V02_OCUPACION_VIVIENDA').



```r
# Applying additional filters and creating 'Filtros' values
censo4 %<>% mutate(Filtros = case_when(
  is.na(Filtros) & H01A_TOTAL_PERSONAS > 0 ~ "Número de personas mayor a 0",
  is.na(Filtros) & RESUL_ENTREVISTA_VIV %in% c(1) & H01A_TOTAL_PERSONAS == 0 ~ "Entrevista igual a 1 y Número de personas igual a 0",
  is.na(Filtros) & RESUL_ENTREVISTA_VIV %in% c(3,4) ~ "Entrevista  es 3 o 4", 
  is.na(Filtros) & is.na(greenpoint) & is.na(personas) ~ "Sin conteo de personas",
  is.na(Filtros) & V02_OCUPACION_VIVIENDA == "8" ~ "Ocupación de la vivienda es 8", 
  TRUE ~ Filtros
))
```

- Resumir datos basados en las variables 'greenpoint2' y 'Filtros'

Generamos un resumen de los datos en base a las variables 'greenpoint2' y 'Filtros' actualizadas. El resumen proporciona información sobre la distribución de hogares en diferentes categorías.


```r
# Summarizing data based on 'greenpoint2' and 'Filtros' variables
summary_greenpoint2_filtros <- censo4 %>% 
  group_by(greenpoint2, Filtros) %>% tally() %>% 
  ungroup() %>% 
  mutate(percentage = 100 * n / sum(n))
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:left;"> Filtros </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> Entrevista igual a 1 y Número de personas igual a 0 </td>
   <td style="text-align:right;"> 175921 </td>
   <td style="text-align:right;"> 10.0916 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:left;"> Número de personas mayor a 0 </td>
   <td style="text-align:right;"> 776478 </td>
   <td style="text-align:right;"> 44.5422 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> Entrevista  es 3 o 4 </td>
   <td style="text-align:right;"> 285810 </td>
   <td style="text-align:right;"> 16.3953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Criterio WorldPop </td>
   <td style="text-align:right;"> 135370 </td>
   <td style="text-align:right;"> 7.7654 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Fuera de periodo(20 días) </td>
   <td style="text-align:right;"> 151354 </td>
   <td style="text-align:right;"> 8.6823 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Sin conteo de personas </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 12.5231 </td>
  </tr>
</tbody>
</table>

- Resumir datos basados en la variable 'greenpoint2'

De manera similar, creamos otro resumen de los datos, esta vez centrándonos únicamente en la variable 'puntoverde2'. Este resumen nos ayuda a comprender el impacto del tercer filtro en la clasificación de los hogares.


```r
# Summarizing data based on 'greenpoint2' variable
summary_greenpoint2 <- censo4 %>% 
  group_by(greenpoint2) %>% tally() %>% 
  mutate(percentage = 100 * n / sum(n))
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:right;"> n </th>
   <th style="text-align:right;"> percentage </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:right;"> 175921 </td>
   <td style="text-align:right;"> 10.0916 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:right;"> 776478 </td>
   <td style="text-align:right;"> 44.5422 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:right;"> 285810 </td>
   <td style="text-align:right;"> 16.3953 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:right;"> 505032 </td>
   <td style="text-align:right;"> 28.9709 </td>
  </tr>
</tbody>
</table>

### Combinando casas no coordinadas

Selección de casas no coordinadas del conjunto de datos 'censo_sexo_edad' y unión interna con 'Viviendas_sin_coordenadas'



```r
# Filtering non-coordinated houses from 'censo_sexo_edad' dataset
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
```

### Agregación de estadísticas y resúmenes


```r
# Summarizing statistics for the 'censo4' dataset based on 'greenpoint2' column
summary1 <- censo4 %>% 
  group_by(greenpoint2) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> num_na </th>
   <th style="text-align:right;"> total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 175921 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 261 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 776478 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Papel n=0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1334 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Papel n&gt;0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3500 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 23344 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 285810 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 505032 </td>
  </tr>
</tbody>
</table>

- Resumir estadísticas para el conjunto de datos 'censo4' basado en las columnas 'greenpoint2' y 'Filtros'


```r
summary2 <- censo4 %>% 
  group_by(greenpoint2, Filtros) %>% 
  summarise(min = min(H01A_TOTAL_PERSONAS), 
            max = max(H01A_TOTAL_PERSONAS),
            num_na = sum(is.na(H01A_TOTAL_PERSONAS)),
            total = n())
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:left;"> Filtros </th>
   <th style="text-align:right;"> min </th>
   <th style="text-align:right;"> max </th>
   <th style="text-align:right;"> num_na </th>
   <th style="text-align:right;"> total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 175921 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 261 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 776478 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Papel n=0 </td>
   <td style="text-align:left;"> Censado en papel </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1334 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Papel n&gt;0 </td>
   <td style="text-align:left;"> Censado en papel </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 3500 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 23344 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 285810 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 505032 </td>
  </tr>
</tbody>
</table>

- Resumir estadísticas para el conjunto de datos 'censo4' basado en las columnas 'greenpoint2' y 'Filtros'



```r
summary3 <- censo4 %>% 
  group_by(greenpoint2, Filtros) %>% 
  summarise(total = n(),
            nas = sum(is.na(H01A_TOTAL_PERSONAS)))
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:left;"> Filtros </th>
   <th style="text-align:right;"> total </th>
   <th style="text-align:right;"> nas </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:right;"> 175921 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:right;"> 776478 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Papel n=0 </td>
   <td style="text-align:left;"> Censado en papel </td>
   <td style="text-align:right;"> 1334 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Papel n&gt;0 </td>
   <td style="text-align:left;"> Censado en papel </td>
   <td style="text-align:right;"> 23344 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:right;"> 285810 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:right;"> 505032 </td>
   <td style="text-align:right;"> 218308 </td>
  </tr>
</tbody>
</table>


- Contar apariciones de 'un_ID' y filtrar duplicados


```r
duplicated_un_ID <- censo4 %>% 
  group_by(un_ID) %>% 
  tally() %>% 
  filter(n > 1)
duplicated_un_ID
```

### Extraer y guardar subconjunto


```r
# Selecting columns from 'censo4' that match 'Nombre_Columna' and contain 'GRUPO'
paso <- censo4 %>% select(
  all_of(Nombre_Columna[11]),
  matches("GRUPO")
)

# Saving the 'paso' dataset as an RDS file in the specified directory
saveRDS(paso, 
        file = "Recursos/02_Census_Filters/data/censo_viviendas.rds")
```




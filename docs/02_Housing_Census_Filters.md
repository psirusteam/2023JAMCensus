



# Filtering and Refining Census Data

In the process of enhancing and purifying census database, it is imperative to establish consistent and replicable rules. In this context, the procedure of "Census Data Filtering and Refinement" becomes essential in improving data quality and eliminating irrelevant information. Throughout the following code blocks, we will explore how diverse filters and refinement techniques were applied to census data. These codes will guide us through a crucial process to ensure that the data is reliable and suitable for further analysis. Collectively, these steps will empower us to obtain more precise and valuable insights from census data.


## Reading Libraries, Databases, and Other Inputs

In this section, we start by loading the necessary libraries used throughout the processing. Additionally, we define the columns that will be retained after applying the filters. We also make necessary adjustments to the databases, considering updates in the UGM codes.


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
  "Filters"
) 
```

### Reading Housing Data without Coordinates.  {-}

In this section, we read the housing data from a CSV file that does not include coordinates. We then transform the data into the required format, including variables such as province ID, canton ID, and district ID based on the given codes. The resulting dataset will be used for further analysis and processing.


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
### Changing UGM Codes in the Housing Data. {-}

In this section, we modify the UGM (Urban Geographic Micro-data) codes in the housing data to ensure consistency and accuracy. Certain UGM codes are updated according to predefined mappings. This step is crucial for maintaining uniformity in the data for subsequent analysis.


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

### Read the Standardized Census Data. {-}

In this section, we read the standardized census data from a stored RDS (R Data Serialization) file. Similar to the previous step, we adjust the UGM codes to maintain data consistency. The standardized census data will serve as the foundation for the subsequent filtering and refinement processes.


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

### Adding the Age-Sex Base. {-}

In this section, we incorporate the age-sex base into the analysis. The age-sex base is read from a stored RDS file. As in previous steps, we ensure consistency by adjusting the UGM codes. The age-sex base provides valuable demographic information and will be utilized in the subsequent filtering and refinement procedures.


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

### Inner Join to Add the Age-Sex Base

In this section, an inner join operation is performed to integrate the age-sex base with the census data. The difference in row count between the two bases corresponds to paper-censused households that are included later in the process. The number of rows in the age-sex base is compared to the census data and the housing data without coordinates to verify the match.


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

The code begins by calculating the difference in row count between the age-sex base (`censo_sexo_edad`) and the existing census data (`censo1`). This difference represents the number of paper-censused households that are not yet included in the census data. Additionally, the number of rows in the housing data without coordinates (`Viviendas_sin_coordenadas`) is determined for reference.

The `inner_join` operation is then applied to merge the age-sex base with the census data. The `join_by` function specifies the columns used for the join operation, ensuring a comprehensive integration of data from both sources. This process enhances the dataset by incorporating important demographic information for further analysis.

## Applying filters and analyzing the data

In this section, we'll walk through the process of applying various filters and performing data analysis on the refined census data.

### Applying the first filter: categorizing households with residents and determining greenpoint status

    In this code block, we introduce the first filter by categorizing households as having residents ('si') or being empty ('no') based on the total number of residents in each household. Additionally, we determine the greenpoint status of each household, considering whether the greenpoint value is '0' and the 'personas' value is 'si'. The 'greenpoint' column is updated accordingly.


```r
# Create 'personas' column to categorize households with or without residents
censo1 %<>% 
  mutate(personas = if_else(H01A_TOTAL_PERSONAS > 0, "si", "no")) 

# Assign greenpoint status based on conditions

censo1 %<>% mutate(greenpoint = if_else(greenpoint == "0" & personas == "si", "1", greenpoint))  
```

*greenpoint*: The house is reported as censused on the point map 

-   Analyzing the greenpoint distribution

    This code calculates the distribution of greenpoint status among households. It groups the data by 'greenpoint' status, counts the number of households in each category, and computes the percentage distribution. The results provide insights into the prevalence of greenpoint status among the census data.


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

-   Summarizing household characteristics by greenpoint status

    This code block summarizes household characteristics based on their greenpoint status. It calculates minimum and maximum numbers of residents in households, counts missing values for the total number of residents, and provides the total count of households for each greenpoint category.


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

-   Creating a contingency table for occupancy and greenpoint status

    Here, a contingency table is generated to explore the relationship between occupancy and greenpoint status. The table cross-tabulates the 'V02_OCUPACION_VIVIENDA' column (occupancy) with the 'greenpoint' column, accounting for missing values as well. This provides a visual representation of how these two variables are distributed among households.


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
-   Applying the Second Filter and Refinement

Continuing with the refinement process, the next filter is applied to further categorize households based on additional criteria. 


```r
censo2 <- censo1 %>% mutate(greenpoint2 = case_when(
  H01A_TOTAL_PERSONAS > 0 ~ "Censado con informacion n>0",
  RESUL_ENTREVISTA_VIV %in% c(1) & H01A_TOTAL_PERSONAS == 0 ~ "Censado con informacion n=0",
  RESUL_ENTREVISTA_VIV %in% c(3,4) ~ "Sin informacion pero  n>0", 
  is.na(greenpoint) & is.na(personas) ~ "Sin informacion pero n>=0",
  V02_OCUPACION_VIVIENDA == "8" ~ "Sin informacion pero n>=0", 
  TRUE ~ "Resto"
))
```

We introduce the "greenpoint2" status to further categorize households based on various conditions. This is based on a combination of factors such as the number of residents, interview outcomes, and housing occupation. Each household is assigned to a specific category such as "Censado con informacion n>0", "Censado con informacion n=0", and "Sin informacion pero  n>0", among others. This provides a more detailed way to describe the status of households based on different criteria.



```r
readRDS("Recursos/02_Census_Filters/RecurseBooks/census2.rds") %>% 
  head(10) %>% tba()
```

### Applying the second filter: WorldPop criterion

 Starting from step 1, we also include all households with the 
 WorldPop variable (WP) that are within 1 standard deviation from its
 average value. However, if these households have zero residents in the 
 variable of interest, we mark that variable as "Not Available" (NA).


-   Calculate summary statistics for the 'wpop_sum' variable

Firstly, we calculate summary statistics for the 'wpop_sum' variable, which is a covariate related to WorldPop. The summary statistics include the mean, standard deviation, minimum, and maximum values of 'wpop_sum'. These statistics help us establish the thresholds for the filter and are saved in a summary file.



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
-   Calculation of Lower and Upper Thresholds

We use the summary statistics to calculate the lower and upper thresholds based on one standard deviation from the mean. These thresholds will help us identify households that meet the criteria of the second filter.



```r
# Calculate the lower and upper thresholds based on one standard deviation from the mean
li <- 96.96515 - 143.1986 * 1  # Lower threshold
ls <- 96.96515 + 143.1986 * 1  # Upper threshold
```

We identify and count households that meet the criteria of the second filter. We focus on households with zero residents ('H01A_TOTAL_PERSONAS') but have 'wpop_sum' values outside the calculated threshold. We perform this count and group it by the 'V02_OCUPACION_VIVIENDA' variable.


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

-   Application of the Second Filter and Column Updates

We apply the second filter to households and update the 'greenpoint2' and 'Filtros' columns accordingly. The 'greenpoint2' column is updated to reflect the new classification based on the WorldPop Criterion, while the 'Filtros' column indicates the application of the WorldPop Criterion or is set as NA as appropriate.


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

-   Summary of Data Based on 'greenpoint2'

We summarize the data based on the updated 'greenpoint2' variable. We calculate the distribution and percentages of households in each 'greenpoint2' category. These summaries help us understand the impact of the filter on household classification.


```r
# Summarizing the data based on the 'greenpoint2' variable
summary_greenpoint2 <- censo3 %>%
  group_by(greenpoint2) %>%
  tally() %>%
  mutate(percentage = 100 * n / sum(n))
summary_greenpoint2
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

-   Summary of Data Based on 'greenpoint2' and 'Filtros'

We generate an additional summary that considers the combination of the 'greenpoint2' and 'Filtros' variables. This provides more detailed information on how the WorldPop Criterion affects the existing categories.


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

### Summary of Statistics Based on 'greenpoint2'

We calculate additional statistics for the 'greenpoint2' categories. These statistics include the minimum, maximum, number of missing values, and the total number of households in each category. This data is essential for understanding the distribution of residents in the filtered households.

Each of these stages contributes to the process of applying the second filter and refining census data based on the WorldPop Criterion. The generated summaries and data are useful for further analysis and are saved for future reference.


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

### Defining the Third Filter

In this section, we introduce the implementation of the third filter, building upon the foundation laid by Filters 1 and 2. The third filter addresses households within UGMs that were surveyed after an interval greater than 20 days, and despite being classified as unoccupied, there is a lack of certainty regarding their occupancy status. These households are reclassified as having an unknown status. 

-   Reading the 'Desocupadas fuera periodo.xlsx' File

We start by reading the 'Desocupadas fuera periodo.xlsx' file to gather information about households that were vacant but visited outside the standard interval. We specifically extract the 'UGM_ID' column for further analysis.



```r
# Reading the 'Desocupadas fuera periodo.xlsx' file and selecting the UGM_ID column

upms_reporte <- openxlsx::read.xlsx(
  xlsxFile = "Recursos/02_Census_Filters/Data/Desocupadas fuera periodo.xlsx") %>%
  select(UGM_ID = ID_UGM)
```

-   Applying Filters Based on 'upms_reporte' and Specific Conditions

Using the gathered information from 'upms_reporte' and considering certain conditions, we apply additional filters to the existing data. We update the 'greenpoint2' and 'Filtros' columns based on the specified criteria.


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

-   Applying Additional Filters and Creating 'Filtros' Values

We proceed by further refining the data by applying additional filters. The 'Filtros' values are updated based on various conditions such as the number of residents, the result of the interview ('RESUL_ENTREVISTA_VIV'), and the occupation of the dwelling ('V02_OCUPACION_VIVIENDA').


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

-   Summarizing Data Based on 'greenpoint2' and 'Filtros' Variables

We generate a summary of the data based on the updated 'greenpoint2' and 'Filtros' variables. The summary provides insights into the distribution of households across different categories.


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

-   Summarizing Data Based on 'greenpoint2' Variable

Similarly, we create another summary of the data, this time focusing solely on the 'greenpoint2' variable. This summary helps us understand the impact of the third filter on the classification of households.


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

### Combining non-coordinated houses 

Selecting non-coordinated houses from the 'censo_sexo_edad' dataset and inner joining with 'Viviendas_sin_coordenadas'


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

### Aggregating statistics and summaries


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

-   Summarizing statistics for the 'censo4' dataset based on 'greenpoint2' and 'Filtros' columns


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
   <td style="text-align:left;"> Entrevista igual a 1 y Número de personas igual a 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 175921 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:left;"> Número de personas mayor a 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 261 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 776478 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> Entrevista  es 3 o 4 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 285810 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Criterio WorldPop </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 135370 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Fuera de periodo(20 días) </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 151354 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Sin conteo de personas </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 218308 </td>
  </tr>
</tbody>
</table>

-   Summarizing statistics for the 'censo4' dataset based on 'greenpoint2' and 'Filtros' columns



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
   <td style="text-align:left;"> Entrevista igual a 1 y Número de personas igual a 0 </td>
   <td style="text-align:right;"> 175921 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:left;"> Número de personas mayor a 0 </td>
   <td style="text-align:right;"> 776478 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:left;"> Entrevista  es 3 o 4 </td>
   <td style="text-align:right;"> 285810 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Criterio WorldPop </td>
   <td style="text-align:right;"> 135370 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Fuera de periodo(20 días) </td>
   <td style="text-align:right;"> 151354 </td>
   <td style="text-align:right;"> 0 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:left;"> Sin conteo de personas </td>
   <td style="text-align:right;"> 218308 </td>
   <td style="text-align:right;"> 218308 </td>
  </tr>
</tbody>
</table>


-   Counting occurrences of 'un_ID' and filtering for duplicates

```r
duplicated_un_ID <- censo4 %>% 
  group_by(un_ID) %>% 
  tally() %>% 
  filter(n > 1)
duplicated_un_ID
```

### Extracting and Saving Subset


```r
# Selecting columns from 'censo4' that match 'Nombre_Columna' and contain 'GRUPO'
paso <- censo4 %>% select(
  all_of(Nombre_Columna),
  matches("GRUPO")
)

# Saving the 'paso' dataset as an RDS file in the specified directory
saveRDS(paso, 
        file = "Recursos/02_Census_Filters/data/censo_viviendas.rds")
```




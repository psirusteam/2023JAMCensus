

# Binomial unit model for occupied dwellings

In a first instance, a statistical model was defined to predict the probability of a house being occupied. Due to a significant number of houses in the census that never responded to survey calls, refused to be interviewed, or were simply unreachable, it was necessary to assign an occupancy probability to these houses. The model used was a Bayesian mixed model with a binomial response and random effects for provinces, cantons, and districts, as detailed below:

-   $U_{ij}$ is a dichotomous variable indicating the occupancy status of the $i$-th house in the $j$-th geographic area.

-   $p_{ij}$ is the probability that the $i$-th house in the $j$-th geographic area is occupied.
-   $x_{ij}^\prime \beta$ represents the fixed effects considered in the model that influence the occupancy probability.
-   $z_{ij}^\prime \gamma$ represents the random effects that also influence the occupancy probability.
-   We used the logit function to relate these probabilities and effects, as per the equation $\text{logit}(p_{ij}) = x_{ij}^\prime \beta + z_{ij}^\prime \gamma$.

To achieve more accurate results, non-informative prior distributions for the parameters of the Bayesian model were implemented. The final outcome provides an estimate of the number of unoccupied houses in different geographical subdivisions of the country.



## Preparing the Environment and Data

In the following code block, we initiate the process by preparing our R environment. This involves clearing the workspace to remove any existing variables and loading the necessary libraries for data manipulation, visualization, and statistical analysis. We use libraries such as 'tidyverse' for data manipulation and visualization, 'data.table' for efficient data manipulation, 'openxlsx' for reading Excel files, 'magrittr' for data manipulation using pipe operators, 'lme4' for fitting linear mixed-effects models, 'rstan' for Bayesian data analysis using Stan, and 'rstanarm' for fitting Bayesian regression models. Additionally, we define the 'select' function from the 'dplyr' package for column selection.

After setting up the environment, we proceed to read the census data. We read the census information from the 'censo_viviendas.rds' file and extract specific columns that are relevant to our analysis, including 'un_ID' and 'Filtros'. Similarly, we read the standardized UGM covariates from the 'Base_ugms_estandarizada.rds' file. These initial steps lay the foundation for our subsequent data analysis and modeling.


```r
# Clear the workspace by removing all variables
rm(list = ls())
# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(data.table)  # For efficient data manipulation
library(openxlsx)  # For reading Excel files
library(magrittr)  # For data manipulation using pipe operators
library(lme4)  # For fitting linear mixed-effects models
library(rstan)  # For Bayesian data analysis using Stan
library(rstanarm)  # For fitting Bayesian regression models
select <-
  dplyr::select  # Define the 'select' function from dplyr package
cat("\f")  # Clear the console

## Reading census data.
# Read census data from the 'censo_viviendas.rds' file and select specific columns
censo_vivienda <- readRDS("Recursos/04_Model_binomial/Data/censo_viviendas.rds") %>%
  select(un_ID:Filtros)

Covariables_UGM <- readRDS("Recursos/04_Model_binomial/Data/Base_ugms_estandarizada.rds")
# Read UGM covariates from the 'Base_ugms_estandarizada.rds' file
```

## Defining 'Desocupada' Column: Classifying Occupancy Status

This section focuses on defining the 'Desocupada' column in the census data, which indicates whether a dwelling is unoccupied. The values are determined based on specific conditions derived from the 'greenpoint2' and 'Filtros' columns, as well as the number of individuals living in the dwelling. The classification process is essential for subsequent analysis and modeling.


```r
# Define the 'Desocupada' column based on specific conditions
censo_vivienda %<>% mutate(
  Desocupada = case_when(
    greenpoint2 == "Censado con informacion n=0" ~ 1,
    greenpoint2 == "Censado con informacion n>0" ~ 0,
    greenpoint2 == "Sin informacion pero  n>0" ~ 0,
    Filtros == "Censado en papel" & H01A_TOTAL_PERSONAS > 0 ~ 0,
    Filtros == "Censado en papel" & H01A_TOTAL_PERSONAS == 0 ~ 1,
    greenpoint2 == "Sin informacion pero n>=0" ~ NA_real_
  )
)
```

### Counting Combinations of Occupancy Status and Greenpoint Values

In this section, we perform a data aggregation to calculate the counts of various combinations of occupancy status and 'greenpoint2' values. By grouping the data based on these variables, we can observe the distribution of dwelling occupancy patterns and the corresponding greenpoint categories. The resulting summary, including the counts, is saved to a file for reference and further analysis.



```r
# Grouping and summarizing to get counts for different combinations

conteos <- censo_vivienda %>% 
  group_by(greenpoint2, Desocupada) %>% 
  summarise(total = n(), .groups = "drop")
conteos
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> greenpoint2 </th>
   <th style="text-align:right;"> Desocupada </th>
   <th style="text-align:right;"> total </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> Censado con informacion n=0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 175921 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Censado con informacion n&gt;0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 776478 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Papel n=0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1334 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Papel n&gt;0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 23344 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero  n&gt;0 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 285810 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> Sin informacion pero n&gt;=0 </td>
   <td style="text-align:right;"> NA </td>
   <td style="text-align:right;"> 505032 </td>
  </tr>
</tbody>
</table>



```r
# Grouping and summarizing data to create a binomial model base dataset
base_conteo_viviendas <- censo_vivienda %>% group_by(UGM_ID) %>%
  summarise(
    Desocupadas = sum(Desocupada, na.rm = TRUE),
    Ocupadas = sum(1 - Desocupada, na.rm = TRUE),
    n_vivienda = n()
  )
base_conteo_viviendas
```


<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> UGM_ID </th>
   <th style="text-align:right;"> Desocupadas </th>
   <th style="text-align:right;"> Ocupadas </th>
   <th style="text-align:right;"> n_vivienda </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 10101001 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101002 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101003 </td>
   <td style="text-align:right;"> 5 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 6 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101004 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101006 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101007 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101008 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101009 </td>
   <td style="text-align:right;"> 8 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101010 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101011 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101012 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101013 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101018 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101019 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101020 </td>
   <td style="text-align:right;"> 7 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 9 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101021 </td>
   <td style="text-align:right;"> 3 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 4 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101022 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 34 </td>
   <td style="text-align:right;"> 122 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101026 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101028 </td>
   <td style="text-align:right;"> 1 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 1 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10101036 </td>
   <td style="text-align:right;"> 2 </td>
   <td style="text-align:right;"> 0 </td>
   <td style="text-align:right;"> 2 </td>
  </tr>
</tbody>
</table>

-   Summarize the numeric columns in 'base_conteo_viviendas' and save the results


```r
readRDS("Recursos/04_Model_binomial/RecurseBooks/base_conteo2.rds") %>% 
  tba()
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> Desocupadas </th>
   <th style="text-align:right;"> Ocupadas </th>
   <th style="text-align:right;"> n_vivienda </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 177255 </td>
   <td style="text-align:right;"> 1085632 </td>
   <td style="text-align:right;"> 1767919 </td>
  </tr>
</tbody>
</table>

-   Inner join the binomial model base dataset with standardized covariates


```r
base_conteo_viviendas <- inner_join(base_conteo_viviendas,
                                    Covariables_UGM, by = "UGM_ID")
```

The provided code includes the following options:

1. `options(mc.cores = parallel::detectCores())`:
   This option sets the number of cores used for parallel computation in Stan models. The `parallel::detectCores()` function automatically detects the number of available CPU cores on your machine. By setting the number of cores, you can leverage parallel processing to speed up the estimation process of the Stan model.

2. `rstan::rstan_options(auto_write = TRUE)`:
   This option is related to writing compiled Stan models to disk for caching purposes. When `auto_write` is set to `TRUE`, it indicates that compiled Stan models should be automatically saved to disk to speed up the compilation process in future runs. This can improve the running time of the model, especially if you run the same model multiple times.

Both of these options contribute to improving the efficiency and speed of fitting Stan models by utilizing parallel processing and caching compiled models.



```r
options(mc.cores = parallel::detectCores())
rstan::rstan_options(auto_write = TRUE) # Speed up running time 

modelo_binomial <- stan_glmer(
  cbind(Desocupadas, Ocupadas) ~ 1 +
    (1 | PROV_ID) +
    (1 | CANT_ID) +
    (1 | DIST_ID) +
     ugm_peligrosidad +                        
     ugm_problema_de_acceso +                  
     ugm_riesgos_amenazas +                    
     ugm_cobertura_telecomunicaciones +        
     dist_permisos_de_construccion_2011_2022 + 
     dist_poblacion_proyeccion_ajustada_2022 + 
     dist_poblacion_ccss_abril_2023 +          
     dist_matricula_educacion_primaria_2021 +  
     dist_codigo_urbanidad +                   
     GHS_BUILT_S_E2020_GLOBE_R2023A_5367_CRI + 
     urban_coverfraction +                     
     crops_coverfraction +                     
     asent +                                   
     ppp_CRI_v2 +                              
     elev +                                    
     indig +                                   
     aprot +  
     ebais_tt +                                
     escu_tt +                                 
     igl_tt +                                  
     dist_nl_mean 
  ,
  data = base_conteo_viviendas,
  family = binomial(link = "logit"),
  iter = 15000,            # total number of iterations per chain
  cores = 4, 
)

saveRDS(object = modelo_binomial, 
        file = "Recursos/04_Model_binomial/Data/Binomial_bayes_vivienda_desocupadas.rds")
```

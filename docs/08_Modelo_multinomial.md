


# Modelo multinomial para estimar personas por rango de edad y sexo

En el paso final, modelamos la estructura demográfica de las Unidades Geográficas Pequeñas a nivel de provincia. Esta estructura consta de 40 subgrupos basados en la combinación de género y grupos de edad. Para estimar la demografía de hogares no observados, utilizamos la predicción condicional basada en los resultados del paso anterior.

Dada la naturaleza del fenómeno en estudio, se considera adecuada una distribución multinomial para el recuento de individuos en los 40 grupos. Aquí tienes una explicación del modelo:

- $G_i$ sigue una distribución Multinomial $Multinomial(p_{G_i})$.

- Las log-odds de $p_{G_i}$ están modeladas por $x_i^T\beta$.

En este contexto, $G_i$ representa el recuento de cada uno de los cuarenta grupos demográficos. $p_{G_i}$ es un vector de tamaño 40 que contiene las probabilidades de que una persona clasificada en cada grupo resida en un hogar.


- Limpieza del entorno de R

El código elimina todos los objetos del entorno de R actual, asegurando un punto de partida limpio para las operaciones posteriores.


```r
# Loading necessary libraries for data analysis.
library(tidyverse)   # Data manipulation and visualization
library(data.table)  # Fast data manipulation
library(openxlsx)    # Reading Excel files
library(magrittr)    # Pipe operator
library(lme4)        # For fitting linear mixed-effects models
library(nnet)        # For fitting neural networks
cat("\f")  # Clears console output
```



-   Leyendo datos


```r
# Reading the preprocessed census data file containing information about households.
censo_vivienda <- readRDS("Recursos/06_Model_Multinomial/Data/02_censo_vivienda_personas.rds") 

# Filtering the census data to retain relevant columns for age and gender analysis.
censo_vivienda_age_gender <- censo_vivienda %>% 
  filter( !greenpoint2 %in% c("Sin informacion pero  n>0", 
                              "Sin informacion pero n>=0")) %>% 
  dplyr::select(PROV_ID, HOMBRES_GRUPO1_sum:MUJERES_GRUPO20_sum)
```

-   Preparación de datos:

    El código crea un nuevo conjunto de datos, censo_vivienda_edad_sexo, filtrando entradas específicas de censo_vivienda. Se seleccionan columnas relacionadas con grupos de edad y género y provincias. Luego, este conjunto de datos filtrado se agrega a nivel de PROV_ID utilizando la función summarise_if.
    


```r
# Summarizing the age and gender data by grouping it based on province (PROV_ID).
censo_personas <- censo_vivienda_age_gender %>% 
  group_by(PROV_ID) %>% 
  summarise_if(is.numeric, sum)
```

-   Modelo Multinomial:

    Se crea un modelo multinomial utilizando la función `multinom`. Este modelo predice la distribución de grupos de edad y género dentro de los hogares según la provincia (`PROV_ID`). El modelo se almacena en la variable `model`.


```r
# Fitting a multinomial model to estimate the distribution of age and gender within households,
# using province as the predictor.
model <- multinom( censo_personas[,-1] %>% as.matrix() ~ censo_personas$PROV_ID)
```

```
## # weights:  320 (273 variable)
## initial  value 9098525.262706 
## iter  10 value 8725423.620363
## iter  20 value 8724936.747528
## iter  30 value 8724362.718376
## iter  40 value 8723575.429512
## iter  50 value 8719973.199033
## iter  60 value 8714415.379026
## iter  70 value 8692534.417677
## iter  80 value 8664249.357504
## iter  90 value 8663007.095699
## iter 100 value 8662293.319310
## final  value 8662293.319310 
## stopped after 100 iterations
```

- Predicción del Modelo:

    La función `predict` se utiliza para predecir las probabilidades de distribución para el modelo multinomial. Los resultados de la predicción no se muestran aquí, pero se pueden obtener utilizando la función `predict`.
    


```r
# Predicting the distribution probabilities using the fitted model.
predict(model,type = "probs")
```
- Guardar el Modelo

    El modelo multinomial entrenado se guarda como un archivo RDS ("Recursos/06_Model_Multinomial/Data/Multinomial_model.rds") utilizando la función `saveRDS`.


```r
# Saving the fitted multinomial model to a file.
saveRDS(model, "Recursos/06_Model_Multinomial/Data/Multinomial_model.rds")
```


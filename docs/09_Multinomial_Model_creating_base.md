# Creando la base censal con predicciones del modelo multinomial


```r
### Cleaning R environment ###
rm(list = ls())

### Libraries ###

library(tidyverse)  # Set of packages for data manipulation and visualization
library(data.table)  # For efficient data operations
library(openxlsx)    # Excel file manipulation
library(magrittr)    # Pipe operations (%>%)
library(lme4)        # Linear mixed-effects models
cat("\f")             # Page break in R console
```

-   Leer datos del censo.

Este fragmento de código realiza las siguientes acciones:

1. **Lectura de Datos del Censo**: Lee los datos del censo de viviendas y personas desde un archivo en formato RDS llamado "02_censo_vivienda_personas.rds".

2. **Lectura del Modelo Multinomial Previamente Creado**: Lee el modelo multinomial previamente creado desde un archivo en formato RDS llamado "Multinomial_model.rds".

3. **Cálculo de Probabilidades**: Calcula las probabilidades de pertenecer a cada categoría de resultado utilizando el modelo multinomial. Estas probabilidades se almacenan en un nuevo DataFrame llamado "probabilidad".

4. **Copia de los Datos del Censo**: Crea una copia de los datos del censo de viviendas y personas en un nuevo DataFrame llamado "censo_vivienda_pred". Esto se hace para trabajar con una copia de los datos originales sin alterar los datos originales.

5. **Cálculo de la Suma de Recuentos Predichos**: Calcula la suma de los recuentos predichos de personas en el censo y la muestra. Este cálculo utiliza la columna "pred_conteos" en el DataFrame "censo_vivienda_pred".

6. **Identificación de Nombres de Columnas de Grupos de Edad**: Identifica las columnas en el DataFrame "censo_vivienda_pred" que corresponden a los grupos de edad. Estas columnas se almacenan en la variable "var_grupo" para su posterior uso.


```r
censo_vivienda <- readRDS("Recursos/06_Model_Multinomial/Data/02_censo_vivienda_personas.rds") 

# Reading the previously created multinomial model.
model <- readRDS("Recursos/06_Model_Multinomial/Data/Multinomial_model.rds")

# Calculate probabilities for each outcome category using the model.
probabilidad <-
  predict(model, type = "probs") %>% as.data.frame() %>%
  select_all(~paste0(.,"_prob")) %>% 
  mutate(PROV_ID = as.character(1:7))

# Create a copy of the census data to work with.
censo_vivienda_pred <- censo_vivienda 

# Calculate the sum of predicted counts.
sum(censo_vivienda_pred$pred_conteos)

# Identify column names corresponding to age groups.
var_grupo <- grep(x = names(censo_vivienda_pred), pattern = "*_GRUPO\\d{,2}_sum$", value = TRUE)
```

## Resultados por Provincias

Este fragmento de código realiza las siguientes acciones:

1. **Filtrado de Datos del Censo para PROV_ID = 1 y Categorías Específicas de 'greenpoint2'**: Filtra los datos del censo para aquellas observaciones donde el valor de la columna "PROV_ID" es igual a "1" y la columna "greenpoint2" tiene uno de los siguientes valores: "Sin informacion pero n>=0" o "Sin informacion pero n>0". Los resultados de este filtro se almacenan en un nuevo DataFrame llamado "PROV_1".

2. **Cálculo de Recuentos Predichos para Grupos de Edad en PROV_1**: Calcula los recuentos predichos para cada grupo de edad en "PROV_1". Utiliza la matriz de recuentos predichos "pred_conteos" en "PROV_1" y multiplica esta matriz por la matriz de probabilidades de pertenecer a cada grupo de edad específico en la provincia 1. Los resultados se almacenan en las columnas correspondientes a grupos de edad.

3. **Cálculo del Límite Inferior de Recuentos Predichos para Grupos de Edad en PROV_1**: Calcula el límite inferior de los recuentos predichos para cada grupo de edad en "PROV_1". Utiliza la matriz de límites inferiores "MEInf_pred_conteos" en "PROV_1" y realiza una operación similar a la anterior para obtener los recuentos inferiores para cada grupo de edad.

4. **Cálculo del Límite Superior de Recuentos Predichos para Grupos de Edad en PROV_1**: Calcula el límite superior de los recuentos predichos para cada grupo de edad en "PROV_1". Utiliza la matriz de límites superiores "MESup_pred_conteos" en "PROV_1" y realiza una operación similar a la anterior para obtener los recuentos superiores para cada grupo de edad.

5. **Cálculo de la Suma de Recuentos Predichos para Grupos de Edad en PROV_1**: Calcula la suma de los recuentos predichos para cada grupo de edad en "PROV_1". Esto se realiza mediante la función `rowSums`, que suma los valores de cada fila en las columnas correspondientes a grupos de edad.

6. **Cálculo de la Suma de Recuentos Predichos en PROV_1**: Calcula la suma de los recuentos predichos en "PROV_1" sumando la columna "pred_conteos".

7. **Cálculo de la Suma de Límites Inferiores de Recuentos Predichos en PROV_1**: Calcula la suma de los límites inferiores de los recuentos predichos en "PROV_1" sumando la columna "MEInf_pred_conteos".

8. **Cálculo de la Suma de Límites Superiores de Recuentos Predichos en PROV_1**: Calcula la suma de los límites superiores de los recuentos predichos en "PROV_1" sumando la columna "MESup_pred_conteos".

Ten en cuenta que el código está etiquetado se repite para cada una delas provincias.


-    PROV_ID = 1


```r
# Filter census data for PROV_ID = 1 and specific greenpoint2 categories.
PROV_1 <- censo_vivienda_pred %>% filter(PROV_ID == "1" , 
                                         greenpoint2 %in% c("Sin informacion pero n>=0",
                                                            "Sin informacion pero  n>0")) 

# Calculate predicted counts for each age group in PROV_1.
PROV_1[, var_grupo] <-
  matrix(PROV_1$pred_conteos, nrow = nrow(PROV_1)) %*%
  matrix(as.numeric(probabilidad[1, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate lower bound of predicted counts for each age group in PROV_1.
PROV_1[,paste0(var_grupo, "_MEInf") ] <- matrix(PROV_1$MEInf_pred_conteos,nrow = nrow(PROV_1)) %*% 
  matrix(as.numeric(probabilidad[1,paste0(var_grupo, "_prob") ]),ncol = 40 ) 

# Calculate upper bound of predicted counts for each age group in PROV_1.
PROV_1[,paste0(var_grupo, "_MESup") ] <- matrix(PROV_1$MESup_pred_conteos,nrow = nrow(PROV_1)) %*% 
  matrix(as.numeric(probabilidad[1,paste0(var_grupo, "_prob") ]),ncol = 40 ) 

# Calculate the sum of predicted counts for each age group in PROV_1.
rowSums(PROV_1[,var_grupo])

# Calculate the sum of predicted counts in PROV_1.
sum(PROV_1[,var_grupo])
sum(PROV_1$pred_conteos)

# Calculate the sum of lower bounds of predicted counts in PROV_1.
sum(PROV_1[,paste0(var_grupo, "_MEInf") ])
sum(PROV_1$MEInf_pred_conteos)

# Calculate the sum of upper bounds of predicted counts in PROV_1.
sum(PROV_1[,paste0(var_grupo, "_MESup") ])
sum(PROV_1$MESup_pred_conteos)
```

-   PROV_ID = 2


```r
# Filter census data for PROV_ID = 2 and specific greenpoint2 categories.
PROV_2 <- censo_vivienda_pred %>% filter(PROV_ID == "2" , 
                                         greenpoint2 %in% c("Sin informacion pero n>=0",
                                                            "Sin informacion pero  n>0")) 

# Calculate predicted counts for each age group in PROV_2.
PROV_2[,var_grupo] <- matrix(PROV_2$pred_conteos,nrow = nrow(PROV_2)) %*% 
  matrix(as.numeric(probabilidad[2,paste0(var_grupo, "_prob") ]),ncol = 40 ) 

# Calculate lower bound of predicted counts for each age group in PROV_2.
PROV_2[,paste0(var_grupo, "_MEInf") ] <- matrix(PROV_2$MEInf_pred_conteos,nrow = nrow(PROV_2)) %*% 
  matrix(as.numeric(probabilidad[2,paste0(var_grupo, "_prob") ]),ncol = 40 ) 

# Calculate upper bound of predicted counts for each age group in PROV_2.
PROV_2[,paste0(var_grupo, "_MESup") ] <- matrix(PROV_2$MESup_pred_conteos,nrow = nrow(PROV_2)) %*% 
  matrix(as.numeric(probabilidad[2,paste0(var_grupo, "_prob") ]),ncol = 40 ) 

# Calculate the sum of predicted counts for each age group in PROV_2.
rowSums(PROV_2[,var_grupo])

# Calculate the sum of predicted counts in PROV_2.
sum(PROV_2[,var_grupo])
sum(PROV_2$pred_conteos)

# Calculate the sum of lower bounds of predicted counts in PROV_2.
sum(PROV_2[,paste0(var_grupo, "_MEInf") ])
sum(PROV_2$MEInf_pred_conteos)

# Calculate the sum of upper bounds of predicted counts in PROV_2.
sum(PROV_2[,paste0(var_grupo, "_MESup") ])
sum(PROV_2$MESup_pred_conteos)
```

-   PROV_ID = 3


```r
PROV_3 <- censo_vivienda_pred %>% filter(
  PROV_ID == "3" ,
  greenpoint2 %in% c("Sin informacion pero n>=0",
                     "Sin informacion pero  n>0")
)

# Display summary statistics of the selected age group data in PROV_3.
summary(PROV_3[, var_grupo])

# Calculate predicted counts for each age group in PROV_3.
PROV_3[, var_grupo] <-
  matrix(PROV_3$pred_conteos, nrow = nrow(PROV_3)) %*%
  matrix(as.numeric(probabilidad[3, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate lower bound of predicted counts for each age group in PROV_3.
PROV_3[, paste0(var_grupo, "_MEInf")] <-
  matrix(PROV_3$MEInf_pred_conteos, nrow = nrow(PROV_3)) %*%
  matrix(as.numeric(probabilidad[3, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate upper bound of predicted counts for each age group in PROV_3.
PROV_3[, paste0(var_grupo, "_MESup")] <-
  matrix(PROV_3$MESup_pred_conteos, nrow = nrow(PROV_3)) %*%
  matrix(as.numeric(probabilidad[3, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate the sum of predicted counts for each age group in PROV_3.
rowSums(PROV_3[,var_grupo])

# Calculate the sum of predicted counts in PROV_3.
sum(PROV_3[,var_grupo])
sum(PROV_3$pred_conteos)

# Calculate the sum of lower bounds of predicted counts in PROV_3.
sum(PROV_3[,paste0(var_grupo, "_MEInf") ])
sum(PROV_3$MEInf_pred_conteos)

# Calculate the sum of upper bounds of predicted counts in PROV_3.
sum(PROV_3[,paste0(var_grupo, "_MESup") ])
sum(PROV_3$MESup_pred_conteos)
```

-   PROV_ID = 4


```r
PROV_4 <- censo_vivienda_pred %>% filter(
  PROV_ID == "4" ,
  greenpoint2 %in% c("Sin informacion pero n>=0",
                     "Sin informacion pero  n>0")
)
summary(PROV_4[,var_grupo])

# Calculate predicted counts for each age group in PROV_4.
PROV_4[, var_grupo] <-
  matrix(PROV_4$pred_conteos, nrow = nrow(PROV_4)) %*%
  matrix(as.numeric(probabilidad[4, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate lower bound of predicted counts for each age group in PROV_4.
PROV_4[, paste0(var_grupo, "_MEInf")] <-
  matrix(PROV_4$MEInf_pred_conteos, nrow = nrow(PROV_4)) %*%
  matrix(as.numeric(probabilidad[4, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate upper bound of predicted counts for each age group in PROV_4.
PROV_4[, paste0(var_grupo, "_MESup")] <-
  matrix(PROV_4$MESup_pred_conteos, nrow = nrow(PROV_4)) %*%
  matrix(as.numeric(probabilidad[4, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate the sum of predicted counts for each age group in PROV_4.
rowSums(PROV_4[,var_grupo])

# Calculate the sum of predicted counts in PROV_4.
sum(PROV_4[,var_grupo])
sum(PROV_4$pred_conteos)

# Calculate the sum of lower bounds of predicted counts in PROV_4.
sum(PROV_4[,paste0(var_grupo, "_MEInf") ])
sum(PROV_4$MEInf_pred_conteos)

# Calculate the sum of upper bounds of predicted counts in PROV_4.
sum(PROV_4[,paste0(var_grupo, "_MESup") ])
sum(PROV_4$MESup_pred_conteos)
```

-   PROV_ID = 5


```r
PROV_5 <- censo_vivienda_pred %>% filter(PROV_ID == "5" , 
                                         greenpoint2 %in% c("Sin informacion pero n>=0",
                                                            "Sin informacion pero  n>0")) 
summary(PROV_5[,var_grupo])

# Calculate predicted counts for each age group in PROV_5.
PROV_5[,var_grupo] <- matrix(PROV_5$pred_conteos,nrow = nrow(PROV_5)) %*% 
  matrix(as.numeric(probabilidad[5,paste0(var_grupo, "_prob") ]),ncol = 40 ) 

# Calculate lower bound of predicted counts for each age group in PROV_5.
PROV_5[,paste0(var_grupo, "_MEInf")] <- matrix(PROV_5$MEInf_pred_conteos,nrow = nrow(PROV_5)) %*% 
  matrix(as.numeric(probabilidad[5,paste0(var_grupo, "_prob") ]),ncol = 40 ) 

# Calculate upper bound of predicted counts for each age group in PROV_5.
PROV_5[,paste0(var_grupo, "_MESup")] <- matrix(PROV_5$MESup_pred_conteos,nrow = nrow(PROV_5)) %*% 
  matrix(as.numeric(probabilidad[5,paste0(var_grupo, "_prob") ]),ncol = 40 ) 

# Calculate the sum of predicted counts for each age group in PROV_5.
rowSums(PROV_5[,var_grupo])

# Calculate the sum of predicted counts in PROV_5.
sum(PROV_5[,var_grupo])
sum(PROV_5$pred_conteos)

# Calculate the sum of lower bounds of predicted counts in PROV_5.
sum(PROV_5[,paste0(var_grupo, "_MEInf") ])
sum(PROV_5$MEInf_pred_conteos)

# Calculate the sum of upper bounds of predicted counts in PROV_5.
sum(PROV_5[,paste0(var_grupo, "_MESup") ])
sum(PROV_5$MESup_pred_conteos)
```

-   PROV_ID = 6


```r
PROV_6 <- censo_vivienda_pred %>% filter(
  PROV_ID == "6" ,
  greenpoint2 %in% c("Sin informacion pero n>=0",
                     "Sin informacion pero  n>0")
)
summary(PROV_6[,var_grupo])

# Calculate predicted counts for each age group in PROV_6.
PROV_6[, var_grupo] <-
  matrix(PROV_6$pred_conteos, nrow = nrow(PROV_6)) %*%
  matrix(as.numeric(probabilidad[6, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate lower bound of predicted counts for each age group in PROV_6.
PROV_6[, paste0(var_grupo, "_MEInf")] <-
  matrix(PROV_6$MEInf_pred_conteos, nrow = nrow(PROV_6)) %*%
  matrix(as.numeric(probabilidad[6, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate upper bound of predicted counts for each age group in PROV_6.
PROV_6[, paste0(var_grupo, "_MESup")] <-
  matrix(PROV_6$MESup_pred_conteos, nrow = nrow(PROV_6)) %*%
  matrix(as.numeric(probabilidad[6, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate the sum of predicted counts for each age group in PROV_6.
rowSums(PROV_6[,var_grupo])

# Calculate the sum of predicted counts in PROV_6.
sum(PROV_6[,var_grupo])
sum(PROV_6$pred_conteos)

# Calculate the sum of lower bounds of predicted counts in PROV_6.
sum(PROV_6[,paste0(var_grupo, "_MEInf") ])
sum(PROV_6$MEInf_pred_conteos)

# Calculate the sum of upper bounds of predicted counts in PROV_6.
sum(PROV_6[,paste0(var_grupo, "_MESup") ])
sum(PROV_6$MESup_pred_conteos)
```

-   PROV_ID = 7


```r
PROV_7 <- censo_vivienda_pred %>% filter(
  PROV_ID == "7" ,
  greenpoint2 %in% c("Sin informacion pero n>=0",
                     "Sin informacion pero  n>0")
)
summary(PROV_7[, var_grupo])

# Calculate predicted counts for each age group in PROV_7.
PROV_7[, var_grupo] <-
  matrix(PROV_7$pred_conteos, nrow = nrow(PROV_7)) %*%
  matrix(as.numeric(probabilidad[7, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate lower bound of predicted counts for each age group in PROV_7.
PROV_7[, paste0(var_grupo, "_MEInf")] <-
  matrix(PROV_7$MEInf_pred_conteos, nrow = nrow(PROV_7)) %*%
  matrix(as.numeric(probabilidad[7, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate upper bound of predicted counts for each age group in PROV_7.
PROV_7[, paste0(var_grupo, "_MESup")] <-
  matrix(PROV_7$MESup_pred_conteos, nrow = nrow(PROV_7)) %*%
  matrix(as.numeric(probabilidad[7, paste0(var_grupo, "_prob")]), ncol = 40)

# Calculate the sum of predicted counts for each age group in PROV_7.
rowSums(PROV_7[, var_grupo])

# Calculate the sum of predicted counts in PROV_7.
sum(PROV_7[, var_grupo])
sum(PROV_7$pred_conteos)

# Calculate the sum of lower bounds of predicted counts in PROV_7.
sum(PROV_7[, paste0(var_grupo, "_MEInf")])
sum(PROV_7$MEInf_pred_conteos)

# Calculate the sum of upper bounds of predicted counts in PROV_7.
sum(PROV_7[, paste0(var_grupo, "_MESup")])
sum(PROV_7$MESup_pred_conteos)
```

- Combinar los resultados de las provincias con información faltante.


```r
prov_sin_informacion <-
  list(
    PROV_1,
    PROV_2,
    PROV_3,
    PROV_4,
    PROV_5,
    PROV_6,
    PROV_7) %>% bind_rows()
```

- Algunas validaciones

Este fragmento de código lleva a cabo las siguientes acciones:

1. **Filtrado y Recopilación de Datos para Todas las Provincias con Información Faltante (Missing Information)**: Filtra los datos de censo para todas las provincias donde el valor de la columna "greenpoint2" corresponde a uno de los siguientes: "Sin informacion pero n>=0" o "Sin informacion pero n>0". Los resultados se almacenan en un nuevo DataFrame llamado "PROV_todas".

2. **Cálculo de Sumas por Fila y Suma Total de Recuentos Predichos para Provincias con Información Faltante**: Utiliza la función `rowSums` para calcular las sumas por fila de los valores en las columnas correspondientes a grupos de edad en "PROV_todas". Luego, calcula la suma total de los recuentos predichos sumando la columna "pred_conteos" en "PROV_todas".

3. **Filtrado de Datos para Provincias con Información del Censo Completa (Censada)**: Filtra los datos de censo para todas las provincias donde el valor de la columna "greenpoint2" no corresponde a "Sin informacion pero n>=0" ni "Sin informacion pero n>0". Los resultados se almacenan en un nuevo DataFrame llamado "PROV_censada".



```r
# Filter and gather data for all provinces with missing information
PROV_todas <-
  censo_vivienda_pred %>% filter(greenpoint2 %in% c("Sin informacion pero n>=0",
                                                    "Sin informacion pero  n>0"))

# Calculate row sums and total sum of predicted counts for provinces with missing information
rowSums(prov_sin_informacion[, var_grupo])
sum(prov_sin_informacion[, var_grupo])
sum(PROV_todas$pred_conteos)

# Filter data for provinces with complete census information
PROV_censada <-
  censo_vivienda_pred %>% filter(!greenpoint2 %in% c("Sin informacion pero n>=0",
                                                     "Sin informacion pero  n>0"))
```

- Inicializar columnas para los límites inferior y superior de los recuentos previstos

Este fragmento de código realiza las siguientes acciones:

1. **Inicialización de Columnas para Límites Inferiores y Superiores de Recuentos Predichos en Provincias con Censo Completo**: Asigna valores iniciales de cero a las columnas correspondientes a los límites inferiores (MEInf) y superiores (MESup) de los recuentos predichos en el DataFrame "PROV_censada". Estas columnas se inicializan con ceros.

2. **Combinación de DataFrames para Grupos de Edad**: Combina los DataFrames de "PROV_censada" (provincias con censos completos) y "prov_sin_informacion" (provincias con información faltante) para cada grupo de edad. Los resultados se almacenan en un nuevo DataFrame llamado "censo_vivienda_grupo_edad". Este nuevo DataFrame incluye las columnas "un_ID" (identificador único de vivienda), "var_grupo" (grupo de edad) y las columnas "var_grupo_MEInf" y "var_grupo_MESup" que representan los límites inferior y superior de los recuentos predichos.

3. **Unión Interna de Datos del Censo con Datos Agrupados por Grupos de Edad**: Realiza una unión interna (inner join) entre los datos de censo originales (almacenados en un archivo RDS) y los datos del DataFrame "censo_vivienda_grupo_edad". Esto agrega la información de los límites inferiores y superiores de los recuentos predichos al conjunto de datos del censo original. El resultado de esta unión se almacena en un nuevo archivo RDS llamado "05_censo_vivienda_personas_grupo_edad.rds".




```r
# in provinces with complete census
PROV_censada[,paste0(var_grupo, "_MEInf")] <- 0
PROV_censada[,paste0(var_grupo, "_MESup")] <- 0

# Combine data frames of provinces with complete and missing information for each age group
censo_vivienda_grupo_edad <-
  bind_rows(PROV_censada, prov_sin_informacion) %>%
  dplyr::select(un_ID,
         var_grupo,
         paste0(var_grupo, "_MEInf"),
         paste0(var_grupo, "_MESup"))

# Inner join the census data with the grouped age data and save the result
readRDS("Recursos/06_Model_Multinomial/Data/04_censo_vivienda_personas.rds") %>% 
  inner_join(censo_vivienda_grupo_edad) %>% 
  saveRDS("Recursos/06_Model_Multinomial/Data/05_censo_vivienda_personas_grupo_edad.rds")
```



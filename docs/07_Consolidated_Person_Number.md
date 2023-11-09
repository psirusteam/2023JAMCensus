


# Consolidar el número de personas por hogar


```r
#################
### Libraries ###
#################
library(tidyverse)
library(data.table)
library(openxlsx)
library(magrittr)
library(lme4)  # For fitting linear mixed-effects models
library(rstan)  # For Bayesian data analysis using Stan
library(rstanarm)  # For fitting Bayesian regression models
library(merTools)
cat("\f")
```

- Leer datos y modelos censales.


```r
censo_vivienda <-
  readRDS("Recursos/05_Model_for_people/Data/01_censo_vivienda_desocupadas.rds")
Base_ugms <- readRDS("Recursos/05_Model_for_people/Data/Base_ugms_estandarizada.rds")

modelo_todas <-
  readRDS("Recursos/05_Model_for_people/Data/fit_poisson_todas.rds")
modelo_ocupadas <-
  readRDS("Recursos/05_Model_for_people/Data/fit_poisson_ocupadas.rds")
```

- Hacer predicciones utilizando los modelos.


1. **Predicciones de Modelos Multinomiales**: Utiliza dos modelos multinomiales, `modelo_todas` y `modelo_ocupadas`, para hacer predicciones sobre los datos en `Base_ugms`. Las predicciones se almacenan en los archivos "pred_todas.rds" y "pred_ocupadas.rds". Estos archivos contienen las predicciones y errores estándar de las predicciones para los dos modelos.

2. **Lectura de Predicciones desde Archivos RDS**: Lee las predicciones de los archivos RDS recién creados, "pred_todas.rds" y "pred_ocupadas.rds", y almacena las predicciones y errores estándar nuevamente en las columnas correspondientes de `Base_ugms`. Estas columnas se llaman "pred_todas" y "pred_todas_se" para las predicciones del modelo "modelo_todas", y "pred_ocupadas" y "pred_ocupadas_se" para las predicciones del modelo "modelo_ocupadas".

3. **Histogramas de Predicciones**: Genera histogramas de las predicciones de ambos modelos. Estos histogramas muestran la distribución de las predicciones.

4. **Resúmenes de Predicciones**: Calcula resúmenes estadísticos de las predicciones, incluyendo estadísticas de resumen como la media, mediana, mínimo, máximo y otros.



```r
pred_todas <- predict(modelo_todas,
                      newdata =  Base_ugms,
                      type = "response",
                      se.fit = TRUE)

saveRDS(pred_todas,file = "Recursos/05_Model_for_people/Data/pred_todas.rds")

pred_todas <-  readRDS(file = "Recursos/05_Model_for_people/Data/pred_todas.rds")

Base_ugms$pred_todas <- pred_todas$fit 
Base_ugms$pred_todas_se <- pred_todas$se.fit

hist(Base_ugms$pred_todas)

pred_ocupadas <- predict(modelo_ocupadas,
                         newdata =  Base_ugms,
                         type = "response",
                         se.fit = TRUE)

saveRDS(pred_ocupadas,file = "Recursos/05_Model_for_people/Data/pred_ocupadas.rds")

pred_ocupadas <-  readRDS(file = "Recursos/05_Model_for_people/Data/pred_ocupadas.rds")

Base_ugms$pred_ocupadas <- pred_ocupadas$fit
Base_ugms$pred_ocupadas_se <- pred_ocupadas$se.fit

hist(Base_ugms$pred_ocupadas)

summary(Base_ugms$pred_todas)
summary(Base_ugms$pred_ocupadas)
```

- Fusionar información del censo con predicciones.

1. **Unión de Datos**: Combina dos conjuntos de datos, `censo_vivienda` y `Base_ugms`, mediante un `full_join`. La unión se basa en una columna común, posiblemente "UGM_ID", y se agrupan los datos por esta columna.

2. **Cálculo de Predicciones**: Dentro de cada grupo definido por "UGM_ID", se calculan las predicciones para el número de ocupantes de viviendas. Estos cálculos se basan en la columna "greenpoint2" y se realizan mediante una serie de condiciones (usando `case_when`):

   - Si "greenpoint2" es igual a "Sin informacion pero n>0", se usa la predicción del modelo "pred_ocupadas".
   
   - Si "greenpoint2" es igual a "Sin informacion pero n>=0", se usa la predicción del modelo "pred_todas".
   
   - De lo contrario, se utiliza el valor de "H01A_TOTAL_PERSONAS" existente en los datos.

3. **Cálculo de Límites Inferiores y Superiores**: También se calculan límites inferiores ("MEInf_pred_conteos") y superiores ("MESup_pred_conteos") para las predicciones de acuerdo a las mismas condiciones y teniendo en cuenta los errores estándar de las predicciones. Se utiliza un valor de 1.96 (correspondiente al percentil 97.5 de una distribución normal estándar) para calcular estos límites en el caso de "Sin informacion pero n>0" o "Sin informacion pero n>=0".




```r
censo_temp <- censo_vivienda %>%
  full_join(Base_ugms) %>%
  group_by(UGM_ID) %>%
  mutate(
    pred_conteos = case_when(
      greenpoint2  == "Sin informacion pero  n>0" ~ pred_ocupadas,
      greenpoint2  == "Sin informacion pero n>=0"  ~ pred_todas ,
      TRUE ~ H01A_TOTAL_PERSONAS
    ),
    MEInf_pred_conteos = case_when(
      greenpoint2  == "Sin informacion pero  n>0" ~ 1.96*pred_ocupadas_se,
      greenpoint2  == "Sin informacion pero n>=0"  ~ 1.96*pred_todas_se ,
      TRUE ~ 0
    ),
    MESup_pred_conteos = case_when(
      greenpoint2  == "Sin informacion pero  n>0" ~ 1.96*pred_ocupadas_se,
      greenpoint2  == "Sin informacion pero n>=0"  ~ 1.96*pred_todas_se ,
      TRUE ~ 0
    )
  )
```

- Calcular la suma de las predicciones.


```r
sum(censo_temp$pred_conteos)

# Summary of estimates per UGM
censo_temp %>%  
  group_by(UGM_ID) %>% 
  summarise(Min_ugm = min(pred_conteos),
            Max_ugm = max(pred_conteos)) %>% 
  View()

# Summary of estimates filtered by greenpoint2
censo_temp %>% filter(greenpoint2  == "Sin informacion pero n>=0" ) %>% 
  group_by(UGM_ID) %>% 
  summarise(Min_ugm = min(pred_conteos),
            Max_ugm = max(pred_conteos))%>% 
  View()
```

- Guardar información consolidada en un archivo.


```r
saveRDS(censo_temp, 
        file = "Recursos/05_Model_for_people/Data/02_censo_vivienda_personas.rds")
```



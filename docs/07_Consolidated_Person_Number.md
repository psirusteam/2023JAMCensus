


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



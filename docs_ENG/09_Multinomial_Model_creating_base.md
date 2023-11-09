# Creating the census base with predictions from the multinomial model 


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

-   Reading the census data.


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

## Results by Provinces

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

-   Combine data frames of provinces with missing information


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

-   some validations


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

-   Initialize columns for lower and upper bounds of predicted counts


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



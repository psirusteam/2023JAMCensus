


# Multinomial model for estimating people by age range and sex

In the final step, we model the demographic structure of Small Geographic Units (UGMs) at the province level. This structure consists of 40 subgroups based on the combination of gender and age groups. To estimate the demographics of unobserved households, we make use of conditional prediction based on the outcomes from the previous step.

Given the nature of the phenomenon under study, a multinomial distribution is considered suitable for the count of individuals across the 40 groups. Here's an explanation of the model:

- \(G_i\) follows a Multinomial distribution \(Multinomial(p_{G_i})\).
- The log-odds of \(p_{G_i}\) are modeled by \(x_i^' \beta\).

In this context, \(G_i\) represents the count for each of the forty demographic groups. \(p_{G_i}\) is a vector of size 40 containing the probabilities that a person classified in each group resides in a household.


-   Cleaning R environment
The code removes all objects from the current R environment, ensuring a clean slate for the subsequent operations.


```r
# Clearing the R environment by removing all variables from memory.
rm(list = ls())
# Loading necessary libraries for data analysis.
library(tidyverse)   # Data manipulation and visualization
library(data.table)  # Fast data manipulation
library(openxlsx)    # Reading Excel files
library(magrittr)    # Pipe operator
library(lme4)        # For fitting linear mixed-effects models
library(nnet)        # For fitting neural networks
cat("\f")  # Clears console output
```



-   Data Reading


```r
# Reading the preprocessed census data file containing information about households.
censo_vivienda <- readRDS("Recursos/06_Model_Multinomial/Data/02_censo_vivienda_personas.rds") 

# Filtering the census data to retain relevant columns for age and gender analysis.
censo_vivienda_age_gender <- censo_vivienda %>% 
  filter( !greenpoint2 %in% c("Sin informacion pero  n>0", 
                              "Sin informacion pero n>=0")) %>% 
  dplyr::select(PROV_ID, HOMBRES_GRUPO1_sum:MUJERES_GRUPO20_sum)
```

-   Data Preparation:

    The code creates a new dataset censo_vivienda_edad_sexo by filtering out specific entries from censo_vivienda. It selects columns related to age and gender groups and provinces. This filtered dataset is then aggregated at the PROV_ID level using the summarise_if function.


```r
# Summarizing the age and gender data by grouping it based on province (PROV_ID).
censo_personas <- censo_vivienda_age_gender %>% 
  group_by(PROV_ID) %>% 
  summarise_if(is.numeric, sum)
```

-   Multinomial Model:

    A multinomial model is created using the `multinom` function. It predicts the distribution of age and gender groups within households based on the province (`PROV_ID`). The model is stored in the variable `model`.


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

-   Model Prediction

    The `predict` function is used to predict the distribution probabilities for the multinomial model. The prediction results are not displayed here but can be obtained using the `predict` function.


```r
# Predicting the distribution probabilities using the fitted model.
predict(model,type = "probs")
```
-   Saving Model

    The trained multinomial model is saved as an RDS file ("Recursos/06_Model_Multinomial/Data/Modelo_multinomial.rds") using the `saveRDS` function.


```r
# Saving the fitted multinomial model to a file.
saveRDS(model, "Recursos/06_Model_Multinomial/Data/Multinomial_model.rds")
```


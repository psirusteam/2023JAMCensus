


# Consolidation of CENSUS housing databases


```r
# Clear the workspace by removing all variables
rm(list = ls())
# Load required libraries
library(tidyverse)  # For data manipulation and visualization
library(data.table)  # For efficient data manipulation
library(openxlsx)  # For reading Excel files
library(magrittr)  # For data manipulation using pipe operators
library(lme4)  # For fitting linear mixed-effects models
library(posterior)  # For Bayesian data analysis
library(rstanarm)  # For fitting Bayesian regression models
library(rstan)  # For Bayesian data analysis using Stan
cat("\f")  # Clear the console

# Read census data from the 'censo_viviendas.rds' file
censo_vivienda <- readRDS("Recursos/04_Model_binomial/Data/censo_viviendas.rds")
# Read UGMS covariates from the 'Base_ugms_estandarizada.rds' file
Base_ugms <- readRDS("Recursos/04_Model_binomial/Data/Base_ugms_estandarizada.rds")
# Read binomial Bayesian model for unoccupied housing from the 'Binomial_bayes_vivienda_desocupadas.rds' file
modelo_binomial <- readRDS("Recursos/04_Model_binomial/Data/Binomial_bayes_vivienda_desocupadas.rds")
```

## Defining occupied and unoccupied houses.


```r
censo_vivienda %<>% mutate(Desocupada = case_when(
  greenpoint2 == "Censado con informacion n=0" ~ 1,
  greenpoint2 == "Censado con informacion n>0" ~ 0,
  greenpoint2 == "Sin informacion pero  n>0" ~ 0, 
  Filtros == "Censado en papel" & H01A_TOTAL_PERSONAS > 0 ~ 0,
  Filtros == "Censado en papel" & H01A_TOTAL_PERSONAS == 0 ~ 1,
  greenpoint2 == "Sin informacion pero n>=0" ~ NA_real_
) )
```

The code first calculates the linear predictor using the posterior_linpred function for the modelo_binomial model, based on the Base_ugms data. It then transforms these linear predictions to obtain predicted probabilities of unoccupied dwellings using the logistic function (plogis).


```r
# Waiting time of 5 to 10 minutes 
pred_linear <- posterior_linpred(modelo_binomial,
                                 newdata = Base_ugms,
                                 draws = 1000) 
pred_unoccupied <- plogis(pred_linear)
```

The next code block (commented out) deals with saving and loading the predicted unoccupied values from a file named "pred_unoccupied.rds." It checks the dimensions of the predicted values and the Base_ugms, and summarizes the means of the predicted values, checking for values below 0 and above 1


```r
#saveRDS(pred_unoccupied, "Recursos/04_Model_binomial/Data/pred_unoccupied.rds")
pred_unoccupied <- readRDS("Recursos/04_Model_binomial/Data/pred_unoccupied.rds")

# Check dimensions of the predicted values and the base
dim(pred_unoccupied)
dim(Base_ugms)

# Count the number of predicted values below 0 and above 1
sum(colMeans(pred_unoccupied) < 0)
sum(colMeans(pred_unoccupied) > 1)

# Summarize the means of predicted values
summary(colMeans(pred_unoccupied))
```

-   Calculate the 2.5th and 97.5th percentiles of predicted values


```r
q0.025 <- apply(pred_unoccupied, MARGIN = 2,
                function(x) quantile(x, 0.025))
q0.975 <- apply(pred_unoccupied, MARGIN = 2,
                function(x) quantile(x, 0.975))

# Calculate the standard deviation of predicted values
sd_pred <- apply(pred_unoccupied, MARGIN = 2, sd)
```

-   Create a data frame with prediction intervals


```r
intervalos <- data.frame(UGM_ID = Base_ugms$UGM_ID,
                         Pred_unoccupied = colMeans(pred_unoccupied),
                         UpperLim_unoccupied = colMeans(pred_unoccupied) + 3 * sd_pred * q0.975,
                         LowerLim_unoccupied = colMeans(pred_unoccupied) - 3 * sd_pred * q0.975
)
```

-   Inner join between censo_vivienda and intervalos based on UGM_ID


```r
censo_vivienda %<>% inner_join(intervalos, by = "UGM_ID")

# Calculate new values for Desocupada and prediction intervals
censo_vivienda %<>%
  mutate(
    Desocupada2  = case_when(is.na(Desocupada) ~ Pred_unoccupied,
                             TRUE ~ Desocupada),
    LimInf_desocupadas  = case_when(is.na(Desocupada) ~ LowerLim_unoccupied,
                                    TRUE ~ LimInf_desocupadas),
    LimSup_desocupadas  = case_when(is.na(Desocupada) ~ UpperLim_unoccupied,
                                    TRUE ~ LimSup_desocupadas)
  )
```

## Summary Measures and Results Validation

In this section, we calculate various summary measures to validate the results of our model. We calculate the mean and sum of different variables for the original unoccupied dwellings, the updated unoccupied dwellings, and the predicted unoccupied dwellings. We also proceed to calculate the mean estimation, confidence intervals, and percentages of unoccupied dwellings.


```r
# Calculate means and sums of different variables
mean_original_desocupada <- mean(censo_vivienda$Desocupada, na.rm = TRUE)
mean_updated_desocupada <- mean(censo_vivienda$Desocupada2)
mean_predicted_desocupada <- mean(censo_vivienda$Pred_unoccupied)

sum_original_desocupada <- sum(censo_vivienda$Desocupada, na.rm = TRUE)
sum_updated_desocupada <- sum(censo_vivienda$Desocupada2)
sum_predicted_desocupada <- sum(censo_vivienda$Pred_unoccupied)
```

-   Calculate mean estimation, confidence intervals, and percentages


```r
result_summary <- censo_vivienda %>%
  mutate(MEInf_desocupadas = Desocupada2 - LimInf_desocupadas,
         MESup_desocupadas = LimSup_desocupadas - Desocupada2) %>% 
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2) ,
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas)/sqrt(n()))) * 100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas)/sqrt(n()))) * 100,
    Porcen = Porcen * 100
  )
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:right;"> total </th>
   <th style="text-align:right;"> Porcen </th>
   <th style="text-align:right;"> LimInf_desocupadas </th>
   <th style="text-align:right;"> LimSup_desocupadas </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:right;"> 220951.2 </td>
   <td style="text-align:right;"> 12.4978 </td>
   <td style="text-align:right;"> 12.4978 </td>
   <td style="text-align:right;"> 12.4979 </td>
  </tr>
</tbody>
</table>


-   Group by PROV_ID and calculate MEInf_desocupadas and MESup_desocupadas


```r
prov_summary <- censo_vivienda %>% group_by(PROV_ID) %>%   
  mutate(
    MEInf_desocupadas = Desocupada2 - LimInf_desocupadas,
    MESup_desocupadas = LimSup_desocupadas - Desocupada2
  ) %>%
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2),
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) / sqrt(n()))) * 100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas) / sqrt(n()))) * 100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  )
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> PROV_ID </th>
   <th style="text-align:right;"> total </th>
   <th style="text-align:right;"> Porcen </th>
   <th style="text-align:right;"> LimInf_desocupadas </th>
   <th style="text-align:right;"> LimSup_desocupadas </th>
   <th style="text-align:right;"> Leng_IC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> 45527.58 </td>
   <td style="text-align:right;"> 8.5831 </td>
   <td style="text-align:right;"> 8.5830 </td>
   <td style="text-align:right;"> 8.5831 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 2 </td>
   <td style="text-align:right;"> 41170.38 </td>
   <td style="text-align:right;"> 11.1458 </td>
   <td style="text-align:right;"> 11.1457 </td>
   <td style="text-align:right;"> 11.1459 </td>
   <td style="text-align:right;"> 0.0002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 3 </td>
   <td style="text-align:right;"> 17471.04 </td>
   <td style="text-align:right;"> 9.4608 </td>
   <td style="text-align:right;"> 9.4607 </td>
   <td style="text-align:right;"> 9.4608 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 4 </td>
   <td style="text-align:right;"> 13335.66 </td>
   <td style="text-align:right;"> 7.6901 </td>
   <td style="text-align:right;"> 7.6900 </td>
   <td style="text-align:right;"> 7.6901 </td>
   <td style="text-align:right;"> 0.0002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 5 </td>
   <td style="text-align:right;"> 32033.49 </td>
   <td style="text-align:right;"> 21.4337 </td>
   <td style="text-align:right;"> 21.4330 </td>
   <td style="text-align:right;"> 21.4343 </td>
   <td style="text-align:right;"> 0.0013 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 6 </td>
   <td style="text-align:right;"> 45353.84 </td>
   <td style="text-align:right;"> 23.4434 </td>
   <td style="text-align:right;"> 23.4432 </td>
   <td style="text-align:right;"> 23.4436 </td>
   <td style="text-align:right;"> 0.0005 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 7 </td>
   <td style="text-align:right;"> 26059.18 </td>
   <td style="text-align:right;"> 15.5944 </td>
   <td style="text-align:right;"> 15.5943 </td>
   <td style="text-align:right;"> 15.5945 </td>
   <td style="text-align:right;"> 0.0002 </td>
  </tr>
</tbody>
</table>

-   Group by CANT_ID and calculate MEInf_desocupadas and MESup_desocupadas


```r
cant_summary <- censo_vivienda %>% group_by(CANT_ID) %>%   
  mutate(
    MEInf_desocupadas = Desocupada2 - LimInf_desocupadas,
    MESup_desocupadas = LimSup_desocupadas - Desocupada2
  ) %>%
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2),
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) / sqrt(n()))) * 100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas) / sqrt(n()))) * 100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  )
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> CANT_ID </th>
   <th style="text-align:right;"> total </th>
   <th style="text-align:right;"> Porcen </th>
   <th style="text-align:right;"> LimInf_desocupadas </th>
   <th style="text-align:right;"> LimSup_desocupadas </th>
   <th style="text-align:right;"> Leng_IC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 101 </td>
   <td style="text-align:right;"> 7167.090 </td>
   <td style="text-align:right;"> 7.3052 </td>
   <td style="text-align:right;"> 7.3052 </td>
   <td style="text-align:right;"> 7.3053 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 102 </td>
   <td style="text-align:right;"> 1331.140 </td>
   <td style="text-align:right;"> 5.5780 </td>
   <td style="text-align:right;"> 5.5780 </td>
   <td style="text-align:right;"> 5.5781 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 103 </td>
   <td style="text-align:right;"> 3421.247 </td>
   <td style="text-align:right;"> 4.9177 </td>
   <td style="text-align:right;"> 4.9176 </td>
   <td style="text-align:right;"> 4.9178 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 104 </td>
   <td style="text-align:right;"> 1902.283 </td>
   <td style="text-align:right;"> 12.7739 </td>
   <td style="text-align:right;"> 12.7737 </td>
   <td style="text-align:right;"> 12.7740 </td>
   <td style="text-align:right;"> 0.0003 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 105 </td>
   <td style="text-align:right;"> 1791.283 </td>
   <td style="text-align:right;"> 24.9587 </td>
   <td style="text-align:right;"> 24.9576 </td>
   <td style="text-align:right;"> 24.9597 </td>
   <td style="text-align:right;"> 0.0021 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 106 </td>
   <td style="text-align:right;"> 1782.314 </td>
   <td style="text-align:right;"> 8.0368 </td>
   <td style="text-align:right;"> 8.0366 </td>
   <td style="text-align:right;"> 8.0370 </td>
   <td style="text-align:right;"> 0.0004 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 107 </td>
   <td style="text-align:right;"> 1844.362 </td>
   <td style="text-align:right;"> 14.1569 </td>
   <td style="text-align:right;"> 14.1566 </td>
   <td style="text-align:right;"> 14.1572 </td>
   <td style="text-align:right;"> 0.0006 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 108 </td>
   <td style="text-align:right;"> 2024.503 </td>
   <td style="text-align:right;"> 4.8761 </td>
   <td style="text-align:right;"> 4.8761 </td>
   <td style="text-align:right;"> 4.8761 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 109 </td>
   <td style="text-align:right;"> 1212.657 </td>
   <td style="text-align:right;"> 5.3105 </td>
   <td style="text-align:right;"> 5.3104 </td>
   <td style="text-align:right;"> 5.3106 </td>
   <td style="text-align:right;"> 0.0003 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 110 </td>
   <td style="text-align:right;"> 1621.001 </td>
   <td style="text-align:right;"> 6.3187 </td>
   <td style="text-align:right;"> 6.3186 </td>
   <td style="text-align:right;"> 6.3188 </td>
   <td style="text-align:right;"> 0.0002 </td>
  </tr>
</tbody>
</table>


-   Group by DIST_ID and calculate MEInf_desocupadas and MESup_desocupadas


```r
dist_summary <- censo_vivienda %>% group_by(DIST_ID) %>%   
  mutate(
    MEInf_desocupadas = Desocupada2 - LimInf_desocupadas,
    MESup_desocupadas = LimSup_desocupadas - Desocupada2
  ) %>%
  summarise(
    total = sum(Desocupada2),
    Porcen = mean(Desocupada2),
    LimInf_desocupadas = (Porcen - (mean(MEInf_desocupadas) / sqrt(n()))) * 100,
    LimSup_desocupadas = (Porcen  + (mean(MESup_desocupadas) / sqrt(n()))) * 100,
    Porcen = Porcen * 100,
    Leng_IC = LimSup_desocupadas - LimInf_desocupadas
  )
```

<table class="table table-striped lightable-classic" style="width: auto !important; margin-left: auto; margin-right: auto; font-family: Arial Narrow; width: auto !important; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:left;"> DIST_ID </th>
   <th style="text-align:right;"> total </th>
   <th style="text-align:right;"> Porcen </th>
   <th style="text-align:right;"> LimInf_desocupadas </th>
   <th style="text-align:right;"> LimSup_desocupadas </th>
   <th style="text-align:right;"> Leng_IC </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> 10101 </td>
   <td style="text-align:right;"> 379.9948 </td>
   <td style="text-align:right;"> 26.1344 </td>
   <td style="text-align:right;"> 26.1309 </td>
   <td style="text-align:right;"> 26.1380 </td>
   <td style="text-align:right;"> 0.0070 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10102 </td>
   <td style="text-align:right;"> 416.2242 </td>
   <td style="text-align:right;"> 8.6569 </td>
   <td style="text-align:right;"> 8.6565 </td>
   <td style="text-align:right;"> 8.6573 </td>
   <td style="text-align:right;"> 0.0009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10103 </td>
   <td style="text-align:right;"> 490.6836 </td>
   <td style="text-align:right;"> 7.7006 </td>
   <td style="text-align:right;"> 7.7004 </td>
   <td style="text-align:right;"> 7.7009 </td>
   <td style="text-align:right;"> 0.0005 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10104 </td>
   <td style="text-align:right;"> 800.4298 </td>
   <td style="text-align:right;"> 15.3251 </td>
   <td style="text-align:right;"> 15.3244 </td>
   <td style="text-align:right;"> 15.3258 </td>
   <td style="text-align:right;"> 0.0014 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10105 </td>
   <td style="text-align:right;"> 901.9284 </td>
   <td style="text-align:right;"> 13.3106 </td>
   <td style="text-align:right;"> 13.3104 </td>
   <td style="text-align:right;"> 13.3108 </td>
   <td style="text-align:right;"> 0.0004 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10106 </td>
   <td style="text-align:right;"> 430.7183 </td>
   <td style="text-align:right;"> 6.2288 </td>
   <td style="text-align:right;"> 6.2287 </td>
   <td style="text-align:right;"> 6.2288 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10107 </td>
   <td style="text-align:right;"> 226.8546 </td>
   <td style="text-align:right;"> 2.0129 </td>
   <td style="text-align:right;"> 2.0129 </td>
   <td style="text-align:right;"> 2.0129 </td>
   <td style="text-align:right;"> 0.0000 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10108 </td>
   <td style="text-align:right;"> 555.5816 </td>
   <td style="text-align:right;"> 13.3393 </td>
   <td style="text-align:right;"> 13.3388 </td>
   <td style="text-align:right;"> 13.3397 </td>
   <td style="text-align:right;"> 0.0009 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10109 </td>
   <td style="text-align:right;"> 1108.8313 </td>
   <td style="text-align:right;"> 5.2467 </td>
   <td style="text-align:right;"> 5.2466 </td>
   <td style="text-align:right;"> 5.2468 </td>
   <td style="text-align:right;"> 0.0002 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> 10110 </td>
   <td style="text-align:right;"> 1077.4863 </td>
   <td style="text-align:right;"> 6.6767 </td>
   <td style="text-align:right;"> 6.6766 </td>
   <td style="text-align:right;"> 6.6768 </td>
   <td style="text-align:right;"> 0.0001 </td>
  </tr>
</tbody>
</table>


-   Save modified censo_vivienda without Pred_desocupadas column


```r
censo_vivienda_modified <- censo_vivienda %>% dplyr::select(-Pred_desocupadas)
saveRDS(censo_vivienda_modified,
        file = "Recursos/04_Model_binomial/Data/01_censo_vivienda_desocupadas.rds")
```

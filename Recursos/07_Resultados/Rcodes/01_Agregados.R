###############################################################
# Small Area Estimation Models for Population Total Estimation #
# Reading and Preparing Databases                             #
# Author: Stalyn Guerrero & Andrés Gutiérrez                  #
###############################################################

### Cleaning R environment ###
# rm(list = ls())

#################
### Libraries ###
#################
library(tidyverse)
library(data.table)
library(openxlsx)
library(magrittr)
cat("\f")

#' Plotting the Density of a Normal Distribution
#'
#' This function plots the density of a normal distribution with specified mean and standard deviation.
#' Additionally, it highlights a specific interval of the distribution with shaded area and segments in the plot.
#'
#' @param mean The mean of the normal distribution.
#' @param deviation The standard deviation of the normal distribution.
#' @param lower_limit The lower limit of the interval to highlight in the plot.
#' @param upper_limit The upper limit of the interval to highlight in the plot.
#' @param dist The distribution to plot. It can be "Vacant" or "Total".
#' @return A plot of the density of the normal distribution with the highlighted interval.
#' @examples
#' plot_densidad(mean = 0, deviation = 1, lower_limit = -1, upper_limit = 1, dist = "Vacant")
#' plot_densidad(mean = 2, deviation = 0.5, lower_limit = 1, upper_limit = 3, dist = "Total")
#' @import ggplot2
#' @importFrom latex2exp TeX
#' @export
#' 
plot_densidad <- function(media = 0,
                          desviacion = 1,
                          lim_inf = -1,
                          lim_sup = 1, 
                          dist = "Desocupadas"
){
  if(dist == "Desocupadas"){
    media <- media
    cuantil_2.5 <- lim_inf
    cuantil_97.5 <- lim_sup
    desviacion <-
      ((cuantil_97.5 - cuantil_2.5) / (2 * qnorm(0.975) - 2 * qnorm(0.025))) 
    
    li <- cuantil_2.5 - desviacion
    ls <- cuantil_97.5 + desviacion
    x <- seq(li, ls, length = 100)
  
  }else if(dist == "Total"){
    
    # Crear una secuencia de valores x
  z <- seq(-4, 4, length.out = 150)
  x <- z*desviacion + media
  }
  
  # Calcular los valores de densidad correspondientes
  y <- dnorm(x, mean = media, sd = desviacion)
  
  densidad_inf <- dnorm(lim_inf, mean = media, sd = desviacion)
  densidad_sup <- dnorm(lim_sup, mean = media, sd = desviacion)
  # Crear un data frame con los valores de x e y
  datos <- data.frame(x = x, y = y)
  
  # Crear el gráfico
  ggplot(datos, aes(x = x, y = y)) +
    geom_line() +
    geom_area(data = subset(datos, x >= lim_inf & x <= lim_sup),
              aes(y = y), fill = "skyblue", alpha = 0.3) +
    geom_segment(aes(x = lim_inf, xend = lim_inf, y = 0, yend = densidad_inf),
                 linetype = "dashed", color = "red") +
    geom_segment(aes(x = lim_sup, xend = lim_sup, y = 0, yend = densidad_sup),
                 linetype = "dashed", color = "red") +
    geom_text(aes(x = lim_inf, y = densidad_inf, label = round(lim_inf,2)), 
              vjust = -0.5, hjust = 1, color = "red") +
    geom_text(aes(x = lim_sup, y = densidad_sup, label = round(lim_sup,2)), 
              vjust = -0.5, hjust = -0.2, color = "red") +
    xlab(latex2exp::TeX("\\theta")) + theme_classic()+
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) 
  
}


#' Auxiliary Function for Calculation and Visualization of Unemployment in a Census
#'
#' This function performs calculations and visualizations related to unemployment data in a census.
#'
#' @param census The census dataset containing information about unemployment.
#' @param group_by_var An optional variable for grouping the census data. It should be a string representing the name of a column in the census dataset.
#' @param filter_value An optional value for filtering the census data. It should be a unique value corresponding to the grouping variable specified in 'group_by_var'.
#' @param Plot A logical value indicating whether to generate a plot. The default value is FALSE.
#' @return A data frame containing the results of the calculations performed on the census data.
#' @examples
#' Pred_desocupado(census_housing, group_by_var = NULL, Plot = TRUE, filter_value = NULL)
#' Pred_desocupado(census_housing, group_by_var = "DIST_ID", Plot = TRUE, filter_value = "10101")
#' @import dplyr
#' @importFrom ggplot2 ggtitle
#' @export
#' 
Pred_desocupado <- function(censo,
                           agrega = NULL,
                           filtro = NULL,
                           Plot = FALSE) {
  
  if (!is.null(agrega)) {
    censo <- censo %>% group_by(!!sym(agrega))
  }
  
  tab <- censo %>%
      summarise(
        total = sum(Desocupadas),
        Porcen = mean(Desocupadas) ,
        LimInf = (Porcen  - mean(Desocupadas_MEInf)) * 100,
        LimSup = (Porcen  + mean(Desocupadas_MESup)) * 100,
        Porcen = Porcen * 100,
        Len_IC = LimSup - LimInf,
        SE  = Len_IC/(2*1.96))
        
  
  if (Plot) {
    if (nrow(tab) == 1) {
      tab_temp <- tab
    } 
    
    if (!is.null(agrega)) {
      if (is.null(filtro)) {
        stop("El filtro no puede ser NULL")
      }  
      
      if (length(filtro) != 1) {
        stop("El filtro debe tener una longitud igual a 1")
      }
      
      tab_temp <- filter(tab, !!sym(agrega) == filtro)
    }
    
    p1 <- plot_densidad(
      media = tab_temp$Porcen,
      desviacion = tab_temp$SE, 
      lim_inf = tab_temp$LimInf,
      lim_sup = tab_temp$LimSup,
      dist = "Desocupadas"
    ) +
      ggtitle(label = paste0(agrega, " = ", filtro))
    
    print(p1)
    tab <- tab_temp
  }
  
  return(tab)
}


#' Auxiliary Function for Calculation and Visualization of Total Population in a Census
#'
#' This function performs calculations and visualizations related to total population data in a census.
#'
#' @param census The census dataset containing information about total population.
#' @param group_by_var An optional variable for grouping the census data. It should be a string representing the name of a column in the census dataset.
#' @param filter_value An optional value for filtering the census data. It should be a unique value corresponding to the grouping variable specified in 'group_by_var'.
#' @param Plot A logical value indicating whether to generate a plot. The default value is FALSE.
#' @return A data frame containing the results of the calculations performed on the census data.
#' @examples
#' Pred_totPob(census_data, group_by_var = "PROV_ID", filter_value = "1", Plot = TRUE)
#' @import dplyr
#' @importFrom ggplot2 ggtitle
#' @export


Pred_totPob <- function(censo,
                       agrega = NULL,
                       filtro = NULL,
                       Plot = FALSE) {
  
  if (!is.null(agrega)) {
    censo <- censo %>% group_by(!!sym(agrega))
  }
  
  tab <- censo %>%
    summarise(
      total = sum(Tot_persona_vivienda),
      L1 = total - sum(Tot_persona_vivienda_MEInf),
      L2 = total  + sum(Tot_persona_vivienda_MESup),
      SE = (L2 - L1)/(2*qnorm(0.975)),
      LimInf = total - 1.64*SE,
      LimSup = total + 1.64*SE,
      Len_IC = LimSup - LimInf
    ) %>% mutate(L1 = NULL, 
                 L2 = NULL, 
    )
  
  if (Plot) {
    if (nrow(tab) == 1) {
      tab_temp <- tab
    }
    
    if (!is.null(agrega)) {
      if (is.null(filtro)) {
        stop("El filtro no puede ser NULL ")
      }
      
      if (length(filtro) != 1) {
        stop("El filtro debe ser de longitud igual a 1 ")
      }
      
      tab_temp <- filter(tab, !!sym(agrega) == filtro)
    }
    
    p1 <- plot_densidad(
      media = tab_temp$total,
      desviacion = tab_temp$SE,
      lim_inf = tab_temp$LimInf,
      lim_sup = tab_temp$LimSup
    ) +
      ggtitle(label = paste0(agrega, " = ", filtro))
    print(p1)
    
    tab <- tab_temp
  }
  
  tab
}

#' Population Pyramid Plot
#'
#' Generates a population pyramid plot with bars and confidence intervals.
#'
#' @param data A data frame containing population data disaggregated by sex and age.
#'             It should have columns "grupo", "total", "LimInf", and "LimSup".
#'             The "grupo" column should follow the format "Sex_Age" (e.g., "MALES_GROUP1").
#'             The columns "total", "LimInf", and "LimSup" should contain the values of total population,
#'             lower limit, and upper limit respectively.
#'
#' @return A population pyramid plot.
#'
#' @examples
#' data <- data.frame(grupo = c("MALES_GROUP1", "FEMALES_GROUP1"),
#'                    total = c(100, 150),
#'                    LimInf = c(90, 140),
#'                    LimSup = c(110, 160))
#' plot_piramide_pob(data)
#'
#' @import ggplot2
#' @import tidyr
#' @import dplyr
#'
#' @export

plot_piramide_pob <- function(data) {
  select <- dplyr::select
  # Separar las columnas de Sexo y Edad en un nuevo data frame
  temp2 <- data %>%
    separate(grupo, sep = "_", into = c("Sexo", "Edad"))
  

  temp3 <- temp2 %>%
    select(Sexo:total) %>%
    spread(key = "Sexo", value = total) %>%
    mutate(
      Edad = factor(Edad, levels = paste0("GRUPO", 1:20))
    )
  
  # Obtener los límites de confianza para hombres y mujeres
  hombre_lims <- temp2 %>% filter(Sexo == "HOMBRES") %>% 
    select(Edad, LimInf, LimSup) %>% 
    mutate(
      Edad = factor(Edad, levels = paste0("GRUPO", 1:20))
    )
  
  mujeres_lims <- temp2 %>% filter(Sexo == "MUJERES") %>% 
    select(Edad, LimInf, LimSup) %>% 
    mutate(
      Edad = factor(Edad, levels = paste0("GRUPO", 1:20))
    )
  
  # Calcular el límite máximo para la escala del eje y
  lim_Max <- max(c(mujeres_lims$LimSup, hombre_lims$LimSup)) 
  
  # Generar el gráfico de pirámide poblacional utilizando ggplot
  ggplot(temp3) +
    geom_bar(aes(x = Edad, y = MUJERES, fill = "Mujeres"), stat = "identity") +
    geom_bar(aes(x = Edad, y = -HOMBRES, fill = "Hombres"), stat = "identity") +
    geom_errorbar(data = hombre_lims, aes(x = Edad, ymin = -LimInf, ymax = -LimSup), width = 0.2) +
    geom_errorbar(data = mujeres_lims, aes(x = Edad, ymin = LimInf, ymax = LimSup), width = 0.2) +
    scale_fill_manual(values = c("Mujeres" = "pink", "Hombres" = "lightblue")) +
    coord_flip() +
    labs(x = "Edad", y = "Población", title = "Pirámide Poblacional") +
    theme_minimal() +
    scale_y_continuous(
      breaks = round(seq(-lim_Max, lim_Max, len = 10), 2), 
      labels = round(abs(seq(-lim_Max, lim_Max, len = 10)))
    )
}


#' Population Pyramid
#'
#' Calculates and visualizes the population pyramid from census data.
#'
#' @param censo A data frame containing census data, with columns representing
#'              aggregation variables and columns ending in "_sum_personas", "_sum_MEInf",
#'              and "_sum_MESup" representing the person counts and confidence limits.
#' @param agrega An optional text string indicating the aggregation variable to use for grouping the data.
#'               If provided, statistics will be calculated for groups of that variable.
#' @param Plot A logical value indicating whether the population pyramid plot should be generated. Default is TRUE.
#' @param filtro An optional value allowing filtering the data by a specific value of the aggregation variable.
#'               Only applied if the aggregation variable is provided. Default is "10110".
#'
#' @return A data frame containing summarized population pyramid data.
#'
#' @examples
#' data <- data.frame(MALES_GROUP1_sum_personas = c(100, 150),
#'                    MALES_GROUP1_sum_MEInf = c(90, 140),
#'                    MALES_GROUP1_sum_MESup = c(110, 160),
#'                    FEMALES_GROUP1_sum_personas = c(120, 140),
#'                    FEMALES_GROUP1_sum_MEInf = c(110, 130),
#'                    FEMALES_GROUP1_sum_MESup = c(130, 150))
#' piramide_pob(data, agrega = "AggregationVariable")
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @export

piramide_pob <- function(censo,
                         agrega = NULL,
                         Plot = TRUE,
                         filtro = "10110"
) {
  if (!is.null(agrega)) {
    censo <- censo %>%  group_by(!!sym(agrega))
  }
  
  var_sel <-   grep(pattern = "sum$",
                    x = names(censo),
                    value = TRUE)
  var_sel <-
    gsub(pattern = "_sum",
         x = var_sel,
         replacement = "")
  
  tab <- purrr::map_dfr(var_sel, ~ {
    x <- .x
    censo %>% 
      summarise(
        total = sum(!!sym(paste0(x, "_sum"))),
        L1 = total - sum(!!sym(paste0(x, "_sum_MEInf"))),
        L2 = total + sum(!!sym(paste0(x, "_sum_MESup")))
      ) %>% mutate(grupo = .x)
  }) 
  
  if (!is.null(agrega)) {
    tab <- tab %>% 
      dplyr::transmute(
        !!sym(agrega),
        grupo,
        total,
        SE = (L2 - L1)/(2*qnorm(0.975)),
        LimInf = total - 1.64*SE,
        LimSup = total + 1.64*SE,
        Len_IC = LimSup - LimInf
      )
  } else {
    tab <- tab %>% 
      dplyr::transmute(
        grupo,
        total,
        SE = (L2 - L1)/(2*qnorm(0.975)),
        LimInf = total - 1.64*SE,
        LimSup = total + 1.64*SE,
        Len_IC = LimSup - LimInf
      ) 
  }
  
  if (Plot) {
    if (nrow(tab) == 40) {
      tab_temp <- tab
    } 
    
    if (!is.null(agrega)) {
      if (is.null(filtro)) {
        stop("El filtro no puede ser NULL")
      }  
      
      if (length(filtro) != 1) {
        stop("El filtro debe tener una longitud igual a 1")
      }
      
      tab_temp <- dplyr::filter(tab, !!sym(agrega) == filtro)
    }
    
    p1 <- plot_piramide_pob(tab_temp)+
      labs(subtitle = paste0(agrega, " = ", filtro))
    
    print(p1)
    tab <- tab_temp
  }
  
  tab
  
}





# Installation of Libraries and Required Software for Bayesian Area Models.

## Step 1: Installing Software

Below is a list of the necessary software for the proper development of the training. It is recommended to install these packages before starting with the practical development.

1. Download and install **Rbase** (<https://cran.r-project.org/bin/windows/base/>)
2. Download and install **Rtools** (<https://cran.r-project.org/bin/windows/Rtools/>)
3. Download and install **Rstudio** (<https://posit.co/download/rstudio-desktop/>)
4. Download and install **Quarto** (<https://quarto.org/docs/get-started/>)
5. Download and install **Anaconda** (<https://www.anaconda.com/products/individual>)
6. Download and install **Google Cloud** (<https://cloud.google.com/sdk/docs/install?hl=es-419>)


## Step 2: Installing the following libraries in *R.*

### Data Visualization and Manipulation:
- **tidyverse**: A collection of packages for data manipulation and visualization.
- **magrittr**: Provides a pipe `%>%` operator to make code more readable.
- **scales**: Tools for scaling visualizations, like adjusting axis breaks.
- **sf**: For working with spatial data and maps.
- **tmap**: Creates thematic maps and overlays.

### Statistical Modeling:
- **lme4**: Fits linear and generalized linear mixed-effects models.
- **rstanarm**: Fits Bayesian models using Stan for various statistical tasks.

### Survey Analysis:
- **srvyr**: Tools for working with survey data alongside the `dplyr` package.
- **survey**: For analyzing complex survey data.

### Data Manipulation and Transformation:
- **dplyr**: Data manipulation tools.
- **tidyr**: Tools for reshaping and tidying data.
- **reshape2**: Reshaping data frames.

### Bayesian Analysis:
- **bayesplot**: Visualization of Bayesian models.
- **posterior**: Tools for working with posterior distributions.
- **rstan**: R interface to Stan, a platform for Bayesian modeling.

### Geospatial Analysis:
- **rgee**: Interface to Google Earth Engine.
- **trafo**: Tools for transforming spatial data.
- **maptools**: Tools for reading and manipulating geographic data.
- **usmap**: Maps of the United States.

### Miscellaneous:
- **sampling**: Tools for survey sampling.
- **haven**: For reading and writing SPSS, Stata, and SAS files.
- **RColorBrewer**: Provides color palettes.
- **kableExtra**: Enhances table rendering in R Markdown.
- **formatR**: Formatting tools for R code.
- **printr**: Custom printing of data frames and tables.
- **remotes**: Tools for package development and installation.
- **latex2exp**: Converts LaTeX code into expressions.

To install each package, use the command `install.packages("package_name")`.


```r
install.packages("patchwork")
install.packages("lme4")
install.packages("tidyverse")
install.packages("rstanarm")
install.packages("magrittr")
install.packages("reticulate") 
install.packages("rgee") 
install.packages("sf")
install.packages("tmap")
install.packages("trafo")
install.packages("scales")
install.packages("srvyr")
install.packages("survey")
install.packages("haven")
install.packages("sampling")
install.packages("RColorBrewer")
install.packages("maptools")
install.packages("data.table")
install.packages("forcats")
install.packages("tidyr")
install.packages("reshape2")
install.packages("bayesplot")
install.packages("posterior")
install.packages("gridExtra")
install.packages("ggalt")
install.packages("usmap")
install.packages("kableExtra")
install.packages("formatR")
install.packages("printr")
install.packages("remotes")
install.packages("latex2exp")
install.packages("gtsummary")
```

### Step-by-Step Guide to Install rstan

Follow these steps to install the `rstan` package:

1. Install Rtools (if using Windows):
   - Download and install Rtools.

2. Install `StanHeaders`:
   - Open R or RStudio.
   - Run the command: `install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))`.

3. Install `rstan`:
   - Run the command: `install.packages("rstan", repos=c("https://mc-stan.org/r-packages/", getOption("repos")))`.

4. Verify the installation:
   - Load the package using: `library(rstan)`.

5. Validate the installation:
   - Run a simple model to ensure that `rstan` is working properly. You can use the example code provided in the documentation.

Remember to follow these steps carefully to ensure a successful installation of `rstan` and its dependencies.

## Step 3: Validation of Installation - Ensuring Proper `rstan` Installation



```r
library(rstan)
library(posterior)
library(bayesplot)

# Example Stan code
stan_code <- "
parameters {
  real y[2]; 
} 
model {
  y[1] ~ normal(0, 1);
  y[2] ~ double_exponential(0, 2);
} 
"
# Fit the model
fit1 <- stan(model_code = stan_code, iter = 10, verbose = FALSE) 

# Print the fit1 object
print(fit1)

# Further fitting and summarizing
fit2 <- stan(fit = fit1, iter = 10000, verbose = FALSE) 
summary(fit2)$summary 
```

In this section, we validate the correct installation of `rstan` by running a simple Bayesian model. We load the required packages, including `rstan`, `posterior`, and `bayesplot`. We then define a basic Stan model using the Stan code provided. We fit this model using `stan()` with a small number of iterations (`iter = 10`) to quickly verify the installation.

Next, we demonstrate fitting the model further (`fit2`) with more iterations (`iter = 10000`) to show how to perform a more comprehensive analysis. The summary of the fitted model is printed using the `summary()` function.

Make sure to evaluate this code in an R environment after installing the required packages to verify that `rstan` has been installed correctly and is functioning as expected.

## Step 4: Creating a Google Earth Engine Account:

<https://developers.google.com/earth-engine/datasets/>

After successfully creating your account, it's important to follow these steps to ensure everything is set up correctly:

1. Access the provided link: <https://developers.google.com/earth-engine/datasets/catalog/WHRC_biomass_tropical>.

2. Scroll down to the bottom of the page and locate the code displayed in the image below:


<img src="Recursos/00_Instalar_rstan/Validar cuenta.png" width="476" />

3. Click on the **Open in Code Editor** option, which will open a new browser tab. Follow the instructions provided until you achieve the result shown in the image below:

<img src="Recursos/00_Instalar_rstan/Validar cuenta2.png" width="788" />

4. In the previous tab, find and click the **Run** button to obtain the outcome displayed in the image below:

<img src="Recursos/00_Instalar_rstan/Validar cuenta3.png" width="958" />

**Note**: Repeat the process as needed to ensure you achieve the desired outcome.

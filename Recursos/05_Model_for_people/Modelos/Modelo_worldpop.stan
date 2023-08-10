data {
  int<lower=1> D; // Número de UGMs 
  int<lower=1> K; // Cantidad de regresores
  int<lower=1> Kz; // Cantidad de efectos aleatorios
  int<lower=0> Y_obs [D]; // conteos de poblacion por UGM 
  int<lower=0> V_obs [D]; // Número de viviendas censadas 
  matrix[D, K] X_obs; // matriz de covariables
  matrix[D, Kz] Z_obs; // matriz de dummis
}

parameters {
  vector[K] beta; // matriz de parámetros 
  vector[Kz] gamma; // Efectos aleatorios 
  real<lower=0> densidad [D]; 
  real<lower=0> sigma;
}

transformed parameters {
  real<lower=0> lambda [D];
  vector[D] lp; // vector de parámetros
  
  lp = X_obs * beta + Z_obs * gamma;
  for(d in 1:D){
     lambda[d] = densidad[d] * V_obs[d];
  }
 
}
model {
  // Prior
  gamma ~ normal(0, 10);
  beta ~ normal(0, 1000);
  sigma ~ inv_gamma(0.001, 0.001);
  
  // Likelihood
  for (d in 1:D) {
    Y_obs[d] ~ poisson(lambda[d]);
  }
  
  // Log-normal distribution for densidad
  for (d in 1:D) {
    densidad[d] ~ lognormal(lp[d], sigma);
  }
}



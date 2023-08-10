data {
  int<lower=1> D; // Número de UGMs
  int<lower=1> K; // Cantidad de regresores
  int<lower=1> Kz; // Cantidad de efectos aleatorios
  int<lower=0> N_obs[D]; // tamaño de muestra por UGM
  int<lower=0> Y_obs[D]; // número de éxitos por UGM
  matrix[D, K] X_obs; // matriz de covariables
  matrix[D, Kz] Z_obs; // matriz de dummies
}

parameters {
  vector[K] beta; // matriz de parámetros
  vector[Kz] gamma; // Efectos aleatorios
}

transformed parameters {
  vector[D] eta; // vector de parámetros lineales
  vector[D] p; // probabilidad de éxito
  
  eta = X_obs * beta + Z_obs * gamma;
  p = 1 ./ (1 + exp(-eta));
}
model {
  // Prior
  gamma ~ normal(0, 10);
  beta ~ normal(0, 1000);

  // Likelihood
  for (d in 1:D) {
    Y_obs[d] ~ binomial(N_obs[d], p[d]);
  }
}


data {
  int<lower=0> ndat; // Number of data points
  real<lower=0> minc; // minimum concentration
  real<lower=0> maxc; // maximum concentration
  real<lower=0> x[ndat]; // Concentration
  real<lower=0> t[ndat]; // Measurement date
  real<lower=0> tprec[ndat]; // Previous measurement date
  int<lower=0> y[ndat]; // Number of survivors
  int<lower=0> Nprec[ndat]; // Previous number of species
  real<lower=0> m0_min; // Prior bounds for the parameters
  real<lower=0> m0_max; 
  real<lower=0> ks_min;
  real<lower=0> ks_max;
}

transformed data {
  real prior_loc_lm0;
  real prior_loc_lks;
  real prior_loc_lNEC;
  real<lower=0> prior_scale_lm0;
  real<lower=0> prior_scale_lks;
  real<lower=0> prior_scale_lNEC;
  
  prior_loc_lm0 = 0.5 * (log10(m0_min) + log10(m0_max));
  prior_loc_lks = 0.5 * (log10(ks_min) + log10(ks_max));
  prior_loc_lNEC = 0.5 * (log10(maxc) + log10(minc));
  
  prior_scale_lm0 = 0.25 * (log10(m0_max) - log10(m0_min));
  prior_scale_lks = 0.25 * (log10(ks_max) - log10(ks_min));
  prior_scale_lNEC = 0.25 * (log10(maxc) - log10(minc));
}

parameters {
  real<lower=0> lm0;
  real lks;
  real lNEC;
}

model {
  real ks;
  real m0;
  real NEC;
  vector[ndat] log_psurv;
  vector[ndat] psurv;
  vector[ndat] tNEC;
  vector[ndat] tref;
  
  ks=pow(10.,lks);
  NEC=pow(10.,lNEC);
  m0=pow(10.,lm0);
  
  for (i in 1:ndat) {
    log_psurv[i] = - m0 * (t[i]-tprec[i]);
    
    if (x[i]>NEC){
        log_psurv[i] = log_psurv[i] - ks* (x[i]-NEC) * t[i];
    }
          psurv[i] = exp( log_psurv[i] );
  }
  
  lm0 ~ normal(prior_loc_lm0, prior_scale_lm0);
  lks ~ normal(prior_loc_lks, prior_scale_lks);
  lNEC ~ normal(prior_loc_lNEC, prior_scale_lNEC);

  y ~ binomial(Nprec,psurv);
}

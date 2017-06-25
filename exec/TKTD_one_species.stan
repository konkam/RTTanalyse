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
  real<lower=0> ke_min;
  real<lower=0> ke_max;
  real<lower=0> ks_min;
  real<lower=0> ks_max;
}

transformed data {
  real prior_loc_m0;
  real prior_loc_ke;
  real prior_loc_lks;
  real prior_loc_lNEC;
  real<lower=0> prior_scale_m0;
  real<lower=0> prior_scale_ke;
  real<lower=0> prior_scale_lks;
  real<lower=0> prior_scale_lNEC;

  prior_loc_m0 = 0.5 * (m0_min + m0_max);
  prior_loc_ke = 0.5 * (ke_min + ke_max);
  prior_loc_lks = 0.5 * (log10(ks_min) + log10(ks_max));
  prior_loc_lNEC = 0.5 * (log10(maxc) + log10(minc));

  prior_scale_m0 = 0.25 * (m0_max - m0_min);
  prior_scale_ke = 0.25 * (ke_max - ke_min);
  prior_scale_lks = 0.25 * (log10(ks_max) - log10(ks_min));
  prior_scale_lNEC = 0.25 * (log10(maxc) - log10(minc));
}

parameters {
  real lNEC_raw;
  real m0_raw;
  real lks_raw;
  real ke_raw;
}

transformed parameters {
  real<lower=0> m0;
  real lks;
  real<lower=0> ke;
  real lNEC;

  m0 = prior_loc_m0 + m0_raw * prior_loc_m0;
  lks = prior_loc_lks + lks_raw * prior_loc_lks;
  ke = prior_loc_ke + ke_raw * prior_loc_ke;
  lNEC = prior_loc_lNEC + lNEC_raw * prior_loc_lNEC;
}

model {
  real ks;
  real NEC;
  vector[ndat] log_psurv;
  vector[ndat] psurv;
  vector[ndat] tNEC;
  vector[ndat] tref;

  ks=pow(10.,lks);
  NEC=pow(10.,lNEC);

  for (i in 1:ndat) {
    log_psurv[i] = - m0 * (t[i]-tprec[i]);

    if (x[i]>NEC){
      tNEC[i] = -1/ke * log(1-NEC/x[i]);

      if (t[i]>tNEC[i]){
        tref[i] = fmax(tprec[i],tNEC[i]);
        log_psurv[i] = log_psurv[i] - ks*( (x[i]-NEC) * (t[i]-tref[i]) + 1/ke * x[i] * exp(-ke*tref[i]) * ( exp(-ke * (t[i] - tref[i])) - 1) );
      }
    }
    psurv[i] = exp( log_psurv[i] );
  }

  m0_raw ~ normal(0, 1);
  ke_raw ~ normal(0, 1);
  lks_raw ~ normal(0, 1);
  lNEC_raw ~ normal(0, 1);

  y ~ binomial(Nprec,psurv);
}

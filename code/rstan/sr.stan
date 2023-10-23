#include likelihoods.stan
data{
  int N_rev; // number of individuals who seroreverted during the study
  real person_time; // total person time for those seropositive at baseline
}

parameters{
  real <lower=0> sigma; // seroreversion rate (per person per year)

}


model{
  
  sigma~gamma(1,1);
  target+= poisson_lpmf(N_rev|sigma*person_time);
}

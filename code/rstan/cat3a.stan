#include likelihoods.stan
data{
  int A; // number of age classes
  int N[A]; // number of individuals per age class
  int pos[A]; // number of seropositive individuals per age class
  vector[A] age1; // lower bound on age class (yrs)
  vector[A] age2; // upper bound on age class (yrs)
}

parameters{
  real <lower = 0.00001, upper = 1> foi; // Force of Infection
  real <lower = 0, upper = 10> sigma; // Rate of seroreversion
}

model{
  for(a in 1:A){
    target+= cat2_lpmf(pos[a]|N[a], foi, sigma, age1[a], age2[a]);
  }
}

generated quantities{
  vector[A] log_lik_a;
  vector[A] seroprevalence;
  real log_lik;
  
  for(a in 1:A){
   log_lik_a[a] = cat2_lpmf(pos[a]|N[a], foi, sigma, age1[a], age2[a]);
   seroprevalence[a] = p_seroprev_2(age1[a], age2[a], foi, sigma);
  }
  
  log_lik = sum(log_lik_a);
  
}
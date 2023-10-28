#include likelihoods.stan
data{
  int A; // number of age classes
  int N[A]; // number of individuals per age class
  int pos[A]; // number of seropositive individuals per age class
  int age0; // cut point for FoI change
  vector[A] age1; // lower bound on age class (yrs)
  vector[A] age2; // upper bound on age class (yrs)
}

parameters{
  real <lower = 0.00001, upper = 1> foi1; // Force of Infection in those <age0
  real <lower = 0.00001, upper = 1> foi2; // Force of Infection in those >=age0
}

model{
  for(a in 1:A){
    target+= cat1_varFoI_lpmf(pos[a]|N[a], foi1, foi2, age0, age1[a], age2[a]);
  }
}

generated quantities{
  vector[A] log_lik_a;
  vector[A] seroprevalence;
  real log_lik;
  
  for(a in 1:A){
   log_lik_a[a] = cat1_varFoI_lpmf(pos[a]|N[a], foi1, foi2, age0, age1[a], age2[a]);
   if(age2[a] <= age0){
        seroprevalence[a] = p_seroprev_1(age1[a], age2[a], foi1);
   } else{
     seroprevalence[a] = p_seroprev_1_varFoI(age1[a], age2[a], age0, foi1, foi2);
   }
  }
  
  log_lik = sum(log_lik_a);
  
}

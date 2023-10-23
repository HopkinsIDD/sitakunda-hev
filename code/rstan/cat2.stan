#include likelihoods.stan
data{
  int A; // number of age classes
  int I; // number of samples to take from posterior
  int N[A]; // number of individuals per age class
  int pos[A]; // number of seropositive individuals per age class
  int age_foi; // cut off at which point FoI changes
  int age_sigma; // cut off at which point sigma changes
  vector[A] age1; // lower bound on age class (yrs)
  vector[A] age2; // upper bound on age class (yrs)
  vector[I] sigma1; // fix sigma for those < age_sigma based on longitudinal data
  vector[I] sigma2; // fix sigma for those > age_sigma based on longitudinal data
}

parameters{
  real <lower = 0.00001, upper = 1> foi1; // Force of Infection
  real <lower = 0.00001, upper = 1> foi2; // Force of Infection
}

model{
      for(i in 1:I){
  for(a in 1:A){
    target+= cat2_varFoIvarSigma_lpmf(pos[a]|N[a], foi1, foi2, sigma1[i], sigma2[i], age_foi, age_sigma, age1[a], age2[a])/I;
  }
      }
}

generated quantities{
  matrix[I,A] log_lik_a;
  matrix[I,A] seroprevalence;
  real log_lik;
  
    for(i in 1:I){
  for(a in 1:A){
   log_lik_a[i,a] = cat2_varFoIvarSigma_lpmf(pos[a]|N[a], foi1, foi2, sigma1[i], sigma2[i], age_foi, age_sigma, age1[a], age2[a])/I;
   
   if(age2[a] <= age_sigma){
      
      seroprevalence[i,a] = p_seroprev_2(age1[a], age2[a], foi1, sigma1[i]);
      
    } else if(age2[a] > age_sigma && age2[a] <= age_foi){
      
          seroprevalence[i,a] = p_seroprev_2_varSigma(age1[a], age2[a], age_sigma, foi1, sigma1[i], sigma2[i]);
          
          } else if (age2[a] > age_foi){
            
            seroprevalence[i,a] = p_seroprev_3_varSigma(age1[a], age2[a], age_foi, age_sigma, foi1, foi2, sigma1[i], sigma2[i]);
            
          } 
  }
    }
  log_lik = sum(log_lik_a);
  
}


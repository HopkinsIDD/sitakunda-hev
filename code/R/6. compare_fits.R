# compare model fit
source(here("code", "R", "functions.R"))

# read in stanfits
m1 <- readRDS(here("generated_data", "model_fits", "chains", "1a.rds"))
m2 <- readRDS(here("generated_data", "model_fits", "chains", "2a.rds"))
m3 <- readRDS(here("generated_data", "model_fits", "chains", "3a.rds"))

######################################
# assume constant FoI with age/time ##
######################################

# model 1a
m1_ll <- extract_log_lik(m1, parameter_name = "log_lik_a", merge_chains = FALSE)
r_eff_m1 <- loo::relative_eff(exp(m1_ll))
m1_loo <- loo(m1_ll, r_eff = r_eff_m1)
saveRDS(m1_loo, "generated_data/model_fits/loo/m1a.rds")

# model 2a - use sample of 100 values from the posterior estimate of sr from longitudinal data
m2_ll <- extract_log_lik(m2, parameter_name = "log_lik_a", merge_chains = FALSE)
  ## collapse down 100 posterior samples --> 1 (already divided through in stan model)
m2_ll_coll <- array(dim = c(3000,4,39))
for(i in 0:38){
 m2_ll_coll[,,i+1] <- colSums(aperm(m2_ll[,,i+(seq(1,3900,39))], c(3,1,2))) 
}
r_eff_m2 <- loo::relative_eff(exp(m2_ll_coll))
m2_loo <- loo(m2_ll_coll, r_eff = r_eff_m2)
saveRDS(m2_loo, "generated_data/model_fits/loo/m2a.rds")

# model 3a - fit seroreversion rate to cross-sectional data
m3_ll <- extract_log_lik(m3, parameter_name = "log_lik_a", merge_chains = FALSE)
r_eff_m3 <- loo::relative_eff(exp(m3_ll))
m3_loo <- loo(m3_ll, r_eff = r_eff_m3)
saveRDS(m3_loo, "generated_data/model_fits/loo/m3a.rds")


##############################
# assume FoI varies with age #
##############################

# vector of ages at which FoI changes
age_foi_c <- seq(12,36,2)

# model 1b
m1b_loo <- list(length = length(age_foi_c))
for(i in 1:(length(age_foi_c))){
  m1b <- readRDS(paste0("generated_data/model_fits/chains/1b/", age_foi_c[i], ".rds"))
m1b_ll <- extract_log_lik(m1b, parameter_name = "log_lik_a", merge_chains = FALSE)
r_eff_m1b <- loo::relative_eff(exp(m1b_ll))
m1b_loo[[i]] <- loo(m1b_ll, r_eff = r_eff_m1b)
}
saveRDS(m1b_loo, "generated_data/model_fits/loo/m1b.rds")

# model 2 - assume seroreversion - sample from posterior distribution
m2b_loo <- list(length = length(age_foi_c))
for(j in 1:(length(age_foi_c))){
  m2b <- readRDS(paste0("generated_data/model_fits/chains/2b/", age_foi_c[j], ".rds"))
  m2b_ll <- extract_log_lik(m2b, parameter_name = "log_lik_a", merge_chains = FALSE)
  
  ## collapse down 100 posterior samples --> 1 (already divided through in stan model)
m2b_ll_coll <- array(dim = c(3000,4,39))
for(i in 0:38){
  m2b_ll_coll[,,i+1] <- colSums(aperm(m2b_ll[,,i+(seq(1,3900,39))], c(3,1,2))) 
}
r_eff_m2b <- loo::relative_eff(exp(m2b_ll_coll))
m2b_loo[[j]] <- loo(m2b_ll_coll, r_eff = r_eff_m2b)
}
saveRDS(m2b_loo, "generated_data/model_fits/loo/m2b.rds")


# model 3b - fit seroreversion rate
m3b_loo <- list(length = length(age_foi_c))
for(i in 1:(length(age_foi_c))){
  m3b <- readRDS(paste0("generated_data/model_fits/chains/3b/", age_foi_c[i], ".rds"))
  m3b_ll <- extract_log_lik(m3b, parameter_name = "log_lik_a", merge_chains = FALSE)
  r_eff_m3b <- loo::relative_eff(exp(m3b_ll))
  m3b_loo[[i]] <- loo(m3b_ll, r_eff = r_eff_m3b)
}
saveRDS(m3b_loo, "generated_data/model_fits/loo/m3b.rds")



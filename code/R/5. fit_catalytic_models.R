source(here("code", "R", "functions.R"))

# read in the cross-sectional seroprevalence data from baseline
dfR1 <- readRDS(here("data", "reshaped_stanR1.rds"))

# read in empirical estimates of seroreversion rate (sigma) for children and adults
# sr_posterior_a <- (rstan::extract(readRDS("generated_data/empirical/sr_posterior_a10.rds")))$sigma
# # sample
# sr_samp_a <- sample(sr_posterior_a, 100)
# saveRDS(sr_samp_a, file = "generated_data/empirical/sr_samp_a.rds")
sigma_samp_a <- readRDS(here("generated_data", "empirical", "sigma_samp_a.rds"))

# sr_posterior_c <- (rstan::extract(readRDS("generated_data/empirical/sr_posterior_c10.rds")))$sigma
# # sample
# sr_samp_c <- sample(sr_posterior_c, 100)
# saveRDS(sr_samp_c, file = "generated_data/empirical/sr_samp_c.rds")
sigma_samp_c <- readRDS(here("generated_data", "empirical", "sigma_samp_c.rds"))


# MODEL 1a: constant FoI, no seroreversion
fit_cat1a <- stan(
  file = here::here("code", "rstan", "cat1a.stan"),
  data = list(
    A = nrow(dfR1),
    N = dfR1$N,
    pos = dfR1$pos,
    age1 = dfR1$age1,
    age2 = dfR1$age2 
  ),
  chains = 4,
  iter = 5000,
  warmup = 2000,
  verbose = TRUE
)
# save model output
cat1a <- rstan::summary(fit_cat1a)
saveRDS(cat1a, file = here("generated_data", "model_fits", "summaries", "1a.rds"))
saveRDS(fit_cat1a, file = here("generated_data", "model_fits", "chains", "1a.rds"))


# MODEL 2a: constant FoI, seroreversion based on empirical measures in <10 and >10 yr-olds
fit_cat2a <- stan(
  file = here::here("code", "rstan", "cat2.stan"),
  data = list(
    A = nrow(dfR1),
    I = length(sigma_samp_c),
    N = dfR1$N,
    pos = dfR1$pos,
    age_foi = 110, # set age of change above oldest age class --> constant FoI
    age_sigma = 10, # age at which rate of seroreversion changes
    age1 = dfR1$age1,
    age2 = dfR1$age2,
    sigma1 = sigma_samp_c,
    sigma2 = sigma_samp_a
  ),
  chains = 4,
  iter = 5000,
  warmup = 2000,
  verbose = TRUE
)
# save the model output
cat2a <- rstan::summary(fit_cat2a)
saveRDS(cat2a, file = here("generated_data", "model_fits", "summaries", "2a.rds"))
saveRDS(fit_cat2a, file = here("generated_data", "model_fits", "chains", "2a.rds"))


# MODEL 3a: constant FoI, seroreversion fit to cross-sectional data
fit_cat3a <- stan(
  file = here::here("code", "rstan", "cat3a.stan"),
  data = list(
    A = nrow(dfR1),
    N = dfR1$N,
    pos = dfR1$pos,
    age1 = dfR1$age1,
    age2 = dfR1$age2 
  ),
  chains = 4,
  iter = 5000,
  warmup = 2000,
  verbose = TRUE
)
# save the model output
cat3a <- rstan::summary(fit_cat3a)
saveRDS(cat3a, file = here("generated_data", "model_fits", "summaries", "3a.rds"))
saveRDS(fit_cat3a, file = here("generated_data", "model_fits", "chains", "3a.rds"))


# MODEL 1b: age-varying FoI, no seroreversion
age0_c <- seq(12, 36, 2)
for(i in 1:(length(age0_c))){
  i = 7
  fit_cat1b <- stan(
    file = here::here("code", "rstan", "cat1b.stan"),
    data = list(
      A = nrow(dfR1),
      N = dfR1$N,
      pos = dfR1$pos,
      age0 = age0_c[i],
      age1 = dfR1$age1,
      age2 = dfR1$age2 
    ),
    chains = 4,
    iter = 5000,
    warmup = 2000,
    verbose = TRUE
  )
  
  # save the model output
  cat1b <- rstan::summary(fit_cat1b)
  saveRDS(cat1b, file = here("generated_data", "model_fits", "summaries", "1b", paste0(age0_c[i], ".rds")))
  saveRDS(fit_cat1b, file = here("generated_data", "model_fits", "chains", "1b", paste0(age0_c[i], ".rds")))
}


# MODEL 2b: age-varying FoI, seroreversion based on empirical measures in <10 and >10 yr-olds
age0_c <- seq(12, 36, 2)
for(i in 2:(length(age_foi_c))){
  i = 11
  fit_cat2b <- stan(
    file = here::here("code", "rstan", "cat2.stan"),
    data = list(
      A = nrow(dfR1),
      I = length(sigma_samp_c),
      N = dfR1$N,
      pos = dfR1$pos,
      age_foi = age0_c[i],
      age_sigma = 10,
      age1 = dfR1$age1,
      age2 = dfR1$age2,
      sigma1 = sigma_samp_c,
      sigma2 = sigma_samp_a
    ),
    chains = 4,
    iter = 5000,
    warmup = 2000,
    verbose = TRUE
  )
  
  # save the model output
  cat2b <- rstan::summary(fit_cat2b)
  saveRDS(cat2b, file = here("generated_data", "model_fits", "summaries", "2b", paste0(age0_c[i], ".rds")))
  saveRDS(fit_cat2b, file = here("generated_data", "model_fits", "chains", "2b", paste0(age0_c[i], ".rds")))
}


# MODEL 3b: age-varying FoI, seroreversion rate fit to cross-sectional data
age0_c <- seq(12, 36, 2)
for(i in 1:(length(age0_c))){
  i = 10
  fit_cat3b <- stan(
    file = here::here("code", "rstan", "cat3b.stan"),
    data = list(
      A = nrow(dfR1),
      N = dfR1$N,
      pos = dfR1$pos,
      age0 = age0_c[i],
      age1 = dfR1$age1,
      age2 = dfR1$age2 
    ),
    chains = 4,
    iter = 5000,
    warmup = 2000,
    verbose = TRUE
  )
  
  # save the model output
  cat3b <- rstan::summary(fit_cat3b)
  saveRDS(cat3b, file = here("generated_data", "model_fits", "summaries", "3b", paste0(age0_c[i], ".rds")))
  saveRDS(fit_cat3b, file = here("generated_data", "model_fits", "chains", "3b", paste0(age0_c[i], ".rds")))
}


############################################
# Fit best fitting model to follow-up data #
############################################

dfR3 <- readRDS(here("data", "reshaped_stanR3.rds"))

# MODEL 2b: age-varying FoI, seroreversion based on empirical measures in <10 and >10 yr-olds
age_foi_c <- seq(12, 36, 2)
for(i in 2:(length(age_foi_c))){
  fit_cat2b_R3 <- stan(
    file = here::here("code", "rstan", "cat2.stan"),
    data = list(
      A = nrow(dfR3),
      I = length(sigma_samp_c),
      N = dfR3$N,
      pos = dfR3$pos,
      age_foi = age_foi_c[i],
      age_sigma = 10,
      age1 = dfR3$age1,
      age2 = dfR3$age2,
      sigma1 = sigma_samp_c,
      sigma2 = sigma_samp_a
    ),
    chains = 4,
    iter = 5000,
    warmup = 2000,
    verbose = TRUE
  )
  
  # save the model output
  cat2b_R3 <- rstan::summary(fit_cat2b_R3)
  saveRDS(cat2b_R3, file = here("generated_data", "model_fits", "summaries", "2b_follow_up", paste0(age0_c[i], ".rds")))
  saveRDS(fit_cat2b_R3, file = here("generated_data", "model_fits", "chains", "2b_follow_up", paste0(age0_c[i], ".rds")))
}

# generate posterior distributions for the empirical rate of seroreversion 
# then we can propogate this uncertainty in the catalytic models
source(here("code", "R", "functions.R"))

# read in longitudinal data 
sr <- readRDS(here("generated_data", "empirical", "sr.rds"))
n_rev <- sr$sr_n
pt <-  sr$tot_py_pos

##############################
# for the overall population #
##############################

sr_mod <- stan(
  file = here::here("code", "rstan", "sr.stan"),
  data = list(
    N_rev = n_rev,
    person_time = pt
  ),
  chains = 4,
  iter = 5000,
  warmup = 2000,
  verbose = TRUE
)

# save the model output
sr_stan <- rstan::summary(sr_mod)
saveRDS(sr_stan, file = here("generated_data", "empirical", "sr_sum.rds"))
saveRDS(sr_mod, file = here("generated_data", "empirical", "sr_posterior.rds"))


#################
# for those >10 #
#################
sr_o10 <- readRDS("generated_data/empirical/sr_o10.rds")
n_rev_o10 <- sr_o10$sr_n
pt_o10 <-  sr_o10$tot_py_pos

sr_mod <- stan(
  file = here::here("code", "rstan", "sr.stan"),
  data = list(
    N_rev = n_rev_o10,
    person_time = pt_o10
  ),
  chains = 4,
  iter = 5000,
  warmup = 2000,
  verbose = TRUE
)

# save the model output
sr_stan <- rstan::summary(sr_mod)
saveRDS(sr_stan, file = here("generated_data", "empirical", "sr_sum_a10.rds"))
saveRDS(sr_mod, file = here("generated_data", "empirical", "sr_posterior_a10.rds"))

####################
# for children <10 #
####################
sr_u10 <- readRDS("generated_data/empirical/sr_u10.rds")
n_rev_u10 <- sr_u10$sr_n
pt_u10 <-  sr_u10$tot_py_pos

sr_mod <- stan(
  file = here::here("code", "rstan", "sr.stan"),
  data = list(
    N_rev = n_rev_u10,
    person_time = pt_u10
  ),
  chains = 4,
  iter = 5000,
  warmup = 2000,
  verbose = TRUE
)

# save the model output
sr_stan <- rstan::summary(sr_mod)
saveRDS(sr_stan, file = here("generated_data", "empirical", "sr_sum_c10.rds"))
saveRDS(sr_mod, file = here("generated_data", "empirical", "sr_posterior_c10.rds"))

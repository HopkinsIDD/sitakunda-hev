# reshape data for rstan
source(here("code", "R", "functions.R"))

data <- readRDS(here::here("data", "merged_clean.rds"))
age_breaks <- c(seq(0,76,2), 106)

# baseline
df1 <- reshape_data(data, "serostatus_r1_ud", age_breaks, pop = pop_sitk)
# follow-up
df2 <- reshape_data(data, "serostatus_r3_ud", age_breaks, pop = pop_sitk)

saveRDS(df1, file = here("data", "reshaped_stanR1.rds"))
saveRDS(df2, file = here("data", "reshaped_stanR3.rds"))

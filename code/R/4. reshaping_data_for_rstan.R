# reshape data for rstan
source(here("code", "R", "functions.R"))

data <- read_csv(here::here("data", "merged_clean.csv"))
age_breaks <- c(seq(0,76,2), 106)

# baseline
df1 <- reshape_data(data, "serostatus_r1_ud", age_breaks, pop = pop_sitk)
# follow-up
df2 <- reshape_data(data, "serostatus_r3_ud", age_breaks, pop = pop_sitk)

write_csv(df1, file = here("data", "reshaped_stanR1.csv"))
write_csv(df2, file = here("data", "reshaped_stanR3.csv"))

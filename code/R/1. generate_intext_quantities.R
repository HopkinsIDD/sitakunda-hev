#############################################################
## generates in-text quantities reported in the manuscript ##
#############################################################

source(here("code","R","functions.R"))

# read in the data
df <- read_csv(here("data", "merged_clean.csv"))

# range of follow-up times #####################################################
range(df$time_in_study, na.rm = T)


# seroprevalence at baseline ###################################################

## overall

### number
hh_sp <- df %>% filter(serostatus_r1_ud == "positive") %>% pull(hh_ID)
N_sp <- length(hh_sp)
N_hh_sp <- length(unique(hh_sp)) # across how many households

### percent
sp_esti <- sp(data = df, sero_col = "serostatus_r1_ud", pop = pop_sitk)
saveRDS(sp_esti, here("generated_data", "empirical", "sp.rds"))

## by sex - CIs accounting for study design
df1 <- sp_sex_clust(data = df, sero_col = "serostatus_r1_ud", pop = pop_sitk)

## in the south-eastern cluster
sp_cluster <- sp_clust_clust(data = df, sero_col = "serostatus_r1_ud", pop = pop_sitk)


# self-reported jaundice #######################################################

## how many people had jaundice who were tested at both timepoints?
jaundice <- df %>% filter(!is.na(seroconverted_ud)) %>% pull(ind_hev_r3)
sum(jaundice == "yes", na.rm = T)

## how many people had ACUTE jaundice overall
jaundice_acute <- df %>% pull(jaundice_acute)
sum(jaundice_acute == "yes", na.rm = T)

## who had jaundice between round 1 and 3?
df <- df %>% mutate(jaun_during = case_when(dt_hev_r3 > dt_interview_r1 ~ 1,
                                            dt_hev_r3 < dt_interview_r1 ~ 0,
                                            TRUE ~ as.numeric(NA)))
## were these all acute jaundice? YES
df %>% filter(jaun_during == 1) %>% pull(ind_hev_time_r3)

## were any of these seroconverters?
df %>% filter(jaun_during == 1) %>% pull(seroconverted_ud)


# seroconverters - number and annual risk ######################################

## overall
sc_n <- sum(df$seroconverted_ud == "YES", na.rm = T)
sc_text <- sc_rate(df, "time_at_risk")
saveRDS(sc_text, here("generated_data", "empirical", "sc.rds"))

## by age
sc_text_u18 <- sc_rate(df %>% filter(n_age<=18), "time_at_risk") # arbitrary child/adult cutoff
sc_text_o18 <- sc_rate(df %>% filter(n_age>18), "time_at_risk") 

sc_text_u30 <- sc_rate(df %>% filter(n_age<=30), "time_at_risk") # 30 chosen due to model fit
sc_text_o30 <- sc_rate(df %>% filter(n_age>30), "time_at_risk")
saveRDS(sc_text_u30, here("generated_data", "empirical", "sc_u30.rds"))
saveRDS(sc_text_o30, here("generated_data", "empirical", "sc_o30.rds"))

## by sex
sc_text_m <- sc_rate(df %>% filter(cat_sex == "male"), "time_at_risk")
sc_text_f <- sc_rate(df %>% filter(cat_sex == "female"), "time_at_risk")

## high seroprevalence cluster
sc_text_clust <- sc_rate(df %>% filter(cluster == 1), "time_at_risk")

## household spread
length(unique(df %>% filter(seroconverted_ud == "YES") %>% pull(hh_ID)))

# seroreverters - number and annual risk #######################################

## overall
sr_n <- sum(df$seroreverted_ud == "YES", na.rm = T)
sr_text <- sr_rate(df, "time_at_risk")
saveRDS(sr_text, here("generated_data", "empirical", "sr.rds"))

## by age
sr_text_u10 <- sr_rate(df %>% filter(n_age<=10), "time_at_risk") # chosen due to appearance of age-stratified data
sr_text_o10 <- sr_rate(df %>% filter(n_age>10), "time_at_risk")
saveRDS(sr_text_u10, here("generated_data", "empirical", "sr_u10.rds"))
saveRDS(sr_text_o10, here("generated_data", "empirical", "sr_o10.rds"))

sr_text_u18 <- sr_rate(df %>% filter(n_age<=18), "time_at_risk") # arbitrary child/adult cut-off
sr_text_o18 <- sr_rate(df %>% filter(n_age>18), "time_at_risk")

## by sex
sr_text_f <- sr_rate(df %>% filter(cat_sex == "female"), "time_at_risk")
sr_text_m <- sr_rate(df %>% filter(cat_sex == "male"), "time_at_risk")

## household spread
length(unique(df %>% filter(seroreverted_ud == "YES") %>% pull(hh_ID)))

# infections ###################################################################

## read in seroprevalence and seroconversion rate
sp <- readRDS(here("generated_data", "empirical", "sp.rds"))[1]
sc <- readRDS(here("generated_data", "empirical", "sc.rds"))$sc

## take away those who are seropositive
susc_2021 <- pop_sitk * (1 - sp)

## apply rate of infection
infections <- susc_2021 * sc


source(here("code", "R", "functions.R"))

## read in clean data
df_hh <- readRDS(here("data", "merged_clean.rds"))

data_m1 <- df_hh %>% 
  filter(serostatus_r1_ud == "positive" | serostatus_r1_ud == "negative")

## format na
data_m1$ind_source_primary_r1[data_m1$ind_source_primary_r1 == "na"] <- NA
data_m1$ind_source_primary_r1[data_m1$ind_source_primary_r1 == "na"] <- NA
data_m1$ind_hev_r3[data_m1$ind_hev_r3 == "decline"] <- NA
data_m1$cat_activity_month_r1[data_m1$cat_activity_month_r1 == "decline"] <- NA


## re-code dependent var as 0 and 1 
data_m1$serostatus_r1_ud <- recode(data_m1$serostatus_r1_ud, positive = 1, negative = 0)
data_m1$seroconverted_ud <- recode(data_m1$seroconverted_ud, YES = 1, NO = 0)

## bin age
data_m1 <- data_m1 %>%
  mutate(age_bin = cut(n_age, breaks = c(0, 5, 15, 40, 100)))


## re-level factors for reference categories
data_m1 <- data_m1 %>%
  mutate(last_left = factor(last_left, 
                            levels = c("> 1 year", "1 month - 1 year", 
                                       "1 week - 1 month", "yes", "< 1 week")),
         ind_source_primary_r1 = factor(ind_source_primary_r1, 
                                        levels = c("sufficient", "insufficient")),
         income_bin = factor(income_bin, levels = c( ">10k", 
                                                     "<10k")),
         cat_type = factor(cat_type, 
                           levels = c("house", "separate_structures",
                                      "flat_multistory", "flat", "room")),
         sanitation_rank = factor(sanitation_rank,
                                  levels = c("improved private",
                                             "improved shared", "unimproved")),
         cat_activity_month_r1 = factor(cat_activity_month_r1,
                                        levels = c("homeworker", "business/outside", "farmer",
                                                   "student", "child", "none", "other")))

############################################
# perform some sanity checks on occupation #
############################################

# occupation (cat_activity_month_r1)

# are any children listed as having no occupation rather than as "child"?
ggplot(data_m1 %>% filter(cat_activity_month_r1 == "none"))+
  geom_histogram(aes(x = n_age))+
  theme_minimal()

# are any adults listed as "child"?
ggplot(data_m1 %>% filter(cat_activity_month_r1 == "child"))+
  geom_histogram(aes(x = n_age))+
  theme_minimal()

# are students mostly children and young adults?
ggplot(data_m1 %>% filter(cat_activity_month_r1 == "student"))+
  geom_histogram(aes(x = n_age))+
  theme_minimal()

# what does "other" represent?
table(data_m1 %>% filter(cat_activity_month_r1 == "other") %>% pull(txt_activity_month_other_r1))


############################################
# univariate (unadjusted ORs), binned age ##
############################################

data_uni <- data_m1 %>% 
  dplyr::select(serostatus_r1_ud, cat_sex, age_bin, cat_activity_month_r1,
                last_left, ind_source_primary_r1, piped_r1, 
                tubewell_r1, tap_r1, jaundice_acute, cat_type, income_bin, 
                sanitation_rank, mammals, hh_ID)


glmer_gen <- function(var) {
  
  m <- lme4::glmer(serostatus_r1_ud ~ var + (1|hh_ID), 
                   data = data_uni, family = "binomial")
  se <- sqrt(diag(vcov(m)))
  tab <- as.data.frame(exp(cbind(Est = fixef(m), 
                                 LL = fixef(m) - 1.96 * se, 
                                 UL = fixef(m) + 1.96 * se)))
}

ORs_uni <- lapply(data_uni[,2:((dim(data_uni)[2]) - 1)], glmer_gen)
ORs_uni <- bind_rows(ORs_uni, .id = "column_label")
tab <- data.frame(characteristic = ORs_uni$column_label,
                  options = rownames(ORs_uni), 
                  OR = paste0(round(ORs_uni$Est, 1), 
                              " (", round(ORs_uni$LL, 1), ", ", 
                              round(ORs_uni$UL, 1), ")"))

# save
saveRDS(file = "generated_data/regression/ORs_uni.rds", tab)
write.csv(file = "generated_data/regression/ORs_uni.csv", tab)

#############################################
## let's look at occupation in adults only ##
#############################################

# 1. univariate
data_uni_adult <- data_m1 %>%
  filter(n_age >=18) %>%
  dplyr::select(serostatus_r1_ud, cat_sex, age_bin, cat_activity_month_r1, hh_ID)

glmer_gen <- function(var) {
  
  m <- lme4::glmer(serostatus_r1_ud ~ var + (1|hh_ID), 
                   data = data_uni_adult, family = "binomial")
  se <- sqrt(diag(vcov(m)))
  tab <- as.data.frame(exp(cbind(Est = fixef(m), 
                                 LL = fixef(m) - 1.96 * se, 
                                 UL = fixef(m) + 1.96 * se)))
}


ORs_uni_adult <- lapply(data_uni_adult[,2:((dim(data_uni_adult)[2]) - 1)], glmer_gen)
ORs_uni_adult <- bind_rows(ORs_uni_adult, .id = "column_label")
tab_adult <- data.frame(characteristic = ORs_uni_adult$column_label,
                  options = rownames(ORs_uni_adult), 
                  OR = paste0(round(ORs_uni_adult$Est, 1), 
                              " (", round(ORs_uni_adult$LL, 1), ", ", 
                              round(ORs_uni_adult$UL, 1), ")"))

# 2. adjusting for age and sex
m1_adult <- lme4::glmer(serostatus_r1_ud ~ age_bin + cat_sex + cat_activity_month_r1 +
                    (1|hh_ID), data = data_uni_adult, family = "binomial", nAGQ = 0)

## summary
sum_glmer <- summary(m1_adult)


## standard error
se <- sqrt(diag(vcov(m1_adult)))


## table of ORs with 95% CI (note exp to get ORs)
tab_adult_m <- exp(cbind(Est = fixef(m1_adult), 
                 LL = fixef(m1_adult) - 1.96 * se, 
                 UL = fixef(m1_adult) + 1.96 * se))


# convert to dataframe
tab_adult_m <- as.data.frame(tab_adult_m)

tab_adult_m


###################################################################
## multivariate analysis - inc. significant/borderline univar +  ##
###################################################################

m1 <- lme4::glmer(serostatus_r1_ud ~ age_bin + cat_sex + ind_source_primary_r1 + 
                    piped_r1 + income_bin + cat_activity_month_r1 +
                    (1|hh_ID), data = data_m1, family = "binomial", nAGQ = 0)

## summary
sum_glmer <- summary(m1)


## standard error
se <- sqrt(diag(vcov(m1)))


## table of ORs with 95% CI (note exp to get ORs)
tab <- exp(cbind(Est = fixef(m1), 
                 LL = fixef(m1) - 1.96 * se, 
                 UL = fixef(m1) + 1.96 * se))


# convert to dataframe
tab <- as.data.frame(tab)


# pretty table
tab <- data.frame(characteristic_option = rownames(tab),
                  OR = paste0(round(tab$Est, 1), 
                              " (", round(tab$LL, 1), ", ", 
                              round(tab$UL, 1), ")"))

# save
saveRDS(file = "generated_data/regression/ORs_multi.rds", tab)
write.csv(file = "generated_data/regression/ORs_multi.csv", tab)


#########
## ICC ##
#########

# ICC without covariates etc
m3 <- lme4::glmer(serostatus_r1_ud ~ (1|hh_ID),
                  data = data_m1, family = "binomial", nAGQ = 0)
ICC_0 <- performance::icc(m3)
saveRDS(file = "generated_data/regression/ICC_0.rds", ICC_0)

# look at design effect
hh_sum <- hh_summarise(data_m1)

mean_n <- mean(hh_sum$n)

1 + ICC_0 * (mean_n - 1)


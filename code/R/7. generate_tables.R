# generate tables for manuscript
source(here("code", "R", "functions.R"))
# read in the data
df <- read_csv(here("data", "merged_clean.csv"))


#############################################################################
# Table S1: Number of individuals and households sampled during the study. ##
#############################################################################

## id

visited_id <- c(r1 = dim(df %>% filter(!is.na(round_r1)))[1],
                r3 = dim(df %>% filter(!is.na(round_r3)))[1],
                both = dim(df %>% filter(round_r3 == "r3f"))[1])

sampled_id <- c(r1 = dim(df %>% filter(blood_sampled == "r1_only" | blood_sampled == "both_rounds"))[1],
                r3 = dim(df %>% filter(blood_sampled == "r3_only" | blood_sampled == "both_rounds"))[1],
                both = dim(df %>% filter(blood_sampled == "both_rounds"))[1])

results_id <- c(r1 = dim(df %>% filter(results_ud == "r1_only" | results_ud == "both_rounds"))[1],
                r3 = dim(df %>% filter(results_ud == "r3_only" | results_ud == "both_rounds"))[1],
                both = dim(df %>% filter(results_ud == "both_rounds"))[1])

clear_id <- c(r1 = dim(df %>% filter(serostatus_r1_ud == "positive" | serostatus_r1_ud == "negative" ))[1],
              r3 = dim(df %>% filter(serostatus_r3_ud == "positive" | serostatus_r3_ud == "negative" ))[1],
              both = dim(df %>% filter((serostatus_r1_ud == "positive" | serostatus_r1_ud == "negative" ) &
                                         (serostatus_r3_ud == "positive" | serostatus_r3_ud == "negative") ))[1])

## hh

visited_hh <- c(r1 = length(unique(df %>% filter(!is.na(round_r1)) %>% pull(hh_ID))),
                r3 = length(unique(df %>% filter(!is.na(round_r3)) %>% pull(hh_ID))),
                both = length(unique(df %>% filter(round_r3 == "r3f") %>% pull(hh_ID))))

sampled_hh <- c(r1 = length(unique(df %>% filter(blood_sampled == "r1_only" | blood_sampled == "both_rounds") %>%
                                     pull(hh_ID))),
                r3 = length(unique(df %>% filter(blood_sampled == "r3_only" | blood_sampled == "both_rounds") %>%
                                     pull(hh_ID))),
                both = length(unique(df %>% filter(blood_sampled == "both_rounds") %>%
                                       pull(hh_ID))))

results_hh <- c(r1 = length(unique(df %>% filter(results_ud == "r1_only" | results_ud == "both_rounds") %>%
                                     pull(hh_ID))),
                r3 = length(unique(df %>% filter(results_ud == "r3_only" | results_ud == "both_rounds") %>%
                                     pull(hh_ID))),
                both = length(unique(df %>% filter(results_ud == "both_rounds") %>%
                                       pull(hh_ID))))

clear_hh <- c(r1 = length(unique(df %>% filter(serostatus_r1_ud == "positive" | serostatus_r1_ud == "negative" ) %>% 
                                   pull(hh_ID))),
              r3 = length(unique(df %>% filter(serostatus_r3_ud == "positive" | serostatus_r3_ud == "negative" ) %>%
                                   pull(hh_ID))),
              both = length(unique(df %>% filter((serostatus_r1_ud == "positive" | serostatus_r1_ud == "negative" ) &
                                                   (serostatus_r3_ud == "positive" | serostatus_r3_ud == "negative")) %>% pull(hh_ID))))

tableS1 <- rbind(visited_id, visited_hh, 
                 sampled_id, sampled_hh, 
                 results_id, results_hh,
                 clear_id, clear_hh)

write.csv(tableS1, file = "tables/tableS1.csv")


#####################################################################################
# Table 1. and Table S2: Potential risk factors for past HEV infection at baseline ##
#####################################################################################

key_vars <- c("cat_sex", "age_bin", "cat_activity_month_r1","last_left",  
              "ind_source_primary_r1", "source_rank_r1", 
              "piped_r1", "tubewell_r1", "tap_r1",
              "jaundice_acute",
              "cat_type", "income_bin", "sanitation_rank", "mammals")

df$source_rank_r1[df$source_rank_r1 == "na"] <- NA
df$ind_source_primary_r1[df$ind_source_primary_r1 == "na"] <- NA
df$ind_hev_r3[df$ind_hev_r3 == "decline"] <- NA
df$cat_activity_month_r1[df$cat_activity_month_r1 == "decline"] <- NA

df_HEV_tested <- df %>% filter(serostatus_r1_ud == "positive" | serostatus_r1_ud == "negative") %>%
  mutate(age_bin = cut(n_age, breaks = c(0, 5, 15, 40, 100)),
         last_left = factor(last_left, 
                            levels = c("> 1 year", "1 month - 1 year", 
                                       "1 week - 1 month", "yes", "< 1 week")),
         ind_source_primary_r1 = factor(ind_source_primary_r1, 
                                        levels = c("sufficient", "insufficient")),
         cat_type = factor(cat_type, 
                           levels = c("house", "separate_structures",
                                      "flat_multistory", "flat", "room")),
         sanitation_rank = factor(sanitation_rank,
                                  levels = c("improved private",
                                             "improved shared", "unimproved")),
         cat_activity_month_r1 = factor(cat_activity_month_r1,
                                        levels = c("homeworker", "business/outside", "farmer",
                                                   "student", "child", "none", "other"))) %>%
  mutate(piped_r1 = as.character(piped_r1),
         tubewell_r1 = as.character(tubewell_r1),
         tap_r1 = as.character(tap_r1),
         mammals = as.character(mammals))


df_HEV_tested <- df_HEV_tested %>% 
  dplyr::select(serostatus_r1_ud, cat_sex, age_bin, cat_activity_month_r1,
                last_left, ind_source_primary_r1, source_rank_r1, piped_r1, 
                tubewell_r1, tap_r1, jaundice_acute, cat_type, income_bin, 
                sanitation_rank, mammals, cluster)

group_sum <- function(data, key_vars){
  
  grouped_sum <- list(length = length(key_vars) - 1)
  
  for(i in 2:(length(key_vars)+1)){
    
    names(data)[names(data) == key_vars[i-1]] <- "var"
    
    grouped_sum[[i-1]] <- data %>% group_by(var) %>%
      summarise(n())
    
    names(data)[2:(length(key_vars)+1)] <- key_vars
    
  }
  names(grouped_sum) <- key_vars
  grouped_sum_df <- bind_rows(grouped_sum, .id = "column_label") %>%
    mutate(percent_tot = 100 * `n()` / dim(data)[1])
  
  return(grouped_sum_df)
}

g_all <- group_sum(data = df_HEV_tested, key_vars = key_vars)%>% 
  mutate("N(%)" = paste0(`n()`, " (", round(percent_tot, 0), "%)")) %>%
  dplyr::select(column_label, var, `N(%)`)

g_male <- group_sum(data = df_HEV_tested %>% filter(cat_sex == "male"), key_vars = key_vars)%>% 
  mutate("N(%)" = paste0(`n()`, " (", round(percent_tot, 0), "%)")) %>%
  dplyr::select(column_label, var, `N(%)`)

g_female <- group_sum(data = df_HEV_tested %>% filter(cat_sex == "female"), key_vars = key_vars)%>% 
  mutate("N(%)" = paste0(`n()`, " (", round(percent_tot, 0), "%)")) %>%
  dplyr::select(column_label, var, `N(%)`)

g_cluster <- group_sum(data = df_HEV_tested %>% filter(cluster == 1), key_vars = key_vars)%>% 
  mutate("N(%)" = paste0(`n()`, " (", round(percent_tot, 0), "%)")) %>%
  dplyr::select(column_label, var, `N(%)`)

# now for the seropositive individuals at baseline:
df_HEV_sp <- df_HEV_tested %>% filter(serostatus_r1_ud == "positive")
g_sp <- group_sum(data = df_HEV_sp, key_vars = key_vars)%>% 
  mutate("N(%)" = paste0(`n()`, " (", round(percent_tot, 0), "%)")) %>%
  dplyr::select(column_label, var, `N(%)`)

# now for the seronegative individuals at baseline:
df_HEV_sn <- df_HEV_tested %>% filter(serostatus_r1_ud == "negative")
g_sn <- group_sum(data = df_HEV_sn, key_vars = key_vars)%>% 
  mutate("N(%)" = paste0(`n()`, " (", round(percent_tot, 0), "%)")) %>%
  dplyr::select(column_label, var, `N(%)`)

# put all data frames into list
t1_list <- list(g_all, g_male, g_female, g_cluster, g_sp, g_sn)

# merge all data frames in list
table_1 <- t1_list %>% reduce(full_join, by= c("column_label", "var")) %>%
  mutate(id = 1:(dim(g_all)[1]))

# rename cols intuitively
names(table_1) <- c("characteristic", "options", 
                    "all tested n(%)", "male n(%)", "female n(%)", "cluster n(%)",
                    "seropositive n(%)", "seronegative n(%)",
                    "id")

# read in univariate OR table
ORs_uni <- readRDS("generated_data/regression/ORs_uni.rds")

# drop intercepts
ORs_uni_col <- ORs_uni[- grep(x = ORs_uni$options, pattern = "Intercept"), ] %>%
  transform(options=str_replace(options,"var",""))
# for binary variables, format to include reference as "1" rather than ...
ORs_uni_col$options[grep(ORs_uni_col$options, pattern = "\\...")] <- "1"

# merge into table 1
table_1 <- merge(table_1, ORs_uni_col, by = c("characteristic", "options"), all = T)

## read in multivariate OR tables
ORs_multi1 <- readRDS("generated_data/regression/ORs_multi.rds")
ORs_multi1 <- ORs_multi1[- grep(x = ORs_multi1$characteristic_option, pattern = "Intercept"), ]


# merge into table 1 using characteristic_option
table_1 <- table_1 %>% 
  mutate(characteristic_option = paste0(characteristic, options),
         characteristic_option = recode(characteristic_option, 
                                        piped_r11 = "piped_r1",
                                        mammals1 = "mammals"))

# put all data frames into list
t2_list <- list(table_1, ORs_multi1)

# merge all data frames in list
table_1_complete <- t2_list %>% 
  reduce(full_join, by= c("characteristic_option")) %>%
  dplyr::select(-characteristic_option) %>%
  rename(OR_univariate = OR.x,
         OR_multivariate1 = OR.y) %>%
  arrange(id) %>%
  dplyr::select(-id)

# save
write.csv(file = "tables/table1_2.csv", table_1_complete)

############################################################################
# Table S3. The expected log predicted density (ELPD) as estimated using  ##
#           Leave One Out Cross-validation                                ##
############################################################################

## read in loo to compare
m1a_loo <- readRDS("generated_data/model_fits/loo/m1a.rds")
m1b_loo <- readRDS("generated_data/model_fits/loo/m1b.rds")
m2a_loo <- readRDS("generated_data/model_fits/loo/m2a.rds")
m2b_loo <- readRDS("generated_data/model_fits/loo/m2b.rds")
m3a_loo <- readRDS("generated_data/model_fits/loo/m3a.rds")
m3b_loo <- readRDS("generated_data/model_fits/loo/m3b.rds")

# extract best fitting models for age varying FoI (best fitting age split)
m1b_loo <- m1b_loo[[7]]
m2b_loo <- m2b_loo[[11]]
m3b_loo <- m3b_loo[[10]]

loo_comp <- loo_compare(list(m1a = m1a_loo, m2a = m2a_loo,  m3a = m3a_loo, 
            m1b = m1b_loo, m2b = m2b_loo, m3b = m3b_loo))

write.csv(file = "tables/table_S3.csv", loo_comp)


#############################################################################
# Table S4. A comparison of annual risk of infection estimates when using  ##
#           age-stratified seroprevalence at baseline versus at follow-up. ##
#############################################################################

r1 <- (readRDS(here("generated_data", "model_fits", "summaries", "2b", "30.rds")))$summary
r3 <- (readRDS(here("generated_data", "model_fits", "summaries", "2b_follow_up", "30.rds")))$summary

tb <- data.frame(round = c("baseline", "follow-up"),
                 FoI1 = c(paste0(round(r1[1, "mean"], 3), 
                                 "(", round(r1[1, "2.5%"], 3), ", ",
                                 round(r1[1, "97.5%"], 3), ")"),
                          (paste0(round(r3[1, "mean"], 3), 
                                  "(", round(r3[1, "2.5%"], 3), ", ",
                                  round(r3[1, "97.5%"], 3), ")"))),
                 FoI2 = c(paste0(round(r1[2, "mean"], 3), 
                                 "(", round(r1[2, "2.5%"], 3), ", ",
                                 round(r1[2, "97.5%"], 3), ")"),
                          (paste0(round(r3[2, "mean"], 3), 
                                  "(", round(r3[2, "2.5%"], 3), ", ",
                                  round(r3[2, "97.5%"], 3), ")"))))

write.csv(file = "tables/table_S4.csv", tb)


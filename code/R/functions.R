###############
## FUNCTIONS ##
###############
source("dependencies.R")

## Seroprevalence

### seroprevalence overall
sp <- function(data, sero_col, pop){
  
  df <- data %>%
    rename(serostatus = sero_col) %>%
    mutate(serostatus = case_when(serostatus == "positive" ~1,
                                  serostatus == "negative" ~0,
                                  serostatus == "borderline_n" ~ NA,
                                  serostatus == "borderline_p" ~ NA)) %>%
    drop_na(serostatus)
    
  mydesign <-
    svydesign(
      ids = ~df$hh_ID, # household id
      data = df,  # data
      fpc = ~rep(pop, dim(df)[1])) # total population in Sitakunda
  
  d <- svyciprop(formula = ~serostatus,
                 design = mydesign, 
                 deff = TRUE,
                 level = 0.95)
  
  return(d)
  
}

### seroprevalence by age
#### accounting for study design - FINE
sp_age_clust_fine <- function(data, sero_col, pop, div) {
  
  hev <- data %>% 
    rename(serostatus = sero_col)%>%
    mutate(bin = cut(n_age, breaks = c(seq(0,70,div), 100)),
           serostatus = case_when(serostatus == "positive" ~1,
                                  serostatus == "negative" ~0,
                                  serostatus == "borderline_n" ~ NA,
                                  serostatus == "borderline_p" ~ NA)) %>%
    drop_na(serostatus)
  
  mydesign <-
    svydesign(
      ids = ~hev$hh_ID, # household id
      data = hev,  # data
      fpc = ~rep(pop, dim(hev)[1])) # total population in Sitakunda
  
  d_age <- svyby(formula = ~serostatus, 
                 by = ~bin, 
                 design = mydesign, 
                 deff = TRUE, 
                 svyciprop, 
                 vartype = "ci", 
                 level = 0.95) }

#### accounting for study design - COARSE
sp_age_clust <- function(data, sero_col, pop) {
  
  hev <- data %>% 
    rename(serostatus = sero_col)%>%
    mutate(bin = cut(n_age, breaks = c(0,10, 40, 100)),
           serostatus = case_when(serostatus == "positive" ~1,
                                  serostatus == "negative" ~0,
                                  serostatus == "borderline_n" ~ NA,
                                  serostatus == "borderline_p" ~ NA)) %>%
    drop_na(serostatus)
  
  mydesign <-
    svydesign(
      ids = ~hev$hh_ID, # household id
      data = hev,  # data
      fpc = ~rep(pop, dim(hev)[1])) # total population in Sitakunda
  
  d_age <- svyby(formula = ~serostatus, 
                 by = ~bin, 
                 design = mydesign, 
                 deff = TRUE, 
                 svyciprop, 
                 vartype = "ci", 
                 level = 0.95) }


plot_sp_age_clust <- function(data, sero_col, pop){
  
  d_age <- sp_age_clust(data, sero_col, pop)
  
  ggplot(d_age, aes(x=bin, y= 100*serostatus)) +
    geom_point()+
    geom_errorbar(aes(ymin = 100*ci_l, ymax = 100*ci_u),
                  width = 0)+
    xlab("Age (years)")+
    ylab("Seroprevalence (%)")+
    theme_minimal()
}



### seroprevalence by sex
#### accounting for study design
sp_sex_clust <- function(data, sero_col, pop){
  
  hev <- data %>% 
    rename(serostatus = sero_col)%>%
    mutate(bin = cut(n_age, breaks = c(seq(0,70,5), 100)),
           serostatus = case_when(serostatus == "positive" ~1,
                                  serostatus == "negative" ~0,
                                  serostatus == "borderline_n" ~ NA,
                                  serostatus == "borderline_p" ~ NA)) %>%
    filter(!is.na(serostatus))
  
  mydesign <-
    svydesign(
      ids = ~hev$hh_ID, # household id
      data = hev,  # data
      fpc = ~rep(pop, dim(hev)[1])) # total population in Sitakunda
  
  d_sex <- svyby(formula = ~serostatus, 
                 by = ~cat_sex, 
                 design = mydesign, 
                 deff = TRUE, 
                 svyciprop, 
                 vartype = "ci",
                 level = 0.95) 
}

plot_sp_sex_clust <- function(data, sero_col, pop){
  
  d_sex <- sp_sex_clust(data, sero_col, pop)
  
  ggplot(d_sex, aes(x = cat_sex, y= 100*serostatus,  col = cat_sex)) +
    geom_point()+
    geom_errorbar(aes(ymin = 100*ci_l, ymax = 100*ci_u),
                  width = 0)+
    scale_color_manual(name = "Sex", values = c("firebrick", "dodgerblue"))+
    xlab(" ")+
    ylab("Seroprevalence (%)")+
    ylim(0,NA)+
    theme_minimal()+
    theme(text = element_text(size = 13))
}

### seroprevalence by age and sex

#### accounting for study design
sp_age_sex_clust <- function(data, sero_col, pop){
  
  hev <- data %>% 
    rename(serostatus = sero_col)%>%
    mutate(bin = cut(n_age, breaks = c(seq(0,70,5), 100)),
           serostatus = case_when(serostatus == "positive" ~1,
                                  serostatus == "negative" ~0,
                                  serostatus == "borderline_n" ~ NA,
                                  serostatus == "borderline_p" ~ NA)) %>%
    drop_na(serostatus)
  
  mydesign <-
    svydesign(
      ids = ~hev$hh_ID, # household id
      data = hev,  # data
      fpc = ~rep(pop, dim(hev)[1])) # total population in Sitakunda
  
  d_age_sex <- svyby(formula = ~serostatus, 
                     by = ~(bin + cat_sex), 
                     design = mydesign, 
                     deff = TRUE, 
                     svyciprop, 
                     vartype = "ci", 
                     level = 0.95)
}

plot_sp_age_sex_clust <- function(data, sero_col, pop){
  
  d_age_sex <- sp_age_sex_clust(data, sero_col, pop)
  
    ggplot(d_age_sex, aes(x=bin, y= 100*serostatus, col = cat_sex)) +
    geom_point(position = position_dodge(0.5))+
    geom_errorbar(aes(ymin = 100*ci_l, ymax = 100*ci_u),
                  width = 0, position = position_dodge(0.5))+
    scale_color_manual(name = "Sex", values = c("firebrick", "dodgerblue"))+
    xlab("Age (years)")+
    ylab("Seroprevalence (%)")+
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust=1),
          text = element_text(size = 13))
}

# rate of seroconversion (taking into account person-time)

sc_rate <- function(data, tar_col){
  
  data$tar <- data[, c(tar_col)]
  
  tot_py_neg <- as.numeric(sum(data %>% filter(!is.na(seroconverted_ud),
                                               serostatus_r1_ud == "negative") %>% 
                                 pull(tar)) / 365)
  n <- sum((data %>% pull(seroconverted_ud)) == "YES", na.rm = T)
  sc_py <- dim(data %>% filter(seroconverted_ud == "YES"))[1] / tot_py_neg
  sc_nL <- (poisson.test(n))$conf.int[1] / tot_py_neg
  sc_nU <- (poisson.test(n))$conf.int[2] / tot_py_neg
  sc_text <- c("sc" = sc_py, "low" = sc_nL, "upp" = sc_nU)
  
  return(list(sc = sc_text,
              sc_n = n,
              tot_py_neg = tot_py_neg))
}

# rate of seroreversion (taking into account person-time)

sr_rate <- function(data, tar_col){
  
  data$tar <- data[, c(tar_col)]
  
  tot_py_pos <- as.numeric(sum(data %>% filter(!is.na(seroreverted_ud),
                                               serostatus_r1_ud == "positive") %>% 
                                 pull(tar)) / 365)
  n <- sum((data %>% pull(seroreverted_ud)) == "YES", na.rm = T)
  sr_py <- dim(data %>% filter(seroreverted_ud == "YES"))[1] / tot_py_pos
  sr_nL <- (poisson.test(n))$conf.int[1] / tot_py_pos
  sr_nU <- (poisson.test(n))$conf.int[2] / tot_py_pos
  sr_text <- c("sr" = sr_py, "low" = sr_nL, "upp" = sr_nU)
  
  return(list(sr = sr_text,
              sr_n = n,
              tot_py_pos = tot_py_pos))
}

## seroprevalence by cluster versus non cluster
sp_clust_clust <- function(data, sero_col, pop){
  
  hev <- data %>% 
    rename(serostatus = sero_col)%>%
    mutate(serostatus = case_when(serostatus == "positive" ~1,
                                  serostatus == "negative" ~0,
                                  serostatus == "borderline_n" ~ NA,
                                  serostatus == "borderline_p" ~ NA)) %>%
    filter(!is.na(serostatus))
  
  mydesign <-
    svydesign(
      ids = ~hev$hh_ID, # household id
      data = hev,  # data
      fpc = ~rep(pop, dim(hev)[1])) # total population in Sitakunda
  
  d_cluster <- svyby(formula = ~serostatus, 
                 by = ~cluster, 
                 design = mydesign, 
                 deff = TRUE, 
                 svyciprop, 
                 vartype = "ci",
                 level = 0.95) 
}

## seroconversion by cluster versus non cluster
sc_clust_clust <- function(data, pop){
  
  hev <- data %>% filter(!is.na(seroconverted_ud),
                            serostatus_r1_ud == "negative")%>%
    mutate(seroconverted = case_when(seroconverted_ud == "YES" ~1,
                                    seroconverted_ud == "NO" ~0))
  mydesign <-
    svydesign(
      ids = ~hev$hh_ID, # household id
      data = hev,  # data
      fpc = ~rep(pop, dim(hev)[1])) # total population in Sitakunda
  
  d_age_sc <- svyby(formula = ~seroconverted, 
                    by = ~cluster, 
                    design = mydesign, 
                    deff = TRUE, 
                    svyciprop, 
                    vartype = "ci", 
                    level = 0.95)
}

## seroreversion by cluster versus non cluster
sr_clust_clust <- function(data, pop){
  
  hev <- data %>% filter(!is.na(seroreverted_ud),
                            serostatus_r1_ud == "positive")%>%
    mutate(seroreverted = case_when(seroreverted_ud == "YES" ~1,
                                    seroreverted_ud == "NO" ~0))
  mydesign <-
    svydesign(
      ids = ~hev$hh_ID, # household id
      data = hev,  # data
      fpc = ~rep(pop, dim(hev)[1])) # total population in Sitakunda
  
  d_age_sr <- svyby(formula = ~seroreverted, 
                    by = ~cluster, 
                    design = mydesign, 
                    deff = TRUE, 
                    svyciprop, 
                    vartype = "ci", 
                    level = 0.95)
}

#########
## FoI ##
#########

reshape_data <- function(data, sero_col, age_breaks, pop){
  
  # group by age class and extract N and N pos
  df <- data %>%
    rename(serostatus = sero_col) %>%
    mutate(age_class = cut(n_age, breaks = age_breaks),
           seropositive = case_when(serostatus == "positive" ~1,
                                    serostatus == "negative" ~0,
                                    serostatus == "borderline_n" ~NA,
                                    serostatus == "borderline_p" ~NA)) %>%
    drop_na(seropositive)
  
  mydesign <-
    svydesign(
      ids = ~df$hh_ID, # household id
      data = df,  # data
      fpc = ~rep(pop, dim(df)[1])) # total population in Sitakunda
  
  d_age <- svyby(formula = ~seropositive, 
                 by = ~age_class, 
                 design = mydesign, 
                 deff = TRUE, 
                 svyciprop, 
                 vartype = "ci",
                 level = 0.95)
  
  df <- df%>%
    group_by(age_class) %>%
    summarise(N = n(),
              pos = sum(seropositive))
  
  df <- merge(df, d_age, by = "age_class")
  
  # order by age class before adding age1 and 2
  df <- df[order(df$age_class),]
  
  # age boundaries (cut uses the lower bound as >)
  df$age1 <- c(0, (age_breaks[2:(length(df$age_class))] + 0.1))
  df$age2 <- age_breaks[2:(length(age_breaks))]
  
  return(df)
}

## looking at catalytic model fits

plot_fits <- function(data, model_fit){
  
  sp_fit_df <- as.data.frame((model_fit$summary)[grep("seroprevalence", x = rownames(model_fit$summary)),])
  df <- data %>%
    mutate(s = pos/N,
           age_mid = age1 + ((age2 - age1) / 2))
  df$sp_fit <- sp_fit_df$mean
  df$sp_fit_cil <- sp_fit_df$`2.5%`
  df$sp_fit_ciu <- sp_fit_df$`97.5%`
  
  ggplot(df)+
    geom_errorbar(aes(x = age_mid, y = s, xmin = age1, xmax = age2, ymin = ci_l, ymax = ci_u), width = 0, col = "pink", lwd = 1.5)+
    geom_point(aes(x = age_mid, y = s), col = "red", size = 1.5)+
    geom_line(aes(x = age_mid, y = sp_fit), col = "blue", size = 1)+
    geom_ribbon(aes(x = age_mid, ymin = sp_fit_cil, ymax = sp_fit_ciu), 
                fill = "blue", alpha = 0.5, col = NA)+
    theme_minimal()+
    theme(text = element_text(size = 15),
          axis.title = element_blank())
}

plot_fits_uncert <- function(data, model_fit){
  
  sp_fit_mean <- matrix(nrow = 39, ncol = 100, byrow = F,
                        (model_fit$summary)[grep("seroprevalence", x = rownames(model_fit$summary)),colnames(model_fit$summary) == "mean"])
  sp_fit_cil <- matrix(nrow = 39, ncol = 100, byrow = F,
                       (model_fit$summary)[grep("seroprevalence", x = rownames(model_fit$summary)),colnames(model_fit$summary) == "2.5%"])
  sp_fit_ciu <- matrix(nrow = 39, ncol = 100, byrow = F,
                       (model_fit$summary)[grep("seroprevalence", x = rownames(model_fit$summary)),colnames(model_fit$summary) == "97.5%"])
  
  df <- data %>%
    mutate(s = pos/N,
           age_mid = age1 + ((age2 - age1) / 2))
  df$sp_fit <- rowMeans(sp_fit_mean)
  df$sp_fit_cil <- rowMeans(sp_fit_cil)
  df$sp_fit_ciu <- rowMeans(sp_fit_ciu)
  
  ggplot(df)+
    geom_errorbar(aes(x = age_mid, y = s, xmin = age1, xmax = age2, ymin = ci_l, ymax = ci_u), width = 0, col = "pink", lwd = 1.5)+
    geom_point(aes(x = age_mid, y = s), col = "red", size = 1.5)+
    geom_line(aes(x = age_mid, y = sp_fit), col = "blue", size = 1)+
    geom_ribbon(aes(x = age_mid, ymin = sp_fit_cil, ymax = sp_fit_ciu), 
                fill = "blue", alpha = 0.5, col = NA)+
    theme_minimal()+
    theme(text = element_text(size = 15), axis.title = element_blank())
}


#############
## Mapping ##
#############

## transforms sf file to Bangladesh Transverse Mercator projection
transform_to_btm <- function(my_sf){
  st_transform(my_sf, crs="+proj=tmerc +lat_0=0 +lon_0=90 +k=0.9996 +x_0=500000 +y_0=0 +a=6377276.345 +b=6356075.41314024 +towgs84=283.7,735.9,261.1,0,0,0,0 +units=m +no_defs")
  
}

# for use on the merged clean data in "data/merged_clean.rds" - requires full dataset with GPS
hh_summarise <- function(data){
  
  # summarise --> 1 row per household
  
  # group by household
  
  hh_sum <- data %>% filter(serostatus_r1_ud == "positive" | serostatus_r1_ud == "negative" | 
                              serostatus_r3_ud == "positive" | serostatus_r3_ud == "negative")%>%
    group_by(hh_ID_r1)%>%
    filter(!is.na(hh_ID_r1)) %>%
    summarise(n = n(),
              seroprev_r1 = mean(serostatus_r1_ud == "positive", na.rm = T),
              seroprev_r3 = mean(serostatus_r3_ud == "positive", na.rm = T),
              psource_r1 = mean(ind_source_primary_r1 == "insufficient", na.rm = T),
              psource_r3 = mean(ind_source_primary_r3 == "insufficient", na.rm = T),
              piped_r1 = mean(piped_r1, na.rm = T),
              piped_r3 = mean(piped_r3, na.rm = T),
              ladder_r1 = mean(source_rank_r1 == "unimproved", na.rm = T),
              ladder_r3 = mean(source_rank_r3 == "unimproved", na.rm = T),
              sc = sum(seroconverted_ud == "YES", na.rm = T)>0,
              sr = sum(seroreverted_ud == "YES", na.rm = T)>0,
              sc_n = sum(seroconverted_ud == "YES", na.rm = T),
              sr_n = sum(seroreverted_ud == "YES", na.rm = T),
              sc_per_sp = sum(seroconverted_ud == "YES", na.rm = T) / 
                sum(serostatus_r1_ud == "negative", na.rm = T),
              sr_per_sp = sum(seroreverted_ud == "YES", na.rm = T) / 
                sum(serostatus_r1_ud == "positive", na.rm = T)
    ) %>% 
    mutate_at(vars(sc_per_sp, sr_per_sp), ~replace(., is.nan(.), 0))
  
  # pull across coordinates of each household
  
  # 1.  IDs successfully followed up
  
  hhID3 <- unique(data$hh_ID_r3)
  
  
  # 2. merge
  
  hh_sum <- merge(hh_sum, data[, c("hh_ID_r1", "Latitude_r1", "Longitude_r1",
                                   "Latitude_r3", "Longitude_r3", "n_resident_total.x", 
                                   "cat_income", "ind_livestock","txt_village", 
                                   "cat_sanitation_facility", "cat_sanitation_flush",
                                   "cat_sanitation_share", "cat_type", "cat_walls", "cat_floors")], 
                  by = "hh_ID_r1", all.y = FALSE) %>%
    distinct() %>%
    mutate(round = case_when(hh_ID_r1 %in% hhID3 ~ "both rounds",
                             !hh_ID_r1 %in% hhID3 ~ "round 1 only",
                             TRUE ~ as.character(NA)))
  
  
  # 3. H612510 doesn't have lat and long from r1 so use r3
  
  lat_612510 <- hh_sum %>% filter(hh_ID_r1 == "H612510") %>% pull(Latitude_r3)
  long_612510 <- hh_sum %>% filter(hh_ID_r1 == "H612510") %>% pull(Longitude_r3)
  
  hh_sum <- hh_sum %>%
    mutate(Latitude_r1 = replace(Latitude_r1, is.na(Latitude_r1), lat_612510),
           Longitude_r1 = replace(Longitude_r1, is.na(Longitude_r1), long_612510))
}


# create raster of smoothed serological indicator
# using inverse distance weighting

idw_smoothSeroSit <- function(data, sero_col, win, boundary, power){
  
  names(data)[which(names(data) == sero_col)] <- "var"
  
  # step 1. create a point pattern object using spatstat function ppp
  hhp_ppp <- ppp(data$Longitude_r1, data$Latitude_r1,
                 marks = data$var, 
                 window = as.owin(win))
  
  # step 2. create raster - interpolate using inverse-distance weighting
  idw_raster <- raster(idw(hhp_ppp, power = power, at="pixels"))
  crs(idw_raster) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
  
  # step 3. keep only points that fall within Sitakunda
  idw_masked <- mask(idw_raster, boundary)
  
  # step 4. convert masked raster into df for ggplot
  idw_masked_spdf <- as(idw_masked, "SpatialPixelsDataFrame")
  idw_masked_df <- as.data.frame(idw_masked_spdf)
  colnames(idw_masked_df) <- c("value", "x", "y")
  
  return(idw_masked_df)
}

# compare loo for different age boundaries for age-varying FoI models 1 & 3
extract_elpd <- function(x, n, ages){
  df <- matrix(nrow = n, ncol = 2)
  for(i in 1:n){
    df[i,1] <- x[[i]]$estimates[1,1]
    df[i,2] <- x[[i]]$estimates[1,2]
  }
  df <- as.data.frame(df)
  names(df) <- c("elpd", "SE")
  df$age <- ages
  
  df <- df %>% 
    mutate(low = elpd-SE,
           upp = elpd+SE)
  
  return(df)
}

# same for model 2a and b
extract_elpd_fixed <- function(x, n, ages){
  df <- matrix(nrow = n, ncol = 2)
  for(i in 1:n){
    df[i,1] <- x[[i]]$estimates[1,1]
    df[i,2] <- x[[i]]$estimates[1,2]
  }
  df <- as.data.frame(df)
  names(df) <- c("elpd", "SE")
  df$age <- ages
  
  df <- df %>% 
    mutate(low = elpd-SE,
           upp = elpd+SE)
  
  return(df)
}

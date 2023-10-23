# generate figure 2 for manuscript
source(here("code", "R", "functions.R"))

# read in empirical estimates
sc <- readRDS("generated_data/empirical/sc.rds")
sc_u30 <- readRDS("generated_data/empirical/sc_u30.rds")
sc_o30 <- readRDS("generated_data/empirical/sc_o30.rds")

# read in posteriors from model fits
cat1a <- rstan::extract(readRDS(here("generated_data", "model_fits", "chains", "1a.rds")),
                       inc_warmup = FALSE, permuted = TRUE, pars = c("foi"))
cat1b <- rstan::extract(readRDS(here("generated_data", "model_fits", "chains", "1b", "30.rds")),
                              inc_warmup = FALSE, permuted = TRUE, pars = c("foi1", "foi2"))
cat2a <- rstan::extract(readRDS(here("generated_data", "model_fits", "chains", "2a.rds")),
                             inc_warmup = FALSE, permuted = TRUE, pars = c("foi1"))
cat2b <- rstan::extract(readRDS(here("generated_data", "model_fits", "chains", "2b", "30.rds")),
                                          inc_warmup = FALSE, permuted = TRUE, pars = c("foi1", "foi2"))


# merge data - allow for space between rows
foi_df <- data.frame(empirical = NA,
                     dummy1 = c(rep(NA, 36000)),
                            model1 = c(cat1a$foi, rep(NA, 24000)),
                            model1_var = c(rep(NA, 12000), cat1b$foi1, cat1b$foi2),
                            dummy2 = c(rep(NA, 36000)),
                            dummy3 = c(rep(NA, 36000)),
                            model2 = c(cat2a$foi1, rep(NA, 24000)),
                            model2_var = c(rep(NA, 12000), cat2b$foi1, cat2b$foi2),
                            age = c(rep(c("all", "<30", ">=30"), each = 12000)))
foi_long <- pivot_longer(foi_df, cols = 1:8, names_to = "model", values_to = "foi") %>%
  mutate(model = factor(model, levels = c("empirical", "dummy1", "model2_var", "model2", "dummy3", "dummy2", "model1_var", "model1")))

# plots

sc_age <- as.data.frame(rbind(sc$sc, sc_u30$sc, sc_o30$sc))
sc_age$age <- c("all", "<30", ">=30")
sc_age$model <- "empirical"

p1 <- ggplot()+
  geom_density_ridges(data = foi_long, aes(x = 100*foi, y = model, fill = age, col = age),
                      rel_min_height=.01, size = 0.8, alpha = 0.4)+
  geom_errorbar(data = sc_age, 
                aes(xmin = 100*low, xmax = 100*upp, y = model, colour = age), width = 0.2, lwd = 0.8, position=position_dodge(width=0.5))+
  geom_point(data = sc_age, 
             aes(x = 100*sc, y = model, colour = age), size = 2, position=position_dodge(width=0.5))+
  scale_fill_manual(values = c("darkblue", "goldenrod", "black"))+
  scale_colour_manual(values = c("darkblue", "goldenrod", "black"))+
  scale_y_discrete(labels=c("empirical" = "observed",
                            "dummy1" = " ",
                            "model2_var" = "Model 2b. age-dependent risk of infection",
                            "model2" = "Model 2a. constant risk of infection with age",
                            "dummy2" = " ",
                            "dummy3" = " ",
                            "model1_var" = "Model 1b. age-dependent risk of infection",
                            "model1" = "Model 1a. constant risk of infection with age"))+
  xlab("Annual risk of Infection (%)")+
  ylab("")+
  xlim(0,8)+
  theme_minimal()+
  theme(text = element_text(size = 20))

ggsave(p1, width = 10, height = 6, units = "in", filename = "figs/fig2.png")

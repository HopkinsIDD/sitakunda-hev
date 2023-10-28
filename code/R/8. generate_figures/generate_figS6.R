source(here("code", "R", "functions.R"))
###########################################
## plot ELPD by age at which FoI changes ##
###########################################

# vector of ages at which FoI changes
age_foi_c <- seq(12,36,2)

m1b_loo <- readRDS("generated_data/model_fits/loo/m1b.rds")
m2b_loo <- readRDS("generated_data/model_fits/loo/m2b.rds")
m3b_loo <- readRDS("generated_data/model_fits/loo/m3b.rds")
loo1_df <- extract_elpd(m1b_loo, n = length(m1b_loo), ages = age_foi_c)
loo2_df <- extract_elpd_fixed(m2b_loo, n = length(m2b_loo), ages = age_foi_c)
loo3_df <- extract_elpd(m3b_loo, n = length(m3b_loo), ages = age_foi_c)

pa <-   ggplot(data = loo1_df, aes(x = age, y = elpd))+
  geom_point()+
  geom_errorbar(aes(ymin = low, ymax = upp))+
  theme_minimal()+
  xlab(" ")+
  ylab("ELPD (LOO-CV)")+
  ylim(-225,-85)

pb <-   ggplot(data = loo2_df, aes(x = age, y = elpd))+
  geom_point()+
  geom_errorbar(aes(ymin = low, ymax = upp))+
  theme_minimal()+
  xlab("Age at which infection risk changes (years)")+
  ylab(" ")+
  ylim(-225,-85)

pc <-   ggplot(data = loo3_df, aes(x = age, y = elpd))+
  geom_point()+
  geom_errorbar(aes(ymin = low, ymax = upp))+
  theme_minimal()+
  xlab(" ")+
  ylab(" ")+
  ylim(-225,-85)

pall <- plot_grid(pa, pb, pc, nrow = 1, labels = c("A", "B", "C"))

ggsave(pall, file = "figs/fig_S6.png")

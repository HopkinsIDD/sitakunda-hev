# plot figure S3

source(here("code", "R", "functions.R"))

# read in the data
df <- readRDS(here("data", "merged_clean.rds"))


# seroreversion rate by age

age_rev <- df %>% filter(!is.na(seroconverted_ud),
                         serostatus_r1_ud == "positive") %>% 
  mutate(age_bin = cut(n_age, breaks = c(seq(0,60, by = 10), 100))) %>%
  group_by(age_bin)%>%
  summarise(py = sum(time_at_risk)/365,
            nrev = sum(serostatus_r3_ud == "negative")) %>%
  mutate(py = as.numeric(py))

age_rev <- age_rev %>% mutate(sr = map2_dbl(nrev, py, ~poisson.test(.x, .y)$estimate),
                              sr_l = map2_dbl(nrev, py, ~poisson.test(.x, .y)$conf.int[1]),
                              sr_u = map2_dbl(nrev, py, ~poisson.test(.x, .y)$conf.int[2]))


# seroconversion rate by age

age_conv <- df %>% filter(!is.na(seroconverted_ud),
                          serostatus_r1_ud == "negative") %>% 
  mutate(age_bin = cut(n_age, breaks = c(seq(0,60, by = 10), 100))) %>%
  group_by(age_bin)%>%
  summarise(py = sum(time_at_risk)/365,
            nconv = sum(serostatus_r3_ud == "positive")) %>%
  mutate(py = as.numeric(py))

age_conv <- age_conv %>% mutate(sc = map2_dbl(nconv, py, ~poisson.test(.x, .y)$estimate),
                                sc_l = map2_dbl(nconv, py, ~poisson.test(.x, .y)$conf.int[1]),
                                sc_u = map2_dbl(nconv, py, ~poisson.test(.x, .y)$conf.int[2]))

p1 <-  ggplot(age_rev, aes(x = age_bin, y = sr))+
  geom_point()+
  geom_errorbar(aes(ymin = sr_l, ymax = sr_u), width = 0)+
  theme_minimal()+
  xlab(" ")+
  ylab("seroreversion rate (per person-year)")+
  theme(axis.text.x = element_blank())
p2 <-  ggplot(age_conv, aes(x = age_bin, y = sc))+
  geom_point()+
  geom_errorbar(aes(ymin = sc_l, ymax = sc_u), width = 0)+
  theme_minimal()+
  xlab("age class (years)")+
  ylab("seroconversion rate (per person-year)")+
  scale_x_discrete(labels = c("<10", "10-19", "20-29", "30-39", "40-49", "50-59", "60-100"))

p1_2 <- plot_grid(p1, p2, labels = c("A", "B"), nrow = 2)

ggsave(p1_2, file = "figs/fig_S3.png")


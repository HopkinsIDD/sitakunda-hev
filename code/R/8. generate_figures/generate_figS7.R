# generate figure comparing age-strat-sp for baseline and follow-up
# r1 = baseline, r3 = follow-up

source(here("code", "R", "functions.R"))

# read in the data
df <- readRDS(here("data", "merged_clean.rds"))


# plot seroprevalence by age, by round
# five year age bins
r1 <- sp_age_clust_fine(df, "serostatus_r1_ud", pop_sitk, div = 5)
r1$round <- 1
r3 <- sp_age_clust_fine(df, "serostatus_r3_ud", pop_sitk, div = 5)
r3$round <- 3
dat <- rbind(r1,r3)

p5 <- ggplot(dat, aes(x = bin, y = 100*serostatus, col = as.factor(round)))+
  geom_point(position = position_dodge(0.5))+
  geom_errorbar(aes(ymin = 100*ci_l, ymax = 100*ci_u),
                width = 0,  position = position_dodge(0.5))+
  scale_color_manual(name = "round", values = c("purple","forestgreen"), labels = c("baseline", "follow-up"))+
  xlab("Age (years)")+
  ylab("Seroprevalence (%)")+
  theme_minimal()+
  scale_x_discrete(labels=c("(0,5]" = "<5",
                            "(5,10]" = "5-9",
                            "(10,15]" = "10-14",
                            "(15,20]" = "15-19",
                            "(20,25]" = "20-24",
                            "(25,30]" = "25-29",
                            "(30,35]" = "30-34",
                            "(35,40]" = "35-39",
                            "(40,45]" = "40-44",
                            "(45,50]" = "45-49",
                            "(50,55]" = "50-54",
                            "(55,60]" = "55-59",
                            "(60,65]" = "60-64",
                            "(65,70]" = "65-69",
                            "(70,100]" = ">=70"))
ggsave(p5, file = "figs/fig_S7.png")

## compare overall seroprevalence
sp(data = df, sero_col = "serostatus_r1_ud", pop = pop_sitk)
sp(data = df, sero_col = "serostatus_r3_ud", pop = pop_sitk)


source(here("code", "R", "functions.R"))

# read in data
hev <- readRDS(here("data", "merged_clean.rds"))

# label people by status (stable/converters/reverters)
hev <- hev %>% filter(!is.na(seroconverted_ud)) %>%
  mutate(status = case_when(seroconverted_ud == "YES" ~ "seroconverted",
                            seroreverted_ud == "YES" ~ "seroreverted",
                            serostatus_r1_ud == "positive" & serostatus_r3_ud == "positive" ~ "stable_seropositive",
                            serostatus_r1_ud == "negative" & serostatus_r3_ud == "negative" ~ "stable_seronegative",
                            TRUE ~ as.character(NA)))

## lets look at the od/cutoff values for individuals with two samples

## plot seroreversion events
hev_long <- hev %>%
  pivot_longer(cols = c("abs_cutoff_ratio_r1_ud", "abs_cutoff_ratio_r3_ud"),
               names_to = "round", values_to = "cutoff_ratio")

p1 <- ggplot(hev_long %>% filter(status == "seroreverted"))+
  geom_line(aes(group = sample_ID, x = round, y = cutoff_ratio), col = "grey")+
  theme_minimal()+
  geom_hline(yintercept = 0.9, col = "firebrick", lty = 2)+
  geom_hline(yintercept = 1.1, col = "firebrick", lty = 2)+
  xlab(" ")+
  ylab("od/cut-off")+
  scale_x_discrete(labels=c("abs_cutoff_ratio_r1_ud" = "1", "abs_cutoff_ratio_r3_ud" = "2"))+
  ylim(-0.1,22)+
  ggtitle("seroreverters")+
  theme(plot.title = element_text(hjust = 0.5))

hev_long <- hev_long %>% mutate(age_bin = case_when(n_age<10 ~ "child<10",
                                                    n_age>=10 ~ ">=10"          
))


## plot seroconversions
p2 <- ggplot(hev_long %>% filter(status == "seroconverted"))+
  geom_line(aes(group = sample_ID, x = round, y = cutoff_ratio), col = "grey")+
  theme_minimal()+
  geom_hline(yintercept = 0.9, col = "firebrick", lty = 2)+
  geom_hline(yintercept = 1.1, col = "firebrick", lty = 2)+
  xlab("round")+
  ylab("")+
  scale_x_discrete(labels=c("abs_cutoff_ratio_r1_ud" = "1", "abs_cutoff_ratio_r3_ud" = "2"))+
  ylim(-0.1,22)+
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("seroconverters")


## is there any evidence of boosting/infection in apparent 
## stable seropositive individuals?
hev_stabp <- hev %>% filter(status == "stable_seropositive") %>%
  mutate(diff_abs = abs_cutoff_ratio_r3_ud - abs_cutoff_ratio_r1_ud,
         diff_fold = abs_cutoff_ratio_r3_ud / abs_cutoff_ratio_r1_ud) %>%
  pivot_longer(cols = c("abs_cutoff_ratio_r1_ud", "abs_cutoff_ratio_r3_ud"),
               names_to = "round", values_to = "cutoff_ratio")
hev_stabp$round <- recode(hev_stabp$round, 
                          abs_cutoff_ratio_r1_ud = "1", 
                          abs_cutoff_ratio_r3_ud = "2")

# plot stable seropositives - exclude sample beyond detection limit
p3 <- ggplot(hev_stabp %>% filter(sample_ID != "10491"))+
  geom_line(aes(group = sample_ID, x = round, y = cutoff_ratio, col = diff_abs))+
  theme_minimal()+
  geom_hline(yintercept = 0.9, col = "firebrick", lty = 2)+
  geom_hline(yintercept = 1.1, col = "firebrick", lty = 2)+
  scale_color_gradientn(colours=c("dodgerblue","grey", "firebrick"),
                        limits = c(-25, 25), name = "difference")+
  ylim(-0.1,22)+
  xlab("")+
  ylab("")+
  theme(axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("stable seropositives")

## save multi-panel plot
legend <- get_legend(p3)

prow <- plot_grid(p1, 
                  p2, 
                  p3 + theme(legend.position="none"),
                  nrow = 1,
                  labels = c("A", "B", "C"))
ptot <- plot_grid(prow, legend, rel_widths = c(3, .3))

ggsave(ptot, width = 8, height = 5, units = "in", filename = here("figs", "fig_S4.png"))




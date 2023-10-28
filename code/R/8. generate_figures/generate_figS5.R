source(here("code", "R", "functions.R"))

# read in the data and the model fits
dfR1 <- read_csv(here("data", "reshaped_stanR1.csv"))
cat1a <- readRDS(here("generated_data", "model_fits", "summaries", "1a.rds"))
cat2a <- readRDS(here("generated_data", "model_fits", "summaries", "2a.rds"))
cat3a <- readRDS(here("generated_data", "model_fits", "summaries", "3a.rds"))
# for those with age-varying FoI choose age cut-off with best fit
cat1b <- readRDS(here("generated_data", "model_fits", "summaries", "1b", "24.rds"))
cat2b <- readRDS(here("generated_data", "model_fits", "summaries", "2b", "32.rds"))
cat3b <- readRDS(here("generated_data", "model_fits", "summaries", "3b", "30.rds"))



# plot fits
p1 <- plot_fits(data = dfR1, model_fit = cat1a)
p2 <- plot_fits_uncert(data = dfR1, model_fit = cat2a)
p3 <- plot_fits(data = dfR1, model_fit = cat3a)
p4 <- plot_fits(data = dfR1, model_fit = cat1b)
p5 <- plot_fits_uncert(data = dfR1, model_fit = cat2b)
p6 <- plot_fits(data = dfR1, model_fit = cat3b)


ptot <- plot_grid(p1+theme(axis.text.x = element_blank()), 
                  p2+theme(axis.text.x = element_blank()),
                  p3,
                  p4+theme(axis.text = element_blank()),
                  p5+theme(axis.text = element_blank()), 
                  p6 + theme(axis.text.y = element_blank()), 
                  nrow = 3, byrow = FALSE)

dummy <- data.frame(x = c(0,0), y = c("data", "model"))
p_dummy <- ggplot()+
  geom_line(data = dummy, aes(x = x, y = y, col = y))+
  scale_color_manual(values = c("red", "blue"), name = " ")

legend <- get_legend(
  p_dummy+
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom",
          legend.key=element_rect(fill="white"))
)

y.grob <- textGrob("Seroprevalence", 
                   gp=gpar(fontsize=15), rot=90)

x.grob <- textGrob("Age (midpoint of age-class in years)", 
                   gp=gpar(fontsize=15))

p_labels <- grid.arrange(arrangeGrob(ptot, left = y.grob, 
                                     bottom = x.grob))
p_final <- plot_grid(p_labels, legend, ncol = 1, rel_heights = c(1, .1))

ggsave(p_final, file = "figs/fig_S6.png")



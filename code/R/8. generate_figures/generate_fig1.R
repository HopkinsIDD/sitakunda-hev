# generate figure 1
source(here("code", "R", "functions.R"))

hev <- read_csv(here("data", "merged_clean.csv"))

# save the image file path to the smoothed seroprevalence map
map <- here("figs/Fig1_map.png")

#
p1 <- plot_sp_sex_clust(data = hev, 
                        sero_col = "serostatus_r1_ud", 
                        pop = pop_sitk)
  

# seroprevalence by age and sex
p2 <- plot_sp_age_sex_clust(data = hev, 
                       sero_col = "serostatus_r1_ud", 
                       pop = pop_sitk)

p2 <- p2 + scale_x_discrete(labels=c("(0,5]" = "<5",
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

# create an image of the map of household seroprevalence
a <- ggdraw() + #create blank canvas
  draw_image(map,scale=1) # draw the image and save to object 

# first row of figure 1
legend <- get_legend(
  p1 + theme(legend.position = "bottom")
)

col2 <- plot_grid(p1 + theme(legend.position="none"), 
                  p2 + theme(legend.position = "none"), nrow = 2,
                  labels = c("A", "B"))
ptot2 <- plot_grid(col2, a, labels = c(" ", "C"), rel_widths = c(1.2,2))

ggsave(ptot2, width = 10, height = 7, units = "in", filename = here("figs", "fig1.png"), dpi = 300)

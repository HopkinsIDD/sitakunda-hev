# semi-variogram

source(here("code", "R", "functions.R"))

# read in data (each row is an individual)
id <- readRDS(here("data", "merged_clean.rds"))

# summarise by household
hh_sum <- hh_summarise(id)

# set coordinate reference system and convert to sf
hh_sum_sf <- hh_sum %>%
  st_as_sf(.,coords = c("Longitude_r1","Latitude_r1"),
           crs="+proj=longlat +datum=WGS84 +no_defs")

# convert to UTM
## transforms sf file to Bangladesh Transverse Mercator projection
hh_sum_btm <- transform_to_btm(hh_sum_sf)
## make into spatial points object
hh_sum_sp  <- as_Spatial(hh_sum_btm)


# semi-variogram

# look at the spread of distances between points
df <- as.data.frame(hh_sum_sp)
dists <- dist(df[, c("coords.x1", "coords.x2")])
summary(dists)
pd <- ggplot()+
  geom_histogram(aes(x = dists/1000), binwidth = 0.4)+
  xlab("Distance (km)")+
  theme_minimal()

# plot the semivariogram for household level seroprevalence at baseline
# over a km
svar1 <- gstat::variogram(seroprev_r1~1, hh_sum_sp, cutoff = 1000)
plot(svar1)


# over 10 km
svar10 <- gstat::variogram(seroprev_r1~1, hh_sum_sp, cutoff = 10000)
p10 <- ggplot(svar10)+
  geom_point(aes(x = dist/1000, y = gamma))+
  ylim(0,NA)+
  xlab("Distance (km)")+
  ylab("Semivariance")+
  theme_minimal()


pc <- plot_grid(p10, pd, nrow = 1, labels = c("A", "B"))
ggsave(pc, width = 8, height = 5, units = "in", file = "figs/fig_S2.png")





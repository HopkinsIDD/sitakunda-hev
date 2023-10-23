# generate map (C) for Figure 1

###############################################################
## FOR FIGURE 1: add inset map showing location of Sitakunda ##
###############################################################

source(here("code", "R", "functions.R"))

# read in data (each row is an individual)
hh <- readRDS(here("data", "merged_clean.rds"))

# summarise by household
hh_sum <- hh_summarise(hh)

# set coordinate reference system and convert to sf
hh_sum_sf <- hh_sum %>%
  st_as_sf(.,coords = c("Longitude_r1","Latitude_r1"),
           crs="+proj=longlat +datum=WGS84 +no_defs")


# read in bgd shapefiles
bgd0 <- readRDS(here("data", "shapefiles", "2018", "BGD_adm0.rds")) %>% st_as_sf()
bgd2 <- readRDS(here("data", "shapefiles", "2018", "BGD_adm2.rds")) %>% st_as_sf()
bgd3 <- readRDS(here("data", "shapefiles", "2018", "BGD_adm3.rds")) %>% st_as_sf()

# isolate Sitakunda shapefile
sitakunda <- bgd3 %>%
  dplyr::filter(ADM3_EN=="Sitakunda") %>%
  st_transform(., crs = "+proj=longlat +datum=WGS84 +no_defs") 

# set the window for extrapolation
sitakunda_window <- st_bbox(sitakunda)

## map of bangladesh with Sitakunda highlighted ##
# 1. create base maps 

# coordinates of Chattogram city
chatt_city <- data.frame(22.3090995, 91.8017675) %>% 
  as_tibble() %>% 
  dplyr::rename(lat=X22.3090995, long=X91.8017675) %>% 
  st_as_sf(.,coords = c("long","lat"),crs="+proj=longlat +datum=WGS84 +no_defs")

# isolate Sitakunda
sitakunda <- bgd3 %>%
  dplyr::filter(ADM3_EN=="Sitakunda") %>%
  st_transform(., crs = "+proj=longlat +datum=WGS84 +no_defs") 

# plot Bangladesh with Sitakunda highlighted
plot_bgd_sita_no_o <- ggplot() +
  geom_sf(data=bgd3,fill = "lightgrey", color=NA,lwd=0) +
  geom_sf(data=sitakunda,fill="#414487FF", color = NA, lwd=0, alpha=0.7) +
  coord_sf(datum=NA, expand = FALSE) + 
  labs(x="") + labs(y="") + theme_void()+
  geom_rect(aes(xmin = 91.5,
                ymin = 22.3,
                xmax = 91.85,
                ymax = 22.75), 
            col = "black", fill = NA,
            lwd = 1)+
  theme(panel.border = element_rect(color = "black",
                                    fill = NA,
                                    linewidth = 1))
inset <- plot_bgd_sita_no_o


# base map
base_map <- ggplot() +
  geom_sf(data=bgd3, fill = "lightgrey", color=NA,lwd=0) +
  geom_sf(data=sitakunda, fill="darkgrey", color = NA, lwd=0, alpha = 1) +
  geom_sf(data = chatt_city, size = 6, lty="solid", shape = 15, fill = "black", color="black") +
  coord_sf(datum=NA, expand = FALSE,
           xlim = c(91.45, 91.85),
           ylim = c(22.2, 22.75)) +
  labs(x="") + labs(y="") + theme_void()+
  ggspatial::annotation_north_arrow(location = "tr",
                                    width = unit(.75, "cm"),
                                    height = unit(.75, "cm"))+
  ggspatial::annotation_scale(location = "bl", text_cex = .9)


####################
## seroprevalence ##
####################

# generate smoothed seroprevalence using idw with power = 1
idw_sp1 <- idw_smoothSeroSit(data = hh_sum, sero_col = "seroprev_r1", 
                             win = sitakunda_window, boundary = sitakunda, power = 1)

# generate smoothed seroprevalence using gam
dfr_masked_df <- gam_smoothSeroSit(data = hh_sum, sero_col = "seroprev_r1", 
                                   boundary = sitakunda, gamma = 1.4, method = "REML")

# plot smoothed seroprevalence from idw
sp_plot_idw1 <- base_map +
  geom_raster(data = idw_sp1, aes(x = x, y = y, fill = 100*value)) +
  scale_fill_viridis_c(name = "% household\nseroprevalence",
                       limits = range(100*dfr_masked_df$sp, 100*idw_sp1$value)) +
  geom_sf(data = hh_sum_sf, col = "grey", pch = 3, size = 1.2, alpha = 0.5) +
  geom_sf_text(data = chatt_city, label = "Chattogram City", size = 6, nudge_x = -0.065) +
  coord_sf(datum=NA, expand = FALSE,
           xlim = c(91.5, 91.82),
           ylim = c(22.27, 22.71))+
 theme(legend.key.size = unit(1, 'cm'),
legend.key.height = unit(1, 'cm'),
legend.key.width = unit(1, 'cm'),
legend.title = element_text(size=15),
legend.text = element_text(size=12),
legend.position = c(0.16,0.47),
legend.box.background = element_rect(fill = "white", color = "white", linewidth = 0),
legend.box.margin = margin(6, 6, 140, 6))

# add inset and households to map
p_map <- ggdraw(sp_plot_idw1+theme(panel.border = element_rect(color = "black",
                                                              fill = NA,
                                                              linewidth = 2))) +
  draw_plot(
    {inset},
    # The distance along a (0,1) x-axis to draw the left edge of the plot
    x = 0.11, 
    # The distance along a (0,1) y-axis to draw the bottom edge of the plot
    y = 0.05,
    # The width and height of the plot expressed as proportion of the entire ggdraw object
    width = 0.35, 
    height = 0.35)

# save map
ggsave(p_map, width = 6.75, height = 8, filename = "figs/Fig1_map.png", dpi = 300)






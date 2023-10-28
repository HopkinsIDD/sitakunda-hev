###############################################
# create map of survey area with sample sites #
###############################################

source(here("code", "R", "functions.R"))

# 1. create base maps 

# read in shapefiles
bgd0 <- readRDS(here("data", "shapefiles", "2018", "BGD_adm0.rds")) %>% st_as_sf()
bgd2 <- readRDS(here("data", "shapefiles", "2018", "BGD_adm2.rds")) %>% st_as_sf()
bgd3 <- readRDS(here("data", "shapefiles", "2018", "BGD_adm3.rds")) %>% st_as_sf() 

# coordinates of Chattogram city
chatt_city <- data.frame(22.3090995, 91.8017675) %>% 
  as_tibble() %>% 
  dplyr::rename(lat=X22.3090995, long=X91.8017675) %>% 
  st_as_sf(.,coords = c("long","lat"),crs="+proj=longlat +datum=WGS84 +no_defs")

# isolate Sitakunda
sitakunda <- bgd3 %>%
  dplyr::filter(ADM3_EN=="Sitakunda") %>%
  st_transform(., crs = "+proj=longlat +datum=WGS84 +no_defs") 

# isolate Chattogram
chattogram <- bgd2 %>%
  dplyr::filter(NAME_2=="Chittagong") %>%
  st_transform(., crs = "+proj=longlat +datum=WGS84 +no_defs") 

# plot main map of bangladesh
plot_bgd_sita <- ggplot() +
  geom_sf(data=bgd3,fill = "lightgrey", color=NA,lwd=0) +
  geom_sf(data=sitakunda,fill="#77a182", color = NA, lwd=0, alpha=0.7) +
  coord_sf(datum=NA, expand = FALSE) + 
  labs(x="") + labs(y="") + theme_void() +
  geom_sf(data=chattogram,color="darkgrey",lwd=0.07, alpha=0) 

# plot main map of bangladesh without chattogram outline
plot_bgd_sita_no_o <- ggplot() +
  geom_sf(data=bgd3,fill = "lightgrey", color=NA,lwd=0) +
  geom_sf(data=sitakunda,fill="#77a182", color = NA, lwd=0, alpha=0.7) +
  coord_sf(datum=NA, expand = FALSE) + 
  labs(x="") + labs(y="") + theme_void()


# 2. prepare hh to add sampled household locations

# read in data (each row is an individual)
hh <- read_csv(here("data", "merged_clean.csv"))

# summarise by household - for this you need the full dataset with GPS coordinates
hh_sum <- hh_summarise(hh)

# set coordinate reference system and convert to sf
hh_sum_sf <- hh_sum %>%
  st_as_sf(.,coords = c("Longitude_r1","Latitude_r1"),
           crs="+proj=longlat +datum=WGS84 +no_defs")


# 3. make inset map

# add box round inset on main map
plot_bgd_sita <- 
  plot_bgd_sita +
  geom_rect(aes(xmin = 91.45,
                ymin = 22.2,
                xmax = 91.85,
                ymax = 22.75), 
            col = "black", fill = NA,
            lwd = 1)

# produce version with no outline 
# (bc discrepancies btw bgd 2 and 3 res)
plot_bgd_sita_no_o <- 
  plot_bgd_sita_no_o +
  geom_rect(aes(xmin = 91.45,
                ymin = 22.2,
                xmax = 91.85,
                ymax = 22.75), 
            col = "black", fill = NA,
            lwd = 1)


inset <- plot_bgd_sita_no_o + 
  geom_sf(data = chatt_city, size = 4, lty="solid", shape = 15, fill = "black", color="black") +
  geom_sf_text(data = chatt_city, label = "Chattogram City", size = 4, nudge_x = -0.08) +
  geom_sf(data = hh_sum_sf, aes(fill = round), col = "transparent", stroke = 0, shape = 21, size = 1.2, alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red"), name = "Household sampled") +
  ggspatial::annotation_north_arrow(location = "tr", 
                                    width = unit(.75, "cm"), 
                                    height = unit(.75, "cm"))+
  ggspatial::annotation_scale(location = "bl", text_cex = .9)+
  ggtitle("Sitakunda, Chattogram District")+
  theme(legend.position = "bottom", legend.direction = "vertical",
        legend.text = element_text(size = 11),
        legend.title = element_text(size = 13),
        title = element_text(size = 11)) +
  guides(fill = guide_legend(override.aes = list(size = 6))) +         
  coord_sf(
    xlim = c(91.45, 91.85),
    ylim = c(22.2, 22.75),
    expand = FALSE)

p_map <- plot_grid(plot_bgd_sita_no_o, inset, ncol = 2)

# save map
ggsave(p_map, width = 8.5, height = 6, filename = "figs/fig_S1_maps.png", dpi = 300)


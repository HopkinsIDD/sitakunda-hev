library(tidyverse)
library(here)
library(lme4)
library(broom)
# model fitting
library(rstan)
library(bayesplot)
library(ggcorrplot)
library(magick)
library(cowplot)
library(gridExtra)
# mapping
library(sf)
library(fasterize)
library(exactextractr)
library(sp)
library(geodata)
library(leaflet)
library(survey)
library(spatstat)
library(raster)
library(ggmap)
library(mgcv)
library(ggspatial)
library(ggridges)
library(loo)

# population of Sitakunda is used via survey --> CIs that take into account clustering
# http://203.112.218.65:8008/WebTestApplication/userfiles/Image/PopCenZilz2011/Zila_Chittagong.pdf

# total population of sitakunda in 2011 census extracted from source above
tot <- 387832
tot_0_4 <- 36272

# exclude those under 1 who were not included in our survey
adjusted_1_4 <- 0.8*36272 # 20% assumed <1
adjusted_tot <- tot - tot_0_4 + adjusted_1_4

# account for 1.5% population growth over 10 years
tot_2021 <- adjusted_tot * 1.015^10
pop_sitk <- tot_2021

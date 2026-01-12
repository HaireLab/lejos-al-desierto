## fcvbins.R
## bin the data by fcv value
## differs from previously done--sampled points are not included here
## output a few graphs
## see assemble.data.R for previous steps and 
## fire.data.prep.R, then...select.points_*.R for next steps

library(tidyverse)
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(ggpubr)
library(randomcoloR)
library(patchwork)

## output from assemble.data.R
##climpts_owneco<-read_sf("./data/cpts_owneco.shp")
## don't keep all the columns and rename what's left
x<-climpts_owneco %>% dplyr::select(unique_id, fcvmask, PC1_v2, PC2_v2,
      dem.ex, NA_L3NAME, NA_L2NAME, NA_L3KEY, NA_L2KEY, Complex_Na, 
      GNIS_Name1, STATE, ORIG_NAME, geometry)
unique(x$GNIS_Name1) # 52 incl "N/A"
## bin the data
fcv_bins<-x %>% mutate(bin = cut_interval(fcvmask, n = 10))
## save
st_write(fcv_bins, "./data/fcv_bins_climatedata.shp") # laea

## for plotting
pal1<-distinctColorPalette(62)
pal2<-distinctColorPalette(5)
## set variables here
color="GNIS_Name1"
fill="GNIS_Name1"
size=11
## plot
d<-fcv_bins %>% st_drop_geometry()
ggboxplot(d, x="bin", y="fcvmask", #facet.by="STATE", 
          fill=fill,color=color,
      orientation="horiz",alpha=0.6,palette=pal1, legend="none", xlab="Bin",
      ylab="Forward Climate Velocity (km/yr)", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/fcvbins_climatedata.png")



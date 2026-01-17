## fix.fire.data.R
## start with the dataset assembled in fcv.bins.R
## rejoin the fire data with newly calculated areas
## then repeat the graphs and summaries

library(tidyverse)
library(dplyr)
library(readr)
library(sf)
library(units)
library(ggplot2)
library(ggpubr)
library(randomcoloR)
library(patchwork)

## fire perims
## fire perims
## revised perims (added 3 fires by MLV)
## two perims have the same Fire_ID (342 unique id's but 343 images) 
perims<-read_sf("../madrean_birds/data/fireperims.updated/Sky_island_fire_polys_1985_2017_edits4transects.shp")
areasi<-drop_units(st_area(perims)) / 10000 # m^2 to ha
perims2<-cbind(perims, areasi)
## these are the ones published on sciencebase...not using here
#perims2<-read_sf("./data/Sky_Island_Fire_Polys_1985_2017/Sky_Island_Fire_Polys_1985_2017.shp")
## mtbs perims to cover all of our US ecoregions
mtbsperims<-read_sf("./data/MTBS/MTBS_CASCeco_to_2021.shp")
areamtbs<-drop_units(st_area(mtbsperims)) / 10000 # m^2 to ha
mtbsperims2<-cbind(mtbsperims, areamtbs)
st_write(mtbsperims2, "./data/MTBS/mtbs_hectares.shp")
## data from previous 
stemp<-x_bin2 %>% dplyr::filter(group=="sampledpoints") # 1890
ctemp<-x_bin2 %>% dplyr::filter(group=="climatepoints") # 60251
st_write(stemp, "./data/sampled_bins.shp")
st_write(ctemp, "./data/climate_bins.shp")

############# sampled points
## combine with the fire data 
stemp_f<-st_transform(stemp, crs=st_crs(perims)) ## utm
stemp_f2<-st_join(stemp_f, perims2) # started w 1890, now 2523 obs (633 reburns)
### summarize fires
stempf_dat<-stemp_f2 %>% 
  group_by(unique_id) %>%
  summarise(maxhectares=max(areasi), countid=n()) # get the max ha for each pt 
                                      ##and row count for times burned
## join with original data
stemp_ha<-cbind(stemp_f, stempf_dat)
st_write(stemp_ha, "./data/sampledbins_fire.shp")
######## candidate "climate" points 
## use st_join resulting in tibble
ctemp_f<-st_transform(ctemp, crs=st_crs(mtbsperims)) ## 
ctemp_f2<-st_join(ctemp_f, mtbsperims2) # started w 60251, now 66294 obs (6043 reburns)
### summarize fires
ctempf_dat<-ctemp_f2 %>% 
  group_by(unique_id) %>%
  summarise(maxhectares=max(areamtbs), countid=n()) # get the max ha for each pt
## join with original data
ctemp_ha<-cbind(ctemp_f, ctempf_dat)
st_write(ctemp_ha, "./data/candidateptbins_fire.shp")
############## skip this section to select priority points
## output climate and sampled points
stemp_ha_tr<-st_transform(stemp_ha, crs=st_crs(ctemp_ha))
sampptdat<-stemp_ha %>% st_drop_geometry() 
climptdat<-ctemp_ha %>% st_drop_geometry()

dat<-bind_rows(sampptdat, climptdat)
dat2<-dat%>% dplyr::select(c(1:8,10, 13:15, 17:19))
names(dat2)[c(14,15)]<-c("timesburned", "geometry")
## change 1 to 0 where ha = na for times burned
dat2[is.na(dat2$maxhectares),14]<-0
## change nas in ha col to zero
#dat2[is.na(dat2$maxhectares),13]<-0 NOPE
## change recent fire year na's to 1984 NOPE
#dat2[dat2$mostrecentyear==1950,9]<-NA
write_csv(dat2, "./data/point_attributes.csv")

#################### select priority points
keep2<-c("Kachina Peaks Wilderness", "Miller Peak Wilderness",
  "Rincon Mountain Wilderness", "Saguaro Wilderness")
goodpts<-ctemp_ha %>% 
  dplyr::filter(GNIS_Name1 %in% keep2) # 721 pts
st_write(goodpts, "./data/points_to_keep.shp")
####### plot
## use 'dat2' or 'goodpts' here
goodpts_df<-goodpts %>% st_drop_geometry()
dat=goodpts_df
color="GNIS_Name1"
fill="GNIS_Name1"
size=11
ggboxplot(dat,x="bin", y="maxhectares", facet.by=c("GNIS_Name1"), 
      fill=fill,color=color,
      orientation="horiz",palette=pal2, legend="top", legend.title="",
      xlab="FCV Bin",
      ylab="Fire size (maximum ha)", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/maxhectare4gnis.png")
## change 1 to 0 where ha = na for times burned
dat[is.na(dat$maxhectares),18]<-0
ggboxplot(dat,x="bin", y="countid", facet.by=c("GNIS_Name1"), 
      fill=fill,color=color,
      orientation="horiz",palette=pal2, legend="top", legend.title="",
      xlab="FCV Bin",
      ylab="Times burned", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/timesburned4gnis.png")


p1/p2/p3
ggsave("./plots/candidatepts_climate.png", width=18, height=12)
p4/p5/p6
ggsave("./plots/candidatepts_fire.png", width=18, height=12)



## summarize by bin and gnis name #################################
dat2[is.na(dat2$maxhectares),13]<-0
bin_summary<-dat2 %>% group_by(bin, region, GNIS_Name1) %>% 
  summarise(fcvmean=mean(fcv), fcvmax=max(fcv), fcvmin=min(fcv), 
            timebmax=max(timesburned),
            firesizemax=max(maxhectares))
write_csv(bin_summary, "./data/bin_fcv_fire.csv")
## how many points in each bin, by group
x_bincount<-dat2 %>% group_by(bin, GNIS_Name1, group) %>% summarise(count=n())
write_csv(x_bincount, "./data/bincount_group.csv")


## ggplot.climate_v1.R
## plot the data extracted from fcv center cell and sampled points
## see sample.velocity.fire.R for sampled point data and
## setup_pt.selection.R for generating the data 
## 24 jan 2023
## S. Haire

library(sf)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(ggpubr)

####################### data prep ################################
## read in the data
## start w latest sf
## fcv points
fpts<-read_sf("./data/climpts_v2.shp")
fdat<-fpts %>% st_drop_geometry() %>% 
  rename(fcv=fcvmask, pc1=PC1_v2, pc2=PC2_v2, mountain_eco=Cmplx_N, objectid=OBJECTI,
         GNIS_Name1=GNIS_N1, state=STATE, unique_id=uniqu_d, timesburned=tmsbrnd,
         mostrecentyear=recntyr, maxfiresize=maxsize)
## adjust the times burned depending on if size is a number...
fdat$tb<-fdat$timesburned + 1 # correct for the way I calculated this...
fdat[is.na(fdat$maxfiresize), 12]<-0 # change to zero if fire size = na
fdat$group<-"climatepoints"
fdat$region="Sky Islands"
fdat[fdat$mountain_eco=="Arizona-New Mexico Mountains",14]<-"Arizona-New Mexico Mountains"

## sampled points
spts<-read_sf("./data/samppts_attr.shp")
sptsna<-spts[is.na(spts$Cmplx_N),] ## these points are missing attributes....
write_sf(sptsna, "./data/sampledpts_NA.shp")
sdat<-spts %>% st_drop_geometry() %>% 
  rename(fcv=fcvcrop, pc1=PC1_v2, pc2=PC2_v2, mountain_eco=Cmplx_N, objectid=OBJECTI,
         GNIS_Name1=GNIS_N1, state=STATE, unique_id=uniqu_d, timesburned=tmsbrnd,
         mostrecentyear=recntyr, maxfiresize=maxsize)
## adjust the times burned depending on if size is a number...
sdat$tb<-sdat$timesburned + 1# correct for the way I calculated this...
sdat[is.na(sdat$maxfiresize), 12]<-0# change to zero if fire size = na
sdat$group="sampledpoints"
sdat$region="Sky Islands" 

## all sky island data
sifcv_df<-fdat %>% dplyr::filter(region=="Sky Islands") # 6,923
si_df<-bind_rows(sdat, sifcv_df) # 8,813
si_df_sum<-si_df %>% group_by(group,GNIS_Name1) %>%
  summarize(count=n(), meanfcv=mean(fcv), maxfcv=max(fcv), minfcv=min(fcv),
            maxtb=max(tb))
#minpc1=min(pc1), maxpc1=max(pc1), , maxrecent=max(mostrecentyear),
            #maxfiresize=max(maxfiresize)
si_df_sum[is.na(si_df_sum$GNIS_Name1),2]<-"Sonora/Chihuahua"
write_csv(si_df_sum, "./data/skyisl_pt_summary1.csv")
## all az/nm mountain data
aznm_df<-fdat %>% dplyr::filter(region=="Arizona-New Mexico Mountains") # 53,377
aznm_df$state<-recode(aznm_df$state, AZ="Arizona", NM="New Mexico")
aznm_df_sum<-aznm_df %>% group_by(group,GNIS_Name1) %>%
  summarize(count=n(), meanfcv=mean(fcv), maxfcv=max(fcv), minfcv=min(fcv),
            maxtb=max(tb))
write_csv(aznm_df_sum, "./data/aznm_pt_summary1.csv")

## save a copy of all the points in one table
## fix a few things...
## fcv data pts
fpts$tb<-fpts$tmsbrnd + 1 # correct for the way I calculated this...
fpts[is.na(fpts$maxsize), 13]<-0 # change to zero if fire size = na
fpts$group<-"climatepoints"
fpts$region="Sky Islands"
fpts[fpts$Cmplx_N=="Arizona-New Mexico Mountains",15]<-"Arizona-New Mexico Mountains"
names(fpts)[1]<-c("fcv")
## sampled points
## adjust the times burned depending on if size is a number...
spts$tb<-spts$tmsbrnd + 1# correct for the way I calculated this...
spts[is.na(spts$maxsize), 13]<-0# change to zero if fire size = na
spts$group="sampledpoints"
spts$region="Sky Islands"
spts_4269<-st_transform(spts, crs=4269)
names(spts_4269)[1]<-c("fcv")
allpts<-rbind(fpts, spts_4269) ## 62190 points
st_write(allpts, "./data/allpoints.shp")

############### plots and summaries ###########################
library(randomcoloR)
pal1<-distinctColorPalette(47)
pal2<-distinctColorPalette(2)
library(patchwork)

d1<-aznm_df ## start with these candidate points
color="GNIS_Name1"
fill="GNIS_Name1"
size=20
p1=gghistogram(d1, x="fcv", facet.by="state", bins=12, fill=fill,color=color,
      alpha=0.6,palette=pal1, legend="", xlab="Forward Climate Velocity (km/yr)") +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p2=gghistogram(d1, x="pc1", facet.by="state", bins=12, fill=fill,color=color,
      alpha=0.6,palette=pal1, legend="none", xlab="PC 1")+
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p3=gghistogram(d1, x="pc2", facet.by="state", bins=12, fill=fill,color=color,
      alpha=0.6,palette=pal1, legend="none", xlab="PC 2")+
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p4=gghistogram(d1, x="mostrecentyear", facet.by="state", bins=12, fill=fill,color=color,
      alpha=0.6,palette=pal1, legend="none", xlab="Most recent fire year") +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p5=gghistogram(d1, x="maxfiresize", facet.by="state", bins=12, fill=fill,color=color,
      alpha=0.6,palette=pal1, legend="none", xlab="Maximum fire size")+
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p6=gghistogram(d1, x="tb", facet.by="state", bins=12, fill=fill,color=color,
      alpha=0.6,palette=pal1, legend="none", xlab="Times burned")+
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p1/p2/p3
ggsave("./plots/candidatepts_nmaz_climate.png", width=18, height=12)
p4/p5/p6
ggsave("./plots/candidatepts_nmaz_fire.png", width=18, height=12)

## sky islands: sampled and candidate points
d2<-si_df ## start with these candidate points
#d2$state<-recode(d2$state, AZ="Arizona", "NM-AZ"="New Mexico-Arizona")
d2$group<-recode(d2$group, climatepoints="Candidate Points", "sampledpoints"="Sampled Points")
color="mountain_eco"
fill="mountain_eco"
size=20
palette=pal1
alpha=0.6
p1=gghistogram(d2, x="fcv", bins=12, fill=fill,color=color, facet.by="group",
      alpha=alpha,palette=palette, legend="", xlab="Forward Climate Velocity (km/yr)") +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p2=gghistogram(d2, x="pc1", bins=12, fill=fill,color=color, facet.by="group",
      alpha=alpha,palette=palette, legend="", xlab="PC 1") +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p3=gghistogram(d2, x="pc2", bins=12, fill=fill,color=color, facet.by="group",
      alpha=alpha,palette=palette, legend="", xlab="PC 2") +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p4=gghistogram(d2, x="mostrecentyear", bins=12, fill=fill,color=color, facet.by="group",
      alpha=alpha,palette=palette, legend="", xlab="Most recent fire year") +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p5=gghistogram(d2, x="maxfiresize", bins=12, fill=fill,color=color, facet.by="group",
      alpha=alpha,palette=palette, legend="", xlab="Maximum fire size") +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p6=gghistogram(d2, x="tb", bins=12, fill=fill,color=color, facet.by="group",
      alpha=alpha,palette=palette, legend="", xlab="Times burned") +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
p1/p2/p3
ggsave("./plots/allpts_skyisl_climate2.png", width=18, height=12)
p4/p5/p6
ggsave("./plots/allpts_skyisl_fire2.png", width=18, height=12)


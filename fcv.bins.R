## fcv.bins.R
## bin the data: sampled points and random points accessible in US
## summarize the fcv distribution of each set of points in each bin
## summarize the fires in each bin
## 30 jan 2023
## S. Haire

library(tidyverse)
library(dplyr)
library(readr)
library(sf)
library(ggplot2)
library(ggpubr)
library(randomcoloR)
library(patchwork)

## latest version of all points: candidate and previously sampled
## fire data have been adjusted to avoid problem w na (except fireyear)
## see ggplot_climate_v1.R
x<-read_sf("./data/allpoints.shp") # 62,190
x2<-x %>% #st_drop_geometry() %>% # keep geom for now
  rename(pc1=PC1_v2, pc2=PC2_v2, mountain_eco=Cmplx_N, objectid=OBJECTI,
         GNIS_Name1=GNIS_N1, state=STATE, unique_id=uniqu_d, timesburned=tb,
         mostrecentyear=recntyr, maxfiresize=maxsize)

## eliminate NPS except Chiricahua NM
## keep na's--these are mx pts 
## includes sampled points on "Ceniza" and "Purica" that fall off island
## and one pt on the rim (delete below)
#x3<-x2 %>%
#mutate(across(c(mountain_eco, GNIS_Name1, state), ~ replace(., is.na(.), "Mexico")))
## use this file to get the gnis names to keep
tempnames<-read_csv("./data/point_attributes.csv")
u_keep<-c(unique(tempnames$GNIS_Name1), NA) ## na's in previous vers = Mexico
x3<-x2 %>% dplyr::filter(GNIS_Name1 %in% u_keep) ## 62141
#x3 %>% group_by(GNIS_Name1) %>% summarise(count=n()) #53
                      ## should drop geom first!!
## now try changing na's...
x4<-x3 %>%
mutate(across(c(mountain_eco, GNIS_Name1, state), ~ replace(., is.na(.), "Mexico")))

## classify the random points into 12 bins
x_bin<-x4 %>% mutate(bin = ntile(fcv, 12))
ggboxplot(x_bin, x="bin", y="fcv") # this one has very small diff in fcv btwn bins
## another option...use this one
## makes n groups with equal range
x_bin2<-x4 %>% mutate(bin = cut_interval(fcv, n = 12)) # only gives 10
x_bin2<-x4 %>% mutate(bin = cut_interval(fcv, n = 10))
x_bin2_df<-x_bin2 %>% st_drop_geometry()
write_csv(x_bin2_df, "./data/group_df.csv")
## for plotting
pal1<-distinctColorPalette(62)
pal2<-distinctColorPalette(5)
## set variables here
color="group"
fill="group"
size=11
## see fix.fire.data.R for this new dataset used below
ggboxplot(x_bin2, x="bin", y="fcv", facet.by="region", fill=fill,color=color,
      orientation="horiz",alpha=0.6,palette=pal2, legend="top", xlab="Bin",
      ylab="Forward Climate Velocity (km/yr)", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/fcvbins.png")
ggboxplot(x_bin2, x="bin", y="pc1", facet.by="region", fill=fill,color=color,
      orientation="horiz",alpha=0.6,palette=pal2, legend="top", xlab="FCV Bin",
      ylab="PC 1", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/pc1bins.png")

ggboxplot(x_bin2, x="bin", y="pc2", facet.by="region", fill=fill,color=color,
      orientation="horiz",alpha=0.6,palette=pal2, legend="top", xlab="FCV Bin",
      ylab="PC 2", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/pc2bins.png")

ggboxplot(dat2, x="bin", y="mostrecentyear", facet.by="region", fill=fill,color=color,
      orientation="horiz",alpha=0.6,palette=pal2, legend="top", xlab="FCV Bin",
      ylab="Most recent fire year", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/recentyearbins.png")

ggboxplot(dat2, x="bin", y="timesburned", facet.by="region", fill=fill,color=color,
      orientation="horiz",alpha=0.6,palette=pal2, legend="top", xlab="FCV Bin",
      ylab="Times burned", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/timesburnedbins.png")

################# see fix.fire.size.R for new area calc and plotting
temp<-x_bin2[x_bin2$maxfiresize> 4020000,] # outliers

x_bin2 %>% dplyr::filter(maxfiresize < 1090000) %>% 
ggboxplot(x="bin", y="maxfiresize", facet.by="region", fill=fill,color=color,
      orientation="horiz",palette=pal2, legend="top", xlab="FCV Bin",
      ylab="Fire size (max)", ggtheme=theme_grey()) +
  theme(axis.title.x = element_text(face="bold", size=size)) +
  theme(strip.text.x = element_text(face="bold",size = size))
ggsave("./plots/maxfiresizebins.png")


## summarize by bin and gnis name #################################
x_bin2$bin.name<-paste("bin", x_bin2$bin, sep="_") # bin is already a factor
bin_summary<-x_bin2 %>% group_by(bin, mountain_eco, GNIS_Name1) %>% summarise(fcvmean=mean(fcv), fcvmax=max(fcv),
                    fcvmin=min(fcv), timebmax=max(timesburned), 
                    firesizemax=max(newfiresize))
write_csv(bin_summary, "./data/binsummary.csv")
## how many points in each bin, by group
x_bincount<-x_bin2 %>% group_by(bin, GNIS_Name1, group) %>% summarise(count=n())
write_csv(x_bincount, "./data/bincount1.csv")


################ plots...........previous............
library(randomcoloR)
newpal4<- distinctColorPalette(4)
newpal3<- distinctColorPalette(3)

newpal30<- distinctColorPalette(30)
rancol30<-randomColor(30)
ggdensity(d, x="fcvcrop", color="group", palette=newpal4, size=3,
          xlab="Forward Climate Velocity (km/yr)")

ggdensity(d, x="timesburned",color="group", fill="group",palette=newpal4, xlab="Times burned",
          facet.by="group")

ggscatter(d, x="PC1_v2", y="PC2_v2", color="group", size=1, ellipse=TRUE, palette=newpal4)

d2<-d[!is.na(d$Complex_Na), ]
d2$region<-"SI"
d2[d2$Complex_Na=="Arizona-New Mexico Mountains",15]<-"Plateau"
d2[d2$Complex_Na=="Sierra Madre Norte",15]<-"Sierra Madre Norte"
ggscatter(d2, x="PC1_v2", y="PC2_v2", color="region", size=1, ellipse=TRUE, palette=newpal3,
          facet.by="group", legend.title="")
ggsave("./plots/pcsbig.png", width=15, height=12)

ggdensity(d2, x="fcvcrop", color="region", palette=newpal3, size=3,
          xlab="Forward Climate Velocity (km/yr)")

## select.pts.NM.R
## see fire.data.prep.R and distance.to.road.R for previous steps
## For all points in NM:
## calculate the stats needed to determine sample size for each bin 
                                            #(burned and unburned)
## try selecting points in unburned that occur in the same range of elevation
## as the burned points that were selected (this was not helpful)
## output the data as table and add: 
## 1. distance to road
## and later:
## 2. fire name, size and year

library(readr)
library(tidyverse) 
library(dplyr)
library(ggplot2)
library(ggpubr)
library(raster)
library(sf)

## start with fcv bin point feature
## this one has "fireallyears" field == times burned incl 2022
fcvbins_firedat<-read_sf("./data/fcvbins_fireallyears.shp")
## grab the NM data
## pts on n mtns 53,328
nmpts<-fcvbins_firedat %>% 
  dplyr::filter(Complex_Na=="Arizona-New Mexico Mountains") %>%
dplyr::filter(STATE=="NM" | STATE=="NM-AZ") ## 21,438

## proportions are roughly 57% in AZ; 32% in NM; 10% in SI
## 57,32,10....114, 64, 20....171, 96, 30 (includes num in hi bins:
## si's=9, az=6)
## maybe 456 AZ, 256 NM, 80 SIs
## determine # pts /bin
## 8998 bin1, 4821 bin 2, 7618 bin 3, 1 in bin 4 (no pts in bins 5 and up)
nmpts %>% group_by(bin) %>% count() %>% arrange(-n) # 8869, 4706,7858,5
bin123=c("[0.1,1.9]",   "(1.9,3.71]",  "(3.71,5.51]")
otherbins<-c("(5.51,7.31]", "(7.31,9.11]",
"(9.11,10.9]", "(10.9,12.7]", "(12.7,14.5]", "(14.5,16.3]", "(16.3,18.1]") 
nmbin4<-nmpts %>% dplyr::filter(bin==otherbins[1])
nm123<-nmpts %>% dplyr::filter(bin %in% bin123) ## 21,437
## aiming for approx 256 points
## so...256/3 bins = 85 in each
################ bin 1
nmbin1<-nm123 %>% dplyr::filter(bin==bin123[1]) # 8,998
bin1_unb<-nmbin1 %>% dplyr::filter(fireallyears==0) # 5947
## elev range of unb pts
summary(bin1_unb$dem.ex) #1167 2657
bin1_bur<-nmbin1 %>% dplyr::filter(fireallyears>0) # 3051
summary(bin1_bur$dem.ex) #1370 3104
# 5947/8998 = 66% unburned, 34% burned
# 85 * .66 = 56 unburned; 85 * .34 = 29 burned
nm_randbin1b<-bin1_bur %>% 
    slice_sample(n = 29, replace=FALSE)
nm_randbin1u<-bin1_unb %>% 
    slice_sample(n = 56, replace=FALSE)

## bin 2
nmbin2<-nm123 %>% dplyr::filter(bin==bin123[2]) # 4821
bin2_unb<-nmbin2 %>% dplyr::filter(fireallyears==0) # 2728
bin2_bur<-nmbin2 %>% dplyr::filter(fireallyears>0) # 2093

# 2728/4821 = 57% unburned, 43% burned
# 85 * .57 = 48 unburned; 85 * .43 = 37 burned
nm_randbin2b<-bin2_bur %>% 
    slice_sample(n = 37, replace=FALSE)
nm_randbin2u<-bin2_unb %>% 
    slice_sample(n = 48, replace=FALSE)

## bin 3
nmbin3<-nm123 %>% dplyr::filter(bin==bin123[3]) # 7618
bin3_unb<-nmbin3 %>% dplyr::filter(fireallyears==0) # 4634
bin3_bur<-nmbin3 %>% dplyr::filter(fireallyears>0) # 2984
# 4634/7618 = 61% unburned, 39% burned
# 85 * .61 = 52 unburned; 85 * .39 = 33 burned
nm_randbin3b<-bin3_bur %>% 
    slice_sample(n = 33, replace=FALSE)
nm_randbin3u<-bin3_unb %>% 
    slice_sample(n = 52, replace=FALSE)

## put all the bins together.....256 pts
nm_all<-bind_rows(nm_randbin1u, nm_randbin1b, nm_randbin2u, nm_randbin2b,
                  nm_randbin3u, nm_randbin3b,nmbin4)
## check for problems
nm_all %>% group_by(GNIS_Name1) %>% count() %>% arrange(-n)
st_write(nm_all, "./data/nm_randpts_v2.shp", append=FALSE)
nm_all<-read_sf("./data/nm_randpts_v2.shp")
## add distance to road
## 38463 rows 256 col
## this is a matrix, but each column has a "name" (which is a number)
d = gDistance(as_Spatial(nm_all), as_Spatial(roads_tr), byid=TRUE)
names(d) #NULL
dcol1<-d[,1]
str(d)
#num [1:38463, 1:256] 101765 364955 376153 389831 240924 ...
 #- attr(*, "dimnames")=List of 2
  #..$ : chr [1:38463] "1" "2" "3" "4" ...
  #..$ : chr [1:256] "44610" "45887" "40604" "49016" ...
## get the minimum distance for each column
d2 <- as_tibble(d) 
mindist<-d2 %>% summarise(across(where(is.numeric),min))
mindistvec<-mindist %>% slice(1) %>% 
           unlist(., use.names=FALSE)
nm_all_dist<-cbind(nm_all, mindistvec)  
st_write(nm_all_dist, "./data/nm_randpts_v2.shp", append=FALSE)
nm_all_df<-nm_all_dist %>% st_drop_geometry()
write_csv(nm_all_df, "./data/nm_randpts_v2.csv")

## add ranger district
rd<-read_sf("./data/Ranger_District_Boundaries_(Feature_Layer).shp")
nmdat_tr<-st_transform(nm_all_dist, crs=3857)
rd_over<-over(as_Spatial(nmdat_tr), as_Spatial(rd))
rd_over2<-rd_over %>% dplyr::select(3:6)
nm_all_dist2<-cbind(nmdat_tr, rd_over2)
st_write(nm_all_dist2, "./data/nm_randpts_v2.shp", append=FALSE)
nm_all_df2<-nm_all_dist2 %>% st_drop_geometry()
write_csv(nm_all_df2, "./data/nm_randpts_v2.csv")

############# earlier tries
## only 1 result d2[1]=4.8 which is the minimum of all distances btwn
## point and line
d2 = gDistance(as_Spatial(nm_all), as_Spatial(roads_tr), byid=FALSE)


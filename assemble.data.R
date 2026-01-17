## assemble.data.R
## put together these data:
## start with points for each fcv cell (3 ecoregions)
## add attributes: ecoregion name, FS management unit, pc values, elevation,
## burned and unburned
## see fcvbins.R for next steps
## S. Haire

library(raster)
library(sf)
library(tidyr)
library(dplyr)
library(readr)

############################## data prep ###################################
## ecoregions
eco3<-read_sf("../envspace/data/studyareapoly/Final_CASC_studyarea.shp")
eco3xy<-st_zm(eco3) # drop z dim or nothing works...
## transform to crs of climate velocity
bioprj<-"+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
eco3_tr_bio<-st_transform(eco3xy, bioprj)

## forward climate velocity
fcv<-raster("../envspace/data/fcvcrop.tif")
fcvmask<-raster("../envspace/data/fcvmask.tif") ## this one is masked to eco3

## new pc's
pc1<-raster("../envspace/data/PCA/PC1_v2.tif")
pc2<-raster("../envspace/data/PCA/PC2_v2.tif")
pc_stack<-stack(pc1, pc2)

## elevation in lat long (global)
dem<-raster("./data/wc2.1_30s_elev.tif")
eco_tr_dem<-st_transform(eco3xy, crs(dem))
dem_crop<-crop(dem, as_Spatial(eco_tr_dem))
writeRaster(dem_crop, "./data/dem_3eco.tif") ## looks good
## project to bioclim
dem_bio<-projectRaster(from=dem_crop, to=fcv)
writeRaster(dem_bio, "./data/dem_crsbio.tif")
## ownership: federal lands
fed<-read_sf("../envspace/data/fedlands/fedlanp010g.shp")
## transform the ecoregions to match fed
eco_tr_fed<-st_transform(eco3xy, crs=4269)

####################### ownership: 3 ecoregions (US only) ############ 
## FS in AZ  & NM 
## and don't forget NM-AZ--there is one poly in Coronado NF with this state designation
fed_aznm<- dplyr::filter(fed, STATE=="AZ" | STATE=="NM" | STATE=="NM-AZ")
fedprj<-st_crs(fed_aznm)

## narrow down to areas of interest
aznm_2<-fed_aznm %>% dplyr::filter(ADMIN1=="FS")
aznm_other<-fed_aznm %>% dplyr::filter(GNIS_Name1=="Rincon Mountain Wilderness" |
         GNIS_Name1=="Saguaro Wilderness" | GNIS_Name1=="Fort Huachuca Military Reservation" |
         GNIS_Name1=="Chiricahua National Monument Wilderness")
aznm_combo<-rbind(aznm_2, aznm_other) ## 117
st_write(aznm_combo, "./data/aznm_fedlands.shp")
################################ end data prep ##########################

################### assemble points and attributes ##################
############ extract values and coords for all fcv raster cells #####################
fcv_xy<-coordinates(fcvmask, spatial=TRUE)#678895 pts

## sample fcv and pcs and elevation
fcv.ex<-raster::extract(fcvmask, fcv_xy, df=TRUE)
pc.ex<-raster::extract(pc_stack, fcv_xy, df=TRUE)
dem.ex<-raster::extract(dem, fcv_xy) ## transforms pts to crs of raster
climdat<-cbind(fcv.ex, pc.ex, dem.ex, fcv_xy)
climdat2<-climdat[!is.na(climdat$fcvmask),]
climdatpts<-st_as_sf(climdat2, coords=c("x","y")) ##### 200445
st_crs(climdatpts)<-crs(fcv_xy)
## add attributes to the fcv points#########
## ecoregion
climdatpts_eco<-st_join(climdatpts, eco3_tr_bio)

##just keep the us points in the fed lands selected above
aznm_combo2<-st_transform(aznm_combo, bioprj)
climpts_owneco<-st_join(climdatpts_eco, aznm_combo2) %>%
  drop_na(PERIMETER) ## 60237 pts
#################### add the uid here for future use
climpts_owneco$unique_id=paste("cpt", 1:60237, sep="_")
## write and take a look in qgis
st_write(climpts_owneco, "./data/cpts_owneco.shp", append=FALSE)
names(climpts_owneco)

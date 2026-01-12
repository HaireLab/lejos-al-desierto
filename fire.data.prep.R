## fire.data.prep.R
## output burned/unburned for all points
## start with data from fcv.bins.R
## 13 Feb 2023

####################################

fcv_bins<-read_sf("./data/fcv_bins_climatedata.shp") # laea

## add the burned/unburned data
## fire perims
mtbsperims<-read_sf("../envspace/data/MTBS/MTBS_CASCeco_to_2021.shp")
mtbs_tr<-st_transform(mtbsperims, crs=st_crs(fcv_bins))

## 2022 fires
homedir<-getwd()
setwd("./data/fireperims_region3")
fires22<-list.files('.', pattern="*kmz") %>%
      map(~sf::st_read(., layer="2022"))
names(fires22[[1]])
#[1] "Name"         "description"  "timestamp"    "begin"        "end"         
# [6] "altitudeMode" "tessellate"   "extrude"      "visibility"   "drawOrder"   
#[11] "icon"         "geometry"
fires22all<-bind_rows(fires22) #140 records
fires22xyall<-st_zm(fires22all) # drop z dim 
fires22b<-fires22xyall %>% dplyr::select(Name, geometry)
fires22_tr<-st_transform(fires22b, crs=st_crs(fcv_bins))
## combine into one multipolygon feature set
fires22u<-st_union(fires22_tr)

setwd(homedir)
## save
st_write(fires22u, "./data/fires_2022_region3.shp", append=FALSE)
## summarise mtbs fires and then 2022 fires separately
burn_01_mtbs<-fcv_bins %>% mutate(in_perim = lengths(st_within(fcv_bins, mtbs_tr)))
burn_2022_over<-over(as_Spatial(burn_01_mtbs), as_Spatial(fires22u))
fcvbins_firedat<-cbind(burn_01_mtbs, burn_2022_over)
fcvbins_firedat[is.na(fcvbins_firedat$burn_2022_over),16]<-0
fcvbins_firedat$fireallyears<-fcvbins_firedat$in_perim + fcvbins_firedat$burn_2022_over
st_write(fcvbins_firedat, "./data/fcvbins_fireallyears.shp")

########### this part is not working
## join fires 2022 with mtbs perims
## first match projections
fires_2022_trmtbs<-st_transform(fires22b, crs=st_crs(mtbsperims))
## select columns for rbind
mtbs_name<-mtbsperims %>% dplyr::select(Incid_Name, geometry)
names(mtbs_name)[1]<-c("Name")
combo2022<-st_combine(fires_2022_trmtbs) # geometry multipolygon
mtbs2022<-st_union(mtbs_name, combo2022)
mtbs2022_tr<-st_transform(mtbs2022, crs=st_crs(fcv_bins))
## id burned/unb...this looks like it also records how many fires
## were intersected by each point (values 0 to 6)
burn_01<-fcv_bins %>% mutate(in_out = lengths(st_within(fcv_bins, mtbs2022_tr)))
## not working...burn_01_2022<-fcv_bins %>% mutate(in_2022 = lengths(st_within(fcv_bins, fires22u)))

## not working...problem with the polygon data (topology exception)
burn_01_22<-fcv_bins %>% mutate(in_perim2022 = lengths(st_within(fcv_bins, fires22_tr)))

############## scratch
## extract the first layer that intersects the points
binpts<-as(st_geometry(fcv_bins), "Spatial") # didn't use
## this filters points within fire polys
bin_fires<-st_filter(fcv_bins, mtbs_tr) # 19515 out of 60237 (32%)
## trying for id burned/unb...this looks like it also records how many fires
## were intersected by each point (values 0 to 6)
burn_01<-fcv_bins %>% mutate(in_perim = lengths(st_within(fcv_bins, mtbs_tr)))


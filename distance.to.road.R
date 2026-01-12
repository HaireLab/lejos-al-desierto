## distance.to.road.R
## do this with rgeos
## use the data from fire.data.prep.R

library(raster)
library(sp)
library(sf)
library(rgeos)

#fcvbins_firedat<-read_sf("./data/fcvbins_fireallyears.shp")

roads<-read_sf("./data/USFS_roads.shp")
roads_tr<-st_transform(roads, crs=st_crs(fcvbins_firedat))

## do this for subset of points selected in select.pts.*.R
d = gDistance(fcvbins_firedat, roads_tr, byid=TRUE)

########## or use sf
road_dist<-st_distance(roads_eco, roads_eco_ptprj) ## 3 distances per point
road_dist2<-st_distance(roads_eco)

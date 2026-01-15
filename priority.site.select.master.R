#This code uses the generalized random tessellation stratified (GRTS) sample
#to provide a spatially balanced sample of CASC sites. This is based on
#stevens and Olsen (2004) algorithm

#Author: Jamie Sanderlin
#Date: 3/24/2023

##Main purpose is to generate sample elements and bind together for output
## 1. read in sf data to sframe, 
## 2. generate stratified sample w fixed ratio of burned/unburned and bin categories
## 3. generate replacement sites, and 
## 4. write the results back to simple feature (sf)
## 5. finally, save as shape files

################################################################################
## Load functions 
############# UPDATE WITH DIRECTORY FOR YOUR COMPUTER ############## 
source("C:/Users/jlsanderlin/Box/External Sky Islands Birds and Fire Collaboration/workspace-study site selection/prioritization of selected points/load_packages.R")
source("C:/Users/jlsanderlin/Box/External Sky Islands Birds and Fire Collaboration/workspace-study site selection/prioritization of selected points/sample_functions.R")

################################################################################
#PROVIDE TOTAL SAMPLES HERE
total.samples = 70

#--------------------------------------------
#STEP 1. get spatial features for each region
#--------------------------------------------
setwd(paste0(getwd(), "/allSIpoints"))
skyisl_sf <- read_sf("all_si_pts.shp")
setwd("C:/Users/jlsanderlin/Box/External Sky Islands Birds and Fire Collaboration/workspace-study site selection/prioritization of selected points")

setwd(paste0(getwd(), "/allAZpoints"))
az_sf <- read_sf("all_az_pts.shp")
setwd("C:/Users/jlsanderlin/Box/External Sky Islands Birds and Fire Collaboration/workspace-study site selection/prioritization of selected points")

setwd(paste0(getwd(), "/allNMpoints"))
nm_sf <- read_sf("all_nm_pts.shp")
setwd("C:/Users/jlsanderlin/Box/External Sky Islands Birds and Fire Collaboration/workspace-study site selection/prioritization of selected points")

#---------------------------------------------------------------
#STEP 2. generate stratified sample of burned and bin categories
#---------------------------------------------------------------

#convert unburned/burned variable to categorical
skyisl_mod <- convert_burn(skyisl_sf)
az_mod <- convert_burn(az_sf)
nm_mod <- convert_burn(nm_sf)

#convert bins to categorical
skyisl_mod <- convert_bin(skyisl_mod,'skyisl')
az_mod <- convert_bin(az_mod,'az')
nm_mod <- convert_bin(nm_mod,'nm')

#__________________________________________
#get number of samples needed for each region
#output az, nm, sky islands vector
region.samples <- samples_by_region(total.samples)

#__________________________________________
#get proportions of burn/unburned and bins by region for proportional inclusion probabilities
#output order is burned first and unburned second
skyisl_prop_burn <- proportion_burn(skyisl_mod)$burn_ratio
az_prop_burn <- proportion_burn(az_mod)$burn_ratio
nm_prop_burn <- proportion_burn(nm_mod)$burn_ratio

#output order is the number of bins from 1 to 8 (sky islands, az) and 1 to 4 (nm)
skyisl_prop_bin <- proportion_bin(skyisl_mod)$bin_ratio
az_prop_bin <- proportion_bin(az_mod)$bin_ratio
nm_prop_bin <- proportion_bin(nm_mod)$bin_ratio

#________________________________________
#determine bin stratification first
skyisl_bin_strata <- bin_stratification('skyisl',region.samples,skyisl_prop_bin)
az_bin_strata <- bin_stratification('az',region.samples,az_prop_bin)
nm_bin_strata <- bin_stratification('nm',region.samples,nm_prop_bin)

#________________________________________
#apply burned stratification next
skyisl_binburn_strata <- burn_stratification(skyisl_bin_strata,skyisl_prop_burn)
az_binburn_strata <- burn_stratification(az_bin_strata,az_prop_burn)
nm_binburn_strata <- burn_stratification(nm_bin_strata,nm_prop_burn)

#________________________________________
#check that data exist for bin categories x burn, fix them
skyisl_binburn_strata_mod <- check_binburn(skyisl_mod,skyisl_binburn_strata,'skyisl')
az_binburn_strata_mod <- check_binburn(az_mod,az_binburn_strata,'az')
nm_binburn_strata_mod <- check_binburn(nm_mod,nm_binburn_strata,'nm')

#fix overall strata now
skyisl_bin_strata_mod <- fix_strata(skyisl_binburn_strata_mod)
az_bin_strata_mod <- fix_strata(az_binburn_strata_mod)
nm_bin_strata_mod <- fix_strata(nm_binburn_strata_mod)

#---------------------------------------------------------------
#STEP 3. generate replacement sites and original sites
#---------------------------------------------------------------
#add replacement sites with nearest neighbor and stratification

caty_skyisl <- generate_cat('skyisl',skyisl_binburn_strata_mod)
caty_az <- generate_cat('az',az_binburn_strata_mod)
caty_nm <- generate_cat('nm',nm_binburn_strata_mod)

strata_skyisl <- generate_strata('skyisl',skyisl_bin_strata_mod)
strata_az <- generate_strata('az',az_bin_strata_mod)
strata_nm <- generate_strata('nm',nm_bin_strata_mod)

#The sky Islands region has zeros in strata, so need to modify for grts input
strata_skyisl_mod <- take_away_zero_strata(strata_skyisl)
zero_index_skyisl <- index_of_zero(strata_skyisl)
caty_skyisl_mod <- take_away_zero_caty(caty_skyisl,zero_index_skyisl)

#generate near list
near_list_skyisl <- generate_near_list('skyisl')
near_list_az <- generate_near_list('az')
near_list_nm <- generate_near_list('nm')

skyisl.strata.bins.burn <- grts(skyisl_mod, n_base = strata_skyisl_mod, stratum_var = "bin.cat", caty_var = "burn", 
                                caty_n = caty_skyisl_mod, n_near = near_list_skyisl)
az.strata.bins.burn <- grts(az_mod, n_base = strata_az, stratum_var = "bin.cat", caty_var = "burn", caty_n = caty_az,
                            n_near = near_list_az)
nm.strata.bins.burn <- grts(nm_mod, n_base = strata_nm, stratum_var = "bin.cat", caty_var = "burn", caty_n = caty_nm, 
                            n_near = near_list_nm)

#________________________________________
#plot to see results
plot(skyisl.strata.bins.burn)
plot(az.strata.bins.burn) 
plot(nm.strata.bins.burn) 

#---------------------------------------------------------------
#STEP 4. write results back to simple feature
#---------------------------------------------------------------

samp_skyisl<-sp_rbind(skyisl.strata.bins.burn)
samp_az<-sp_rbind(az.strata.bins.burn)
samp_nm<-sp_rbind(nm.strata.bins.burn)

#---------------------------------------------------------------
#STEP 5. output as shape files
#---------------------------------------------------------------
write_sf(samp_skyisl, "./sample_5_skyisl.shp")
write_sf(samp_az, "./sample_5_az.shp")
write_sf(samp_nm, "./sample_5_nm.shp")


# RMRS 
# Author: Jamie Sanderlin
# Date: 3/24/2023

# Code purpose: contains all the sampling functions for sampling CASC sites

################################################################################

#converts burn variable to categorical burned/unburned
convert_burn <- function(spatial.feature){
  
  spatial.feature$burn <- NA
  spatial.feature$burn[spatial.feature$frllyrs == 0] <- "unburned"
  spatial.feature$burn[spatial.feature$frllyrs > 0] <- "burned"
  
  spatial.feature
}

#converts bins to categorical
#NOTE: if original data set changes with bin categories, this will need modified
convert_bin <- function(spatial.feature,region){
  #each region has different bin numbers and bin names
  
  if(region=='skyisl'){
    spatial.feature$bin.cat <- NA
    spatial.feature$bin.cat[spatial.feature$bin=='[0.1,1.9]'] <- "bin1"
    spatial.feature$bin.cat[spatial.feature$bin=='(1.9,3.71]'] <- "bin2"
    spatial.feature$bin.cat[spatial.feature$bin=='(3.71,5.51]'] <- "bin3"
    spatial.feature$bin.cat[spatial.feature$bin=='(5.51,7.31]'] <- "bin4"
    spatial.feature$bin.cat[spatial.feature$bin=='(7.31,9.11]'] <- "bin5"
    spatial.feature$bin.cat[spatial.feature$bin=='(9.11,10.9]'] <- "bin6"
    spatial.feature$bin.cat[spatial.feature$bin=='(10.9,12.7]'] <- "bin7"
    spatial.feature$bin.cat[spatial.feature$bin=='(12.7,14.5]'] <- "bin8"
  }
  
  if(region=='az'){
    spatial.feature$bin.cat <- NA
    spatial.feature$bin.cat[spatial.feature$bin=='[0.1,1.9]'] <- "bin1"
    spatial.feature$bin.cat[spatial.feature$bin=='(1.9,3.71]'] <- "bin2"
    spatial.feature$bin.cat[spatial.feature$bin=='(3.71,5.51]'] <- "bin3"
    spatial.feature$bin.cat[spatial.feature$bin=='(5.51,7.31]'] <- "bin4"
    spatial.feature$bin.cat[spatial.feature$bin=='(7.31,9.11]'] <- "bin5"
    spatial.feature$bin.cat[spatial.feature$bin=='(9.11,10.9]'] <- "bin6"
    spatial.feature$bin.cat[spatial.feature$bin=='(10.9,12.7]'] <- "bin7"
    spatial.feature$bin.cat[spatial.feature$bin=='(16.3,18.1]'] <- "bin8"
  }
  
  if(region=='nm'){
    spatial.feature$bin.cat <- NA
    spatial.feature$bin.cat[spatial.feature$bin=='[0.1,1.9]'] <- "bin1"
    spatial.feature$bin.cat[spatial.feature$bin=='(1.9,3.71]'] <- "bin2"
    spatial.feature$bin.cat[spatial.feature$bin=='(3.71,5.51]'] <- "bin3"
    spatial.feature$bin.cat[spatial.feature$bin=='(5.51,7.31]'] <- "bin4"  
  }
  
  spatial.feature
}

#calculuates the total samples by region proportional to full samples
samples_by_region <- function(total.samp){
  #57% in AZ, 33% in NM and 10% in Sky Islands
  final.samp <- c(0.57,0.33,0.10)*total.samp
  final.samp <- ceiling(final.samp)
  sum.samp <- sum(final.samp)
  
  if(sum.samp>total.samp){
    final.samp <- final.samp - c(1,0,0)
    sum.samp2 <- sum(final.samp)
    if(sum.samp2>total.samp){
      final.samp <- final.samp - c(0.1,0)
    }
  }
  
  final.samp
}

#calculates the proportions for inclusion probabilities for burned/unburned
proportion_burn <- function(spatial.feature){
  spatial.feature %>%
    group_by(burn) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::mutate(burn_ratio = Count/sum(Count))
}

#calculates the proportions for inclusion probabilities for bin categories
proportion_bin <- function(spatial.feature){
  spatial.feature %>%
    group_by(bin) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::mutate(bin_ratio = Count/sum(Count))
}

#determine the number of samples in bin strata
bin_stratification <- function(region,region_samples,region_prop_bin){
  if(region=='az'){
    total.region <- region_samples[1]
  }
  
  if(region=='nm'){
    total.region <- region_samples[2]  
  }
  
  if(region=='skyisl'){
    total.region <- region_samples[3]  
  }
  
  region.strata <- adjust_sample(total.region,region_prop_bin)
  region.strata
  
}

#determine the number of samples in burn strata
burn_stratification <- function(region_samples,region_prop_burn){
  region.strata <- matrix(0,length(region_samples),2)
  
  for(i in 1:length(region_samples)){
    temp <- adjust_sample(region_samples[i],region_prop_burn)
    region.strata[i,1]<-temp[1]
    region.strata[i,2]<-temp[2]
  }
  region.strata
  
}

adjust_sample <- function(total,categories){
  total.samp <- categories*total
  total.samp2 <- ceiling(total.samp)
  
  #adjust to get total samp
  sum.samp <- sum(total.samp2)
  diff.samp <- sum.samp-total
  
  final.samp <- total.samp2
  
  if(diff.samp>0){
    #take away a multinomial sample of remainder
    sub.samp <- total.samp2[total.samp2>0]
    sub.id <- which(total.samp2 >0 )
    prob <- sub.samp/sum(sub.samp)
    test <-0
    while(test==0){
      take.away <- rmultinom(1,diff.samp,prob = prob)
    
      for(i in 1:length(total.samp2)){
        if(i==sub.id[i]){
          final.samp[i] <- total.samp2[i]-take.away[i]
        }
      }
      if(length(which(final.samp<0))==0){test <- 1}
    }
  }
  
  final.samp
}

#generate category list of strata
generate_cat <- function(region,region_strata){
  
  if(region=='az'| region=='skyisl'){
    caty_n <- list(
      bin1 = c(burned=region_strata[1,1], unburned=region_strata[1,2]),
      bin2 = c(burned=region_strata[2,1], unburned=region_strata[2,2]),
      bin3 = c(burned=region_strata[3,1], unburned=region_strata[3,2]),
      bin4 = c(burned=region_strata[4,1], unburned=region_strata[4,2]),
      bin5 = c(burned=region_strata[5,1], unburned=region_strata[5,2]),
      bin6 = c(burned=region_strata[6,1], unburned=region_strata[6,2]),
      bin7 = c(burned=region_strata[7,1], unburned=region_strata[7,2]),
      bin8 = c(burned=region_strata[8,1], unburned=region_strata[8,2])
    )
  }
  
  if(region=='nm'){
    caty_n <- list(
      bin1 = c(burned=region_strata[1,1], unburned=region_strata[1,2]),
      bin2 = c(burned=region_strata[2,1], unburned=region_strata[2,2]),
      bin3 = c(burned=region_strata[3,1], unburned=region_strata[3,2]),
      bin4 = c(burned=region_strata[4,1], unburned=region_strata[4,2])
    )
  }
  
caty_n

}

#generate category list of strata (no burn)
generate_strata <- function(region,region_strata){
  
  if(region=='az'| region=='skyisl'){
    caty_n <- c(bin1=region_strata[1],bin2=region_strata[2],bin3=region_strata[3],bin4=region_strata[4],
      bin5=region_strata[5],bin6=region_strata[6],bin7=region_strata[7],bin8=region_strata[8])
  }
  
  if(region=='nm'){
    caty_n <- c(bin1=region_strata[1],bin2=region_strata[2],bin3=region_strata[3],bin4=region_strata[4])
  }
  
  caty_n
  
}

#take away strata that have zero in it for grts function
take_away_zero_strata <- function(region_strata){
  new_strata <- region_strata[region_strata!=0]
  new_strata
}

#get index of rows that have zero in it for grts function input
index_of_zero <- function(region_strata){
  indexes <- which(region_strata==0)
  indexes
}

#taky away categories of strata that have zero in them for grts function
take_away_zero_caty <- function(caty_region,zero_index_region){
    new_caty <- caty_region[-zero_index_region]
    new_caty
}

#checks for bin and burn categories if those combos exist and modifies sample if not

check_binburn <- function(spatial.feature, region_binburn_strata, region){
  spatial.feature %>%
    group_by(bin.cat,burn) %>%
    dplyr::summarise(Count = n()) %>%
    dplyr::mutate(binburn_ratio = Count/sum(Count))
  
  if(region=='az'| region=='skyisl'){bins <- c('bin1','bin2','bin3','bin4','bin5','bin6','bin7','bin8')}
  if(region=='nm'){ bins <- c('bin1','bin2','bin3','bin4')}
  burn <- c('burned','unburned')
  
  for(i in 1:dim(region_binburn_strata)[1]){
    for(j in 1:dim(region_binburn_strata)[2]){
      if(region_binburn_strata[i,j]>0){
        binname <- paste0("bin",i,collapse = "")
        if(j==1){burnname="burned"}
        if(j==2){burnname="unburned"}
        #check if there are samples
        if(length(which(spatial.feature$bin.cat==binname & spatial.feature$burn==burnname))==0){
          if(j==1){new=2}
          if(j==2){new=1}
          newvalue <- region_binburn_strata[i,j]
          region_binburn_strata[i,new] <- newvalue + region_binburn_strata[i,new]
          region_binburn_strata[i,j] <- 0
          #now check if there are enough samples in the new category, fix it if not
          if(j==1){newburnname="unburned"}
          if(j==2){newburnname="burned"}
          if(region_binburn_strata[i,new]>length(which(spatial.feature$bin.cat==binname & spatial.feature$burn==newburnname))){
            add.samples <- region_binburn_strata[i,new]- length(which(spatial.feature$bin.cat==binname & spatial.feature$burn==newburnname))
            region_binburn_strata[i,new] <- length(which(spatial.feature$bin.cat==binname & spatial.feature$burn==newburnname))
            #add to either bin 1 or bin 2
            bin.sample <- sample.int(2,add.samples,replace=TRUE)
            for(k in 1:length(bin.sample)){
              region_binburn_strata[bin.sample[k],j] <- region_binburn_strata[bin.sample[k],j] + 1
            }
            
          }
        }
      }
    }
  }
  
  region_binburn_strata
}

#fixes strata sums after checking bin by burn categories
fix_strata <- function(region_binburn_strata){
  new_strata <- rowSums(region_binburn_strata)
  new_strata
}

#generate near list (controls number of replacement plots by category), but not the higher bins
generate_near_list <- function(region){
  if(region=='az'| region=='skyisl'){
    nearlist <- list(bin1 = 1,bin2 = 1)
  }
  
  if(region=='nm'){
    nearlist <- list(bin1 = 1,bin2 = 1)
  }
  nearlist
}

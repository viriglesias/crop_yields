# # This script models differences in irrigated corn yield as a function of mean vpd (1950-2019) and differences in vpd calculated for consequtive periods of length win (with gams)
# # It requires differences in vpd and differences in irrigated corn yield and fips.
# # It returns a data frame with deviance explained and p-val for each period length (win) and plots the datapoints
# 
# library(tidyverse)
# library(mgcv)
# library(scales)
# library(sf)
# library(ggthemes)
# library(tidymv)
# library(foreach)

source('0_all_functions.R')
crop <- 'winter_wheat'#options are corn, spring_wheat and winter_wheat
if(!crop %in% c('corn', 'spring_wheat', 'winter_wheat')){'Crop not supported'}
variable <- list('winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 
                 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 
                 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd',
                 'spring_gdd', 'summer_gdd', 'spring_kdd', 'summer_kdd')
management <- 'nonirrigated'#Options are nonirrigated, irrigated
if(!management %in% c('irrigated', 'nonirrigated')){print('Management not supported')}
type <- 'relative'
directory <- paste0('Data/Processed/', crop, '/gams')
if(!dir.exists(directory)){
  dir.create(directory)
}#Create directories if they don't exist

foreach(i = 1:length(variable))%do%
  get_dev(crop = crop, variable = variable[[i]], management = management, type = type)

rm(list=ls())
gc()


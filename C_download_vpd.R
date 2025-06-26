#This script: 
# -downloads gridded monthly vpd data from PRISM. Files are saved in ./Raw data/bils.
# - calculates monthly summaries per county and landcover and saves them in ./Summaries
# - requires couunty-landcover-prism cw

source('0_all_functions.R')
setwd('/Users/viig7608/Desktop/CCP/VPD/max')

lib <- c('prism', 'tidyverse', 'raster', 'Rcpp', 'reshape', 'rgdal', 'sf', 'parallel', 'pbapply','ggpubr')#required packages
sapply(lib, load.me)#install and load required packages

# Get montly vpd min from PRISM
prism_set_dl_dir('./Raw data', create = TRUE)#Create folder for raw data
get_prism_monthlys(type = 'vpdmax', year = 1950:2019, mon = 1:12, keepZip = FALSE)

#Convert to clean df
bils <- list.files(pattern = '.bil.bil$',
                   recursive = TRUE,
                   full.names = TRUE)#List of files with gridded monthly vpd min data

# file.remove(bils[seq(1, 391, by = 13)])#remove unnecessary files. double check that these files exist (!)

#Get vpdmax time series
cw_lc <- read.csv('/Users/viig7608/Desktop/CCP/Precip/PRISM/Summaries/county-landcover-prism_cw.csv')[,-1]#landcover-county-prism crosswalk

cl <- makeCluster(getOption("cl.cores", detectCores() -1 ), type = 'FORK')#Create cluster to process in parallel
vpdmax_df <- pblapply(bils,
                      FUN = to_df,
                      cw = cw_lc[,3:7],
                      cl = cl)
stopCluster(cl)

#Convert to dataframe 
vpdmax_df <- bind_rows(vpdmax_df)#404570040 rows

#Calculate monthly climate summaries for counties
vpdmax_county_crops_sum <- vpdmax_df %>% 
  filter(landcover_prism %in% '82') %>% #only keep crops
  group_by(STATEFP, NAME, Year, Month) %>% 
  summarize(Mean_vpdmax = mean(vpdmax, na.rm = T),
            SD_vpdmax = sd(vpdmax, na.rm = T),
            SE_vpdmax = SD_vpdmax/sqrt(n())) %>% 
  ungroup()

write_csv(vpdmax_county_crops_sum, '/Users/viig7608/Desktop/CCP/VPD/max/Summaries/monthly_vpdmax_county_crops-only_summaries.csv')#Save monthly summaries


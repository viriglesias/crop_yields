#This script: 
# - downloads gridded monthly tmean data from PRISM. Files are saved in ./Raw data/bils.
# - calculates monthly summaries per county and landcover and saves them in ./Summaries
# - requires county-landcover-prism crosswalk

setwd('/Users/viig7608/Desktop/CCP/Tmean')
source('0_all_functions.R')

lib <- c('prism', 'tidyverse', 'raster', 'Rcpp', 'reshape', 'rgdal', 'sf', 'parallel', 'pbapply','ggpubr')#required packages
sapply(lib, load.me)#install and load required packages

# Get monthly climate data from PRISM
prism_set_dl_dir('./Raw data', create = TRUE)#Create folder for raw data
get_prism_monthlys(type = 'tmean', year = 1950:2019, mon = 1:12, keepZip = FALSE)#tmean for all months in 1950-2019

#Convert to clean df
bils <- list.files(pattern = '.bil.bil$',
                   recursive = TRUE,
                   full.names = TRUE)#List of files with gridded monthly vpd min data

# file.remove(bils[seq(1, 391, by = 13)])#remove unnecessary files. double check that these files exist (!)
# Get tmean time series
cw_lc <- read.csv('/Users/viig7608/Desktop/CCP/Precip/PRISM/Summaries/county-landcover-prism_cw.csv')[,-1]#landcover-county-prism crosswalk

cl <- makeCluster(getOption("cl.cores", detectCores() -1 ), type = 'FORK')#Create cluster to process in parallel
tmean_df <- pblapply(bils,
                      FUN = to_df,
                      cw = cw_lc[,3:7],
                      cl = cl)
stopCluster(cl)

#Convert to dataframe 
tmean_df <- bind_rows(tmean_df)#404570040 rows

#Calculate monthly climate summaries for counties
tmean_county_crops_sum <- tmean_df %>% 
  filter(landcover_prism %in% '82') %>% #only keep crops
  group_by(STATEFP, NAME, Year, Month) %>% 
  summarize(Mean_tmean = mean(tmean, na.rm = T),
            SD_tmean = sd(tmean, na.rm = T),
            SE_tmean = SD_tmean/sqrt(n())) %>% 
  ungroup()

write_csv(tmean_county_crops_sum, '/Users/viig7608/Desktop/CCP/Tmean/Summaries/monthly_tmean_county_crops-only_summaries.csv')#Save monthly summaries


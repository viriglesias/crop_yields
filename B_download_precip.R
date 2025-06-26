#This script: 
# - creates prism-county cross-walk
# - calculates monthly and yearly summaries per conuty and landcovertype and saves them in ./Summaries
# - requires county boundaries + landcover polygons + gridded monthly precip data from PRISM generated with monthly_huc8 (Files are saved in .Precip/PRISM/Raw data).

source('0_all_functions.R')
lib <- c('prism', 'tidyverse', 'raster', 'Rcpp', 'reshape', 'rgdal', 'sf', 'parallel', 'pbapply','ggpubr')#required packages

sapply(lib, load.me)#install and load required packages

#Get precip time series
cw_lc <- read.csv('Precip/PRISM/Summaries/county-landcover-prism_cw.csv')[,-1]
bils <- list.files(path = 'Precip/PRISM/Raw data/',
                   pattern = '.bil.bil$',
                   recursive = TRUE,
                   full.names = TRUE)#List of files with gridded monthly precip data
cl <- makeCluster(getOption("cl.cores", detectCores() -1 ), type = 'FORK')#Create cluster to process in parallel
prec_df <- pblapply(bils,
                    FUN = to_df,
                    cw = cw_lc[,3:7],
                    cl = cl)
stopCluster(cl)


#Convert to dataframe 
prec_df <- bind_rows(prec_df)#404570040 rows

#Calculate monthly climate summaries for counties
precip_county_crops_sum <- prec_df %>% 
  filter(landcover_prism %in% '82') %>% #only keep crops
  group_by(STATEFP, NAME, Year, Month) %>% 
  summarize(Mean_prec = mean(ppt, na.rm = T),
            SD_prec = sd(ppt, na.rm = T),
            SE_prec = SD_prec/sqrt(n())) %>% 
  ungroup()

write_csv(precip_county_crops_sum, 'Precip/PRISM/Summaries/monthly_precip_county_crops-only_summaries.csv')#Save monthly summaries


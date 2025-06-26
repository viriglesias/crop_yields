#This script splits the crop data into training and validation sets and computes summary statistics and Monte-Carlo Geary's C for each set. 
library(tidyverse)
library(data.table)
library(foreach)
library(sf)
library(ggthemes)
library(ggpubr)
library(dplyr)
library(simpleboot)
library(scales)
library(spdep)
library(boot)

source('0_all_functions.R')

# Crop yield data---------------------------------------------------------------------
crop <- 'corn'#options are corn, spring_wheat and winter_wheat
if(!crop %in% c('corn', 'spring_wheat', 'winter_wheat')){'Crop not supported'}

df <- read.csv(paste0('Data/Raw/', crop, '_raw.csv'))

#Select irrigated data
irrigated <- df %>% 
  dplyr::select(id, contains('__IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE'), contains('YIELD.in.BU...ACRE.IRRIGATED'))
names(irrigated) <- c('id', 'yield')

irrigated_sum <- irrigated %>% 
  group_by(id) %>% 
  summarise(Mean_irrigated = mean(yield, na.rm = T)) 

#Select nonirrigated data
nonirrigated <- df %>% 
  dplyr::select(id, contains('NON_IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE'), contains('YIELD.in.BU...ACRE.NON_IRRIGATED'))
names(nonirrigated) <- c('id', 'yield')

nonirrigated_sum <- nonirrigated %>% 
  group_by(id) %>% 
  summarise(Mean_nonirrigated = mean(yield, na.rm = T)) 

df_sum <- full_join(irrigated_sum, nonirrigated_sum, by = 'id')#Join the two data frames

# Calculate summary statistics --------------------------------------------
df_sum <- df_sum %>% 
  mutate(non_over_irr = Mean_nonirrigated/Mean_irrigated,
         irrig_minus_non = Mean_irrigated - Mean_nonirrigated)

# Perform bootstrap resampling to estimate mean and standard deviation 
#Irrigated yield
a <- df_sum %>% 
  dplyr::select(Mean_irrigated) %>% 
  unlist() %>% 
  unname()
boot_y <- boot(data = a, statistic = mean_fun, R = 999)  # R is the number of bootstrap samples
mean(boot_y$t)#Corn = 137.13 bu/acre; spring wheat = 62.09; winter wheat = 53.79
sd(boot_y$t)#Corn = 1.48 bu/acre; spring wheat = 1.19; winter wheat = .91

#Nonirrigated yield
a <- df_sum %>% 
  dplyr::select(Mean_nonirrigated) %>% 
  unlist() %>% 
  unname()
boot_y <- boot(data = a, statistic = mean_fun, R = 999)  # R is the number of bootstrap samples
mean(boot_y$t)#Corn = 69.79 bu/acre; spring wheat = 28.07; winter wheat = 31.51
sd(boot_y$t)#Corn = 1.25 bu/acre; spring wheat = .76; ; winter wheat = .55

#Difference between irrig and non
a <- df_sum %>% 
  dplyr::select(irrig_minus_non) %>% 
  unlist() %>% 
  unname()
boot_y <- boot(data = a, statistic = mean_fun, R = 999)  # R is the number of bootstrap samples
mean(boot_y$t)#Corn = 67.61; spring wheat = 34.07; ; winter wheat = 22.32
sd(boot_y$t)#Corn = 1.65; spring wheat = 1.28; winter wheat = .76

# Nonirrig-to-irrig ratio
a <- df_sum %>% 
  dplyr::select(non_over_irr) %>% 
  unlist() %>% 
  unname()
boot_y <- boot(data = a, statistic = mean_fun, R = 999)  # R is the number of bootstrap samples
mean(boot_y$t)#Corn = 0.52; spring wheat = 0.46
sd(boot_y$t)#Corn = 0.009; spring wheat = 0.01

# Define training & validation data sets ------------------------------------------------
if(file.exists(paste0('Data/Raw/', crop, '_training.csv'))){
  training <- read.csv(paste0('Data/Raw/', crop, '_training.csv'))
}else{
  ctys_training <- sample(x = unique(df$id), size = round(length(unique(df$id))*0.7))#randomly keep 70% of the counties for training
  training <- filter(df, id %in% ctys_training)
  write.csv(training, paste0('Data/Raw/', crop, '_training.csv'))
}

if(file.exists(paste0('Data/Raw/', crop, '_validation.csv'))){
  validation <- read.csv(paste0('Data/Raw/', crop, '_validation.csv'))
}else{
  ctys_validation <- sample(x = unique(df$id), size = round(length(unique(df$id))*0.7))#randomly keep 70% of the counties for training
  validation <- filter(df, !(id %in% ctys_training))
  write.csv(validation, paste0('Data/Raw/', crop, '_validation.csv'))
}

if(nrow(df) != nrow(training) + nrow(validation)){
  print('Something went wrong')
}

# Compute Monte-Carlo Geary's C ----------------------------------------------
# County boundaries 
county_st <- st_read( '/Users/viig7608/Desktop/CCP/cb_2019_us_county_500k/cb_2019_us_county_500k.shp') %>% 
  st_transform(crs ='+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96
                          +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0
                          +units=m +no_defs')#Albers equal area projection
county_st <- subset(county_st, !(STATEFP %in% c('78', '72', '69', '66', '60', '15', '02')))#keep lower 48 + DC
county_st <- county_st %>% 
  mutate(NAME = as.factor(toupper(NAME)),
         STATEFP = as.factor(STATEFP))

fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
fips <- fips %>%  
  mutate(STATE = toupper(State),
         STATEFP = as.factor(ifelse(nchar(STATEFP)<2, paste0('0', STATEFP), 
                                    STATEFP)))
county_st <- left_join(county_st, fips)
county_st <- county_st %>% 
  mutate(id = paste(STATE, NAME, sep = '_'),
         coords = st_centroid(geometry))#Get centroid of each county
county_coords <- county_st %>% 
  data.frame() %>% 
  mutate(lat = st_coordinates(coords)[,1],
         long = st_coordinates(coords)[,2]) %>% 
  dplyr::select(id, lat, long) 
write.csv(county_coords, 'Data/Processed/county_coords.csv')
ctw_q <- poly2nb(county_st, queen = TRUE)## Queen weights matrix for spatial autocorrelation
listw <- nb2listw(ctw_q, style = "W", zero.policy = TRUE)

#Subset data frames and calculate Geary's C
id_train <- unique(training$id)
short <-  df_sum %>% 
  filter(id %in% id_train)

#Irrigated
x_irrig <- short %>% 
  select(id, 'Mean_irrigated')
x_irrig_st <- county_st %>% 
  left_join(x_irrig) %>% 
  tibble() %>% 
  select('Mean_irrigated') %>% 
  unlist() %>% 
  unname()
  
geary.mc(x_irrig_st, 
         listw = listw,
         nsim = 999, na.action = na.omit, 
         zero.policy = TRUE)
#Corn: statistic = 0.20862, observed rank = 1, p-value = 0.001. There is evidence of significant positive spatial autocorrelation in the data set.
#Spring wheat: statistic = 0.29383, observed rank = 1, p-value = 0.001. There is evidence of significant positive spatial autocorrelation in the data set.
#Winter wheat: statistic = 0.13666, observed rank = 1, p-value = 0.001. There is evidence of significant positive spatial autocorrelation in the data set.

#Nonirrigated
x_nonirrig <- short %>% 
  select(id, 'Mean_nonirrigated')
x_nonirrig_st <- county_st %>% 
  left_join(x_nonirrig) %>% 
  tibble() %>% 
  select('Mean_nonirrigated') %>% 
  unlist() %>% 
  unname()

geary.mc(x_nonirrig_st, 
         listw = listw,
         nsim = 999, na.action = na.omit, zero.policy = TRUE)
#Corn: statistic = 0.13984, observed rank = 1, p-value = 0.001. There is evidence of significant positive spatial autocorrelation in the data set.
#Spring wheat: statistic = 0.3554, observed rank = 1, p-value = 0.001. There is evidence of significant positive spatial autocorrelation in the data set.
#Winter wheat: statistic = 0.13248, observed rank = 1, p-value = 0.001. There is evidence of significant positive spatial autocorrelation in the data set.



# Calculate the difference in mean winter wheat yield per county for consecutive windows of length determined by 'win'. 
library(tidyverse)
library(zoo)
library(ggthemes)

source('0_all_functions.R')
crop <- 'corn'#options are corn, spring_wheat and winter_wheat
if(!crop %in% c('corn', 'spring_wheat', 'winter_wheat')){'Crop not supported'}

if(!dir.exists(paste0('Data/Processed/', crop))){
  dir.create(paste0('Data/Processed/', crop))
}#Create directories if they dont exist

if(!dir.exists(paste0('Data/Processed/', crop))){
  dir.create(paste0('Data/Processed/', crop))
}#Create directories if they dont exist

if(!dir.exists(paste0('Data/Processed/', crop, '/', crop, '_county_diff'))){
  dir.create(paste0('Data/Processed/', crop, '/', crop, '_county_diff'))
}#Create directories if they dont exist


# Crop yield data  --------------------------------------------------------
dataset <- 'validation'

df <- read.csv(paste0('Data/Raw/', crop, '_', dataset, '.csv'))

#there are missing years so, create complete data set:
ts_comp <- data.frame(Year = rep(1980:2020, length(unique(df$id))), 
                      id = rep(unique(df$id), each = length(1980:2020)))

names(df)[names(df)%in%'YEAR'] <- 'Year'

df_compl <- left_join(ts_comp, df, by = c('id', 'Year')) 

if(crop %in% 'corn'){
  df_sum <- df_compl %>% 
    group_by(id) %>% 
    summarize(Mean_irrigated = mean(CORN__GRAIN__IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE, na.rm = T),
            SD_irrigated = sd(CORN__GRAIN__IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE, na.rm = T),
            Mean_non_irrigated = mean(CORN__GRAIN__NON_IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE, na.rm = T),
            SD_non_irrigated = sd(CORN__GRAIN__NON_IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE, na.rm = T))
  }else{
    df_sum <- df_compl %>% 
      group_by(id) %>% 
      summarize(Mean_irrigated = mean(YIELD.in.BU...ACRE.IRRIGATED, na.rm = T),
              SD_irrigated = sd(YIELD.in.BU...ACRE.IRRIGATED, na.rm = T),
              Mean_non_irrigated = mean(YIELD.in.BU...ACRE.NON_IRRIGATED, na.rm = T),
              SD_non_irrigated = sd(YIELD.in.BU...ACRE.NON_IRRIGATED, na.rm = T))
}

df <- left_join(df_compl, df_sum)            
df <- df %>% 
  arrange(id, Year)
#or loop for all windows of length 1:10:
for (i in seq_along(1:10)){
  win <- i
  if(crop %in% 'corn'){
    df2 <- df %>% 
      group_by(id) %>%
      reframe(irrigated_mean = rollapply(CORN__GRAIN__IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE, 
                                         function(x) mean(x, na.rm = T), 
                                         width = win , by = win, align='right'),
              irrigated_sd = rollapply(CORN__GRAIN__IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE, 
                                       function(x) sd(x, na.rm = T),
                                       width = win , by = win, align='right'),
              non_irrigated_mean = rollapply(CORN__GRAIN__NON_IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE, 
                                             function(x) mean(x, na.rm = T), 
                                             width = win , by = win, align='right'),
              non_irrigated_sd = rollapply(CORN__GRAIN__NON_IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE, 
                                           function(x) sd(x, na.rm = T), 
                                           width = win , by = win, align='right'),
              Year = rollapply(Year, min, width = win , by = win, align='right'))#calculate mean and sd per county at  non-overlapping windows of size 'win'
  }else{
    df2 <- df %>% 
      group_by(id) %>%
      reframe(irrigated_mean = rollapply(YIELD.in.BU...ACRE.IRRIGATED, 
                                         function(x) mean(x, na.rm = T), 
                                         width = win , by = win, align='right'),
              irrigated_sd = rollapply(YIELD.in.BU...ACRE.IRRIGATED, 
                                       function(x) sd(x, na.rm = T),
                                       width = win , by = win, align='right'),
              non_irrigated_mean = rollapply(YIELD.in.BU...ACRE.NON_IRRIGATED, 
                                             function(x) mean(x, na.rm = T), 
                                             width = win , by = win, align='right'),
              non_irrigated_sd = rollapply(YIELD.in.BU...ACRE.NON_IRRIGATED, 
                                           function(x) sd(x, na.rm = T), 
                                           width = win , by = win, align='right'),
              Year = rollapply(Year, min, width = win , by = win, align='right'))#calculate mean and sd per county at  non-overlapping windows of size 'win'
  }
  df2 <- df2[order(df2$id, df2$Year),]#sort by county and year
  df_diff <- df2 %>% 
    group_by(id) %>%
    mutate(irrigated_mean_diff = diff2(irrigated_mean),
           irrigated_sd_diff = diff2(irrigated_sd),
           non_irrigated_mean_diff = diff2(non_irrigated_mean),
           non_irrigated_sd_diff = diff2(non_irrigated_sd)) %>% 
    ungroup() #calculate difference between consecutive periods of length 'win' 
  df_diff <- left_join(df_sum, df_diff, by = c('id'))
  df_diff <- df_diff %>% 
    mutate(irrigated_relative_diff_mean_perc = 100*irrigated_mean_diff/Mean_irrigated,
           non_irrigated_relative_diff_mean_perc = 100*non_irrigated_mean_diff/Mean_non_irrigated)#calculate difference in yield relative to mean yield for 1950-2018
  df_diff <- filter(df_diff, is.na(irrigated_mean_diff) == F)#remove rows with NA's))
  write.csv(df_diff, paste0(getwd(),'/Data/Processed/', crop, '/', crop, '_county_diff/', dataset, '_', win, '_yr.csv'))#save file
  print(unique(df_diff$Year))
}

rm(list=ls())
gc()


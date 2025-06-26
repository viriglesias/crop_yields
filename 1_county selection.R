#This script is used to select counties with irrigated and nonirrigated crops and at least 20 years of data. It saves csv files in Data/Raw
library(tidyverse)
library(readxl)
library(parallel)
library(pbapply)

source('0_all_functions.R')

# Create directories  -----------------------------------------------------
if (!dir.exists('Data')) {dir.create('Data')}
if (!dir.exists('Data/Raw')) {dir.create('Data/Raw')}
if (!dir.exists('Data/Processed')) {dir.create('Data/Processed')}

# Corn --------------------------------------------------------------------
xls_file <- '/Users/viig7608/Desktop/CCP/Agric/Raw data/Corn Yield - All States.xlsx'
corn_df <- excel_sheets(xls_file)[3:length(excel_sheets(xls_file))] %>%
  map_df(read_excel,
         path = xls_file)#read and create data frame

corn_df <- filter(corn_df, Year>1979)#remove years before 1980
names(corn_df) <- str_replace_all(names(corn_df), c(" " = "_" , "," = "_", "/" = 'per', "-" = '_'))#remove spaces and commas from col names

corn_df <- corn_df %>% 
  mutate(id = paste(State, County, sep = '_'))#create id column
length(unique(corn_df$id))#number of unique counties = 2709

corn_df %>% 
  summarise(production = sum(!is.na(CORN__GRAIN___PRODUCTION__MEASURED_IN_BU)))#78248

corn_sum_df <- corn_df %>% 
  group_by(id) %>% 
  summarise(No_years_with_data = n()) %>% #count number of years with data for each county
  ungroup()

corn_sum_df <- filter(corn_sum_df, No_years_with_data>=20)#remove counties with less than 20 years of data
length(unique(corn_sum_df$id))#number of unique counties with and w/o irrigation and at least 20 years of data = 2112

corn_df <- filter(corn_df, id %in% corn_sum_df$id)#remove counties with less than 20 years of data
corn_df %>% 
  summarise(production = sum(!is.na(CORN__GRAIN___PRODUCTION__MEASURED_IN_BU)))#73687

corn_df <- corn_df %>% 
  filter(!is.na(CORN__GRAIN__NON_IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE) & !is.na(CORN__GRAIN__IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE)) 
corn_sum2 <- corn_df %>% 
  group_by(id) %>% 
  summarise(Mean_nonirrigated = mean(CORN__GRAIN__NON_IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE),
            Mean_irrigated = mean(CORN__GRAIN__IRRIGATED___YIELD__MEASURED_IN_BU_per_ACRE)) %>% #count number of years with data for each county
  ungroup() %>% 
  filter(Mean_nonirrigated>0 & Mean_irrigated>0)#remove counties with missing values for nonirrigated and irrigated yields

corn_df <- filter(corn_df, id %in% corn_sum2$id)#remove rows with missing values for nonirrigated and irrigated yields
length(unique(corn_df$id))#number of unique counties with and w/o irrigation = 371
corn_df %>% 
  summarise(production = sum(!is.na(CORN__GRAIN___PRODUCTION__MEASURED_IN_BU)))#7220

write.csv(corn_df, 'Data/Raw/corn_raw.csv', row.names = FALSE)#write to csv

# Winter wheat ------------------------------------------------------------
#Read wheat data and filter
csv_files <- list.files(path = '/Users/viig7608/Desktop/CCP/Agric/Raw data/wheat',
                        full.names = TRUE)#List of files with wheat data. 

cl <- makeCluster(getOption("cl.cores", detectCores() -1 ), type = 'FORK')#Create cluster to process in parallel
wheat <- pblapply(csv_files,
                  FUN = filter_wheat,
                  cl = cl)
stopCluster(cl)

wheat_df <- bind_rows(wheat)

wheat_sum_df <- wheat_df %>% 
  group_by(LOCATION, STATE.ANSI, COUNTY.ANSI) %>% 
  summarise(No_years_with_data = n()) %>% #count number of years with data for each county
  ungroup()
dim(wheat_sum_df)#number of unique counties with or w/o irrigation  = 1054

wheat_df %>% 
  summarise(non_irrg_production = sum(!is.na(PRODUCTION.in.BU.NON_IRRIGATED)),#21285
            irrg_production = sum(!is.na(PRODUCTION.in.BU.IRRIGATED)))#11872; total = 33157

wheat_sum_df <- filter(wheat_sum_df, No_years_with_data>=20)#remove counties with less than 20 years of data
dim(wheat_sum_df)#number of unique counties with and w/o irrigation and at least 20 years of data = 679

wheat_df <- filter(wheat_df, LOCATION %in% wheat_sum_df$LOCATION)#remove counties with less than 20 years of data
wheat_df %>% 
  summarise(non_irrg_production = sum(!is.na(PRODUCTION.in.BU.NON_IRRIGATED)),#17686
            irrg_production = sum(!is.na(PRODUCTION.in.BU.IRRIGATED)))#10204; total = 27890

wheat_df  <-  wheat_df %>% 
  mutate(id = paste(sapply(LOCATION, function(x){str_split(x, ', ')[[1]][1]}), 
                           sapply(LOCATION, function(x){str_split(x, ', ')[[1]][3]}), sep = '_'))

wheat_df <- wheat_df %>% 
  filter(!is.na(YIELD.in.BU...ACRE.IRRIGATED) & !is.na(YIELD.in.BU...ACRE.NON_IRRIGATED))#remove rows with missing values for nonirrigated and irrigated yields

wheat_sum2 <- wheat_df %>% 
  group_by(id) %>% 
  summarise(Mean_nonirrigated = mean(YIELD.in.BU...ACRE.IRRIGATED),
            Mean_irrigated = mean(YIELD.in.BU...ACRE.NON_IRRIGATED)) %>% #count number of years with data for each county
  ungroup() %>% 
  filter(Mean_nonirrigated>0 & Mean_irrigated>0)#remove counties with missing values for nonirrigated and irrigated yields

wheat_df <- filter(wheat_df, id %in% wheat_sum2$id)#remove rows with missing values for nonirrigated and irrigated yields

length(unique(wheat_df$id))#number of unique counties with and w/o irrigation = 489
wheat_df %>% 
  summarise(non_irrg_production = sum(!is.na(PRODUCTION.in.BU.NON_IRRIGATED)),#9363
            irrg_production = sum(!is.na(PRODUCTION.in.BU.IRRIGATED)))#9363; total = 18738

write.csv(wheat_df, 'Data/Raw/winter_wheat_raw.csv', row.names = FALSE)#write to csv

# Spring wheat ------------------------------------------------------------
#Read wheat data and filter
csv_files <- list.files(path = '/Users/viig7608/Desktop/CCP/Agric/Raw data/wheat',
                        full.names = TRUE)#List of files with wheat data. 

cl <- makeCluster(getOption("cl.cores", detectCores() -1 ), type = 'FORK')#Create cluster to process in parallel
wheat <- pblapply(csv_files,
                  FUN = filter_spring,
                  cl = cl)
stopCluster(cl)

wheat_df <- bind_rows(wheat)

wheat_sum_df <- wheat_df %>% 
  group_by(LOCATION, STATE.ANSI, COUNTY.ANSI) %>% 
  summarise(No_years_with_data = n()) %>% #count number of years with data for each county
  ungroup()
dim(wheat_sum_df)#number of unique counties with or w/o irrigation  = 413
wheat_df %>% 
  summarise(non_irrg_production = sum(!is.na(PRODUCTION.in.BU.NON_IRRIGATED)),#7086
            irrg_production = sum(!is.na(PRODUCTION.in.BU.IRRIGATED)))#4619; total = 11705

wheat_sum_df <- filter(wheat_sum_df, No_years_with_data>=20)#remove counties with less than 20 years of data
dim(wheat_sum_df)#number of unique counties with and w/o irrigation and at least 20 years of data = 243

wheat_df <- filter(wheat_df, LOCATION %in% wheat_sum_df$LOCATION)#remove counties with less than 20 years of data
wheat_df %>% 
  summarise(non_irrg_production = sum(!is.na(PRODUCTION.in.BU.NON_IRRIGATED)),#5524
            irrg_production = sum(!is.na(PRODUCTION.in.BU.IRRIGATED)))#3404; total = 8928
wheat_df  <-  wheat_df %>% 
  mutate(id = paste(sapply(LOCATION, function(x){str_split(x, ', ')[[1]][1]}), 
                    sapply(LOCATION, function(x){str_split(x, ', ')[[1]][3]}), sep = '_'))

wheat_df <- wheat_df %>% 
  filter(!is.na(YIELD.in.BU...ACRE.IRRIGATED) & !is.na(YIELD.in.BU...ACRE.NON_IRRIGATED))#remove rows with missing values for nonirrigated and irrigated yields

wheat_sum2 <- wheat_df %>% 
  group_by(id) %>% 
  summarise(Mean_nonirrigated = mean(YIELD.in.BU...ACRE.IRRIGATED),
            Mean_irrigated = mean(YIELD.in.BU...ACRE.NON_IRRIGATED)) %>% #count number of years with data for each county
  ungroup() %>% 
  filter(Mean_nonirrigated>0 & Mean_irrigated>0)#remove counties with missing values for nonirrigated and irrigated yields

wheat_df <- filter(wheat_df, id %in% wheat_sum2$id)#remove rows with missing values for nonirrigated and irrigated yields


length(unique(wheat_df$id))#number of unique counties with and w/o irrigation = 491
wheat_df %>% 
  summarise(non_irrg_production = sum(!is.na(PRODUCTION.in.BU.NON_IRRIGATED)),#2680
            irrg_production = sum(!is.na(PRODUCTION.in.BU.IRRIGATED)))#2680; total = 5360

write.csv(wheat_df, 'Data/Raw/spring_wheat_raw.csv', row.names = FALSE)#write to csv





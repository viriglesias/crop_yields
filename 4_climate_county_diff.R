library(tidyverse)
library(readxl)
library(sf)
library(ggthemes)
crop <- 'corn'
i_ni <- read.csv(paste0('/Users/viig7608/Desktop/CCP/Agric/', crop, '/', crop, '_county_diff_not_detrended/nonirrigated/', crop, '_nonirrigated_county_diff_not_detrended_1_yr.csv'))[,-1]
colnames(i_ni)[2] <- 'ni_mean'
#
i_i <- read.csv(paste0('/Users/viig7608/Desktop/CCP/Agric/', crop, '/', crop, '_county_diff_not_detrended/irrigated/', crop, '_irrigated_county_diff_not_detrended_1_yr.csv'))[,-1]
colnames(i_i)[2] <- 'i_mean'
#
info <- full_join(i_ni, i_i)
if(crop %in% c('spring_wheat', 'winter_wheat')) {colnames(info)[1] <- 'id'}
#
info <- info %>%
  mutate(period = ifelse(Year<1984, 'early', 'late')) %>%
  filter(!is.na(ni_mean) & !is.na(i_mean))
#
info <- info %>%
  group_by(id, period) %>%
  summarise(counties = n()) %>%
  ungroup()

info <- pivot_wider(info, names_from = period, values_from = counties)
info <- filter(info, late>10)
length(unique(info$id))

#get precip data
prec <- read.csv('/Users/viig7608/Desktop/CCP/Precip/PRISM/Summaries/monthly_precip_county_crops-only_summaries.csv')#precip in croplands grouped by county
prec <- filter(prec, Month %in% 4:9)#only focus on growing season
#
prec_sum_complete <- prec %>%
  group_by(STATEFP, NAME, Year) %>%
  summarize(Mean_prec = mean(Mean_prec, na.rm = T)) %>%
  ungroup()


fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
prec_sum_complete <- left_join(prec_sum_complete, fips) 
prec_sum_complete <- prec_sum_complete %>% 
  mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))

prec_all_counties <- left_join(corn_sum_df, prec_sum_complete, by = 'id')
prec_all_counties <- prec_all_counties %>% 
  mutate(period = ifelse(Year<1984, 'early', 'late')) %>% 
  group_by(id, STATEFP, NAME, period) %>% 
  summarise(mean_prec = mean(Mean_prec, na.rm = T),
            sd_prec = sd(Mean_prec, na.rm = T)) 
prec_all_counties_wide <- prec_all_counties %>% 
  ungroup() %>% 
  filter(!is.na(mean_prec)) %>% 
  pivot_wider(names_from = period, values_from = c(mean_prec, sd_prec))

prec_all_counties_wide <- prec_all_counties_wide %>% 
  mutate(Mean_prec_perc = mean_prec_late/mean_prec_early,
         Sd_prec_perc = sd_prec_late/sd_prec_early)

prec_some_counties <- left_join(info, prec_sum_complete, by = 'id')
prec_some_counties <- prec_some_counties %>% 
  mutate(period = ifelse(Year<1984, 'early', 'late')) %>% 
  group_by(id,  STATEFP, NAME, period) %>% 
  summarise(mean_prec = mean(Mean_prec, na.rm = T),
            sd_prec = sd(Mean_prec, na.rm = T)) 
prec_some_counties_wide <- prec_some_counties %>% 
  ungroup() %>% 
  filter(!is.na(mean_prec)) %>% 
  pivot_wider(names_from = period, values_from = c(mean_prec, sd_prec))

prec_some_counties_wide <- prec_some_counties_wide %>% 
  mutate(Mean_prec_perc = mean_prec_late/mean_prec_early,
         Sd_prec_perc = sd_prec_late/sd_prec_early)

prec_m <- ggplot() +
  geom_density(aes(mean_prec_early), col = 'blue', data = prec_all_counties_wide) +
  geom_density(aes(mean_prec_late), col = 'red', data = prec_all_counties_wide) +
  geom_density(aes(mean_prec_early), col = 'blue', linetype = 'dashed', data = prec_some_counties_wide) +
  geom_density(aes(mean_prec_late), col = 'red', linetype = 'dashed', data = prec_some_counties_wide) +
  theme_bw() +
  xlab('Mean monthly precipitation (mm)') +
  ylab('Density')

prec_sd <- ggplot() +
  geom_density(aes(sd_prec_early), col = 'blue', data = prec_all_counties_wide) +
  geom_density(aes(sd_prec_late), col = 'red', data = prec_all_counties_wide) +
  geom_density(aes(sd_prec_early), col = 'blue', linetype = 'dashed', data = prec_some_counties_wide) +
  geom_density(aes(sd_prec_late), col = 'red', linetype = 'dashed', data = prec_some_counties_wide) +
  theme_bw() +
  xlab('SD monthly precipitation (mm)') +
  ylab('Density')


#Get tmean data
tmean <- read.csv('/Users/viig7608/Desktop/CCP/Tmean/Summaries/monthly_tmean_county_crops-only_summaries.csv')#tmeanip in croplands grouped by county
tmean <- filter(tmean, Month %in% 4:9)#only focus on growing season

tmean_sum_complete <- tmean %>% 
  group_by(STATEFP, NAME, Year) %>%
  summarize(Mean_tmean = mean(Mean_tmean, na.rm = T)) %>% 
  ungroup()

tmean_sum_complete <- left_join(tmean_sum_complete, fips) 
tmean_sum_complete <- tmean_sum_complete %>% 
  mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))

tmean_all_counties <- left_join(corn_sum_df, tmean_sum_complete, by = 'id')
tmean_all_counties <- tmean_all_counties %>% 
  mutate(period = ifelse(Year<1984, 'early', 'late')) %>% 
  group_by(id, STATEFP, NAME, period) %>% 
  summarise(mean_tmean = mean(Mean_tmean, na.rm = T),
            sd_tmean = sd(Mean_tmean, na.rm = T)) 
tmean_all_counties_wide <- tmean_all_counties %>% 
  ungroup() %>% 
  filter(!is.na(mean_tmean)) %>% 
  pivot_wider(names_from = period, values_from = c(mean_tmean, sd_tmean))

tmean_all_counties_wide <- tmean_all_counties_wide %>% 
  mutate(Mean_tmean_perc = mean_tmean_late/mean_tmean_early,
         Sd_tmean_perc = sd_tmean_late/sd_tmean_early)

tmean_some_counties <- left_join(info, tmean_sum_complete, by = 'id')
tmean_some_counties <- tmean_some_counties %>% 
  mutate(period = ifelse(Year<1984, 'early', 'late')) %>% 
  group_by(id, STATEFP, NAME, period) %>% 
  summarise(mean_tmean = mean(Mean_tmean, na.rm = T),
            sd_tmean = sd(Mean_tmean, na.rm = T)) 
tmean_some_counties_wide <- tmean_some_counties %>% 
  ungroup() %>% 
  filter(!is.na(mean_tmean)) %>% 
  pivot_wider(names_from = period, values_from = c(mean_tmean, sd_tmean))

tmean_some_counties_wide <- tmean_some_counties_wide %>% 
  mutate(Mean_tmean_perc = mean_tmean_late/mean_tmean_early,
         Sd_tmean_perc = sd_tmean_late/sd_tmean_early)

tmean_m <- ggplot() +
  geom_density(aes(mean_tmean_early), col = 'blue', data = tmean_all_counties_wide) +
  geom_density(aes(mean_tmean_late), col = 'red', data = tmean_all_counties_wide) +
  geom_density(aes(mean_tmean_early), col = 'blue', linetype = 'dashed', data = tmean_some_counties_wide) +
  geom_density(aes(mean_tmean_late), col = 'red', linetype = 'dashed', data = tmean_some_counties_wide) +
  theme_bw() +
  xlab('Mean monthly tmean (C)') +
  ylab('Density')

tmean_sd <- ggplot() +
  geom_density(aes(sd_tmean_early), col = 'blue', data = tmean_all_counties_wide) +
  geom_density(aes(sd_tmean_late), col = 'red', data = tmean_all_counties_wide) +
  geom_density(aes(sd_tmean_early), col = 'blue', linetype = 'dashed', data = tmean_some_counties_wide) +
  geom_density(aes(sd_tmean_late), col = 'red', linetype = 'dashed', data = tmean_some_counties_wide) +
  theme_bw() +
  xlab('SD monthly tmean (C)') +
  ylab('Density')

#Get VPD
vpd_max <- read.csv('/Users/viig7608/Desktop/CCP/VPD/max/Summaries/monthly_vpdmax_county_crops-only_summaries.csv')#vpdip in croplands grouped by county
vpd_min <- read.csv('/Users/viig7608/Desktop/CCP/VPD/min/Summaries/monthly_vpdmin_county_crops-only_summaries.csv')#vpdip in croplands grouped by county

vpd <- left_join(vpd_max, vpd_min) %>% 
  mutate(Mean_vpd = (Mean_vpdmax + Mean_vpdmin)/2)
vpd <- filter(vpd, Month %in% 4:9)#only focus on growing season

vpd_sum_complete <- vpd %>% 
  group_by(STATEFP, NAME, Year) %>%
  summarize(Mean_vpd = mean(Mean_vpd, na.rm = T)) %>% 
  ungroup()

vpd_sum_complete <- left_join(vpd_sum_complete, fips) 
vpd_sum_complete <- vpd_sum_complete %>% 
  mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))

vpd_all_counties <- left_join(corn_sum_df, vpd_sum_complete, by = 'id')
vpd_all_counties <- vpd_all_counties %>% 
  mutate(period = ifelse(Year<1984, 'early', 'late')) %>% 
  group_by(id, STATEFP, NAME, period) %>% 
  summarise(mean_vpd = mean(Mean_vpd, na.rm = T),
            sd_vpd = sd(Mean_vpd, na.rm = T)) 
vpd_all_counties_wide <- vpd_all_counties %>% 
  ungroup() %>% 
  filter(!is.na(mean_vpd)) %>% 
  pivot_wider(names_from = period, values_from = c(mean_vpd, sd_vpd))

vpd_all_counties_wide <- vpd_all_counties_wide %>% 
  mutate(Mean_vpd_perc = mean_vpd_late/mean_vpd_early,
         Sd_vpd_perc = sd_vpd_late/sd_vpd_early)

vpd_some_counties <- left_join(info, vpd_sum_complete, by = 'id')
vpd_some_counties <- vpd_some_counties %>% 
  mutate(period = ifelse(Year<1984, 'early', 'late')) %>% 
  group_by(id, STATEFP, NAME, period) %>% 
  summarise(mean_vpd = mean(Mean_vpd, na.rm = T),
            sd_vpd = sd(Mean_vpd, na.rm = T)) 
vpd_some_counties_wide <- vpd_some_counties %>% 
  ungroup() %>% 
  filter(!is.na(mean_vpd)) %>% 
  pivot_wider(names_from = period, values_from = c(mean_vpd, sd_vpd))

vpd_some_counties_wide <- vpd_some_counties_wide %>% 
  mutate(Mean_vpd_perc = mean_vpd_late/mean_vpd_early,
         Sd_vpd_perc = sd_vpd_late/sd_vpd_early)

vpd_m <- ggplot() +
  geom_density(aes(mean_vpd_early), col = 'blue', data = vpd_all_counties_wide) +
  geom_density(aes(mean_vpd_late), col = 'red', data = vpd_all_counties_wide) +
  geom_density(aes(mean_vpd_early), col = 'blue', linetype = 'dashed', data = vpd_some_counties_wide) +
  geom_density(aes(mean_vpd_late), col = 'red', linetype = 'dashed', data = vpd_some_counties_wide) +
  theme_bw() +
  xlab('Mean monthly VPD') +
  ylab('Density')

vpd_sd <- ggplot() +
  geom_density(aes(sd_vpd_early), col = 'blue', data = vpd_all_counties_wide) +
  geom_density(aes(sd_vpd_late), col = 'red', data = vpd_all_counties_wide) +
  geom_density(aes(sd_vpd_early), col = 'blue', linetype = 'dashed', data = vpd_some_counties_wide) +
  geom_density(aes(sd_vpd_late), col = 'red', linetype = 'dashed', data = vpd_some_counties_wide) +
  theme_bw() +
  xlab('SD monthly VPD') +
  ylab('Density')


ggpubr::ggarrange(tmean_m, tmean_sd,
                  prec_m, prec_sd,
                  vpd_m, vpd_sd,
                  ncol = 2,
                  nrow = 3)

ggsave(paste0('/Users/viig7608/Desktop/CCP/Agric/Figures/climate_corn_counties.jpeg'),  width = 8.93, height = 6)

county_st <- st_read('/Users/viig7608/Desktop/CCP/cb_2019_us_county_500k/cb_2019_us_county_500k.shp')#county boundaries

county_st <- subset(county_st, !(STATEFP %in% c('78', '72', '69', '66', '60', '15', '02')))#keep lower 48 + DC

county_st <- county_st %>% 
  mutate(id = paste(as.numeric(STATEFP), toupper(NAME), sep = '_'))

prec_all_counties_wide <- prec_all_counties_wide %>% 
  mutate(STATEFP = as.character(STATEFP))
tmean_all_counties_wide <- tmean_all_counties_wide %>% 
  mutate(STATEFP = as.character(STATEFP))
vpd_all_counties_wide <- vpd_all_counties_wide %>% 
  mutate(STATEFP = as.character(STATEFP))

county_corn_all <- left_join(county_st, prec_all_counties_wide, by = c("STATEFP", "NAME"))
county_corn_all <- left_join(county_corn_all, tmean_all_counties_wide, by = c("STATEFP", "NAME"))
county_corn_all <- left_join(county_corn_all, vpd_all_counties_wide, by = c("STATEFP", "NAME"))

prec_some_counties_wide <- prec_some_counties_wide %>% 
  mutate(STATEFP = as.character(STATEFP))
tmean_some_counties_wide <- tmean_some_counties_wide %>% 
  mutate(STATEFP = as.character(STATEFP))
vpd_some_counties_wide <- vpd_some_counties_wide %>% 
  mutate(STATEFP = as.character(STATEFP))

county_corn_some <- left_join(county_st, prec_some_counties_wide, by = c("STATEFP", "NAME"))
county_corn_some <- left_join(county_corn_some, tmean_some_counties_wide, by = c("STATEFP", "NAME"))
county_corn_some <- left_join(county_corn_some, vpd_some_counties_wide, by = c("STATEFP", "NAME"))

#CONUS boundaries
us <- st_read('/Users/viig7608/Documents/Proposals/DISES/cb_2018_us_state_500k')
us <- subset(us, !(NAME %in% c('Puerto Rico', 'Alaska', 'American Samoa',
                               'United States Virgin Islands', 'Hawaii', 'Guam',
                               'Commonwealth of the Northern Mariana Islands')))


tmean_all <- ggplot() +
  geom_sf(aes(fill = Mean_tmean_perc, col = Mean_tmean_perc), data = county_corn_all) +
  scale_fill_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black', guide = 'none') +
  scale_color_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black') +
  geom_sf(data = us, fill = NA, col = 'white') +
  labs(color = 'Change in temperature') +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') 

prec_all <- ggplot() +
  geom_sf(aes(fill = Mean_prec_perc, col = Mean_prec_perc), data = county_corn_all) +
  scale_fill_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black', guide = 'none') +
  scale_color_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black') +
  geom_sf(data = us, fill = NA, col = 'white') +
  labs(color = 'Change in precipitation') +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') 

vpd_all <- ggplot() +
  geom_sf(aes(fill = Mean_vpd_perc, col = Mean_vpd_perc), data = county_corn_all) +
  scale_fill_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black', guide = 'none') +
  scale_color_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black') +
  geom_sf(data = us, fill = NA, col = 'white') +
  labs(color = 'Change in VPD') +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') 

tmean_some <- ggplot() +
  geom_sf(aes(fill = Mean_tmean_perc, col = Mean_tmean_perc), data = county_corn_some) +
  scale_fill_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black', guide = 'none') +
  scale_color_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black') +
  geom_sf(data = us, fill = NA, col = 'white') +
  labs(color = 'Change in temperature') +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') 

prec_some <- ggplot() +
  geom_sf(aes(fill = Mean_prec_perc, col = Mean_prec_perc), data = county_corn_some) +
  scale_fill_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black', guide = 'none') +
  scale_color_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black') +
  geom_sf(data = us, fill = NA, col = 'white') +
  labs(color = 'Change in precipitation') +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') 

vpd_some <- ggplot() +
  geom_sf(aes(fill = Mean_vpd_perc, col = Mean_vpd_perc), data = county_corn_some) +
  scale_fill_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black', guide = 'none') +
  scale_color_gradient2(low = 'steelblue4', high = 'firebrick4', mid = 'white', midpoint = 1, na.value = 'black') +
  geom_sf(data = us, fill = NA, col = 'white') +
  labs(color = 'Change in VPD') +
  theme_map() +
  theme(legend.position = 'bottom',
        legend.direction = 'horizontal') 

ggpubr::ggarrange(tmean_all, prec_all, vpd_all,
                  tmean_some, prec_some, vpd_some,
                  ncol = 3, nrow = 2)

ggsave(paste0('/Users/viig7608/Desktop/CCP/Agric/Figures/climate_corn_counties_maps.jpeg'))


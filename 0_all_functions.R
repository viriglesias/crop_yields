
# Used in steps A, B, ----------------------------------------------------------

to_df <- function(bils, cw){
  bils_df <- raster::raster(bils) %>% 
    as.data.frame(xy = T) %>% 
    mutate(Year = substr(unlist(strsplit(basename(unlist(bils)), split = '_'))[5], 1, 4),
           Month = substr(unlist(strsplit(basename(unlist(bils)), split = '_'))[5], 5, 6))
  colnames(bils_df)[3] <- unlist(strsplit(basename(unlist(bils)), split = '_'))[2]
  bils_df <- cbind(bils_df, cw)
  return(bils_df)
}#Converts bils to df

# Used in Step 1 ----------------------------------------------------------

filter_wheat <- function(file, type){
  wheat_df <- read.csv(file)
  irrigated_wheat  <- filter(wheat_df , 
                             !is.na(YEAR) & YEAR>1979 & PRODN.PRACTICE %in% 'IRRIGATED' & COMMODITY %in% 'WHEAT, WINTER')#remove rows with NA's, and select irrigated winter wheat
  
  nonirrigated_wheat  <- filter(wheat_df , 
                                !is.na(YEAR) & YEAR>1979 & PRODN.PRACTICE %in% 'NON-IRRIGATED' & COMMODITY %in% 'WHEAT, WINTER')
  
  wheat <- full_join(nonirrigated_wheat, irrigated_wheat, by = c('YEAR', 'LOCATION', 'STATE.ANSI', 'ASD.CODE', 'COUNTY.ANSI', 'REFERENCE.PERIOD', 'COMMODITY')) #join the two data frames
  names(wheat) <- str_replace_all(names(wheat), c('x' = 'NON_IRRIGATED', 'y' = 'IRRIGATED'))#remove spaces and commas from col names
  wheat <- wheat %>% 
    dplyr::select(!c(ASD.CODE, REFERENCE.PERIOD, COMMODITY, PRODN.PRACTICE.NON_IRRIGATED, PRODN.PRACTICE.IRRIGATED))
  return(wheat)
}#filters winter wheat

filter_spring <- function(file, type){
  wheat_df <- read.csv(file)
  irrigated_wheat  <- filter(wheat_df , 
                             !is.na(YEAR) & YEAR>1979 & PRODN.PRACTICE %in% 'IRRIGATED' & COMMODITY %in% 'WHEAT, SPRING, (EXCL DURUM)')#remove rows with NA's, and select irrigated spring wheat
  
  nonirrigated_wheat  <- filter(wheat_df , 
                                !is.na(YEAR) & YEAR>1979 & PRODN.PRACTICE %in% 'NON-IRRIGATED' & COMMODITY %in% 'WHEAT, SPRING, (EXCL DURUM)')#remove rows with NA's, and select irrigated spring wheat
  
  wheat <- full_join(nonirrigated_wheat, irrigated_wheat, by = c('YEAR', 'LOCATION', 'STATE.ANSI', 'ASD.CODE', 'COUNTY.ANSI', 'REFERENCE.PERIOD', 'COMMODITY')) #join the two data frames
  names(wheat) <- str_replace_all(names(wheat), c('x' = 'NON_IRRIGATED', 'y' = 'IRRIGATED'))#remove spaces and commas from col names
  wheat <- wheat %>% 
    dplyr::select(!c(ASD.CODE, REFERENCE.PERIOD, COMMODITY, PRODN.PRACTICE.NON_IRRIGATED, PRODN.PRACTICE.IRRIGATED))
  return(wheat)
}#filters spring wheat

# Used in step 2 ----------------------------------------------------------
mean_fun <- function(data, indices) {
  mean(data[indices])
}#Calculates mean for boostrapping

# Used in step 3 ----------------------------------------------------------
diff2 <- function(x){
  y <- diff(x)
  return(c(NA, y))
}#calcuates difference between consective values

# Used in step E ----------------------------------------------------------

to_df2 <- function(bils, cw){
  bils_df <- raster::raster(bils[1]) %>% 
    as.data.frame(xy = T) %>% 
    mutate(Year = substr(unlist(strsplit(basename(unlist(bils)), split = '_'))[5], 1, 4),
           Month = substr(unlist(strsplit(basename(unlist(bils)), split = '_'))[5], 5, 6),
           Day = substr(unlist(strsplit(basename(unlist(bils)), split = '_'))[5], 7, 8))
  colnames(bils_df)[3] <- unlist(strsplit(basename(unlist(bils)), split = '_'))[2]
  bils_df <- cbind(bils_df, cw)
  tmean_county_crops_sum <- bils_df %>% 
    filter(landcover_prism %in% '82') %>% #only keep crops
    group_by(STATEFP, NAME, Year, Month, Day) %>% 
    summarize(Mean_tmean = mean(tmean, na.rm = T)) %>% 
    ungroup()
  return(tmean_county_crops_sum)
}#Converts bils to df

to_df3 <- function(bils, cw){
  bils_df <- raster::raster(bils[1]) %>% 
    as.data.frame(xy = T) %>% 
    mutate(Year = substr(unlist(strsplit(basename(unlist(bils)), split = '_'))[5], 1, 4),
           Month = substr(unlist(strsplit(basename(unlist(bils)), split = '_'))[5], 5, 6),
           Day = substr(unlist(strsplit(basename(unlist(bils)), split = '_'))[5], 7, 8))
  colnames(bils_df)[3] <- unlist(strsplit(basename(unlist(bils)), split = '_'))[2]
  bils_df <- cbind(bils_df, cw)
  tmax_county_crops_sum <- bils_df %>% 
    filter(landcover_prism %in% '82') %>% #only keep crops
    group_by(STATEFP, NAME, Year, Month, Day) %>% 
    summarize(Mean_tmax = mean(tmax, na.rm = T)) %>% 
    ungroup()
  return(tmax_county_crops_sum)
}#Converts bils to df


# Used in step 5 --------------------------------------------------------

get_dev <- function(crop, variable, management, type){
  for (i in seq_along(1:10)){
    win <- i#this loops
    x <- read.csv(paste0('Data/Processed/', crop, '/', crop, '_county_diff/training_', win, '_yr.csv'))[,-1]
    colnames(x)[grepl('non_irrigated', colnames(x))] <- sub('non_irrigated', 'nonirrigated', colnames(x)[grepl('non_irrigated', colnames(x))])
    x <- filter(x, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
    colnames(x)[1] <- 'id'
    if(!variable %in% c('winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd')){
      climate <- read.csv(paste0('Data/Processed/', variable, '/', crop, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
      }else{
        climate <- read.csv(paste0('Data/Processed/', variable, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
        }
    fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
    climate <- left_join(climate, fips) 
    climate <- climate %>% 
      mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))#create unique identifier that matches corn data
    complete <- left_join(x, climate, by = c('id', 'Year'))
    county_coords <- read.csv('Data/Processed/county_coords.csv')[,-1]
    complete <- left_join(complete, county_coords)
    validation <- read.csv(paste0('Data/Processed/', crop, '/', crop, '_county_diff/validation_', win, '_yr.csv'))[,-1]
    colnames(validation)[grepl('non_irrigated', colnames(validation))] <- sub('non_irrigated', 'nonirrigated', colnames(validation)[grepl('non_irrigated', colnames(validation))])
    validation <- filter(validation, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
    validation <- left_join(validation, climate, by = c('id', 'Year'))
    validation <- left_join(validation, county_coords)
    if(type %in% 'absolute'){
      complete <- complete %>% 
        dplyr::select(c(sym(paste0(management, '_mean_diff')), 
                        sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable))), lat, long))
      if(crop %in% 'corn'){
        complete[,1] <- complete[,1]*62.77
        }else if(crop %in% 'spring_wheat'){
          complete[,1] <- complete[,1]*67.25
          }else{
            complete[,1] <- complete[,1]*67.25
            }      
      names(complete)[1:2] <- c('yield', 'climate')
      validation <- validation %>% 
        dplyr::select(c(sym(paste0(management, '_mean_diff')), 
                        sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable))), lat, long))
      if(crop %in% 'corn'){
        validation[,1] <- validation[,1]*62.77
        }else if(crop %in% 'spring_wheat'){
          validation[,1] <- validation[,1]*67.25
          }else{
            validation[,1] <- validation[,1]*67.25
            }      
      names(validation)[1:2] <- c('yield', 'climate')
    }else if(type %in% 'relative'){
      complete <- complete %>% 
        dplyr::select(c(sym(paste0(management, '_relative_diff_mean_perc')), 
                        sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable))), lat, long))
      if(crop %in% 'corn'){
        complete[,1] <- complete[,1]
        }else if(crop %in% 'spring_wheat'){
          complete[,1] <- complete[,1]
          }else{
            complete[,1] <- complete[,1]
            }    
      names(complete)[1:2] <- c('yield', 'climate')
      validation <- validation %>% 
        dplyr::select(c(sym(paste0(management, '_relative_diff_mean_perc')), 
                        sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable))), lat, long))
      
      if(crop %in% 'corn'){
        validation[,1] <- validation[,1]
        }else if(crop %in% 'spring_wheat'){
          validation[,1] <- validation[,1]
          }else{
            validation[,1] <- validation[,1]
            }   
      names(validation)[1:2] <- c('yield', 'climate')
    }else{
      'Double-check'
    }
    complete <- complete[complete.cases(complete),]
    validation <- validation[complete.cases(validation),]
    g2 <- gam(yield~s(climate, k=3) + s(lat, long), method = "REML", data = complete)
    g3 <- gam(yield~s(climate, k=3), method = "REML", data = complete)
    valid <- predict.gam(g2, newdata = validation, type = 'response')
    valid_plot <- predict.gam(g2, type = 'terms', se = T)
    smoothers <- data.frame(climate = complete$climate,
                            yield = unlist(as.numeric(valid_plot$fit[,1])) + coef(g2)[1],
                            se = unlist(as.numeric(valid_plot$se[,1]))) 
    write.csv(smoothers, paste0(directory, '/smoothers_', management, '_', variable, '_', type, '_', win, '_yr.csv'))
    correlation <- cor(x = validation$yield, y = as.numeric(unlist(valid)), method = 'spearman', use = 'na.or.complete')
    ifelse(exists('var_df'), 'Dataframe already exists',
           var_df <- data.frame(window = rep(NA, 10), 
                                dev_expl = rep(NA, 10),
                                p_val_climate = rep(NA, 10),
                                p_val_coords = rep(NA, 10),
                                correlation = rep(NA, 10),
                                full_dev = rep(NA, 10),
                                climate_dev = rep(NA, 10),
                                climate_dev_expl = rep(NA, 10)))
    var_df[win, 1] <- win
    var_df[win, 2] <- summary(g2)$dev
    var_df[win, 3] <- summary(g2)$s.table[1,4]
    var_df[win, 4] <- summary(g2)$s.table[2,4]
    var_df[win, 5] <- correlation
    var_df[win, 6] <- g2$deviance
    var_df[win, 7] <- g3$deviance
    var_df[win, 8] <- summary(g3)$dev
    
    rm(g2)
    write.csv(var_df, paste0(directory, '/deviance_', management, '_', variable, '_', type, '.csv'))
  }
  return(var_df)
}#Extracts deviance explained by gams, correlation between predictions and data, and smoothers for 'winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd'


# Used in Figure_all_gams -------------------------------------------------
read_smoothers <- function(files, variable){
  x <- read.csv(files)[,-1]
  x <- x %>% 
    mutate(win = as.numeric(str_split_fixed(basename(files), '_', 7)[6]))
  return(x)
}#Creates data frame with all smoothers

rename_climate <- function(variable){
  a <- paste(strsplit(variable, '_')[[1]][1], ifelse(strsplit(variable, '_')[[1]][2] %in% 'vpd',
                                                     'VPD',
                                                     ifelse(strsplit(variable, '_')[[1]][2] %in% 'prec',
                                                            'precipitation',
                                                            ifelse(strsplit(variable, '_')[[1]][2] %in% 'tmean',
                                                                   'temperature', 
                                                                   ifelse(strsplit(variable, '_')[[1]][2] %in% 'gdd',
                                                                          'GDD', 
                                                                          ifelse(strsplit(variable, '_')[[1]][2] %in% 'kdd',
                                                                                 'KDD', NA))))), sep = ' ')
  return(a)
}

rename_crop <- function(crop){
  x <- ifelse(crop %in% 'corn',
              'corn',
              sub('_', ' ', crop))
  return(x)
}

rename_management <- function(management){
  x <- ifelse(management %in% 'nonirrigated', 
              'non-irrigated', management)
  return(x)
}

sum_gam_plot <- function(crop, management, type, variable){
  files_smoother <- list.files(path = directory_input,
                               pattern = paste0('smoothers_', management),
                               full.names = T)
  files_smoother <- files_smoother[grepl(type, files_smoother)]
  files_smoother <- files_smoother[grepl(variable, files_smoother)]
  smooth_df <- foreach(i = 1:length(files_smoother), .combine = rbind)%do%
    read_smoothers(files_smoother[[i]], variable)
  smooth_df <- smooth_df %>% 
    group_by(win) %>% 
    mutate(upper = yield + se,
           lower = yield - se,
           mean_yield = mean(yield),
           mean_upper = mean(upper),
           mean_lower = mean(lower),
           sd_yield = sd(yield),
           sd_upper = sd(upper),
           sd_lower = sd(lower)) %>% 
    ungroup() %>% 
    rowwise() %>% 
    mutate(upper_yield = pmax(upper, lower),
           lower_yield = pmin(upper, lower)) %>% 
    ungroup() 
  files_deviance <- list.files(path = directory_input,
                               pattern = paste0('deviance_', management),
                               full.names = T)
  files_deviance <- files_deviance[grepl(type, files_deviance)]
  files_deviance <- files_deviance[grepl(variable, files_deviance)]
  x <- read.csv(files_deviance)[,-1]
  smooth_df <- left_join(smooth_df, x, by = c('win' = 'window'))
  smooth_ns <- filter(smooth_df, p_val_climate>0.05) %>% 
    arrange(win) %>% 
    mutate(win_f = as.factor(paste0(win, '-yr')),
           sig = 'ns')
  smooth_s <- filter(smooth_df, p_val_climate<0.05)%>% 
    arrange(win) %>% 
    mutate(win_f = as.factor(paste0(win, '-yr')),
           sig = 's')
  smooth_c <- rbind(smooth_ns, smooth_s) %>% 
    mutate(sig = as.factor(sig))
  Variable <- sapply(variable, rename_climate)
  Crop <- rename_crop(crop = crop)
  Management <- rename_management(management = management)
  smooth_c$win_f <- factor(smooth_c$win_f, levels = c('1-yr', '2-yr', '3-yr', '4-yr', '5-yr', '6-yr', '7-yr', '8-yr', '9-yr', '10-yr'))
  lv <- if('ns' %in% levels(smooth_c$sig) & 's' %in% levels(smooth_c$sig)){
    c('dotted', 'solid')
    }else if('ns' %in% levels(smooth_c$sig) & !('s' %in% levels(smooth_c$sig))){
      'dotted'
      }else{
        'solid'
      }
  
  f1 <- ggplot() +
    geom_ribbon(aes(x = climate, 
                    y = yield, 
                    ymin = lower_yield, 
                    ymax = upper_yield, 
                    # group = win_f,
                    fill = win_f), 
                data = smooth_c,
                alpha = .6) +
    scale_fill_viridis_d(option = 'H', 'Window size') +
    geom_line(aes(x = climate, 
                  y = yield, 
                  group = win,
                  linetype = sig),
              data = smooth_c,
              linewidth = .3) +
    scale_linetype_manual(values = lv) +
    theme_bw(base_size = 9,
             base_family = 'Myriad Pro') +
    guides(fill = guide_legend(title.position = "top"),
           linetype = F) +
    theme(plot.tag = element_text(size = 9, face = 'bold'),
          plot.title = element_text(size = 9),
          legend.title.align = .5,
          legend.key.height = unit(0.25, 'cm'),
          legend.position = 'bottom',
          strip.background = element_rect(fill = NA),
          legend.text = element_text(size = 9)) +
    ylab(paste0('Relative change in \n ', Management, ' \n', Crop, ' yield (%)')) +
    xlab(paste0('Relative change in \n ', Variable, ' (%)')) 
  
  #Cross-correlogram
  w1 <- filter(smooth_c, win %in% 1)
  nd <- data.frame(climate = seq(min(w1$climate), max(w1$climate), by = max(abs(diff(sort(w1$climate))), na.rm = T)))
  pred_corr <- function(df, nd){
    g1 <- gam(yield~s(climate, k = 3), data = df)
    pred <- predict.gam(g1, newdata = nd, type = 'response')
    smoothers <- data.frame(yield = pred,
                            climate = nd$climate,
                            win = df$win[1])
    # smoothers <- smoothers %>% 
    #   mutate(yield = ifelse(climate>=min(df$climate) & climate<=max(df$climate), 
    #                         yield, NA))
    # # smoothers_w <- pivot_wider(smoothers, names_from = climate, values_from = yield)
    Name <- paste0(smoothers$win[1], '_yr')
    smoothers <- data.frame(x = smoothers[,1])
    colnames(smoothers) <- Name
    return(smoothers)
  }
  smooth_cc <- smooth_c %>% 
    as.data.table()
  sm_split <- split(smooth_cc, by = 'win_f')
  full <- foreach(i = 1:length(sm_split), .combine = cbind)%do%
    pred_corr(sm_split[[i]], nd)
  names(full) <- gsub('_', '-', names(full))
  cor_m <- full[,c('1-yr', '2-yr', '3-yr', '4-yr', '5-yr', 
                   '6-yr', '7-yr', '8-yr', '9-yr', '10-yr')] %>% 
    as.matrix %>%
    cor(use = "complete.obs") 
  f2 <- ggcorrplot(cor_m, hc.order = F, type = 'upper',
                   outline.color = 'white',
                   colors = c("#6D9EC1", "white", "#E46726"),
                   insig = "blank", legend.title = "r", lab_size = 2) +
    scale_y_discrete(limits=rev) +
    theme_bw(base_size = 9) +
    xlab('Window') +
    ylab('Window') +
    # labs(tag = 'B') +
    guides(fill = guide_colorbar(title.position = 'top')) +
    theme_bw(base_size = 8,
             base_family = 'Myriad Pro') +
    theme(plot.tag = element_text(size = 9, face = 'bold'),
          plot.title = element_text(size = 9),
          legend.text = element_text(size = 6),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          legend.title.align = 0.5,
          legend.key.height = unit(0.25, 'cm'),
          legend.position = 'bottom')
  
  #Panel 3
  files_dev <- list.files(path = directory_input,
                          pattern = paste0('deviance_', management),
                          full.names = T)
  files_dev <- files_dev[grepl(type, files_dev)]
  files_dev <- files_dev[grepl(variable, files_dev)]
  dev_df <- foreach(i = 1:length(files_dev), .combine = rbind)%do%
    read_csv(files_dev[[i]])[,-1]
  N <- (max(dev_df$correlation) - min(dev_df$correlation))*.05
  f3 <- ggplot() +
    geom_point(aes(climate_dev_expl * 100, 
                   correlation),
               data = filter(dev_df, p_val_climate<0.05)) +
    geom_point(aes(climate_dev_expl * 100, 
                   correlation),
               data = filter(dev_df, p_val_climate>0.05),
               color = 'gray') +
    geom_text(aes(climate_dev_expl * 100, 
                  correlation,
                  label = paste0(window, '-yr')),
              nudge_y = N,
              size = 2,
              data = filter(dev_df, p_val_climate<0.05)) +
    geom_text(aes(climate_dev_expl * 100, 
                  correlation,
                  label = paste0(window, '-yr')),
              nudge_y = N,
              size = 2,
              data = filter(dev_df, p_val_climate>0.05),
              color = 'gray') +
    theme_bw(base_size = 9) +
    xlab('Deviance expalained in \n training data set (%)') +
    ylab('Cross-correlation') +
    # labs(tag = 'A') +
    theme_bw(base_size = 8,
             base_family = 'Myriad Pro') +
    theme(plot.tag = element_text(size = 9, face = 'bold'))
  fig <- ggarrange(ggarrange(f3, f2, nrow = 1, hjust = 0, labels = 'AUTO',
                             font.label = list(size = 9, face = 'bold', base_family = 'Myriad Pro')),
                   f1, nrow = 2,
                   hjust = 0, labels = c('', 'C'),
                   font.label = list(size = 9, face = 'bold', base_family = 'Myriad Pro'))
  ggsave(paste0(directory_output, '/', crop, '_', management, '_', variable, '_gam_info.jpeg'),
         plot = fig, width = 5, height = 4.5, dpi = 1000)
}


# Used in Fig climate correlation -----------------------------------------

prep_data_cor <- function(file){
  climate <- read.csv(file)[,-1]
  variable <- paste(str_split(basename(file), '_')[[1]][1], str_split(basename(file), '_')[[1]][2], sep = '_') 
  climate <- left_join(climate, fips) 
  climate <- climate %>% 
    mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))
  names(x) <- ifelse(names(x) %in% 'YEAR', 'Year', names(x))
  complete <- inner_join(climate, x, by = c('id', 'Year'))
  complete <- complete %>% 
    select(id, Year, sym(paste0('mean_', strsplit(variable, '_')[[1]][2]))) %>% 
    mutate(variable = variable)
  names(complete)[3] <- 'value'
  return(complete)
}


# Used in Fig climate density ---------------------------------------------


plot_dens <- function(crop, x, variable, Plot, L){
  climate <- if(grepl('kdd', variable) | grepl('gdd', variable)){
    read.csv(paste0('Data/Processed/', variable, '/', crop, '/', variable, '_diff_1980-2020_1_yr.csv'))[,-1]
  }else{
    read.csv(paste0('Data/Processed/', variable, '/', variable, '_diff_1980-2020_1_yr.csv'))[,-1]
  }
  fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
  climate <- left_join(climate, fips) 
  climate <- climate %>% 
    mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))#create unique identifier that matches corn data
  complete <- left_join(x, climate, by = 'id')
  complete <- complete %>% 
    select(area, sym(paste0('Mean_', strsplit(variable, '_')[[1]][2])))
  names(complete)[2] <- 'climate'
  variable2 <- paste0(stringr::str_to_sentence(str_split_fixed(rename_climate(variable), ' ', 2)[1,1]), ' ', str_split_fixed(rename_climate(variable), ' ', 2)[1,2])
  complete$area <- factor(complete$area, levels=L)
  new_deg <- function(x) as.expression(substitute(A~"("*degree~"K)",list(A = as.name(x))))
  if(Plot %in% 'yes'){
    ggplot(complete) +
      stat_density(aes(climate, linetype = area), geom = 'line', position = 'identity') +
      # scale_linetype_manual(values = c('solid', 'dashed')) +
      theme_bw(base_size = 9,
               base_family = 'Myriad Pro') +
      theme(plot.tag = element_text(size = 9, face = 'bold'),
            plot.title = element_text(size = 9),
            legend.title=element_blank(),
            legend.key.height = unit(0.25, 'cm'),
            legend.position = 'bottom',
            strip.background = element_rect(fill = NA),
            legend.text = element_text(size = 9)) +
      ylab('') +
      xlab(ifelse(variable2 %like% 'temperature', new_deg(variable2),variable2)) 
  }else{
    a <- complete %>% 
      group_by(area) %>% 
      summarise(climate_median = median(climate, na.rm = T),
                climate_mean = mean(climate, na.rm = T),
                climate_sd = sd(climate, na.rm = T)/sqrt(n())) %>% 
      ungroup() %>% 
      mutate(variable = variable)
    return(a)
  }
}

# Used in Figure deviance tables ------------------------------------------
read_dev <- function(file){
  read.csv(file)[,-1] %>% 
  mutate(variable1 = str_to_sentence(paste0(str_split_fixed(basename(file), '_', 5)[1,3], '_',
                                            str_split_fixed(basename(file), '_', 5)[1,4])),
         management = str_to_sentence(str_split_fixed(basename(file), '_', 5)[1,2]),
         variable = rename_climate(variable1))
}


# Used in 6_plot_all_gams -------------------------------------------------

plot_gam_crop <- function(crop, variable, win, Absolute, min_yr){
  Absolute <- Absolute
  management <- 'irrigated'#Options are nonirrigated, irrigated
  x <- read.csv(paste0('Data/Processed/', crop, '/', crop, '_county_diff/training_', win, '_yr.csv'))[,-1]
  colnames(x)[grepl('non_irrigated', colnames(x))] <- sub('non_irrigated', 'nonirrigated', colnames(x)[grepl('non_irrigated', colnames(x))])
  x <- filter(x, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
  if(!variable %in% c('winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd')){
    climate <- read.csv(paste0('Data/Processed/', variable, '/', crop, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
  }else{
    climate <- read.csv(paste0('Data/Processed/', variable, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
  }
  fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
  climate <- left_join(climate, fips) 
  climate <- climate %>% 
    mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))#create unique identifier that matches corn data
  complete <- left_join(x, climate, by = c('id', 'Year'))
  complete1 <- complete %>% 
    select(c(sym(paste0(management, '_mean_diff')), 
             sym(paste0('Mean_', sub('.*_', '', variable))), 
             sym(paste0('mean_diff_', sub('.*_', '', variable))),
             sym(paste0(management, '_relative_diff_mean_perc')),
             sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable)))))
  management2 <- 'nonirrigated'#Options are nonirrigated, irrigated
  if(!management2 %in% c('nonirrigated', 'irrigated')){print('Management not supported')}
  complete2 <- complete %>% 
    select(c(sym(paste0(management2, '_mean_diff')), 
             sym(paste0('Mean_', sub('.*_', '', variable))), 
             sym(paste0('mean_diff_', sub('.*_', '', variable))),
             sym(paste0(management2, '_relative_diff_mean_perc')),
             sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable)))))
  if(Absolute %in% 'yes'){
    compl1 <- complete1[,c(1, 5)] %>% 
      mutate(type = paste(capitalize(management), crop, sep = ' '))
    names(compl1) <- c('yield', 'clim','type')
    compl2 <- complete2[,c(1, 5)] %>% 
      mutate(type = paste('Non-irrigated', crop, sep = ' '))
    names(compl2) <- c('yield', 'clim', 'type')
    compl12 <- rbind(compl1, compl2) %>% 
      mutate(type = factor(type))
    ifelse(crop %in% 'corn', 
           compl12 <- compl12 %>% 
             mutate(yield = yield*62.77),
           compl12 <- compl12 %>% 
             mutate(yield = yield*67.25,
                    type = gsub('_', ' ', type))) #Convert to kg/ha
    ifelse(crop %in% 'corn', 
           y1 <- -5000,
           ifelse(crop %in% 'spring_wheat', 
                  y1 <- -2000, 
                  y1 <- -1500))
    ifelse(crop %in% 'corn', 
           y2 <- 5000,
           ifelse(crop %in% 'spring_wheat', 
                  y2 <- 2000, 
                  y2 <- 1500))
    y <-  rename_climate(variable)
    comp <- ggplot(compl12) +
      geom_smooth(aes(y = yield, x = clim, fill = type, group = type),
                  col = 'black',
                  method = 'gam', 
                  formula = y~s(x, k = 3),
                  linewidth = .35) +
      scale_fill_manual(values = c('5ab4ac', '#d8b365'), name = '') +
      scale_color_manual(values = c('black', 'black'), name = '') +
      theme_bw(base_size = 9,
               base_family = 'Myriad Pro') +
      theme(plot.tag = element_text(size = 9, face = 'bold'),
            plot.title = element_text(size = 9),
            legend.title.align = 0.5,
            legend.key.height = unit(0.25, 'cm'),
            legend.position = 'bottom',
            strip.background = element_rect(fill = NA),
            legend.text = element_text(size = 9)) +
      geom_rug(sides = 'tr',
               length = unit(0.02, "npc"),
               data = compl12, aes(clim, yield),
               col = 'gray') +
      xlab(paste0('Relative change in \n ', y, ' (%)')) +
      ylab('') +
      ylim(y1, y2)
  } else {
    compl1 <- complete1[,c(4, 5)] %>% 
      mutate(type = paste(capitalize(management), crop, sep = ' '))
    names(compl1) <- c('yield', 'clim','type')
    compl2 <- complete2[,c(4, 5)] %>% 
      mutate(type = paste('Non-irrigated', crop, sep = ' '))
    names(compl2) <- c('yield', 'clim', 'type')
    compl12 <- rbind(compl1, compl2) %>% 
      mutate(type = factor(type))
    if(!crop %in% 'corn'){
      compl12 <- compl12 %>% 
        mutate(type = gsub('_', ' ', type))
    } 
    y <- rename_climate(variable)
    comp <- ggplot(compl12) +
      geom_smooth(aes(y = yield, x = clim, fill = type, group = type),
                  col = 'black',
                  method = 'gam', 
                  formula = y~s(x, k = 3),
                  size = .35) +
      scale_fill_manual(values = c('5ab4ac', '#d8b365'), name = '') +
      theme_bw(base_size = 9,
               base_family = 'Myriad Pro') +
      theme(plot.tag = element_text(size = 9, face = 'bold'),
            plot.title = element_text(size = 9),
            legend.title.align = 0.5,
            legend.key.height = unit(0.25, 'cm'),
            legend.position = 'bottom',
            strip.background = element_rect(fill = NA),
            legend.text = element_text(size = 9)) +
      geom_rug(sides = 'tr',
               length = unit(0.02, "npc"),
               data = compl12, aes(clim, yield),
               col = 'gray') +
      xlab(paste0('Relative change in \n', y, ' (%)')) +
      ylab('') +
      # ylim(-130, 130) +#this is for all gams
      ylim(-100, 100) +#for short version
      geom_rug(sides = 'tr',
               length = unit(0.02, "npc"),
               data = compl12, aes(clim, yield),
               col = 'gray')
  }
}


# Used in case study ------------------------------------------------------

plot_gam_study <- function(crop1, crop2, variable, win, Absolute, colors_crop){
  Absolute <- Absolute
  management <- 'irrigated'#Options are nonirrigated, irrigated
  x <- read.csv(paste0('Data/Processed/', crop1, '/', crop1, '_county_diff/training_', win, '_yr.csv'))[,-1]
  colnames(x)[grepl('non_irrigated', colnames(x))] <- sub('non_irrigated', 'nonirrigated', colnames(x)[grepl('non_irrigated', colnames(x))])
  x <- filter(x, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
  if(crop1 %in% 'corn'){
    x <- x %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*62.77,
             nonirrigated_mean_diff = nonirrigated_mean_diff*62.77)
  }else{
    x <- x %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*67.25,
             nonirrigated_mean_diff = nonirrigated_mean_diff*67.25)
  }
  
  y <- read.csv(paste0('Data/Processed/', crop2, '/', crop2, '_county_diff/training_', win, '_yr.csv'))[,-1]
  colnames(y)[grepl('non_irrigated', colnames(y))] <- sub('non_irrigated', 'nonirrigated', colnames(y)[grepl('non_irrigated', colnames(y))])
  y <- filter(y, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
  if(crop2 %in% 'corn'){
    y <- y %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*62.77,
             nonirrigated_mean_diff = nonirrigated_mean_diff*62.77)
  }else{
    y <- y %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*67.25,
             nonirrigated_mean_diff = nonirrigated_mean_diff*67.25)
  }
  
  
  if(!variable %in% c('winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd')){
    climate <- read.csv(paste0('Data/Processed/', variable, '/', crop1, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
  }else{
    climate <- read.csv(paste0('Data/Processed/', variable, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
  }
  fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
  climate <- left_join(climate, fips) 
  climate <- climate %>% 
    mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))#create unique identifier that matches corn data
  climate <- climate %>% 
    select(c(id, Year, sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable)))))
  
  if(Absolute %in% 'yes'){
    x <- x %>% 
      select(id, Year, irrigated_mean_diff, nonirrigated_mean_diff)
    colnames(x)[3:4] <- paste(crop1, colnames(x)[3:4], sep = '_')
    y <- y %>% 
      select(id, Year, irrigated_mean_diff, nonirrigated_mean_diff)
    colnames(y)[3:4] <- paste(crop2, colnames(y)[3:4], sep = '_')
  }else{
    x <- x %>% 
      select(id, Year, irrigated_relative_diff_mean_perc, nonirrigated_relative_diff_mean_perc)
    colnames(x)[3:4] <- paste(crop1, colnames(x)[3:4], sep = '_')
    y <- y %>% 
      select(id, Year, irrigated_relative_diff_mean_perc, nonirrigated_relative_diff_mean_perc)
    colnames(y)[3:4] <- paste(crop2, colnames(y)[3:4], sep = '_')
  }
  
  crop <- inner_join(x,y)
  crop <- pivot_longer(crop, cols = 3:6)
  crop <- crop %>% 
    mutate(type = factor(paste(ifelse(grepl('nonirrigated', crop$name), 'Non-irrigated', 'Irrigated'),
                               ifelse(grepl(crop1, crop$name), crop1, crop2), sep = ' ')))
  crop <- crop %>% 
    mutate(type = gsub('_', ' ', type))
  complete <- left_join(crop, climate)
  compl <- complete[,c(4,6,5)]
  names(compl) <- c('yield', 'clim','type')
  yl <-  rename_climate(variable)
  compl <- compl %>% 
    mutate(type = factor(type))
  compl$type <- factor(compl$type, levels = levels(compl$type)[c(1, 3, 2, 4)])
  compl_g <- ggplot(compl) +
    geom_smooth(aes(y = yield, x = clim, colour = type, group = type),
                method = 'gam', 
                formula = y~s(x, k = 3),
                size = .65, se = F) +
    scale_colour_manual(values = colors_crop, name = '') +
    theme_bw(base_size = 9,
             base_family = 'Myriad Pro') +
    theme(plot.tag = element_text(size = 9, face = 'bold'),
          plot.title = element_text(size = 9),
          legend.title.align = 0.5,
          legend.key.height = unit(0.25, 'cm'),
          legend.position = 'bottom',
          strip.background = element_rect(fill = NA),
          legend.text = element_text(size = 9)) +
    xlab(paste0('Relative change in \n', yl, ' (%)')) +
    ylab('') +
    geom_rug(sides = 'tr',
             length = unit(0.02, "npc"),
             data = compl, aes(clim, yield),
             col = 'gray') 
}

dev_gam_study <- function(crop1, crop2, variable, win, Absolute){
  Absolute <- Absolute
  management <- 'irrigated'#Options are nonirrigated, irrigated
  x <- read.csv(paste0('Data/Processed/', crop1, '/', crop1, '_county_diff/training_', win, '_yr.csv'))[,-1]
  colnames(x)[grepl('non_irrigated', colnames(x))] <- sub('non_irrigated', 'nonirrigated', colnames(x)[grepl('non_irrigated', colnames(x))])
  x <- filter(x, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
  if(crop1 %in% 'corn'){
    x <- x %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*62.77,
             nonirrigated_mean_diff = nonirrigated_mean_diff*62.77)
  }else{
    x <- x %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*67.25,
             nonirrigated_mean_diff = nonirrigated_mean_diff*67.25)
  }
  
  y <- read.csv(paste0('Data/Processed/', crop2, '/', crop2, '_county_diff/training_', win, '_yr.csv'))[,-1]
  colnames(y)[grepl('non_irrigated', colnames(y))] <- sub('non_irrigated', 'nonirrigated', colnames(y)[grepl('non_irrigated', colnames(y))])
  y <- filter(y, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
  if(crop2 %in% 'corn'){
    y <- y %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*62.77,
             nonirrigated_mean_diff = nonirrigated_mean_diff*62.77)
  }else{
    y <- y %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*67.25,
             nonirrigated_mean_diff = nonirrigated_mean_diff*67.25)
  }
  
  
  if(!variable %in% c('winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd')){
    climate <- read.csv(paste0('Data/Processed/', variable, '/', crop1, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
  }else{
    climate <- read.csv(paste0('Data/Processed/', variable, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
  }
  fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
  climate <- left_join(climate, fips) 
  climate <- climate %>% 
    mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))#create unique identifier that matches corn data
  climate <- climate %>% 
    select(c(id, Year, sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable)))))
  
  if(Absolute %in% 'yes'){
    x <- x %>% 
      select(id, Year, irrigated_mean_diff, nonirrigated_mean_diff)
    colnames(x)[3:4] <- paste(crop1, colnames(x)[3:4], sep = '_')
    y <- y %>% 
      select(id, Year, irrigated_mean_diff, nonirrigated_mean_diff)
    colnames(y)[3:4] <- paste(crop2, colnames(y)[3:4], sep = '_')
  }else{
    x <- x %>% 
      select(id, Year, irrigated_relative_diff_mean_perc, nonirrigated_relative_diff_mean_perc)
    colnames(x)[3:4] <- paste(crop1, colnames(x)[3:4], sep = '_')
    y <- y %>% 
      select(id, Year, irrigated_relative_diff_mean_perc, nonirrigated_relative_diff_mean_perc)
    colnames(y)[3:4] <- paste(crop2, colnames(y)[3:4], sep = '_')
  }
  
  crop <- inner_join(x,y)
  crop <- pivot_longer(crop, cols = 3:6)
  crop <- crop %>% 
    mutate(type = factor(paste(ifelse(grepl('nonirrigated', crop$name), 'Non-irrigated', 'Irrigated'),
                               ifelse(grepl(crop1, crop$name), crop1, crop2), sep = ' ')))
  crop <- crop %>% 
    mutate(type = gsub('_', ' ', type))
  complete <- left_join(crop, climate)
  colnames(complete)[c(4,6)] <- c('yield', 'clim')
  complete <- complete %>% 
    mutate(type = factor(type))
  g1 <- summary(gam(yield~s(clim, k=3), data = filter(complete, type %in% levels(complete$type)[1]), method = "REML"))
  g2 <- summary(gam(yield~s(clim, k=3), data = filter(complete, type %in% levels(complete$type)[2]), method = "REML"))
  g3 <- summary(gam(yield~s(clim, k=3), data = filter(complete, type %in% levels(complete$type)[3]), method = "REML"))
  g4 <- summary(gam(yield~s(clim, k=3), data = filter(complete, type %in% levels(complete$type)[4]), method = "REML"))
  df <- data.frame(crop = c(levels(complete$type)[1], levels(complete$type)[2], levels(complete$type)[3], levels(complete$type)[4]),
                   variable = rep(variable, 4),
                   dev = c(g1$dev.expl*100, g2$dev.expl*100, g3$dev.expl*100, g4$dev.expl*100),
                   pval = c(g1$s.table[1, 4], g2$s.table[1, 4], g3$s.table[1, 4], g4$s.table[1, 4]))
  return(df)
}


# Used in deviance scatterplots -------------------------------------------
dev_cs <- function(files_deviance, crop1, win, type){
  var_df_1 <- read.csv(files_deviance)[,-1]
  management <- str_split_fixed(basename(files_deviance), '_', 4)[1,2]
  management <- paste(ifelse(management %in% 'nonirrigated', 'Non-irrigated', 'Irrigated'), crop1, sep = ' ')
  var_df_1 <- var_df_1 %>% 
    filter(window %in% win) %>% 
    mutate(pval_full = p_val_climate,
           dev_full = climate_dev_expl*100,
           crop = management,
           variable = paste0(str_split_fixed(basename(files_deviance), '_', 5)[1,3], '_', str_split_fixed(basename(files_deviance), '_', 5)[1,4])) %>% 
    select(pval_full, dev_full, crop, variable)
  var_df_1 <- var_df_1 %>% 
    mutate(crop = factor(gsub('_', ' ', management)))
  return(var_df_1)
}

plot_gam_study_short <- function(crop1, crop2, variable, win, Absolute, colors_crop, y1, y2){
  Absolute <- Absolute
  management <- 'irrigated'#Options are nonirrigated, irrigated
  x <- read.csv(paste0('Data/Processed/', crop1, '/', crop1, '_county_diff/training_', win, '_yr.csv'))[,-1]
  colnames(x)[grepl('non_irrigated', colnames(x))] <- sub('non_irrigated', 'nonirrigated', colnames(x)[grepl('non_irrigated', colnames(x))])
  x <- filter(x, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
  if(crop1 %in% 'corn'){
    x <- x %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*62.77,
             nonirrigated_mean_diff = nonirrigated_mean_diff*62.77)
  }else{
    x <- x %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*67.25,
             nonirrigated_mean_diff = nonirrigated_mean_diff*67.25)
  }
  
  y <- read.csv(paste0('Data/Processed/', crop2, '/', crop2, '_county_diff/training_', win, '_yr.csv'))[,-1]
  colnames(y)[grepl('non_irrigated', colnames(y))] <- sub('non_irrigated', 'nonirrigated', colnames(y)[grepl('non_irrigated', colnames(y))])
  y <- filter(y, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
  if(crop2 %in% 'corn'){
    y <- y %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*62.77,
             nonirrigated_mean_diff = nonirrigated_mean_diff*62.77)
  }else{
    y <- y %>% 
      mutate(irrigated_mean_diff = irrigated_mean_diff*67.25,
             nonirrigated_mean_diff = nonirrigated_mean_diff*67.25)
  }
  
  
  if(!variable %in% c('winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd')){
    climate <- read.csv(paste0('Data/Processed/', variable, '/', crop1, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
  }else{
    climate <- read.csv(paste0('Data/Processed/', variable, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
  }
  fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
  climate <- left_join(climate, fips) 
  climate <- climate %>% 
    mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))#create unique identifier that matches corn data
  climate <- climate %>% 
    select(c(id, Year, sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable)))))
  
  if(Absolute %in% 'yes'){
    x <- x %>% 
      select(id, Year, irrigated_mean_diff, nonirrigated_mean_diff)
    colnames(x)[3:4] <- paste(crop1, colnames(x)[3:4], sep = '_')
    y <- y %>% 
      select(id, Year, irrigated_mean_diff, nonirrigated_mean_diff)
    colnames(y)[3:4] <- paste(crop2, colnames(y)[3:4], sep = '_')
  }else{
    x <- x %>% 
      select(id, Year, irrigated_relative_diff_mean_perc, nonirrigated_relative_diff_mean_perc)
    colnames(x)[3:4] <- paste(crop1, colnames(x)[3:4], sep = '_')
    y <- y %>% 
      select(id, Year, irrigated_relative_diff_mean_perc, nonirrigated_relative_diff_mean_perc)
    colnames(y)[3:4] <- paste(crop2, colnames(y)[3:4], sep = '_')
  }
  
  crop <- inner_join(x,y)
  crop <- pivot_longer(crop, cols = 3:6)
  crop <- crop %>% 
    mutate(type = factor(paste(ifelse(grepl('nonirrigated', crop$name), 'Non-irrigated', 'Irrigated'),
                               ifelse(grepl(crop1, crop$name), crop1, crop2), sep = ' ')))
  crop <- crop %>% 
    mutate(type = gsub('_', ' ', type))
  complete <- left_join(crop, climate)
  compl <- complete[,c(4,6,5)]
  names(compl) <- c('yield', 'clim','type')
  yl <-  rename_climate(variable)
  compl <- compl %>% 
    mutate(type = factor(type))
  compl$type <- factor(compl$type, levels = levels(compl$type)[c(1, 3, 2, 4)])
  compl_g <- ggplot(compl) +
    geom_smooth(aes(y = yield, x = clim, colour = type, group = type),
                method = 'gam', 
                formula = y~s(x, k = 3),
                size = .65, se = F) +
    scale_colour_manual(values = colors_crop, name = '') +
    theme_bw(base_size = 9,
             base_family = 'Myriad Pro') +
    theme(plot.tag = element_text(size = 9, face = 'bold'),
          plot.title = element_text(size = 9),
          legend.title.align = 0.5,
          legend.key.height = unit(0.25, 'cm'),
          legend.position = 'bottom',
          strip.background = element_rect(fill = NA),
          legend.text = element_text(size = 9)) +
    xlab(paste0('Relative change in \n', yl, ' (%)')) +
    ylab('') +
    ylim(y1, y2)
}


get_dev_dev <- function(crop, variable, management, type, win){
    x <- read.csv(paste0('Data/Processed/', crop, '/', crop, '_county_diff/training_', win, '_yr.csv'))[,-1]
    colnames(x)[grepl('non_irrigated', colnames(x))] <- sub('non_irrigated', 'nonirrigated', colnames(x)[grepl('non_irrigated', colnames(x))])
    x <- filter(x, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
    colnames(x)[1] <- 'id'
    if(!variable %in% c('winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd')){
      climate <- read.csv(paste0('Data/Processed/', variable, '/', crop, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
    }else{
      climate <- read.csv(paste0('Data/Processed/', variable, '/', variable, '_diff_1980-2020_', win, '_yr.csv'))[,-1]
    }
    fips <- read.csv('/Users/viig7608/Desktop/CCP/state-geocodes-v2016.csv')#fips and state names
    climate <- left_join(climate, fips) 
    climate <- climate %>% 
      mutate(id = paste(toupper(State), toupper(NAME), sep = '_'))#create unique identifier that matches corn data
    complete <- left_join(x, climate, by = c('id', 'Year'))
    county_coords <- read.csv('Data/Processed/county_coords.csv')[,-1]
    complete <- left_join(complete, county_coords)
    validation <- read.csv(paste0('Data/Processed/', crop, '/', crop, '_county_diff/validation_', win, '_yr.csv'))[,-1]
    colnames(validation)[grepl('non_irrigated', colnames(validation))] <- sub('non_irrigated', 'nonirrigated', colnames(validation)[grepl('non_irrigated', colnames(validation))])
    validation <- filter(validation, !is.na(nonirrigated_mean_diff) & !is.na(irrigated_mean_diff))
    validation <- left_join(validation, climate, by = c('id', 'Year'))
    validation <- left_join(validation, county_coords)
    if(type %in% 'absolute'){
      complete <- complete %>% 
        dplyr::select(c(sym(paste0(management, '_mean_diff')), 
                        sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable))), lat, long))
      if(crop %in% 'corn'){
        complete[,1] <- complete[,1]*62.77
      }else if(crop %in% 'spring_wheat'){
        complete[,1] <- complete[,1]*67.25
      }else{
        complete[,1] <- complete[,1]*67.25
      }      
      names(complete)[1:2] <- c('yield', 'climate')
      validation <- validation %>% 
        dplyr::select(c(sym(paste0(management, '_mean_diff')), 
                        sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable))), lat, long))
      if(crop %in% 'corn'){
        validation[,1] <- validation[,1]*62.77
      }else if(crop %in% 'spring_wheat'){
        validation[,1] <- validation[,1]*67.25
      }else{
        validation[,1] <- validation[,1]*67.25
      }      
      names(validation)[1:2] <- c('yield', 'climate')
    }else if(type %in% 'relative'){
      complete <- complete %>% 
        dplyr::select(c(sym(paste0(management, '_relative_diff_mean_perc')), 
                        sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable))), lat, long))
      if(crop %in% 'corn'){
        complete[,1] <- complete[,1]
      }else if(crop %in% 'spring_wheat'){
        complete[,1] <- complete[,1]
      }else{
        complete[,1] <- complete[,1]
      }    
      names(complete)[1:2] <- c('yield', 'climate')
      validation <- validation %>% 
        dplyr::select(c(sym(paste0(management, '_relative_diff_mean_perc')), 
                        sym(paste0('relative_diff_mean_perc_', sub('.*_', '', variable))), lat, long))
      
      if(crop %in% 'corn'){
        validation[,1] <- validation[,1]
      }else if(crop %in% 'spring_wheat'){
        validation[,1] <- validation[,1]
      }else{
        validation[,1] <- validation[,1]
      }   
      names(validation)[1:2] <- c('yield', 'climate')
    }else{
      'Double-check'
    }
    complete <- complete[complete.cases(complete),]
    g3 <- gam(yield~s(climate, k=3), method = "REML", data = complete)
    deviance_observed <- deviance(g3)    
    validation <- validation[complete.cases(validation),]
    valid <- predict.gam(g3, newdata = validation, type = 'response')
    deviance_explained <- Dsquared(obs = validation$yield, pred = valid, 
                                   family = 'gaussian',  dismo.version=TRUE)
    
    ifelse(exists('var_df'), 'Dataframe already exists',
           var_df <- data.frame(deviance_explained_valid <- deviance_explained,
                                deviance_explained_full= summary(g3)$dev))
    rm(g3)
    write.csv(var_df, paste0(directory, '/cv_', management, '_', variable, '_', type, '.csv'))
  return(var_df)
}#Extracts deviance explained by gams, correlation between predictions and data, and smoothers for 'winter_tmean', 'spring_tmean', 'summer_tmean', 'fall_tmean', 'winter_prec', 'spring_prec', 'summer_prec', 'fall_prec', 'winter_vpd', 'spring_vpd', 'summer_vpd', 'fall_vpd'


# Figure deviance-deviance ------------------------------------------------
dev_dev <- function(files){
  x <- read.csv(files) %>% 
    mutate(management = str_to_sentence(rename_management(str_split_fixed(basename(files), '_', 5)[2])),
           variable = factor(rename_climate(paste0(str_to_sentence(str_split_fixed(basename(files), '_', 5)[3]), 
                                                   '_', str_split_fixed(basename(files), '_', 5)[4]))))
  return(x)
}




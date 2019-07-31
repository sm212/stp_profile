source('./helper_funcs.r')

indicator_list = read_csv('./indicator_list.csv')

for (i in 1:nrow(indicator_list)){
  id = indicator_list$id[[i]]
  geog_id = indicator_list$geog_id[[i]]
  
  # Download data
  if (!dir.exists('./data/')){
    dir.create('./data/')
  }
  get_data(id, geog_id)
  get_data(id, 102) # Try to get county level data
  df = load_data(id, geog_id)
  latest_time = tail(df$Timeperiod, 1)
  earliest_time = head(df$Timeperiod, 1)
  
  # Remove NHS & CCG from area names
  df$AreaName = str_replace_all(df$AreaName, 'NHS ', '')
  df$AreaName  = str_replace_all(df$AreaName, ' CCG', '')
  
  area_codes = geog_lookup[as.character(geog_id)][[1]]
  
  # Make outputs for each sex & age combination
  sex_values = unique(df$Sex)
  age_values = unique(df$Age)
  
  for (sex in sex_values){
    for (age in age_values){
      df_filtered = df %>%
        filter(Sex == sex & Age == age)
      
      bar_latest = plot_bars(df_filtered, area_codes, latest_time)
      bar_earliest = plot_bars(df_filtered, area_codes, earliest_time)
      point = plot_tiefighters(df_filtered, area_codes)
      ranks = rank_areas(df_filtered, area_codes, latest_time)
      best = head(ranks$AreaName, 1)
      worst = tail(ranks$AreaName, 1)
      trend = plot_trend(df_filtered, c(best, worst, 'England'))
      rank_out = rank_areas(df_filtered, area_codes, latest_time, 
                            add_comparator = T)
      
      # Write out
      if (!dir.exists('./out/')) {
        dir.create('./out/')
      }
      
      file_suffix = paste0('_', sex, '_', age)
      file_suffix = str_replace(file_suffix, '[><]', '')
      width = 17
      height = 12
      
      ggsave(paste0('./out/bar_early', id, '_', geog_id, file_suffix, '.png'), 
             bar_earliest, width = width, height = height, units = 'cm')
      ggsave(paste0('./out/bar_late', id, '_', geog_id, file_suffix, '.png'), 
             bar_latest, width = width, height = height, units = 'cm')
      ggsave(paste0('./out/point', id, '_', geog_id, file_suffix, '.png'), 
             point, width = width, height = height, units = 'cm')
      ggsave(paste0('./out/trend', id, '_', geog_id, file_suffix, '.png'), 
             trend, width = width, height = height, units = 'cm')
      write_csv(rank_out, paste0('./out/rank', id, '_', geog_id, file_suffix, '.csv'))
    }
  }
}
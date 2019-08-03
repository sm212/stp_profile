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
      
      bar = plot_bar(df_filtered, area_codes)
      point = plot_tiefighters(df_filtered, area_codes)
      
      ranks = rank_areas(df_filtered, area_codes)
      
      best = ranks %>%
        filter(rank == max(rank, na.rm = T)) %>%
        select(AreaName)
      worst = ranks %>%
        filter(rank == min(rank, na.rm = T)) %>%
        select(AreaName)
      trend = plot_trend(df_filtered, c(best, worst, 'England'))
      
      # Write out
      if (!dir.exists('./out/')) {
        dir.create('./out/')
      }
      
      file_suffix = paste0('_', sex, '_', age)
      file_suffix = str_replace(file_suffix, '[><]', '')
      width = 17
      height = 12
      
      ggsave(paste0('./out/bar', id, '_', geog_id, file_suffix, '.png'), 
             bar, width = width, height = height, units = 'cm')
      ggsave(paste0('./out/trend', id, '_', geog_id, file_suffix, '.png'), 
             trend, width = width, height = height, units = 'cm')
      write_csv(ranks, paste0('./out/rank', id, '_', geog_id, file_suffix, '.csv'))
    }
  }
}
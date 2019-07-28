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
  df = load_data(id, geog_id)
  
  # Load data
  area_codes = geog_lookup[as.character(geog_id)][[1]]
  
  
  # Make outputs for each sex & age combination
  sex_values = unique(df$Sex)
  age_values = unique(df$Age)
  
  for (sex in sex_values){
    for (age in age_values){
      df_filtered = df %>%
        filter(Sex == sex & Age == age)
      
      bar = plot_bars(df_filtered, area_codes)
      ranks = rank_areas(df_filtered, area_codes)
      best = head(ranks$AreaName, 1)
      worst = tail(ranks$AreaName, 1)
      trend = plot_trend(df_filtered, c(best, worst, 'England'))
      
      # Write out
      if (!dir.exists('./out/')) {
        dir.create('./out/')
      }
      
      file_suffix = paste0('_', sex, '_', age)
      file_suffix = str_replace(file_suffix, '[><]', '')
      
      ggsave(paste0('./out/bar', id, '_', geog_id, file_suffix, '.png'), bar)
      ggsave(paste0('./out/trend', id, '_', geog_id, file_suffix, '.png'), trend)
      write_csv(ranks, paste0('./out/rank', id, '_', geog_id, file_suffix, '.csv'))
    }
  }
}
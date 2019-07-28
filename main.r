source('./helper_funcs.r')

indicator_list = read_csv('./indicator_list.csv')

for (i in 1:nrow(indicator_list)){
  id = indicator_list$id[[i]]
  geog_id = indicator_list$geog_id[[i]]
  
  # Download data
  if (!dir.exisits('./data/')){
    dir.create('./data/')
  }
  get_data(id, geog_id)
  
  # Make plots
  bar = plot_bars(id, geog_id)
  ranks = rank_areas(id, geog_id)
  best = head(ranks$AreaName, 1)
  worst = tail(ranks$AreaName, 1)
  trend = plot_trend(id, geog_id, c(best, worst, 'England'))
  
  # Write out
  if (!dir.exists('./out/')) {
    dir.create('./out/')
  }
  ggsave(paste0('./out/bar', id, '_', geog_id, '.png'), bar)
  ggsave(paste0('./out/trend', id, '_', geog_id, '.png'), trend)
  write_csv(ranks, paste0('./out/', id, '_', geog_id, '_ranks.csv'))
}
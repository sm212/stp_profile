library(tidyverse)
library(fingertipsR)

source('./helper_funcs.r')

indicator_list = read_csv('./indicator_list.csv')
i= 5
for (i in 1:nrow(indicator_list)){
  id = indicator_list$id[[i]]
  geog_id = indicator_list$geog_id[[i]]
  
  get_data(id, geog_id)
  bar = plot_bars(id, geog_id)
  ranks = rank_areas(id, geog_id)
  trend = plot_trend(id, geog_id, c('Chelmsford', 'Colchester', 'England'))
  
  # Write out
  if (!dir.exists('./out/')) {
    dir.create('./out/')
  }
  ggsave(paste0('./out/bar', id, '_', geog_id, '.png'), bar)
  ggsave(paste0('./out/trend', id, '_', geog_id, '.png'), trend)
  write_csv(ranks, paste0('./out/', id, '_', geog_id, '_ranks.csv'))
}
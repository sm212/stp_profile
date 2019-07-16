library(tidyverse)
library(fingertipsR)

all_indicators = indicators_unique()
all_indicators$IndicatorName = as.character(all_indicators$IndicatorName)

indicator_ids = vector('integer')
indicator_names = vector('character')
geog_ids = vector('integer')
data_starts = vector('character')
data_ends = vector('character')
data_points = vector('integer')

for (i in seq_along(1:nrow(all_indicators))){
  # Get indicator info
  indicator_id = all_indicators$IndicatorID[[i]]
  indicator_name = all_indicators$IndicatorName[[i]]
  
  cat(paste('Trying id', i, 'of', nrow(all_indicators)), '\n',
      indicator_name, '\n')
  
  # See if indicator data is available at geog
  for (geog_id in c(3, 101, 152, 154, 165)){
    msg = paste('\tTrying indicator id:', indicator_id,
                'geog id:', geog_id, '...')
    df = fingertips_data(IndicatorID = indicator_id, AreaTypeID = geog_id)  
    
    if (nrow(df) > 0){
      data_start = head(df$Timeperiod, 1)
      data_end = tail(df$Timeperiod, 1)
      n = length(unique(df$Timeperiod))
      
      indicator_ids = c(indicator_ids, indicator_id)
      indicator_names = c(indicator_names, indicator_name)
      geog_ids = c(geog_ids, geog_id)
      data_starts = c(data_starts, data_start)
      data_ends = c(data_ends, data_end)
      data_points = c(data_points, n)
      
      found = length(indicator_ids)
      msg = paste(msg, 'Data found! Total number of indicators:', found)
    }
    cat(msg, '\n')
  }
}

out = bind_cols(list(id = indicator_ids, name = indicator_names, geog = geog_ids, 
                     start = data_starts, end = data_ends, n = data_points))

indicator_info = indicators()

out %>%
  left_join(indicator_info, by = c('id' = 'IndicatorID')) %>%
  select(id, name, geog, start, end, n, ProfileName) %>%
  mutate(geog = ifelse(geog == 3, 'MSOA',
                        ifelse(geog == 101, 'District', 'CCG'))) %>%
  distinct() %>%
  write_csv('./all_indicators.csv')
library(tidyverse)
library(fingertipsR)


col_spec = cols(
  .default = col_skip(),
  IndicatorName = col_character(),
  AreaCode = col_character(),
  AreaName = col_character(),
  Sex = col_character(),
  Age = col_character(),
  Value = col_double(),
  LowerCI95.0limit = col_double(),
  UpperCI95.0limit = col_double(),
  Count = col_double(),
  Denominator = col_double(),
  Timeperiod = col_character(),
  TimeperiodSortable = col_double()
)

geog_lookup = list('101' = c('E06000033', 'E06000034', 'E07000066', 
                           'E07000067', 'E07000068', 'E07000069', 
                           'E07000070', 'E07000074', 'E07000075'),
                   '152' = c('E38000007', 'E38000030', 'E38000106', 
                           'E38000168', 'E38000185'),
                   '154' = c('E38000007', 'E38000030', 'E38000106', 
                           'E38000168', 'E38000185'),
                   '165' = c('E38000007', 'E38000030', 'E38000106', 
                           'E38000168', 'E38000185'))

get_data = function(indicator_id, geog_type, out_path = './data/'){
  # Get indicator data at a specified geographic level.
  #
  # Checks if file already exists in out_path folder. Function only downloads
  # data if file not found.
  #
  # Saves files with the name <indicator_id>_<geog_type>.csv
  
  file_name = paste0(out_path, indicator_id, '_', geog_type, '.csv')
  
  if (file.exists(file_name)){
    cat('File already exisits in', out_path, '\n')
  }
  else{
    df = fingertips_data(IndicatorID = indicator_id,
                         AreaTypeID = geog_type)
    
    write_csv(df, file_name)
  }
}

plot_bars = function(indicator_id, geog_type, data_path = './data/'){
  # Plots latest data as bar chart, with error bars
  
  # Load data & get latest filter criteria
  data_path = paste0(data_path, indicator_id, '_', geog_type, '.csv')
  df = read_csv(data_path, col_types = col_spec)
  
  geog_ids = geog_lookup[as.character(geog_type)][[1]]
  latest_time = tail(df$Timeperiod, 1)
  
  df_bar = df %>%
    filter(AreaCode %in% geog_ids & Timeperiod == latest_time)
  
  df_comparator = df %>%
    filter(AreaCode == 'E92000001' & Timeperiod == latest_time)
  
  bar_plot =
    ggplot(df_bar, aes(x = AreaName, y = Value)) +
    geom_bar(position = 'dodge', stat = 'identity') +
    geom_errorbar(aes(ymin = LowerCI95.0limit, ymax = UpperCI95.0limit),
                  width = 0.2) +
    geom_hline(data = df_comparator, mapping = aes(yintercept = Value))
  
  
  
  return(bar_plot)
}

plot_bars(93307, 154)


plot_trend = function(indicator_id, geog_type){
  # Plots all data in df
}


plot_map = function(df, geog){
  
}



df = read_csv('./data/1203_101.csv', col_types = col_spec)










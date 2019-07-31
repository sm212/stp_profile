library(tidyverse)
library(fingertipsR)
library(RColorBrewer)

# Specify which columns to read in from .csv's
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

# Lookup - Underlying Essex geographies for each health geography
geog_lookup = list('101' = c('E06000033', 'E06000034', 'E07000066', 
                             'E07000067', 'E07000068', 'E07000069', 
                             'E07000070', 'E07000074', 'E07000075'),
                   '152' = c('E38000007', 'E38000030', 'E38000106', 
                             'E38000168', 'E38000185'),
                   '154' = c('E38000007', 'E38000030', 'E38000106', 
                             'E38000168', 'E38000185'),
                   '165' = c('E38000007', 'E38000030', 'E38000106', 
                             'E38000168', 'E38000185'))

# Colours
blues = brewer.pal(12, 'Blues')
sig = rev(brewer.pal(3, 'Accent'))

get_data = function(indicator_id, geog_type, data_path = './data/'){
  # Get indicator data at a specified geographic level.
  #
  # Checks if file already exists in data_path folder. Function only downloads
  # data if file not found.
  #
  # Saves files with the name <indicator_id>_<geog_type>.csv
  
  file_name = paste0(data_path, indicator_id, '_', geog_type, '.csv')
  
  if (file.exists(file_name)){
    cat('File already exisits in', data_path, '\n')
  }
  else{
    df = fingertips_data(IndicatorID = indicator_id,
                         AreaTypeID = geog_type)
    
    write_csv(df, file_name)
  }
}

load_data = function(indicator_id, geog_type, data_path = './data/'){
  # Loads csv into a dataframe
  
  data_path = paste0(data_path, indicator_id, '_', geog_type, '.csv')
  df = read_csv(data_path, col_types = col_spec)  
  
  return(df)
}

rank_areas = function(df, area_codes, add_comparator = F,
                      data_path = './data/'){
  # Ranks areas by value, compares areas by looking at confidence
  # interval overlap compared to England value
  
  
  # Create plot dataframes & plot
  latest_time = tail(df$Timeperiod, 1)
  comparator = df %>%
    filter(AreaCode == 'E92000001' & Timeperiod == latest_time)
  
  df_rank = df %>%
    filter(AreaCode %in% area_codes & Timeperiod == latest_time) %>%
    group_by(Sex, Age) %>%
    arrange(desc(Value)) %>%
    select(IndicatorName, AreaName, Timeperiod, Sex, Age, Value, 
           LowerCI95.0limit, UpperCI95.0limit) %>%
    mutate(diff_vs_eng = ifelse(UpperCI95.0limit < comparator$LowerCI95.0limit, 
                                'Lower', 
                                ifelse(comparator$UpperCI95.0limit < LowerCI95.0limit,
                                       'Higher', 
                                       'Similar')),
           rank = dense_rank(desc(Value))) %>%
    mutate_if(is.numeric, ~round(., 2))
  
  if (add_comparator == T){
    comparator = comparator %>%
      select(IndicatorName, AreaName, Timeperiod, Sex, Age, Value, 
             LowerCI95.0limit, UpperCI95.0limit)
    
    df_rank = bind_rows(list(df_rank, comparator))
  }
  
  return(df_rank)
}

plot_bars = function(df, area_codes, data_path = './data/'){
  # Plots latest data as bar chart, with error bars
  
  latest_time = tail(df$Timeperiod, 1)
  df_bar = df %>%
    filter(AreaCode %in% area_codes & Timeperiod == latest_time)
  df_comparator = df %>%
    filter(AreaCode == 'E92000001' & Timeperiod == latest_time)
  
  if (nrow(df_bar) > 0){
    bar_plot = ggplot(df_bar, aes(x = AreaName, y = Value)) +
      geom_bar(position = 'dodge', stat = 'identity', fill = 'deepskyblue4') +
      geom_errorbar(aes(ymin = LowerCI95.0limit, ymax = UpperCI95.0limit),
                    width = 0.2) +
      geom_hline(data = df_comparator, mapping = aes(yintercept = Value)) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1)) +
      labs(x = NULL, y = NULL,
           title = df_bar$IndicatorName[[1]],
           subtitle = paste(latest_time, df_bar$Sex[[1]], df_bar$Age[[1]]))
    
    return(bar_plot)  
  }
  
}

plot_trend = function(df, areas, data_path = './data/'){
  # Plots all historic data for the areas specified
  
  df_plot = df %>%
    filter(AreaName %in% areas)
  
  if (nrow(df_plot) > 0){
    p = ggplot(df_plot, aes(x = Timeperiod, y = Value, 
                            colour = AreaName, group = AreaName)) +
      geom_point() +
      geom_line() +
      theme_light() +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1)) +
      labs(x = NULL, y = NULL,
           title = df_plot$IndicatorName[[1]],
           subtitle = paste(df_plot$Sex[[1]], df_plot$Age[[1]])) +
      scale_colour_manual(values = sig)
    
    return(p)
  }
  
}

plot_tiefighters = function(df, area_codes, data_path = './data/'){
  # Similar to plot_bar, but plots each area as a point
  
  df_plot = rank_areas(df, area_codes)
  
  latest_time = tail(df$Timeperiod, 1)
  comparator = df %>%
    filter(AreaCode == 'E92000001' & Timeperiod == latest_time)
  
  if (nrow(df_plot) > 0){
    plot = df_plot %>%
      mutate(AreaName = fct_reorder(AreaName, Value)) %>%
      ggplot(aes(x = AreaName, y = Value)) +
      geom_ribbon(aes(ymin = LowerCI95.0limit, ymax = UpperCI95.0limit), 
                  colour = 'grey80', size = 2.5, alpha = 0.75) +
      geom_hline(aes(yintercept = Value), comparator, 
                 colour = 'grey50', size = 1) +
      geom_point(size = 4, aes(colour = diff_vs_eng)) +
      coord_flip() +
      theme_light() +
      scale_colour_manual(values = sig) +
      labs(x = NULL, y = NULL,
           title = df_plot$IndicatorName[[1]],
           subtitle = paste(df_plot$Sex[[1]], df_plot$Age[[1]]))
    
    return(plot)
  }
  
}
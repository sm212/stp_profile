library(tidyverse)
library(RColorBrewer)

# Specify which columns to read in from .csv's
col_spec = cols(
  .default = col_skip(),
  IndicatorID = col_integer(),
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

get_data = function(indicator_id, geog_type, data_path = './data/'){
  # Get indicator data at a specified geographic level.
  #
  # Checks if file already exists in data_path folder. Function only downloads
  # data if file not found.
  #
  # Saves files with the name <indicator_id>_<geog_type>.csv
  
  file_name = paste0(data_path, indicator_id, '_', geog_type, '.csv')
  
  if (!file.exists(file_name)){
    df = fingertips_data(IndicatorID = indicator_id,
                         AreaTypeID = geog_type)
    
    write_csv(df, file_name)
  }
}

load_data = function(indicator_id, geog_type, data_path = './data/'){
  # Loads csv into a dataframe if it exists in data_path, else return FALSE
  
  file_name = paste0(data_path, indicator_id, '_', geog_type, '.csv')
  if (file.exists(file_name)){
    df = read_csv(file_name, col_types = col_spec)
  }
  else{
    df = read_csv('./data/empty.csv', col_types = col_spec)
  }
  
  df = df %>%
    rename(Lower = LowerCI95.0limit, Upper = UpperCI95.0limit) %>%
    mutate(AreaName = str_replace(AreaName, 'NHS ', ''),
           AreaName = str_replace(AreaName, ' CCG', ''))
  
  return(df)
}

plot_bar = function(df, areas, data_dump = NULL){
  
  cat('plot_bar for', head(df$IndicatorName, 1), '\n')
  
  # Try to get Essex level data - may be empty if data not at County level
  indicator_id = head(df$IndicatorID, 1)
  df_essex = load_data(indicator_id, 102)
  
  # Get earliest & latest common times from each df
  time_start = max(head(df$TimeperiodSortable, 1), 
                   head(df_essex$TimeperiodSortable, 1))
  time_end = min(tail(df$TimeperiodSortable, 1), 
                 tail(df_essex$TimeperiodSortable, 1))
  
  # Get age & sex from df - need to make sure that Essex data is at the same
  # age & sex as the main data
  age = head(df$Age, 1)
  sex = head(df$Sex, 1)
  
  # Filter & append
  df_filtered = df %>%
    filter(AreaCode %in% areas & (TimeperiodSortable == time_start | 
                                    TimeperiodSortable == time_end))
  
  df_england = df %>%
    filter(AreaName == 'England' & (TimeperiodSortable == time_start | 
                                      TimeperiodSortable == time_end))
  
  df_essex = df_essex %>%
    filter(AreaName == 'Essex' & 
           (TimeperiodSortable == time_start | TimeperiodSortable == time_end) &
           Age == age &
           Sex == sex)
  
  df_plot = bind_rows(list(df_filtered, df_england, df_essex)) %>%
    mutate(AreaName = fct_relevel(AreaName, 'England', after = Inf))
  
  # Reorder factors so Essex & England bars are at the end
  if (nrow(df_essex) > 0 & length(levels(df_plot$AreaName)) > 2){
    df_plot = df_plot %>%
      mutate(AreaName = fct_relevel(AreaName, 'Essex', 
                                    after = length(levels(df_plot$AreaName)) - 2))
  }
  
  # Plot
  if(nrow(df_plot) == 0){
    return()
  }
  
  if (!is.null(data_dump)){
    df_plot %>%
      mutate(plot_type = 'bar') %>%
    write.table(data_dump, row.names = F, append = T, sep = ',')
  }
  
  p = df_plot %>%
    ggplot(aes(x = AreaName, y = Value, 
               fill = as.factor(TimeperiodSortable / 10000))) +
    geom_bar(colour = 'black', position = 'dodge', stat = 'identity') +
    geom_errorbar(aes(ymin = Lower, ymax = Upper),
                  position = position_dodge(width = 0.9), width = 0.5) +
    scale_fill_brewer() +
    theme_light() +
    theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.title = element_blank()) +
    labs(x = NULL, y = NULL,
         title = df_plot$IndicatorName[[1]],
         subtitle = paste(df_plot$Sex[[1]], df_plot$Age[[1]]))
  
  return(p)
}

rank_areas = function(df, areas, time_period=NULL){
  
  cat('rank_areas for ', head(df$IndicatorName, 1), '\n')
  
  if (nrow(df) == 0){
    return()
  }
  
  
  # Check if time_period provided
  if (is.null(time_period)){
    # Set to latest Timeperiod in data
    time_period = tail(df$TimeperiodSortable, 1)
  }
  
  # Try to get Essex level data - may be empty if data not at County level
  indicator_id = head(df$IndicatorID, 1)
  df_essex = load_data(indicator_id, 102)
  
  # Get relevant dataframes
  df_filtered = df %>%
    filter(AreaCode %in% areas & TimeperiodSortable == time_period)
  
  df_england = df %>%
    filter(AreaName == 'England' & TimeperiodSortable == time_period)
  
  df_essex = df_essex %>%
    filter(AreaName == 'Essex' & TimeperiodSortable == time_period)
  
  df_rank = df_filtered %>%
    mutate(rank = dense_rank(desc(Value)),
           vs_eng = ifelse(df_filtered$Upper < df_england$Lower, 'lower',
                           ifelse(df_england$Upper < df_filtered$Lower, 'higher', 
                                  'similar'))) %>%
    arrange(desc(Value))
  
  
  # Add in essex comparison column if possible
  if (nrow(df_essex) > 0){
    df_rank = df_rank %>%
      mutate(vs_ess = ifelse(df_filtered$Upper < df_essex$Lower, 'lower',
                             ifelse(df_essex$Upper < df_filtered$Lower, 'higher', 
                                    'similar')))
    
    # Add in comparator rows & round all numbers to 2dp
    df_out = bind_rows(list(df_rank, df_england, df_essex)) %>%
      select(IndicatorName, AreaName, Sex, Age, Timeperiod, 
             Value, rank, vs_eng, vs_ess) %>%
      mutate_if(is.numeric, ~round(., 2))
  }
  else(
    # Add in comparator rows & round all numbers to 2dp
    df_out = bind_rows(list(df_rank, df_england, df_essex)) %>%
      select(IndicatorName, AreaName, Sex, Age, Timeperiod, 
             Value, rank, vs_eng) %>%
      mutate_if(is.numeric, ~round(., 2))
  )
  
  
  
  return(df_out)
}

plot_trend = function(df, areas, data_dump = NULL){
  # Plots all historic data for the areas specified
  
  df_plot = df %>%
    filter(AreaName %in% areas)
  
  if (nrow(df_plot) > 0){
    
    if (!is.null(data_dump)){
      df_plot %>%
        mutate(plot_type = 'trend') %>%
      write.table(data_dump, row.names = F, append = T, sep = ',')
    }
    
    p = ggplot(df_plot, aes(x = Timeperiod, y = Value, 
                            colour = AreaName, group = AreaName)) +
      geom_point() +
      geom_line() +
      theme_light() +
      theme(axis.text.x = element_text(angle = 50, hjust = 1, vjust = 1),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank()) +
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
      geom_ribbon(aes(ymin = Lower, ymax = Upper), 
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
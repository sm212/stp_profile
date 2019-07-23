library(tidyverse)
library(fingertipsR)


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

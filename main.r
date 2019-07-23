library(tidyverse)
library(fingertipsR)

source('./helper_funcs.r')

# Get all data
indicator_list = read_csv('./indicator_list.csv')

for (i in 1:nrow(indicator_list)){
  id = indicator_list$id[[i]]
  geog_id = indicator_list$geog_id[[i]]
  
  get_data(id, geog_id)
}
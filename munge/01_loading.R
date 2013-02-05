start <- now()

source('src/functions.R')
source('src/functions_date.R')
source('src/functions_max_min.R')
source('src/functions_SEI.R')
source('src/functions_pre_processing.R')
source('src/functions_error_corrections.R')

if (!exists('data.dir')) {
  print('No selected data directory, please select a date. Later dates will contain more data')
  ChooseData()
}

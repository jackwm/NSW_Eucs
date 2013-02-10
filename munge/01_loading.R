start <- now()

source('lib/functions.R')
source('lib/functions_date.R')
source('lib/functions_max_min.R')
source('lib/functions_SEI.R')
source('lib/functions_pre_processing.R')
source('lib/functions_error_corrections.R')

if (!exists('data.dir')) {
  print('No selected data directory, please select a date. Later dates will contain more data')
  ChooseData()
}

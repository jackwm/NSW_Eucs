source('src/functions.R')
source('src/functions_date.R')
source('src/functions_max_min.R')
source('src/functions_SEI.R')
source('src/functions_synth.R')
source('src/functions_ZeroOrigin.R')

if (!exists('data.dir')) {
  print('No selected data directory, selecting earliest...')
  data.dir <- ChooseData(1)
}

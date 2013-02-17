# this version of MyGgsave uses a switch instead of multiple if statements
MyGgsave <- function(plot.name, plot.object, script.name){
  if (require("ggplot2") == FALSE ) stop('ggplot2 failed to load, ggsave cannot run')
  for (ft in filetypes) {
    fn <- paste(Sys.Date(), plot.name, script.name, ft, sep ="" )
    switch(ft,
           '.png' = ggsave(filename = fn,
                           path = 'graphs/',
                           plot = plot.object,
                           width=14,
                           height = 12,
                           units='cm', dpi=300),
           '.eps' = ggsave(filename = fn,
                           path = 'graphs/', 
                           plot = plot.object,
                           width = 14, 
                           height = 12, 
                           units = 'cm', scale = 1), 
           '.pdf' = ggsave(filename = fn,
                           path = 'graphs/',
                           plot = plot.object,
                           width = 14,
                           height = 12,
                           units = 'cm', scale = 1))
  }
}

# test 1 
#MyGgsave(plot.name = '_plot.j_', plot.object=j, script.name='functions_plotting')
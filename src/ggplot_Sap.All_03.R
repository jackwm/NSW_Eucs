# Plot dendromter measurements for all sites in a given DF.
#
# Input:
# Dendrometer data, will be stacked.
# User specified plotting options.
#
# Output:
# Various ggplots of raw data (ie not increment data)
#
# To do:
# Change adjustment for stem radius to BAI


###################################################################
## input user specifications here
###################################################################
# do you want to save the plots?
want.save <- TRUE
want.save <- FALSE

# what file types do youw want to save?
filetypes <- c('.pdf', '.png', '.eps')

# selecting certain dates
  spec.s <- as.POSIXct('2012-05-01')
  if (FALSE)  spec.e <- as.POSIXct('2012-05-02') 

# select wether you want 
  spec.radius.adjust <- FALSE #   
  spec.radius.adjust <- TRUE # per unit radius at breast height

# select whether you want to exclude the globoideas (T4s)
spec.keep.glob <- TRUE
spec.keep.glob <- FALSE
                                                                 #
###################################################################

# to load dendrometer data from cache
if(FALSE){
  file.list <- dir(path='./cache/', pattern="Den{1}")
  for (file in file.list) load( paste('./cache/', file, sep = "")); print(file)
}

# getting the dendrometer dataframes in the workshpace and making them into a list
# my.obj.list <- MakeList(pattern = "Den{1}") # in lib/functions_workspace.R
my.obj.list<- list(Den03A, Den03F, Den10A)

# merging those dataframes from the list into one wide dataframe
den.wide <- MergeDataframeList(target.list = my.obj.list, timestamp.col = 'TIMESTAMP')

# relative radius (relative to radius at t =  0)
DF <- den.wide

# subsetting as per user specification
DF <- DF[DF$TIMESTAMP > spec.s, ]
#DF <- DF[DF$TIMESTAMP < spec.e, ]  

# re-running zero-origin
DF <- ZeroOrigin(DF)

## stacking the dataframe
source.with.encoding('./src/fn_stack_SapAll_02.R', encoding='UTF-8') # OLD WAY 
den.tall <- StackTrees(DF)  # OLD WAY 
# den.tall <- melt(data=DF, id.vars='TIMESTAMP') # NEW WAY
  
# renaming 'Dendro' column to 'Radius' # OLD WAY
den.tall$Radius <- den.tall$Dendro
den.tall$Dendro <- NULL
head(den.tall)

# converting site labels to depth
den.tall$Site_numeric <- den.tall$Site
levels(den.tall$Site_numeric) <- list("9" = "03A", "3" = "03F", "39" = "10A")
# Things I tried here before:
# Converting depth to integer - causes probems when comparing means with ggplot
# Sorting the den.tall by depth-to-water - Doesn't change the order in plots.
  
## adjusting by radius at breast height.
  # loading the tree information
  
  tree.info <- read.csv("data/fujiwara_tree_inventory.csv", skip = 4, header = TRUE)
  tree.info$Tree <- substr(tree.info$Tree, start = 6, stop = 6)
  tree.info$Tree <- as.factor(tree.info$Tree)
  tree.info$Port <- as.factor(tree.info$Port)
  tree.info <- tree.info[ , c('Species','Port','DBH_cm', "Site")]

  # merging tree dbh info with timeseries data
  TS1 <- den.tall
  DF2 <- tree.info
  stopifnot (class(TS1$Tree) == class(DF2$Port))
  TS3 <- merge(
    x = TS1 , # [c('TIMESTAMP', 'Mean','Site','Tree')]
    y = DF2, # [c('Site', 'Tree', 'DBH_cm')]
    by.x = c('Tree', 'Site'),
    by.y = c('Port', 'Site'))
  summary(TS3)

  # calculating changes in radius per unit radius
  TS3$DBH_mm <- TS3$DBH_cm * 10 
  TS3$rad_mm <- TS3$DBH_mm / 2 
  # divide (tree growth) by (radius in m)
  TS3$rad_m <- TS3$rad_mm / 1000
  TS3$Radius_bak <- TS3$Radius
  TS3$Radius <- TS3$Radius / TS3$rad_m

# re-arranging the factor levels
# check levels(den.tall$Site_numeric) before and after to see what this does
levels(TS3$Site_numeric)
head(TS3)
#den.tall$Site_numeric <- factor(den.tall$Site_numeric, levels = c('3', '9', '39')) 
TS3$Site_numeric <- factor(TS3$Site_numeric, levels = c('3', '9', '39'))
levels(TS3$Site_numeric)
head(TS3)

# re-arranging column labels for neat plotting
colnames(den.tall)[[3]] <- 'Site_alpha'
colnames(TS3)[[2]] <- 'Site_alpha'
colnames(den.tall)[[5]] <- 'Site'
colnames(TS3)[[5]] <- 'Site'
den.tall$Depth <- den.tall$Site
TS3$Depth <- TS3$Site

# dropping out dendro 4 at 3F and 10A
TS4 <- TS3[ -c(which(TS3$Site_alpha =='10A' & TS3$Tree == 4)) , ]
TS4 <- TS4[ -c(which(TS3$Site_alpha =='03F' & TS3$Tree == 4)) , ]

# getting rid of some dataframs to clear up memory
rm(TS1)
rm(Sap03A, Sap03F, Sap03J, Sap10A)
rm(TRh03A, TRh03F, TRh03J, TRh10A)
rm(DF2)
rm(den.wide)
rm(tmp)
rm(temp)

# Checks to make sure we are ready to plot
stopifnot( is.numeric(den.tall$Radius) )
stopifnot( is.numeric(TS3$Radius) )
stopifnot( is.numeric(TS4$Radius) )

# begin plotting
###################################################################################
# Here we identify which dataframe to use for plotting,
# and set ylab to the appropriate label
if (spec.radius.adjust == FALSE) {plot.df <- den.tall;
                                  my.ylab <- "Relative stem radius (mm)"}
if (spec.keep.glob == TRUE) {plot.df <- TS3;
                             my.ylab <- expression("Relative stem radius, adjusted for \n radius at breast height (mm m" ^{-1}*")")}
if (spec.keep.glob == FALSE) {plot.df <- TS4;
                              my.ylab <- expression("Relative stem radius, adjusted for \n radius at breast height (mm m" ^{-1}*")")}

# Recreating the plot from page 56 Singer & Willett - Applied Longitudinal Data Analysis
# Shows a smoothed line for each individual
#  and one smoothed line for all the data
  plot.df$Individual <- paste(as.character(plot.df$Tree), as.character(plot.df$Site), sep = ".")
  my.xlab <- "Time"
  j <- ggplot(plot.df, aes(x = TIMESTAMP, y = Radius)) +
    theme_bw() +
    stat_smooth(colour = 'black', size = 1.5) + 
    #stat_smooth(aes(group = as.factor(Individual)), colour = 'black', size = 0.25, se = FALSE) +
    geom_line(aes(group = as.factor(Individual)), colour = 'black', size = 0.5, alpha = 0.5) +  
    scale_y_continuous(my.ylab)+
    scale_x_datetime(my.xlab)
  j  
  if (want.save ==TRUE) MyGgsave(plot.name = '_plot.j_24HR_NotSmooth', plot.object=j, script.name='ggplot_Sap.All_03')


# one trace per site - the 'mean' series
k <- ggplot(plot.df, aes(x = TIMESTAMP, y = Radius)) + theme_bw()
k <- k + scale_y_continuous("Relative stem radius (mm)")
k <- k + scale_x_datetime("") + scale_colour_hue("Site\ndtw (m)")
k <- k + stat_summary(aes(group = Site, colour = Site), fun.y = 'mean', geom = 'smooth')
#k <- k + stat_summary(aes(group = Site, colour = Site), fun.y = 'sum', geom = 'line')
k

  # last plots created were named
  # 'mean site series 01 - y mm - incl glob.png'

# one trace per site - the mean series
# ADJUSTED for radius breast height
# globoidea trees dropped or kept as per user specification at the start of the script
if(spec.keep.glob == FALSE){temp <- TS4; labeltext <- 'E. Globoidea excluded' }
if(spec.keep.glob == TRUE){temp <- TS3; labeltext <- 'All trees included'}
k <- ggplot(temp, aes(x = TIMESTAMP, y = Radius)) + theme_bw()
k <- k + scale_y_continuous("Relative stem radius\nper unit radius at breast height (mm/m)")
k <- k + scale_x_datetime("")  + scale_colour_hue("Site\ndtw (m)")
#k <- k + stat_summary(aes(group = Site, colour = Site), fun.data = mean_cl_normal, geom = 'smooth')
#k <- k + stat_summary(aes(group = Site, colour = Site), fun.data = mean_sdl, geom = 'smooth')
#k <- k + stat_summary(aes(group = Site, colour = Site), fun.data = mean_cl_boot, geom = 'smooth')
k <- k + stat_summary(aes(group = Site, colour = Site), fun.y = 'mean', geom = 'line')
#k <- k + stat_summary(aes(group = Site, colour = Site), fun.y = 'sum', geom = 'line')
k
print(paste('E. globoidea trees were kept in this plot is ', spec.keep.glob))



### 3 facets, one facet for each site.
### UN-ADJUSTED for radius
# l - my first attempt
# l2 - my second attempt
# l2a - colour, too crowded
# l2b - black and white, best so far. 
# one method for adjusting x limits - y axis still includes points that aren't plotted
xlim <- c(as.POSIXct('2012-05-01'), as.POSIXct('2012-10-05'))
# another method adjusting x limits - y axis will be best
spec.e <- as.POSIXct('2012-10-05')
# making a copy dataframe we can cut down as we please
plot.DF <- den.tall
# cutting down the dataframe for plotting
plot.DF <- plot.DF[plot.DF$TIMESTAMP < spec.e, ]  
# crucial line to produce a legend in the correct order.
plot.DF$Depth <- factor(plot.DF$Depth, levels = sort(unique(as.numeric(as.character(plot.DF$Depth)))))
levels(plot.DF$Depth)
#
l <- ggplot(plot.DF)
l <- l + scale_y_continuous("Relative stem radius (mm)")
#l <- l + scale_y_log10("log10 Relative stem radius (mm)")
l <- l + scale_x_datetime("", limits = xlim)
l <- l + facet_grid(Site ~ .) + theme_bw()
#l <- l + geom_point(aes(x = TIMESTAMP, y = Radius, col = Tree), size = 1.2, alpha = 0.8)
l <- l + geom_line(aes(x = TIMESTAMP, y = Radius, linetype = Tree), size = 1.0, alpha = 0.8)
l
l+stat_smooth(aes(x = TIMESTAMP, y = Radius), method="auto", colour = "red")
# could not get the mean to work properly
l + stat_summary(aes(x = TIMESTAMP, y = Radius, group = Site), 
                 fun.y = 'mean', 
                 geom = 'line',
                 colour = 'red')
#
## second attempt - putting the aes call up front
# colour, not faceted
l2 <- ggplot(plot.df, 
            aes(x = TIMESTAMP, y = Radius)) +
  theme_bw() +
  scale_y_continuous(my.ylab)
l2a <- l2 + stat_smooth(aes(group = Site, 
                            colour = Depth,
                            linetype = Depth),
                        size = 1,
                        se = FALSE, 
                        method = 'auto') 
l2a
if(want.save == TRUE) MyGgsave(plot.name = '_plot.l2a_timeseries_', plot.object=l2a, script.name='gam')

l2aa <- l2 + stat_smooth(aes(group = Site, colour = Depth), size = 2, method = lm) 

# at this point is OK, but adding individual trees it becomes unreadable
# could possibly be better if we didn't have to use linetype  to differentiate..
l2a + geom_line(aes(linetype = Tree, colour = Depth)) 
# what if we try adding points instead? actually works better
l2a2 <- l2a + geom_point(aes(colour = Site), size = 0.1)
if (want.save == TRUE) {
  for (ft in filetypes) {
    fn <- paste(Sys.Date(), "_l2a2_", "ggplot_Sap.All_03", ft, sep ="" )
    ggsave(filename= fn, path="./graphs/", plot = l2a2)
  }
}
#
## black and white, faceted
l2b <- ggplot(plot.df,
             aes(x = TIMESTAMP, y = Radius))
l2b <- l2b + 
  stat_smooth(aes(group = Site), size = 1.5) + 
  facet_grid(Depth~.) + 
  geom_line(aes(group=Tree)) +
  theme_bw() +
  scale_y_continuous(my.ylab)
l2b
#
if (want.save == TRUE) { }
# combining the above for comparison
require(gridExtra)
grid.arrange(l2a, l2a2, l2b, ncol=2) 

# 3 facets 
# adjusted for radius
# user specified if we want to keep or drop the globoideas
# adding a linear model line
if(spec.keep.glob == TRUE) {temp <- TS3} else {temp <- TS4}
l <- ggplot(temp, aes(x = TIMESTAMP, y = Radius))
l <- l + scale_y_continuous(name = expression("Relative stem radius, adjusted for \n radius at breast height (mm m" ^-1*")") )
l <- l + scale_x_datetime("")
l <- l + facet_grid(Site ~ .) + theme_bw()
# l <- l + geom_point(aes(x = TIMESTAMP, y = Radius, col = Tree), shape = ".", alpha = 0.9)
l <- l + geom_line(aes(lineytype = Tree))
#l <- l + geom_abline(lm(temp$Radius ~ temp$TIMESTAMP), col = temp$Tree)
l
source(file="../src/calc.site.mean.onebyone.R")
 l +  geom_line(data= DF.av,
               mapping=aes(x = TIMESTAMP, y = Mean_rad_per_m),
               colour = "red",
               size = 2)
l

a<- "ggplot.l_"
fn <- paste(a, "ver.B_", b, c, sep = "")  
ggsave(filename = fn, plot = l, path="./graphs/", scale = 0.8, width=4, height = 4)

dt <- data.table(TS4)
dt[,list(mean=mean(Radius),by=group]
                            

# 12 facets, one for each tree. - NOT adjusted for stem radius
# DF.sub <-DF[DF$TIMESTAMP>as.POSIXct('2012-11-26') & DF$Tree == 4 & DF$Site == 39 , ] 
m <- ggplot(den.tall)
m <- m + facet_grid(Site ~ Tree) + theme_bw()
m <- m + scale_x_datetime(name = "")
m <- m + scale_y_continuous(name = "Relative stem radius (mm)", limits = c(-1.5, 1.5))
m <- m + geom_point(aes(x = TIMESTAMP, y = Radius), size = 1, alpha = 1)
m <- m + theme(axis.text.x = element_text(angle =-90, hjust =0, vjust =0 ))
m

ggsave(filename = paste(b, '_den_unadjusted_site10A_port_4_points_5day', c, sep = ""),
       plot = m,
       scale = 0.7 )


# one facet, one colour for each tree.
n <- ggplot(den.tall)
n <- n + theme_bw()
n <- n + geom_line(aes(x = TIMESTAMP, y = Radius, linetype = paste(Depth,Tree)), alpha = 7/8)
n

# 12 facets, one for each tree.  ADJUSTED for stem radius
#my.ylab <- expression("Something per something with superscript (um mm" ^-1*") text at the end")
m2 <- ggplot(TS4)
m2 <- m2 + facet_grid(Site ~ Tree) + theme_bw()
m2 <- m2 + scale_x_datetime(name = "")
m2 <- m2 + scale_y_continuous(name = expression("Relative stem radius, \n adjusted for RBH (mm m" ^{-1}*")") )# ,
#limits = c(-1, 6))
#m2 <- m2 + geom_point(aes(x = TIMESTAMP, y = Radius), size = .5, alpha = 1/2)
m2 <- m2 + geom_line(aes(x = TIMESTAMP, y = Radius))
m2 <- m2 + theme(axis.text.x = element_text(angle =-90, hjust =0, vjust =0 ))

a <- "ggplot_sapall_12_facets_"
fn <- paste(a, "D_", b, c, sep = "")
ggsave(filename = fn, plot = m2, path="./graphs/", scale = 0.6)

# 12 facets, one for each tree. 
  # Adjusted for stem radius. 
  # Showing smoothed curves. 
plot.o <- ggplot(plot.df, aes(x = TIMESTAMP, y = Radius)) +
    theme_bw() +
    facet_grid(Site ~ Tree) + 
  scale_y_continuous(my.ylab)
( plot.o.1 <- plot.o +  stat_smooth(method = 'lm', colour = 'black' ) )
if (want.save == TRUE) MyGgsave(plot.name = '_plot.o.1_', plot.object=plot.o.1, script.name='lm_4x3')
( plot.o.2 <- plot.o + stat_smooth(colour = 'black') )
if (want.save == TRUE) MyGgsave(plot.name = '_plot.o.2_', plot.object=plot.o.2, script.name='smoothed_4x3')

  
  
## for print

# last plots created were named 
#a <- 'mean site series 01 - y mm - incl glob LoRes '
# a <- 'mean site series 02 - y mm p m - incl glob LoRes '
#a <- 'mean site series 03 - y mm p m - excl glob LoRes '
fn <- paste(a, b, c, sep = "")
# png(filename = fn,
#     width = 2400,
#     height = 2400,
#     res = 450)
k
dev.off()
a <- 'mean site series 01 - y mm - incl glob w16h12r3 '
a <- 'mean site series 02 - y mm p m - incl glob w16h12r3 '
#a <- 'mean site series 03 - y mm p m - excl glob w16h12r3 '
fn <- paste(a, b, c, sep = "")
png(filename = fn,
    width = 1600,
    height = 1200,
    res = 300)
k
dev.off()


# a <- "ggplot_sapall_depth_facet_adjusted_inclGlob_dot"
# fn <- paste(a, b, c, sep = "")
# png(filename = fn,
#     width = 2400,
#     height = 1200,
#     res = 200)
# l
# dev.off()

# a <- "ggplot_sapall_12_facets_"
# fn <- paste(a, b, c, sep = "")
# png(filename = fn,
#     height = 1400,
#     width = 2400,
#     res = 200)
# m2
# dev.off()
ggsave(filename = fn, plot = m2, path="./graphs/")


# png(filename = "ggplot_sapall_12_colours.png"
#     width = 2400,
#     height = 2400,
#     res = 200)
# n
# dev.off()


## checking battery voltage in the specified time period
# source.with.encoding('~/Dropbox/phd/r_scripts/sap03F_missing_data.R', encoding='UTF-8')


# for online


# a <- "ggplot_sapall_depth_facet_web_"
# fn <- paste(a, b, c, sep = "")
#   png(filename = fn,
#       width = 800,
#       height = 600,
#       res = 100)
#   l
#   dev.off()

# a <- "ggplot_sapall_12_facets_web_"
# fn <- paste(a, b, c, sep = "")
#   png(filename= fn,
#       height = 800,
#       width = 600,
#       res = 100)
#   m
#   dev.off()
  
#   png(filename = "ggplot_sapall_12_colours.png",
#       width = 2400,
#       height = 2400,
#       res = 200)
#   n
#   dev.off()
  
# Plot dendromter measurements for all sites in a given DF.
#
# Input:
# Dendrometer data, will be stacked.
# User specified plotting options.
#
# Output:
# Various ggplots of raw data (ie not increment data)
#
# Changelog:
# 2012-12-12 version 0.2
#  Output filenames now defined at the start of the script
#   so that plots can be saved at any point. 
#  Added support for den.all
#
# To do:
# Clarify the keep/drop globoideas option
# Clarify the adjust/not adjust for radius option
# Change adjustment for stem radius to BAI
# * WRITE MY OWN WRAPPER FOR GGSAVE WHICH LOOPS THROUGH THE FILETYPES




###################################################################
## input user specifications here
###################################################################
# do you want to save the plots?
want.save <- FALSE
# what file types do youw want to save?
filetypes <- c('.pdf', '.png', '.eps')
#
# selecting certain dates
#spec.s <- as.POSIXct("2012-07-13 00:00:00") 
#spec.e <- as.POSIXct("2012-07-14")
spec.s <- as.POSIXct('2012-05-01')

# after specifying here, this is done in loading_00
#                                                                 #
# select wether you want 
# a) relative stem radius (FALSE) OR  
# b) relative stem radius, per unit radius at breast height
spec.radius.adjust <- FALSE # (a)  
spec.radius.adjust <- TRUE # (b)
#                                                                 #
# select wether you want the origin set to zero
#                                                                 #
# select wether you want to plot absolute radius
#                                                                 #
# select whether you want to exclude the globoideas (T4s)
spec.keep.glob <- TRUE
spec.keep.glob <- FALSE
#                                                                 #
# select where to save output
output.dir <- '~/Dropbox/phd/r_output/'
output.dir <- 'graphs/'
###################################################################

#######################################################
## Define strings to use when saving plots            #
                                #
# variables for plotnames                             #
b <- Sys.Date()                                       #
# str.sta <- format(spec.s, format = "%Y.%m.%d_%H%M") #
# str.end <- format(spec.e, format = "%Y.%m.%d_%H%M") #
# b <- paste(str.sta, str.end, sep = " ")             #
c <- ".png"                                           #
c <- ".eps"
#######################################################

########################################################
# a function to use when saving plots, a wrapper for ggsave.
# sets the correct resolution and dpi for .pngs 
# constructs a filename from the plot.object and script.name
MyGgsave <- function(plot.object, script.name){
  if (want.save == TRUE) {
    for (ft in filetypes) {
      fn <- paste(Sys.Date(), plot.object, script.name, ft, sep ="" )
      if (ft == .png){ 
        ggsave(filename = fn, path = './graphs/', plot = plot.object,
               width=14, units='cm',dpi=300)
      }
      if (ft == .eps){
        ggsave(filename = fn, path = './graphs/', plot = plot.object,
               width = 14, units = 'cm', scale = 0.7)
      }
      if (ft == .pdf){
        ggsave(filename = fn, path = './graphs/', plot = plot.object,
               width = 14, units = 'cm', scale = 0.7)
      }
    }
  }
}  
#######################################################

## grabbing the data from the cache
# loading the Rdata files
setwd('./cache/')
file.list <- dir(pattern="Den")
for (i in file.list) load(i)
setwd("..")
getwd()
# combining the dataframes into a list
my.obj.list <- list(Den03F, Den03A, Den10A)

## merging the dataframes from the list
if (length(my.obj.list) == 3) {
  den.all <- merge(my.obj.list[1], my.obj.list[2], by = 'TIMESTAMP', all = TRUE)
  den.all <- merge(den.all, my.obj.list[3], by = 'TIMESTAMP', all = TRUE)
}  else {
  stop("number of data tables is greater or less than 3. must write new code for this instance.")

}


# deciding wether to plot relative radius or absolute radius:

# absolute radius:
    #   source.with.encoding('~/Dropbox/phd/r_scripts/fn_dendro_to_dbh.R', encoding='UTF-8')
    #   DF <- stacked


# relative radius (relative to radius at t =  0)
  #source.with.encoding('~/Dropbox/phd/r_scripts/loading00.R', encoding='UTF-8')
  source.with.encoding('./src/fn_stack_SapAll_02.R', encoding='UTF-8')
  #Sap.All <- na.contiguous(Sap.All)
  #Sap.All <- GetTimeInt(Sap.All,"2012-4-27","2012-05-24 14:40:00") # subsetting to exclude messy data.
  #DF <- ZeroOrigin(DF) # this line makes the origin start at zero
  DF <- Sap.All
  DF <- den.all

  # subsetting as per user specification
  DF <- DF[DF$TIMESTAMP > spec.s, ]
  # DF <- DF[DF$TIMESTAMP < spec.e, ]  

  # re-running zero-origin
  DF <- ZeroOrigin(DF)
  
  # stacking the dataframe
  ## OLD  
  DF <- StackTrees(DF)
  str(DF)
  ## NEW  2012-12-12
  #   DF <- melt(data=DF, id.vars='TIMESTAMP')
  
  # renaming 'Dendro' column to 'Radius'
  DF$Radius <- DF$Dendro
  DF$Dendro <- NULL

  # converting site labels to depth
  DF$Site_numeric <- DF$Site
  levels(DF$Site_numeric) <- list("9" = "03A", "3" = "03F", "39" = "10A")
  
# Things I tried here before:
# Converting depth to integer - causes probems when comparing means with ggplot
# Sorting the DF by depth-to-water - Doesn't change the order in plots.
  
# adjusting by radius at breast height.
  # loading the tree information
  
  tree.info <- read.csv("data/fujiwara_tree_inventory.csv", skip = 4, header = TRUE)
  tree.info$Tree <- substr(tree.info$Tree, start = 6, stop = 6)
  tree.info$Tree <- as.factor(tree.info$Tree)
  tree.info$Port <- as.factor(tree.info$Port  )

  # merging tree dbh info with timeseries data
  TS1 <- DF
  DF2 <- tree.info
  stopifnot (class(TS1$Tree) == class(DF2$Tree))
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
  TS3$Growth_per_m <- TS3$Radius / TS3$rad_m

# re-arranging the factor levels
# check levels(DF$Site_numeric) before and after to see what this does
levels(TS3$Site_numeric)
head(TS3)
#DF$Site_numeric <- factor(DF$Site_numeric, levels = c('3', '9', '39')) 
TS3$Site_numeric <- factor(TS3$Site_numeric, levels = c('3', '9', '39'))
levels(TS3$Site_numeric)
head(TS3)

# re-arranging column labels for neat plotting
colnames(DF)[[3]] <- 'Site_alpha'
colnames(TS3)[[2]] <- 'Site_alpha'
colnames(DF)[[5]] <- 'Site'
colnames(TS3)[[5]] <- 'Site'
DF$Depth <- DF$Site
TS3$Depth <- TS3$Site

# dropping out dendro 4 at 3F and 10A
TS4 <- TS3[ -c(which(TS3$Site_alpha =='10A' & TS3$Tree == 4)) , ]
TS4 <- TS4[ -c(which(TS3$Site_alpha =='03F' & TS3$Tree == 4)) , ]

head(DF)
head(TS3)
head(TS4)

# getting rid of some dataframs to clear up memory
rm(TS1, TS3)
rm(Sap03A, Sap03F, Sap03J, Sap10A)
rm(TRh03A, TRh03F, TRh03J, TRh10A)
rm(DF2)
rm(den.all)
rm(tmp)
rm(temp)

## constructing site averages
# DF.av <-TS4 
# # # using data.table
# # DF.av <- data.table(TS4)
# # DF.av[,list(mean=mean(Growth_per_m),by=Site]
# # # using ddply
# # ddply(dt,~group,summarise,mean=mean(age),sd=sd(age))
# # using aggregate
# DF.av<- aggregate( . ~ Site * TIMESTAMP, data = DF.av, mean)
# paste(DF.av$Site, DF.av$Timestamp)
# DF.av <- aggregate( DF.av$Growth_per_m, by = list(DF.av$Site), FUN = mean)
# head(DF.av)
# tail(DF.av)
# DF.av$Site
# # using apply
# ddply

# Checks to make sure we are ready to plot
sapply(DF, class)

# begin plotting
###################################################################################
setwd(output.dir)

# Recreating the plot from page 56 Singer & Willett - Applied Longitudinal Data Analysis
# Shows a smoothed line for each individual
#  and one smoothed line for all the data
# Excluding E. glob
# Divided by radius at breast height 
if (spec.keep.glob == FALSE) { 
  plot.df <- TS4
  plot.df$Individual <- paste(as.character(plot.df$Tree), as.character(plot.df$Site), sep = ".")
  my.ylab <- "Relative stem radius\nper unit radius at breast height (mm/m)"
  my.xlab <- "Time"
  j <- ggplot(plot.df, aes(x = TIMESTAMP, y = Growth_per_m)) +
    theme_bw() +
    stat_smooth(colour = 'black', size = 1.5) + 
    stat_smooth(aes(group = as.factor(Individual)), colour = 'black', size = 0.25) +
    scale_y_continuous(my.ylab)+
    scale_x_datetime(my.xlab)
  j
  MyGgsave <- function(j, ggplot_Sap.All_03.R){
}

my.gg

# one trace per site - the 'mean' series
# unadjusted for radius breast height
k <- ggplot(DF, aes(x = TIMESTAMP, y = Radius)) + theme_bw()
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
k <- ggplot(temp, aes(x = TIMESTAMP, y = Growth_per_m)) + theme_bw()
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
plot.DF <- DF
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
l2 <- ggplot(plot.DF, 
            aes(x = TIMESTAMP, y = Radius)) +
  theme_bw() +
  scale_y_continuous("Relative stem radius (mm)")
l2a <- l2 + stat_smooth(aes(group = Site, colour = Depth), size = 2) 
l2a
MyGgsave(l2a, ggplot_Sap.All_03.R)
ggsave(filename='l2a_14cm_300dpi_scale_0.7.pdf',plot=l2a,path='./graphs/',
       units = 'cm', width = 14, scale = 0.7)

if (want.save == TRUE) {
  for (ft in filetypes) {
    fn <- paste(Sys.Date(), "_l2a_", "ggplot_Sap.All_03", ft, sep ="" )
    ggsave(filename= fn, path="./graphs/", plot=l2a)
  }
}
l2aa <- l2 + stat_smooth(aes(group = Site, colour = Depth), size = 2, method = lm) 
if (want.save == TRUE) {
  for (ft in filetypes) {
    fn <- paste(Sys.Date(), "_l2aa_", "ggplot_Sap.All_03", ft, sep ="" )
    ggsave(filename= fn, path="./graphs/", plot=l2aa)
  }
}

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
l2b <- ggplot(plot.DF,
             aes(x = TIMESTAMP, y = Radius))
l2b <- l2b + 
  stat_smooth(aes(group = Site), size = 1.5) + 
  facet_grid(Depth~.) + 
  geom_line(aes(group=Tree)) +
  theme_bw() +
  scale_y_continuous("Relative stem radius (mm)")
l2b
#
if (want.save == TRUE) {
  for (ft in filetypes) {
  fn <- paste(Sys.Date(), "_l2b_", "ggplot_Sap.All_03", ft, sep ="" )
  ggsave(filename= fn, path="./graphs/", plot = l2b)
  }
}
# combining the above for comparison
require(gridExtra)
grid.arrange(l2a, l2a2, l2b, ncol=2) 

# 3 facets 
# adjusted for radius
# user specified if we want to keep or drop the globoideas
# adding a linear model line
if(spec.keep.glob == TRUE) {temp <- TS3} else {temp <- TS4}
l <- ggplot(temp, aes(x = TIMESTAMP, y = Growth_per_m))
l <- l + scale_y_continuous(name = expression("Relative stem radius, adjusted for \n radius at breast height (mm m" ^-1*")") )
l <- l + scale_x_datetime("")
l <- l + facet_grid(Site ~ .) + theme_bw()
# l <- l + geom_point(aes(x = TIMESTAMP, y = Growth_per_m, col = Tree), shape = ".", alpha = 0.9)
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
dt[,list(mean=mean(Growth_per_m),by=group]
                            

# 12 facets, one for each tree. - NOT adjusted for stem radius
# DF.sub <-DF[DF$TIMESTAMP>as.POSIXct('2012-11-26') & DF$Tree == 4 & DF$Site == 39 , ] 
m <- ggplot(DF)
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
n <- ggplot(DF)
n <- n + theme_bw()
n <- n + geom_line(aes(x = TIMESTAMP, y = Radius, linetype = paste(Depth,Tree)), alpha = 7/8)
n

# 12 facets, one for each tree.  ADJUSTED for stem radius
#my.ylab <- expression("Something per something with superscript (um mm" ^-1*") text at the end")
  
m2 <- ggplot(TS4)
m2 <- m2 + facet_grid(Site ~ Tree) + theme_bw()
m2 <- m2 + scale_x_datetime(name = "")
m2 <- m2 + scale_y_continuous(name = expression("Relative stem radius, \n adjusted for RBH (mm m" ^-1*")") )# ,
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
plot.o <- ggplot(TS4, aes(x = TIMESTAMP, y = Growth_per_m)) +
    theme_bw() +
    facet_grid(Site ~ Tree)
( plot.o.1 <- plot.o +  stat_smooth(method = 'lm', colour = 'black' ) )
( plot.o.2 <- plot.o + stat_smooth(colour = 'black') )

  
  
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
  
model.df <- plot.df
if (model.df$Individual == NULL)
  stop('Error: The individual/specimen ID column has not been created. Go back one script and search: plot.df$Individual <- ')

# Model specification (linear). 
mod1.0 <- glm(formula = Radius ~ TIMESTAMP + Site, data= model.df)
mod1.1 <- aov(formula = Radius ~ TIMESTAMP + Site + Individual, data = model.df)
summary(mod1)
anova(mod1.1)

if (FALSE) model.df <- subset(model.df, TIMESTAMP < as.POSIXct('2012-10-01'))
mod1.0a <- lm(formula = Radius ~ TIMESTAMP, subset = Site == '3', data= model.df)
mod1.0b <- lm(formula = Radius ~ TIMESTAMP, subset = Site == '9', data= model.df)
mod1.0c <- lm(formula = Radius ~ TIMESTAMP, subset = Site == '39', data= model.df)

# Comparing linear models.
anova(mod1.0a, mod1.0c)
print('The comparison between two or more models will only be valid if they are fitted to the same dataset. See help(anova) for details.')
mod1.0a$coefficients
mod1.0c$coefficients

if (FALSE){
  # Model specification (non-linear).
  if( require(lme4) == FALSE)
    stop('Error: lme4 could not be loaded')
  (mod2.0 <- lmer(formula = Radius ~ TIMESTAMP + (1 | Site), data = model.df))
  (mod3.0 <- lmer(formula = Radius ~ TIMESTAMP + Site +  (1 | Individual), data = model.df))
  (mod4.0 <- lmer(formula = Radius ~ TIMESTAMP * Site +  (1 | Individual), data = model.df))
  
  # Comparing nonlinear models.
  anova(mod2.0, mod3.0)
  summary(mod3)
}

# Plotting
if(require(ggplot2) == FALSE)
  stop('Error: ggplot2 could not be loaded.')
model.df.sub <- subset(model.df, Individual = '1.3', select = c(TIMESTAMP, Radius))
ggplot(data = na.fail(model.df.sub), aes(x = TIMESTAMP, y = Radius)) + geom_line()

# Cut and paste from ggplot_Sap.All_03.R:
l2 <- ggplot(model.df, 
             aes(x = TIMESTAMP, y = Radius)) +
  theme_bw() +
  scale_y_continuous(my.ylab)
l2a <- l2 + stat_smooth(aes(group = Site, 
                            colour = Depth,
                            linetype = Depth),
                        size = 1,
                        se = TRUE, 
                        method = 'auto') 
l2a
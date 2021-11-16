library(tidyverse)
library(gapminder)

# Set plot width, height & resolution
options(repr.plot.width=6, repr.plot.height=4, repr.plot.res = 100)

# Not print warnings
options(warn = -1)

head(mtcars)

ggplot(mtcars, aes(x = mpg, y = hp))
# Set theme for all following plots

theme_set(theme_bw())

ggplot(data = mtcars, aes(x = mpg, y = hp)) + geom_col()
ggplot(data = mtcars, aes(x = mpg, y = hp)) + geom_line()d

ggplot(data = mtcars, aes(x = mpg, y = disp)) + 
  geom_point(shape = 23, fill = "#00CED1", size = 4) +
  geom_smooth(formula = y ~ x, color = "red", size = 1, se = FALSE) +
  stat_smooth(formula = y ~ x, method = "lm" ,color = "blue", size = 2, se = FALSE)

ggplot(data = mtcars, aes(x = mpg, y = disp)) + 
  geom_boxplot(colour = "red", size = 2) 

p <- ggplot(data = mtcars, aes(x = mpg, y = disp))
p + geom_line(colour = "blue", linetype = 3)

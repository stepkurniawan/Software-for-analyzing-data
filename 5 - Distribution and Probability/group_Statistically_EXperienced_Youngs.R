install.packages("extRemes")
library(gapminder)
library(ggplot2)
library(tidyverse)
library(extRemes)
str(gapminder)
summary(gapminder)


## normal distribution (lifeExp of Burundi (country in Africa))

burundi <- gapminder %>% filter(country == 'Burundi')
burundilifeExp <- burundi$lifeExp
hist(burundilifeExp)
shapiro.test(burundilifeExp)

# p-value > 0.05 so this is a normal distribution
# to simulate we need to know mean and standard deviation

hist(rnorm(12, mean(burundilifeExp), sd(burundilifeExp)))

#---------------------------------------------------------------------

## exponential distribution

hist(gapminder$gdpPercap)

# simulation of exponential distribution

hist(rexp(1000, 1))

#--------------------------------------------------------------------------

## minimum extreme distribution? (I just think it looks pretty similar to one on Wiki)

europe <- gapminder %>% filter(continent == 'Europe')
europelifeExp <- europe$lifeExp
hist(europelifeExp)

#simulation of minimum extrime

hist(revd(350,loc=70,scale=10,shape=-0.5))


#----------------------------------------------------------------------------
# poisson/geometric distribution from gapminder. We are looking at population distribution by countries in 2007

year2007 <- gapminder %>% filter (year == 2007)

ggplot(year2007, aes(x = pop, fill=country)) + 
  geom_histogram(bins = 100) + 
  labs(title = "Histogram of population distribution by number of countries",     
       subtitle = "Countries vs population",
       x = "Population",
       y = "Number of countries") 



# simulation of exponential distribution
hist(year2007$pop, ylim = c(0,200), xlim = c(0,1400000000), xlab = "population") # ori
hist(rgeom(400, 0.000000005), ylim = c(0,200), xlim = c(0,1400000000), xlab = "population")

#remove outliers by limiting the population to 5.10^8
limited_year2007 <- year2007 %>% filter(pop<500000000)
hist(limited_year2007$pop , ylim = c(0,200), xlim = c(0,400000000), xlab = "population") # ori
hist(rgeom(242, 0.00000002), ylim = c(0,200), xlim = c(0,400000000), xlab = "population")

# transforming the data series with the natural logarithm by the use of log() to
# get a normal distribution from Possion distribution

log_year2007 <-log(year2007$pop)
hist(log_year2007)
shapiro.test(log_year2007)

# since p-value > 0.05, this is a normal distribution
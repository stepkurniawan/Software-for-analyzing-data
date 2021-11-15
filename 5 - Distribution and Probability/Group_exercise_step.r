library(gapminder)
library(ggplot2)
library(tidyverse)
str(gapminder)
summary(gapminder)

plot(gapminder)

#population in the world
population_limit <- 800000000
limit_pop <- gapminder %>% filter(pop < population_limit)
hist(limit_pop$pop, xlim=range(c(0, 100000000)), breaks=99)
#creating my own poisson
rpois_pop <- rpois(1200,lambda=0.2)
hist(rpois_pop)
poisson.test(rpois_pop)

norm_limit_pop <- log(limit_pop$pop)
hist(norm_limit_pop, breaks= 10)
shapiro.test(norm_limit_pop) #normal distribution test
summary(norm_limit_pop)
sd(norm_limit_pop)

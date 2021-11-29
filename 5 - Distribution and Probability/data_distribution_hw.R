library(ggplot2)
library(dplyr)
library(scales)
library(gapminder)

#make it portable
if(!require(gapminder)) {install.packages("gapminder"); library(gapminder)}
if(!require(tidyverse)) {install.packages("tidyverse"); library(tidyverse)}
if(!require(tidyverse)) {install.packages("doSNOW"); library(doSNOW)}
if(!require(tidyverse)) {install.packages("doMPI"); library(doMPI)}
if(!require(tidyverse)) {install.packages("doParallel"); library(doParallel)}

#getting useful summary of the data
str(gapminder)
gapminder
summary(gapminder)
table(gapminder$continent, gapminder$year)

#1D Plot
ggplot(gapminder, aes(x=continent, fill=continent)) + 
  geom_bar(aes(y=..count../12)) + #to better represents the number of countries 
  labs(y="Number of countries") +
  guides(fill=FALSE)

mybar <- last_plot() # save plot 

mybar + coord_trans(y="sqrt")
mybar + coord_flip()
mybar + coord_polar()

#--------------------------1D Plot: Density
ggplot(gapminder, aes(x=lifeExp)) +
  geom_density(size=3, fill=2, alpha=0.3) +
  geom_histogram(aes(y=..density.., fill = 3, alpha = 0.2, color= 1), bins = 15)

# usually the density is giving us more info than histogram
# the lifeexp plot looks bimodal -> means something is not right, we expect 
# normal dist. 


ggplot(gapminder, aes(x=lifeExp, fill=continent)) +
  geom_density(size=1, alpha=0.3) 
# so the bimodal is caused by the africa continent

#-----------------Homework------- normal distribution
#filter only africa
africa <- gapminder %>% filter(continent=="Africa")
africa %>%
  ggplot(aes(x=lifeExp, fill=continent)) +
  geom_density()

#check normal distribution of african life exp
shapiro.test(africa$lifeExp) #W = 0.97674, p-value = 2.132e-08
print(shapiro.test(africa$lifeExp)$p.value) 

#check each country in africa
ggplot(africa, aes(x=lifeExp, fill=country, alpha=0.1)) + 
  geom_density()

#using mutate, check the p-value of all african country's life exp
lifeExp_table_africa <- africa %>%
                          group_by(country) %>%
                            summarise(lifeExp.p = shapiro.test(lifeExp)$p.value) %>%
                              arrange(desc(lifeExp.p))


#filter only zimbabwe
zimbabwe <- gapminder %>% filter(country=="Zimbabwe")
ggplot(data = zimbabwe, aes(x=lifeExp, fill=country)) +
  geom_density()

#check normal distribution of zimbabwe's life exp
shapiro.test(zimbabwe$lifeExp) #W = 0.96414, p-value = 0.8409 -> normally distributed
mean_zimbabwe_lifeExp <- mean(zimbabwe$lifeExp) #52.66317
sd_zimbabwe_lifeExp <- sd(zimbabwe$lifeExp) #7.071816

#create a normally distributed data that represents Zimbabwe
hist(zimbabwe$lifeExp, title(main="Original Zimbabwe lifeExp distribution"))
my_zimbabwe_distribution = rnorm(12, mean_zimbabwe_lifeExp, sd_zimbabwe_lifeExp)
hist(my_zimbabwe_distribution)
shapiro.test(my_zimbabwe_distribution)



# ---------------- Boxplot
gap1 <- ggplot(gapminder, aes(x=gapminder$continent, y=gapminder$lifeExp, fill=gapminder$continent))
gap1 + 
  geom_boxplot(outlier.size = 3) +
  labs(y="life expectancy", x="continent") +
  guides(fill="none")

gap1 + 
  geom_boxplot(outlier.size = 3) +
  labs(y="life expectancy", x="continent") +
  guides(fill="none")

# with ordering

gapminder %>% mutate(continent = reorder(continent, lifeExp, FUN=median) ) %>%
  ggplot(aes(x=continent, y=lifeExp, fill=continent)) +
  geom_boxplot()

#------------------- GDP
ggplot(data = gapminder, aes(x=gdpPercap)) +
  geom_density()

#------------------- GDP separated through continent
ggplot(data = gapminder, aes(x=gdpPercap, fill=continent, alpha= 0.3)) +
  geom_density()

plot(gapminder)


library(tidyverse)
library(ggfortify)

theme_set(theme_bw())

#let's do a model for the cars data set
cars
cars<-cars
#define two variables, columnes
car_speed<-c(cars$speed)
car_dist<-c(cars$dist)

#decide on dependent and independent variable
#dependent variable = car distance
#independent variable = car speed
model_cars<-lm(car_dist~car_speed, data = cars)

#inspect residuals of the model
plot(model_cars$residuals)
plot(model_cars)

#check if the model residuals follow a normal distribution
hist(model_cars$residuals)

#my code -----------------------------------------------------------------------
#plot distance vs speed
ggplot(data = cars, aes(x = car_dist, y = car_speed)) + 
  geom_point(alpha = 0.3, size = 4, fill = as.factor(car_dist)) + 
  geom_smooth(formula = y ~ x, method = lm, se = FALSE) +
  labs(title = "Distance vs Speed")

#plot residuals
data.class(model_cars) # not data frame
fortified_model_cars <- fortify(model_cars) # fortify makes it a data frame
plot(fortified_model_cars)

ggplot(data = fortified_model_cars$residuals, aes(x = fortified_model_cars$.fitted, 
                                                  y = fortified_model_cars$.resid , 
                                                  color = as.factor(car_dist),
                                                  size = car_speed)) + 
  geom_point(alpha = 0.7) +
  labs(title = "Residuals of the LM model of Car",
       x = "Index",
       y = "Residuals",
       color = "car_dist") +
  scale_fill_gradient(low = "green", high = "red")

# sigma
ggplot(data = fortified_model_cars$residuals, aes(x = fortified_model_cars$.sigma, 
                                                  y = fortified_model_cars$.resid , 
                                                  color = as.factor(car_dist),
                                                  size = car_speed)) + 
  geom_point(alpha = 0.7) +
  labs(title = "Residuals of the LM model of Car based on Sigma",
       x = "Index",
       y = "Residuals",
       color = "car_dist") +
  scale_fill_gradient(low = "green", high = "red")


# SWISS dataset --------------------------------------------------------------

plot(swiss)
?swiss
plot(swiss$Fertility ~ swiss$Agriculture)
plot(swiss$Examination ~ swiss$Agriculture)

edu <- swiss$Education
exam <- swiss$Examination

ggplot(data = swiss, aes (x = edu , y = exam, size = swiss$Catholic) ) + 
  geom_point() + 
  geom_smooth(method = "lm", se= FALSE)

#principle component analysis 
biplot(prcomp(swiss)) 



# TUTORIAL ------------------------------------------------------------------
root_dir <- "C:\\Users\\steph\\OneDrive - Leuphana Universität\\Semester 1\\2. SfAD - Software for analyzing data\\Tutorial 1\\Data"
setwd(root_dir)
auto <- read.csv("Automobile.csv")
weight <- read.csv("Birthweight.csv") # dataset

weight$Birthweight_g <- (weight$Birthweight * 453.59237) # gram

library("corrplot")  # for command "corrplot()"
library("car")       # for command "qqPlot()"

plot(weight)

#execise 
# check assumption 
plot(auto)
horsepower <- auto$horsepower
engine_size <- auto$engine.size
city_mpg <- auto$city.mpg

boxplot(horsepower , engine_size, city_mpg, names = c("horsepower", "engine size", "city mpg"))
plot(engine_size, horsepower)


shapiro.test(horsepower) #p-value = 2.324e-11
shapiro.test(engine_size) #p-value = 3.057e-14

ggplot(data = auto, aes(x = engine_size, y = horsepower, size = auto$city.mpg )) + 
  geom_point() + 
  geom_smooth( method = "lm", se= F)

#correlation 
cor.test(engine_size, horsepower) #p-value < 2.2e-16

line_model <- lm(horsepower ~ engine_size)
summary(line_model)

line_model$coefficients[[1]] # intercept
line_model$coefficients[[2]] # slope



                        
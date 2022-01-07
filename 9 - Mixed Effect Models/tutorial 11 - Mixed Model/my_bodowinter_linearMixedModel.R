library("lme4")
library(tidyverse)

#get data
politeness = read.csv("http://www.bodowinter.com/tutorial/politeness_data.csv")
politeness2 = read.csv(file.choose()) #  this is how you choose file 
 
#check data
head(politeness)
str(politeness)
plot(politeness)
summary(politeness)
colnames(politeness)

#check for na
which(is.na.data.frame(politeness))
which(!complete.cases(politeness)) #how many rows that are not complete? 

#graph setup
attach(politeness)
theme_set(theme_bw())

#boxplot
ggplot(politeness, aes(y = frequency, x = attitude * gender)) +
  geom_boxplot()

boxplot(frequency ~ attitude*gender, 
        col = c ("white", "lightgray"),
        politeness )

#modeling
lmer (frequency ~ attitude, data = politeness) # error: no random variable

politeness_model = lmer (frequency ~ attitude + (1|subject) + (1|scenario), data = politeness)
summary(politeness_model) 

# because the intercept is just the avg between male and female, we include it in
# our random variable

politeness_model_s = lmer (frequency ~ attitude + gender + (1|subject) + (1|scenario) 
                           , data = politeness)
summary(politeness_model_s) 

# the variance of random effect > subject now is very low thats good
# the intercept is now representing the female informal

# compare significance
politeness_null = lmer (frequency ~ 
                          gender + (1|subject) + (1|scenario) 
                        , data = politeness, REML = FALSE)

politeness_full = lmer (frequency ~ 
                          attitude + gender + (1|subject) + (1|scenario) 
                        , data = politeness, REML = FALSE)
#compare using anova
anova (politeness_null, politeness_full)

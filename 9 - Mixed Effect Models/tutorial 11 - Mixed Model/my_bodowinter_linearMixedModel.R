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


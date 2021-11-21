library(tidyverse)

# ----------------------------------------------------------------------------
OrchardSprays
?OrchardSprays
str(OrchardSprays)
summary(OrchardSprays)
head(OrchardSprays)

#check for unique factors
unique(OrchardSprays$treatment)
unique(OrchardSprays$decrease)

theme_set(theme_bw())
ggplot(OrchardSprays, aes(x=treatment, y=decrease, fill=treatment)) +
  labs(title = "How Lime Sulphur Treatment Decreases Honeybees Apetite",
       x = "Type of Treatment (A highest, H no Lime Sulphur)",
       y = "The decrease of Lime Sulphure solution") +
  stat_boxplot(size=1, geom="errorbar") + 
  geom_boxplot(size=1)

#the more lime sulphure is in the solution, the less the bees like it. 
#hence it repels honeybees

#check for anova test
anova <- aov(decrease ~ treatment, data = OrchardSprays)
summary(anova)
TukeyHSD(anova)


# ----------------------------------------------------------------------------
#lecture example toothGrowth

?ToothGrowth
boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, 
        at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")
boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25,
        at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")
legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c ("yellow", "orange"))
#to apply an ANOVA
model1<-aov(len ~ dose*supp, data = ToothGrowth)
summary(model1) 
#Interaction is significant

summary(ToothGrowth)
unique(ToothGrowth$dose) # they only have does of 0.5, 1.0, and 2.0 

#using ggplot
ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp, shape=as.factor(dose))) + 
  labs(title = "Tooth growth based on different dose of stuff", 
       x = "dose" , 
       y = "tooth length") + 
  stat_boxplot(size = 1, geom='errorbar') + #adding whisker
  geom_boxplot( size = 1) 

model12<-aov(len ~ dose, data = ToothGrowth)
summary(model12) 

# ----------------------------------------------------------------------------
#Lecture Example 2: Insect Sprays

#To find out, what the InsectSprays data set is about: 
?InsectSprays
data(InsectSprays)
attach(InsectSprays)
tapply(count, spray, length)
boxplot(count~spray) 
# can you guess which sprays are effective by looking at the boxplot? Yes, C is the most effective
# to find out which sprays differ significantly without applying many t-tests, you can use a postdoc test
model2<-aov(count~spray)
TukeyHSD(model2)
# compare the results to the boxplot if you like

#recommended graph from InsectSprays
require(stats); require(graphics)
boxplot(count ~ spray, data = InsectSprays,
        xlab = "Type of spray", ylab = "Insect count",
        main = "InsectSprays data", varwidth = TRUE, col = "lightgray")
fm1 <- aov(count ~ spray, data = InsectSprays)
summary(fm1)
opar <- par(mfrow = c(2, 2), oma = c(0, 0, 1.1, 0))
plot(fm1)
fm2 <- aov(sqrt(count) ~ spray, data = InsectSprays)
summary(fm2)
plot(fm2)
par(opar)

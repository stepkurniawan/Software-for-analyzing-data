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

# Evgeniya's code --------------------------------------------------------

# I browsed through the available datasets and decided that this one is okay for One-Way ANOVA.
# It has one factor with 8 levels:
# from A to H, where A is the highest level of lime sulphur, G - lowest level and H - no sulphur (control).


# I am going to analyse the effect of the amount of sulphur on the Response ('decrease' column).
# First I visualized the data with boxplot

boxplot(decrease ~ treatment, data = OrchardSprays)

# Then performed the ANOVA

one.way <- aov(decrease ~ treatment, data = OrchardSprays)
summary(one.way)

# So, since it can be clearly seen from the boxplot and then confirmed by the ANOVA (very small p-value)
# I can reject the H0 hypothesis. 
# And so it is likely that the amount of sulphur have a significant effect on average decrease.

# To find how the treatment levels differ from one another, 
# I performed a Tukey???s Honestly-Significant Difference test.

TukeyHSD(one.way)

# So from this test it can be seen that the difference is insignificant between groups, that are close to each other.
# However, the further the groups from each other, the more significant the difference becomes.


# Corlie's code --------------------------------------------------------
if (!require('MASS')) install.packages('MASS'); library('MASS')


?cabbages
id<-cabbages
head(id)
str(id)
view(cabbages)

unique(cabbages$VitC) #its continuous

#checking sample sizes -> balanced dataset
id%>%group_by(Date)%>%tally()
id%>%group_by(Cult)%>%tally()

#plotting
plot(as.factor(id$Date),id$HeadWt)
plot(as.factor(id$Cult),id$HeadWt)
#boxplot(id$HeadWt~id$Date)
#all overlapping, may show no difference

### Perform one way ANOVA
##test for normality
shapiro.test(id$HeadWt) #p-value = 0.0314 -> not normally distributed

# Preliminary: Fligner-Killeen Test of Homogeneity of variance (non-parametric) 
fligner.test(id$HeadWt ~ id$Date)
fligner.test(id$HeadWt ~ id$Cult)
#the variances are equal.

# Perform one-way ANOVA on Date
model<-aov(id$HeadWt ~ id$Date)

# Model summary
summary(model)
#significant effect on headweight

# Perform one-way ANOVA on Cult
modelC<-aov(id$HeadWt ~ id$Cult)

# Model summary
summary(modelC)
#significant effect on headweight

#Two way ANOVA

# Alternative: Boxplots
boxplot(id$HeadWt ~ id$Date * id$Cult, boxwex = 0.3, cex.axis = 0.8, las = 2)

### Build model

# Without interactions between Cult and Date
model3 <- aov(id$HeadWt~ id$Date + id$Cult)
summary(model3)
summary(lm(model3)) #p-value: 0.0001847

#  With interactions between Cult and Date
model4<-aov(id$HeadWt~ id$Date * id$Cult)
summary(model4)
summary.lm(model4) #p-value: 4.287e-06

#Post-hoc test
TukeyHSD(model4)
?TukeyHSD
##would be best to use Cult52 and Date 20 as together they show a positive effect on headweight
#relationship between date and headweight depends on the cult


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

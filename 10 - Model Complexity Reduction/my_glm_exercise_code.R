#Loading the data 
titanic_csv <- "D:\\OneDrive - Leuphana Universität\\Semester 1\\2. SfAD - Software for analyzing data\\10 - Model Complexity Reduction\\Tutorial Oliver GLM\\titanic.txt"
ds1 <- read.csv(titanic_csv,header=T)

#data inspection
str(ds1)
summary(ds1)
plot(ds1)
attach(ds1)

names(ds1) # the column names of titanic
head(ds1)

hist(ds1$Fare, nclass = 30) #30 because sqrt(891)
hist(ds1$Age)

#Modeling 
model_lm_1 <- lm(ds1$Survived ~ ds1$Age)
summary(model_lm_1)

model_glm_1 <- glm(Survived ~ Age + Pclass + Sex, family = "binomial")
summary(model_glm_1) # analysing the output: check notes

model_glm_2 <- glm(ds1$Survived ~ ds1$Age * ds1$Pclass * ds1$Sex, family = "binomial")
summary(model_glm_2) # analysing the output: 

#Predicting
newdata <- data.frame ( Pclass = c(1,1), Age = c(40,40), Sex = c("female","male")) #check the survival of 1st class, 40 old, woman
predict(model_glm_1, newdata, type = "response")
#just being female in the 1st class will get you whooping 90% of survival rate

#plotting
#how does changing of the age variable is chanign the probabilty of survival
newdata2 <- data.frame(Pclass =rep (1,100), Sex = rep("female", 100), Age = seq(1,100,1))
probs = predict(model_glm_1, newdata2, type = "response")
plot (probs, ylab = "Probability", xlab = "Age", main = "1st class female passenger") 



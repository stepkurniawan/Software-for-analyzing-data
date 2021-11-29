# exercise 3

meanvalues_uniform <- c()
meanvalues_otherform <- c()

for (i in 1:1000){
  uniform <- runif(100)
  meanvalues_uniform[i] <- mean(uniform)
}
shapiro.test(meanvalues_uniform)
#uniform test works

for (i in 1:1000){
  some_distribution <- rweibull(100)
  meanvalues_otherform[i] <- mean(some_distribution)
}
shapiro.test(meanvalues_otherform)
hist(meanvalues_otherform)



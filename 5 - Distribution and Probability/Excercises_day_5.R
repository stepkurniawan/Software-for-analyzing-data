#### Software for analyzing data - Day 5 ####


#### Sample size and type-I/II error ####
pvalues <- c()

for (i in 1:1000){
  testres <- shapiro.test(rnorm(10,0,1)) 
  print(testres)
  pvalues[i] <- testres$p.value
  print(pvalues[i])
}
length(pvalues[pvalues <= 0.05])/1000

# the Shapiro test is pretty robust, even for relatively small samples
# note that the rate of type-I-errors converges towards 5%, even for very small samples
# (i.e. N = 5, 10)


# now let's put in some random uniform numbers
for (i in 1:1000){
  testres <- shapiro.test(runif(10)) 
  pvalues[i] <- testres$p.value
}
length(pvalues[pvalues <= 0.05])/1000


# random weibull numbers

for (i in 1:1000){
  testres <- shapiro.test(rweibull(10, shape = 2)) 
  pvalues[i] <- testres$p.value
}
length(pvalues[pvalues <= 0.05])/1000

# side note: obviously you will want to write a function in the long run,
# so that you can have e.g. the length of the random samples to be drawn
# as a variable like in the following

errortest_weibull <- function(n,k){
  pvalues <- c()
  for (i in 1:k){
    testres <- shapiro.test(rweibull(n, shape = 2)) 
    pvalues[i] <- testres$p.value
  }
  return(length(pvalues[pvalues <= 0.05])/k)
  
}
# a subsequent step could then be to think about ways to pass
# the distribution to be drawn from as an argument to the function


#### Simple tests for the movies data set ####

#importing the data

url <- "https://raw.githubusercontent.com/nitishghosal/IMDB-Data-Analysis/master/movie_metadata.csv"
movies <- as.data.frame(read.csv(url,stringsAsFactors = FALSE,na="NA"))
names(movies)
str(movies)
?hist

hist(movies$gross, nclass = sqrt(length(movies$gross)))

# Did James Cameron work with a different budget than Gore Verbinski?
jc <- movies[movies$director_name == "James Cameron", ]
jc$movie_title
gv <- movies[movies$director_name == "Gore Verbinski", ]
hist(jc$budget)
hist(gv$budget)
boxplot(jc$budget, gv$budget)
length(gv$budget)
length(jc$budget)
shapiro.test(gv$budget)
shapiro.test(jc$budget)
t.test(gv$budget, jc$budget)

# What about variability in their budgets?
var.test(gv$budget, jc$budget)

# What about the gross of their movies?
boxplot(jc$gross, gv$gross)
shapiro.test(gv$gross)
shapiro.test(jc$gross) # too skewed (Titanic ...)
wilcox.test(gv$gross, jc$gross)
var.test(gv$gross, jc$gross)


# What about Sergio Leone and Stanley Kubrick?
sl <- movies[movies$director_name == "Sergio Leone", ]
sk <- movies[movies$director_name == "Stanley Kubrick", ]

boxplot(sl$budget, sk$budget)
length(sl$budget)
length(sk$budget)
shapiro.test(sl$budget)
shapiro.test(sk$budget)
wilcox.test(sl$budget, sk$budget)
var.test(sl$budget, sk$budget)
# What about the gross of their movies?
length(sl$gross)
length(sk$gross)
sl$gross
sk$gross
boxplot(sl$gross, sk$gross) 
# too little data to run any meaningful test here | boxplot tells it all



#### The limits of the Central Limit Theorem ####
library(actuar)

meanvalues <- c()


for (i in 1:1000){
  a <- rweibull(100, shape = 2)
  meanvalues[i] <- mean(a)
}
shapiro.test(meanvalues)
# CLT works
hist(meanvalues)

for (i in 1:1000){
  a <- runif(100)
  meanvalues[i] <- mean(a)
}
shapiro.test(meanvalues)
hist(meanvalues)
# CLT works

for (i in 1:1000){
  a <- rpareto(100, shape = 2, scale = 1)
  meanvalues[i] <- mean(a)
}
shapiro.test(meanvalues)
hist(meanvalues)

# CLT breaks down for sampling from the Pareto, because the Pareto does 
# not have a well-defined mean value (which CLT requires) !


for (i in 1:1000){
  a <- rcauchy(100, location = 2, scale = 1)
  meanvalues[i] <- mean(a)
}
shapiro.test(meanvalues)
hist(meanvalues)

# CLT also breaks down for sampling from the Cauchy distribution !

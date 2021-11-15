#normal distribution
norm(1000)

# Log-normal distribution
dlnorm(1000)

# Poisson distribution
rpois_var <- rpois(1000,lambda=1)
# lambda: Mean=Standard deviation of the distribution

plot(rpois_var)

# Binomial Distribution
rbinom(1000,size=1, prob=0.5)
# size: How many are drawn?
# prob: Probability to draw a "1".

reg <- read.table("regression.txt", header = T)

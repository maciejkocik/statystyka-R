# binom.test(13, 16, 0.5, alternative = "greater")
# binom.test(35, 50, 0.8)
# binom.test(35, 50, 0.8, alternative = "less")

# mu <- 165
# sigma <- 15
# 
# N <- 30
# x <- rnorm(N, mu, sigma)
# mean(x)
# sd(x)
# hist(x)
# 
# means <- replicate(10000, mean(rnorm(N, mu, sigma)))
# 
# mean(means)
# sd(means)
# 
# sigma_x = sigma / sqrt(N)
# hist(means, freq=F)
# curve(dnorm(x, mu, sigma / sqrt(N)), add = T)

# Uniform distribution

a <- 10
b <- 30
mu <- 20
sigma <- sqrt(((b-a)**2) / 12)

x <- runif(N, a, b)
mean(x)
sd(x)
hist(x)

means <- replicate(10000, mean(runif(N, a, b)))
mean(means)
sd(means)
hist(means, freq = F)
curve(dnorm(x, mu, sigma / sqrt(N)), add = T)

N2 <- 4
means <- replicate(10000, mean(runif(N2, a, b)))
mean(means)
sd(means)
hist(means, freq = F)
curve(dnorm(x, mu, sigma / sqrt(N2)), add = T)


# przykład V z notebooka

n <- 166
mu <- 50 # według H0
sigma <- 10
sr <- 55.71

# N(50, 10 / sqrt(166))

# Jak bardzo nieprawdopodobne było uzyskanie tej sredniej jeśli H0 jest prawdą?

pnorm(55.71, 50, 10 / sqrt(166), lower.tail=F)

# Dla hipotezy dwustronnej alternatywnej
qnorm(c(0.025, 0.975), 50, 10 / sqrt(35))

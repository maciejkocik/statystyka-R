s <- replicate(100000, {
  k <- sample(0:1, 100, replace = T)
  (sum(k) - 50) / sqrt(50)
  })

hist(s, freq = F)
curve(dnorm(x, 0, sqrt(1-0.5)), add = T)
points(density(s), type = "l", col="blue")


k <- sample(0:1, 1000, replace = T)
((sum(k) - 500)**2 / 500) + ((1000 - sum(k) - 500)**2/500)

s <- replicate(10000, {
k <- sample(0:1, 1000, replace = T)
((sum(k) - 500)**2 / 500) + ((1000 - sum(k) - 500)**2/500)
})

hist(s, freq = F)
curve(dchisq(x, 1), add = T)

zaobserwowane <- c(123, 157)
oczekiwane <- c(140, 140)

chi_kwadrat <- sum((zaobserwowane - oczekiwane)**2 / oczekiwane)
qchisq(0.95, 1)
pchisq(chi_kwadrat, 1, lower.tail = F)

zaobserwowane <- c(30, 21, 24)
oczekiwane <- c(25, 25, 25)
chi_kwadrat <- sum((zaobserwowane - oczekiwane)**2 / oczekiwane)
qchisq(0.95,2)

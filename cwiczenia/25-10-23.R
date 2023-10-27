# p.,. (np. pnorm, dystrybuanta)
# q... (np. qnorm, odwrócona dystrybuanta)
# d... (gęstość prawdopodobieństwa)
# r... (np. rnorm, losowanie wartości z danego rozkładu)

sr <- 15
odch <- 5
x <- seq(from = -4, to = 4, length.out = 1000) * odch + sr
y <- dnorm(x, sr, odch)
plot(x, y, type="l")

pnorm(25, sr, odch)
abline(v = 25, lty = 3)
text(x = 22, y = 0.01, labels = "97.7")

q <- qnorm(0.2, sr, odch)
abline(v = q, lty  = 3)
text(x = 8, y = 0.01, labels = "20%")

qnorm(0.1, 150, 30)
pnorm(165, 150, 30, F)

qchisq(1-0.01, 3)

bonan <- rnorm(1, 150, 30)

sum(rnorm(100000, 150, 30) > 165) / 100000

set.seed(1613)

kostka <- 1:6
table(sample(kostka, 6000, replace = T))
jedynki <- replicate(10000, table(sample(kostka, 6000, replace = T))[1])

sum(jedynki > 1030)

# p/q/d/r binom

# Jakie jest prawdopodobieństwo wyrzucenia dokładnie 4 orłów przy 10 rzutach?

dbinom(4, size=10, prob=0.5)

# Jakie jest prawdopodobieństwo wyrzucenia mniej niż 4 orłów przy 10 rzutach?

pbinom(3, size=10, prob=0.5) # dla mniej niż 4 dajemy 3
sum(dbinom(0:3, size=10, prob=0.5))

# Jakie jest prawdopodobieństwo wyrzucenia między 3 a 7 orłów przy 10 rzutach?

sum(dbinom(3:7, size=10, prob=0.5))
pbinom(7, size=10, prob=0.5) - pbinom(2, size=10, prob=0.5)

s <- rbinom(10000, 10, 0.5)
sum (s >= 3 & s <= 7)/10000

# zadanie z losowaniem bez zwracania

org <- c(rep("K", 18), rep("M", 15))
org
sum(sample(org, 7, replace=F) == "M")
s <- replicate(10000, sum(sample(org, 7, replace=F) == "M"))
mean (s > 4)

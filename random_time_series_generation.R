###This is a random number genereation exercise

x <- filter(rpois(1000, lambda = 0.5), filter=rep(1,3), circular=TRUE)
plot(x)
hist(x)

x <- diffinv(rnorm(999))

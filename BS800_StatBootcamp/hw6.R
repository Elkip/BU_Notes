1 - choose(35, 5)*.4^5*.6^30

choose(30, 25) * .6^25 * .4^5
(25-.5-30*.6)/sqrt(20*.6*.4)

(5 - .5- 35 * .4)/sqrt(35 * .4 * (1-.4))
1- pnorm(3.967)

pnorm(-3.277811)


z <- (25 - .5 - 30 * .6)/sqrt(30 * .6 * (1-.6))
1- pnorm(z)

(5 - 35 * .4)/sqrt(35 * .4 * (1-.4))
(324632 * .4^5 * .6^30)
pnorm(-3.105295)

(10 + .5 - 30 * .6)/sqrt(30 * .6 * (1 - .6))
# Binomial
# Toss a fair coin 50 times
# a. Odds of no more than 30 heads
pbinom(30, 50, .5)

1-pbinom(24, 30, .6)
# b. Exactly 30 heads
dbinom(30, 50, .5)

# Poisson
# Average number of patients at ER is 7. Find prob
# Less than 5 in an hour
ppois(4,7)
# More than 10 in an hour
1 - ppois(10,7)

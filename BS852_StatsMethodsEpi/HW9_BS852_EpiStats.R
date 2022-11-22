# 1.
# a. Fit the model
r1_c1 <- cbind(AIDS = rep(1, 60), A = rep(1, 60), B = rep(1, 60))
r2_c1 <- cbind(AIDS = rep(1, 30), A = rep(0, 30), B = rep(1, 30))
r3_c1 <- cbind(AIDS = rep(1, 20), A = rep(1, 20), B = rep(0, 20))
r4_c1 <- cbind(AIDS = rep(1, 10), A = rep(0, 10), B = rep(0, 10))
r1_c2 <- cbind(AIDS = rep(0, 40), A = rep(1, 40), B = rep(1, 40))
r2_c2 <- cbind(AIDS = rep(0, 70), A = rep(0, 70), B = rep(1, 70))
r3_c2 <- cbind(AIDS = rep(0, 80), A = rep(1, 80), B = rep(0, 80))
r4_c2 <- cbind(AIDS = rep(0, 90), A = rep(0, 90), B = rep(0, 90))
aids <- as.data.frame(rbind(r1_c1, r2_c1, r3_c1, r4_c1, r1_c2, r2_c2, r3_c2, r4_c2))
aids

# No interaction
log_aids <- glm(AIDS ~ A + B, data = aids, family = binomial)
summary(log_aids)
# With interaction
log_aids_x <- glm(AIDS ~ A + B + A*B, data = aids, family = binomial)
summary(log_aids_x)

# b. Using the model with no interaction calculate:
# i. The odds of AIDS for subjects exposed to A but not B
exp(-2.3945 + 1.1078*1 + 1.6242*0)

# ii. The odds of AIDS for subjects exposed to A and B
exp(-2.3945 + 1.1078*1 + 1.6242*1)

# iii. The probability of AIDS for subjects exposed to A and B
1.401 / (1.401 + 1)

# iv. OR of AIDS exposed to A in comparison of those not exposed to A
exp(1.1078)

# c. Using the model with interaction calculate:
# i. Calculate the OR of AIDS for those exposed to A 
# to those not exposed to in the two strata of B
# OR_case / OR_control
exp(-2.1972 + .8109*1 + 1.3499*0 + .4418*0)/exp(-2.1972 + .8109*0 + 1.3499*0 + .4418*0)
exp(-2.1972 + .8109*1 + 1.3499*1 + .4418*1)/exp(-2.1972 + .8109*0 + 1.3499*1 + .4418*0)

# iii. RERI
# OR_11 - OR_01 - OR_10 + 1
cntrl_den <- exp(-2.1972 + .8109*0 + 1.3499*0 + .4418*0)
or_ab <- exp(-2.1972 + .8109*1 + 1.3499*1 + .4418*1)/cntrl_den
or_a <- exp(-2.1972 + .8109*1 + 1.3499*0 + .4418*0)/cntrl_den
or_b <- exp(-2.1972 + .8109*0 + 1.3499*1 + .4418*0)/cntrl_den
reri <- or_ab - or_a - or_b + 1

# iv. Interaction on the multiplicative scale?
or_ab / (or_a * or_b)

# 2. Focus on Risk Differences for the following data from fhs
p00 <- 74/536
p01 <- 18/123
p10 <- 86/523
p11 <- 141/416

n00 <- 536
n01 <- 123
n10 <- 523
n11 <- 416

# a. What is the risk difference for developing hd for 
# someone with hypertension vs someone with no exposure?
a = p01 - p00

# b. Someone overweight compared to no exposure?
b = p10 - p00

# c. Sum of two risks
c = a + b

# d. Observed RD of both exposures vs neither
d = p11 - p00

# e. Effect of A&B = effects of each?
d - c

# f. Perform a z-test for additive interaction
# T = p11 – p10 – p01 + p00
# Var(T) = sum(sum([pij(1-pij)]/Nij)
# Z = T / Var(T)
t = p11 - p10 - p01 + p00
var_t = ((p00*(1-p00))/n00) + ((p01*(1-p01))/n01) +
  ((p10*(1-p10))/n10) + ((p11*(1-p11))/n11)
z = t / sqrt(var_t)

pnorm(z, 0, 1, lower.tail = F)

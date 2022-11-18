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
aids <- rbind(r1_c1, r2_c1, r3_c1, r4_c1, r1_c2, r2_c2, r3_c2, r4_c2)

# b. Calculate:
# i. The odds of AIDS for subjects exposed to A but not B


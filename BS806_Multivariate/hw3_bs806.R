# 1. Conduct multiple linear regression
# using savings rate as the outcome variable
load(file="~/Datasets/savings.rda")
savings
pairs(savings)
# Model with only beta_0
sr_lm0 <- lm(sr ~ 1, data=savings)
# Full model
sr_lm1 <- lm(sr ~ ., data=savings)

sr_syy <- sum((savings$sr - mean(savings$sr))^2)
sr_rss <- deviance(sr_lm1)

# F = ((SYY -RSS)/((n-1) - (n-2))) / (RSS / (n - 1))
sr_num <- (sr_syy - sr_rss)/(df.residual(sr_lm0) - df.residual(sr_lm1))
sr_den <- sr_rss / df.residual(sr_lm1)
sr_f <- sr_num / sr_den
# dfΩ = n - p, and df𝜔 = n - q
pf(sr_f, df.residual(sr_lm0) - df.residual(sr_lm1), df.residual(sr_lm1), lower.tail = F)

# 2. Using the above, test whether savings rate is linearly associated 
# with % of pop under 15 while adjusting for pop75, dpi and ddpi
sr_lm2 <- lm(sr ~ pop75 + dpi + ddpi, data = savings)
sr_rss2 <- deviance(sr_lm2)
sr_num2 <- (sr_rss2 - sr_rss)/(df.residual(sr_lm2) - df.residual(sr_lm1))
sr_f2 <- sr_num2 / sr_den
pf(sr_f2, df.residual(sr_lm2) - df.residual(sr_lm1), df.residual(sr_lm1), lower.tail = F)


# 3. Test whether Pop15 and Pop75 have the same effect on savings rate
sr_lm3 <- lm(sr ~ I(pop15 + pop75) + dpi + ddpi, data = savings)
sr_rss3 <- deviance(sr_lm3)
sr_num3 <- (sr_rss3 - sr_rss)/(df.residual(sr_lm3) - df.residual(sr_lm1))
sr_f3 <- sr_num3 / sr_den
pf(sr_f3, df.residual(sr_lm3) - df.residual(sr_lm1), df.residual(sr_lm1), lower.tail = F)


# 4. Conduct a hypothesis test to examine whether both variables ddpi and pop75
# may be excluded from the model
sr_lm4 <- lm(sr ~ pop15 + dpi, data = savings)
sr_rss4 <- deviance(sr_lm4)
sr_num4 <- (sr_rss4 - sr_rss)/(df.residual(sr_lm4) - df.residual(sr_lm1))
sr_f4 <- sr_num4 / sr_den
pf(sr_f4, df.residual(sr_lm4) - df.residual(sr_lm1), df.residual(sr_lm1), lower.tail = F)

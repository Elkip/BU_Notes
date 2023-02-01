library(rjags)

# Read in the data
X.E_nosmoking <- c(rep(1, 30), rep(0,207), rep(1, 33), rep(0, 327))
X.E_smoking <- c(rep(1, 117), rep(0,216), rep(1, 25), rep(0, 114))
X.E_total <- c(rep(1, 147), rep(0,423), rep(1, 58), rep(0, 441))
Y.M_nosmoking <- c(rep(1, 237), rep(0, 360))
Y.M_smoking <- c(rep(1, 333), rep(0, 139))
Y.M_total <- c(rep(1,570), rep(0, 499))

# 1.1 Model the crude association between MI and coffee drinking

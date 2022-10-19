pulse <- read.table("/home/elkip/Datasets/pulsedata.csv", header=T, sep = ",")
attach(pulse)
# 1
reg <- lm(Pulse1 ~ Height + Weight + Age + Sex + Smokes + Alcohol + as.factor(Exercise), data = pulse, na.action = na.omit)
summary(reg)

# 2 
par(mfrow=c(2,2))
plot(fitted(reg), residuals(reg), xlab="fitted", ylab="Residuals")

qqnorm(residuals(reg), ylab="Residuals")
qqline(residuals(reg))
hist(reg$residuals)

shapiro.test(reg$residuals)

# 3
n <- nrow(pulse) - sum(is.na(pulse[,10]))
pprime <- 8
stud <- rstudent(reg)
lim = abs(qt(.05/(n*2), df = n - pprime - 1, lower.tail = T))
stud[which(abs(stud) > lim)]

# 4
cook <- cooks.distance(reg)
cook[cook > 4/n]
cook[cook>.5]
cook[(pf(cook, pprime, n-pprime) > .5)]

# 5

library(alr4)
influenceIndexPlot(reg)

hat <- hatvalues(reg)
lev <- 2 * pprime / n
hat[hat > lev]


pulse2 <- pulse[-76,]
reg2 <-  lm(Pulse1 ~ Height + Weight + Age + Sex + Smokes + Alcohol + Exercise
            , data = pulse2)
summary(reg2)

summary(reg)$coefficients
summary(reg2)$coefficients

pulse3 <- pulse[-80,]
reg3 <-  lm(Pulse1 ~ Height + Weight + Age + Sex + Smokes + Alcohol + Exercise
            , data = pulse3)
summary(reg3)

summary(reg)$coefficients
summary(reg3)$coefficients

pulse4 <- pulse[-47,]
reg4 <-  lm(Pulse1 ~ Height + Weight + Age + Sex + Smokes + Alcohol + Exercise
            , data = pulse4)
summary(reg4)

summary(reg)$coefficients
summary(reg4)$coefficients

pulse5 <- pulse[-62,]
reg5 <-  lm(Pulse1 ~ Height + Weight + Age + Sex + Smokes + Alcohol + Exercise
            , data = pulse5)
summary(reg5)

summary(reg)$coefficients
summary(reg5)$coefficients

pulse6 <- pulse[-56,]
reg6 <-  lm(Pulse1 ~ Height + Weight + Age + Sex + Smokes + Alcohol + Exercise
            , data = pulse6)
summary(reg6)

summary(reg)$coefficients
summary(reg6)$coefficients

pulse7 <- pulse[-17,]
reg7 <-  lm(Pulse1 ~ Height + Weight + Age + Sex + Smokes + Alcohol + Exercise
            , data = pulse7)
summary(reg7)

summary(reg)$coefficients
summary(reg7)$coefficients

detach(pulse)

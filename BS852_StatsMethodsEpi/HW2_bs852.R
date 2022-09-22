library(epiR)

# 1
smkrs <- c(41, 39, 192, 199, 145, 36, 205, 102)
dim(smkrs) <- c(2,2,2)

dimnames(smkrs)[[1]] <- c("MI", "No MI")
dimnames(smkrs)[[2]] <- c("5+ Coffee", "0-4 Coffees")
dimnames(smkrs)[[3]] <- c("Never/Former Smoker", "Current Smoker")
print(smkrs)
mi.BD <- epi.2by2(dat = smkrs, method = "case.control", conf.level = .95)

summary(mi.BD)$massoc.detail$OR.homog.woolf
summary(mi.BD)$massoc.detail$OR.homog.brday

mantelhaen.test(smkrs)
mantelhaen.test(smkrs)$estimate

# 2
heart <- read.csv("/home/elkip/Datasets/framdat2.txt", sep = "", na.strings = ".")
heart2 <- heart[ which(heart$SEX == 2 & (heart$CHD == 0 | heart$CHD > 4)),]
attach(heart2)
age <- AGE
age[ which( AGE >= 35 & AGE < 45)] <- 1
age[ which( AGE >= 45 & AGE < 55)] <- 2
age[ which( AGE >= 55 & AGE < 65)] <- 3
age[ which( AGE >= 65 & AGE < 75)] <- 4

chd_sw = CHD < 4
glucose = GLI == 0
fram_data <- table(glucose, chd_sw, age)
res <- epi.2by2(dat=fram_data, method = "cohort.count", conf.level = .95)

names(summary(res)$massoc.detail)
summary(res)$massoc.detail$OR.crude.wal$est
summary(res)$massoc.detail$OR.strata.wald$est
summary(res)$massoc.detail$OR.mh.wald$est
summary(res)$massoc.detail$chi2.mh

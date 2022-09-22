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

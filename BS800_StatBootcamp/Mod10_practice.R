# Module 10 Practice
Q24 <- read.csv("/home/elkip/Documents/BU/BS800/Data/Module10_Q24.csv")

head(Q24)
Q24table <- table(Q24$Status,Q24$Estrogen,Q24$Estrogen)
Q24table
mantelhaen.test(Q24table)

kawa <- read.csv("/home/elkip/Documents/BU/BS800/Data/kawa.csv", header=TRUE)
table(kawa$arm, kawa$anyv34)

# HW 10
# Problem 2
library("xlsx")
preg <- xlsx::read.xlsx("/home/elkip/Documents/BU/BS800/Data/Pregnancy_Trial.xlsx",1)
total <- sum(preg$age != 0)
total_m
a = sum(preg$intervention == 1 & preg$meconium == 1)
b = sum(preg$intervention == 0 & preg$meconium == 1)
c = sum(preg$intervention == 1 & preg$meconium == 0)
d = sum(preg$intervention == 0 & preg$meconium == 0)
dat <- matrix(c(a,b,c,d), ncol=2)
fisher.test(dat)

# Problem 3
abv <- xlsx::read.xlsx("/home/elkip/Documents/BU/BS800/Data/Alcohol_Study.xlsx",1)
abv$agegrp <- factor(abv$agegrp, levels = c(1,2,3), labels = c("Young", "Middle Aged", "Old Fart"))
abv$cancer <- factor(abv$cancer, levels = c(1,0), labels =  c("Cancer", "Control"))
abv$alcgrp <- factor(abv$alcgrp, levels = c(1,0), labels = c("Heavy Drinker", "Light Drinker"))

ftable(table(abv$alcgrp, abv$cancer, abv$agegrp))
mantelhaen.test(abv$alcgrp, abv$cancer, abv$agegrp, correct = FALSE)

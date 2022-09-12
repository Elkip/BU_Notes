### Practice Session 11

n = 6681
mean = 2.95
sd = 1.1
se = sd / sqrt(n)
alpha=.01
t_obs = (mean - 3)/se
t = qt(alpha/2, df=n-1, lower.tail = F)

# 7.20
# h0: Means are equal
# ha: At least one of the means is not equal
t_df = 3 - 1 # Number of groups - 1
total_n = 44 + 44 + 41
error_df = total_n - 3
x_bar = (44 * .25 + 44 * 1.57 + 41 * .63) / total_n # Wieghted avg
SSB = 44*(.25 - x_bar)^2 + 44 * (.157 - x_bar)^2 + 41 * (.63 -x_bar)^2
MSB = SSB / t_df

SoE = 43 * 2.28^2 + 43*2.54^2 + 40*2.38^2
MSE = SoE/(error_df)

F_between = MSB / MSE

qf(.95, t_df, error_df)

# Practice Final Q
t_df = 4
error_df = 25
total_df = 29

SST = 500
SSB = 400
SSE = 100

MSE = SSE / error_df
MSB = SSB / t_df

F = MSB / MSE
qf(.90, t_df, error_df)

### HW 11
cancer <- read.csv("/home/elkip/Documents/BU/BS800/Data/prostate.csv", header=TRUE)

boxplot(cancer$Age ~ cancer$Nodes)
var.test(cancer$Age ~ cancer$Nodes, alternative = "two.sided")
t.test(cancer$Age ~ cancer$Nodes, var.equal=TRUE)

boxplot(cancer$Acid ~ cancer$Nodes)
var.test(cancer$Acid ~ cancer$Nodes, alternative = "two.sided")
t.test(cancer$Acid ~ cancer$Nodes, var.equal=F)

if(!require(lattice)) install.packages("lattice",repos = "http://cran.us.r-project.org")
suppressPackageStartupMessages(library(lattice))

histogram(~cancer$Age | as.factor(cancer$Nodes), data=cancer)
wilcox.test(cancer$Age ~ cancer$Nodes)

histogram(~cancer$Acid | as.factor(cancer$Nodes), data=cancer)
wilcox.test(cancer$Acid ~ cancer$Nodes)


mutans <- read.csv("/home/elkip/Documents/BU/BS800/Data/mutans.streptococci.csv", header=TRUE)
mutans_three <- subset(mutans, mutans$weeks == 3)
mutans_zero <- subset(mutans, mutans$weeks == 0)
mutans_ratio <- na.omit(data.frame(mutans_zero$cfu / mutans_three$cfu, mutans_zero$trt))
colnames(mutans_ratio) <- c("cfu", "trt")

mutans_nodrug <- subset(mutans_ratio, mutans_ratio$trt == "NoDrug")
mutans_drug <- subset(mutans_ratio, mutans_ratio$trt == "Drug")
mutans_plac <- subset(mutans_ratio, mutans_ratio$trt == "Placebo")

histogram(~mutans_ratio$cfu | as.factor(mutans_ratio$trt), data=mutans_ratio)

mean(mutans_plac$cfu)
sd(mutans_nodrug$cfu)
summarar

res <- aov(mutans_ratio$cfu ~ mutans_ratio$trt, data=mutans_ratio)
summary(res)
TukeyHSD(res)

age 
p = .7449
t= 327



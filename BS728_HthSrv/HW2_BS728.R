library(dplyr)
dhs <- read.csv("/home/elkip/Datasets/HW1data_DHS_small.csv")
dim(dhs)
head(dhs)
n <- nrow(dhs)

# 3
# a. Overall estimate of TB prevalence
sum(dhs$TB)/n # 0.02

# b. Analysis on w and w/o TB
no_tb = dhs %>% filter(TB == 0)
tb = dhs %>% filter(TB == 1)

# Aged Based
dhs %>% mutate(agegroup = case_when(age >= 0  & age <= 4 ~ '1',
                                             age >= 5  & age <= 14 ~ '2',
                                             age >= 15  & age <= 39 ~ '4')) %>% 
  group_by(agegroup) %>%
  count(agegroup) %>%
  summarise(agegroup = n, p = n / 50000)

no_tb %>% mutate(agegroup = case_when(age >= 0  & age <= 4 ~ '1',
                                    age >= 5  & age <= 14 ~ '2',
                                    age >= 15  & age <= 39 ~ '4')) %>% 
  group_by(agegroup) %>%
  count(agegroup) %>%
  summarise(agegroup = n, p = n / 50000)

# BMI Based
mean <- mean(dhs$bmi, na.rm = TRUE)
sd <- sd(dhs$bmi, na.rm = TRUE)
t = qt(.975, n - 1, lower.tail = F)
ci_low = mean - t*sd/sqrt(n)
ci_high = mean + t*sd/sqrt(n)
mean
print(paste(ci_low, ci_high, sep = ", "))

mean <- mean(tb$bmi, na.rm = TRUE)
sd <- sd(tb$bmi, na.rm = TRUE)
ci_low = mean - t*sd/sqrt(nrow(tb))
ci_high = mean + t*sd/sqrt(nrow(tb))
mean
print(paste(ci_low, ci_high, sep = ", "))

mean <- mean(no_tb$bmi, na.rm = TRUE)
sd <- sd(no_tb$bmi, na.rm = TRUE)
ci_low = mean - t*sd/sqrt(nrow(no_tb))
ci_high = mean + t*sd/sqrt(nrow(no_tb))
mean
print(paste(ci_low, ci_high, sep = ", "))

# Windows
sum(dhs$windows, na.rm = TRUE)

# Wood fuel
sum(tb$wood_fuel)
sum(no_tb$wood_fuel)

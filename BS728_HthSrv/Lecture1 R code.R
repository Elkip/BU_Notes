x <- 1.5
x

# Read in the dataset and call it hers
hers <- read.csv("/Users/lfwhite/Dropbox/BS 728/Lecture 1 Intro/HERS.csv")

# get the dimension of the data
dim(hers)

# get the variable names of the dataset
names(hers)

# summarize the data
summary(hers)

# look at the first row
hers[1,]

# look at the second row and third element
hers[2,3]

# summarize the 2nd and 35th variables (age and SBP)
summary(hers[,c(2,35)])

summary(hers[,c(2:4,5:7)])

# tabulate the number of smokers and nonsmokers
table(hers[,5])
table(hers$smoking)

# or
table (hers[,5])

# convert the weight in kg to pounds (1 lb = 0.45 kg)
weight.lb <- hers$weight/0.45

# get the height (height, in meters = square root of weight/bmi)
height <- sqrt(hers$weight/hers$BMI)

# summarize the new variables
summary(cbind(weight.lb,height)) # cbind() combines the variables side by side

# now get the height in feet (1 meter=3.28 feet)
height.ft <- height*3.28

# create a scatterplot
plot(hers$BMI,hers$SBP,xlab="BMI",ylab="SBP",main="Plot of BMI and SBP")

plot(hers$BMI,hers$SBP,xlab="BMI",ylab="SBP",main="Plot of BMI and SBP",pch="*")

# add lines at normal (18.5-24.9) and overweight (25-29.9) BMIs
abline(v=18.5,col="blue")
abline(v=24.9,col="purple")
abline(v=29.9,col="red")

# perform a t test comparing SBP for those with and without Hormone replacement therapy
ttest.sbp <- t.test(SBP~HT,data=hers)
ttest.sbp

# regression model to consider linear relationship 
#   between SBP and BMI
fit <- lm(SBP~BMI,data=hers)
summary(fit)



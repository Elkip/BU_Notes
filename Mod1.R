# Introduction to R
# Scalars
a <- 3
# Vectors
b <- c(1, 2, 3, 4)
# Index starts at 1 in R, trying to access
# element 0 results in nothing
b[0]
# Matrices
mat <- matrix(b, 2, 2)
mat[1,] # All values in row 1
mat[,1] # All values in column 1
# Lists
mylist <- list("age" = c(47, 29), "weight" = c(155,203), "dosage"=c("low","med","high"))
mylist
mylist$age
mylist$dosage
# Data frames
df <- data.frame("age" = c(47, 29, 10), "weight" = c(155,203,300), "dosage"=c("low","med","high"))
df
# Functions
# Most functions in R are vectorized, meaning
# that the function will apply to each element
# of a vector without needing a loop
myprintfun <- function(vec) {
  for (i in 1:length(vec)) {
    print(vec[i]*5)
  }
}
mymeanfun(b)
print(b*5)

# Apply
# The apply function extends loops to matrices,
# data frames, and lists
apply(mat, 2 , mean) # 1 indicates rows, 2 indicates columns

# Tidyverse is a library for exploratoru data analysis
# It was created to transform, visualize and model data
# R packages in the tidyverse:
#   - dplyr - grammer of data manipulation
#   - tidyr - long/wide data formatting
#   - stringr - string manipulation
#   - forcats - factor manipulation
#   - tibble - modified version of dataframe with
# fewer automatic conversions
#   - readr - fast import of rectangular data
#   - purr - extensions of apply functions
#   - ggplot2 - ggrammer of graphics
library(tidyverse)

# Piping
# Use the pipe operator %>% to indicate
# how data output is flowing through your analysis
airquality[which(airquality$Temp > 90 & airquality$Wind <=5),]
summary(airquality$Ozone)

airquality %>% 
  filter(Temp > 90 & Wind <=5) %>%
  summary(Ozone)

# Practice 1
students_og <- read.csv("/home/elkip/Documents/BU/BS803/Data/students_raw.csv")
students <- separate(students_og, grade.and.age, sep = "_", into = c("grade", "age"))
students %>% mutate(term.1*100)

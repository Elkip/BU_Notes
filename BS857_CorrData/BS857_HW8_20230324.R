library(foreign)

t <- read.table("/home/elkip/Datasets/JSP.txt", header = FALSE, 
                   colClasses = "character", sep="\n", strip.white = FALSE)
test <- data.frame(school = substr(t$V1, 1, 2), class = substr(t$V1, 3, 3), 
                   gender = substr(t$V1, 4, 4), social = substr(t$V1, 5, 5), 
                   year1 = as.numeric(substr(t$V1, 6, 7)), id = substr(t$V1, 8, 11), 
                   eng = as.numeric(substr(t$V1, 12, 13)), math = as.numeric(substr(t$V1, 14, 15)),
                   year = substr(t$V1, 16, 16))

write.foreign(test, "/home/elkip/Datasets/new_JSP.txt", "/home/elkip/Datasets/JSP.sas", package = "SAS")

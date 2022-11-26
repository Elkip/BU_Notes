wine <- read.csv("/home/elkip/Datasets/Wine.csv")

wine.reg <- lm(Alcohol ~ ., data = wine)
summary(wine.reg)

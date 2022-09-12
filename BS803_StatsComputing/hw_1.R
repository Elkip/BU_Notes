# BS803 HW 1
# 09/14/2022
# Mitchell Henschel

library(tidyverse)
library(ggplot2)

# 1a Using the gapmidner dataset find the 
# average and standard deviation 
# in a single object
gapminder <- readr::read_csv("https://raw.githubusercontent.com/swcarpentry/r-novice-gapminder/gh-pages/_episodes_rmd/data/gapminder-FiveYearData.csv")
gap_df <- data.frame(contient = gapminder$continent, 
                     lifeExp = gapminder$lifeExp)
gap_stats <- gap_df %>% 
    group_by(contient) %>%
      summarise(mean = mean(lifeExp),
                sd = sd(lifeExp))
head(gap_stats)

# 1b Same as above but only for the year 1997
gapminder_1997 <- gapminder %>% 
  filter(year == 1997)
gap1997_df <- data.frame(contient = gapminder_1997$continent, 
                         lifeExp = gapminder_1997$lifeExp)
gap1997_stats <- gap1997_df %>% 
  group_by(gap1997_df$contient) %>%
  summarise(mean = mean(lifeExp),
            sd = sd(lifeExp))
head(gap1997_stats)

# 2a Using the mpg datset from ggplot2 package
# create a plot of Engine Displacement vs Highway mpg
# Color the points by drive type (drv) and facet your 
# plot by the type of car('class'). Customize 
# your figure's themes, titles/labels and color
ggplt <- ggplot(data=mpg, aes(x=displ, y=hwy, color=drv)) 
ggplt <- ggplt + geom_point() 
ggplt <- ggplt + facet_wrap(.~class)
ggplt <- ggplt + labs(x="Engine Displacement", y = "Highway MPG", 
                      title = "Highway MPG vs Engine Displacement by Car Class") 
ggplt <- ggplt + theme_dark()
ggplt

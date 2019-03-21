library(tidyverse)
head(mpg)
#1
ggplot(mpg, aes(x = hwy, y = cyl)) + geom_point()
#2
ggplot(mpg, aes(x = class, y = drv)) + geom_point()
#3
ggplot(mpg) + geom_point(aes(x=displ,y=hwy,color='blue'))
ggplot(mpg) + geom_point(aes(x=displ,y=hwy), color = 'blue')
#4
ggplot(mpg) + geom_point(aes(x = displ, y= hwy)) +
	facet_grid(drv ~ .)
ggplot(mpg) + geom_point(aes(x = displ, y= hwy)) +
	facet_grid(. ~ cyl)
#5
ggplot(mpg, aes(x=displ, y = hwy)) + geom_point() + geom_smooth()
ggplot() + geom_point(data = mpg, mapping = aes(x = displ, y = hwy)) + geom_smooth(data = mpg, mapping = aes(x = displ, y = hwy))
#6
ggplot(mpg,aes(x= displ, y= hwy)) + geom_point() + geom_smooth(aes(class = drv), se	= F)
ggplot(mpg,aes(x= displ, y= hwy)) + geom_point(aes(fill = drv),  shape = 21, color = 'white', stroke = 1.5)
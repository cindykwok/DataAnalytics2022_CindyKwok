library(ggplot2)
diamonds
ggplot(data=diamonds)
attach(diamonds)

str(diamonds)
View(diamonds)
head(diamonds)
tail(diamonds)
hist(x)
hist(x, seq(30,100,1), prob=TRUE)
hist(y)
hist(z)
hist(price)
tf <- is.na(price)
P <- price[!tf]
hist(P)
hist(price)
ggplot(data = diamonds, mapping = aes(x = price)) + geom_histogram(binwidth = 10)
ggplot(data = diamonds, mapping = aes(x = price)) + geom_histogram(binwidth = 1)
ggplot(data = diamonds, mapping = aes(x = price)) + geom_histogram(binwidth = 1000)
ggplot(data = diamonds, mapping = aes(x = x, y = y)) + geom_point(na.rm = TRUE)
ggplot(data = diamonds) + geom_histogram(mapping = aes (x = y),binwidth = 0.5) + coord_cartesian(ylim = c(0,50))



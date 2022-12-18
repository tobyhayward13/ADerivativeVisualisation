#Create a script which takes a function f and a distance/change in x (dx) and computes the derivative function and plots both.
library(tidyverse)


cube <- function(x) {
  return(x^3)
}
dcube <- function(x) {
  return(3 * x^2)
}

dx = 0.1 #length of interval between x1 and x2

n = 10 #The length of the x axis, or amount of points to compute. \\\

x = seq(0, n, dx)

y = cube(x)

dy = vector(mode = 'double', length(x)) #Initialising a vector to insert values of x into.

dy[1] <- y[1] #Initializing first term.

for (i in 2:length(x)) {
  dy[i] <- y[i] - y[i-1]
}

plot(1,type='n', xlim = c(0, n), ylim = c(0, y[length(x)]),
     xlab = 'x', ylab = 'y')

lines(x, y, col = 'red')



lines(x, dy/dx)

lines(x, dcube(x), col = 'green')
data =data.frame(x, y)
p1 <- ggplot(data = data, aes(x = x, y = y)) +
  geom_line()
p1
ggsave('test.jpg', p1)

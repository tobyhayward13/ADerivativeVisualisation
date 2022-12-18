library(tidyverse)

derivative_approx <- function(a, b, dx, fun, dfun) {
  # [a, b] = domain of the function
  # dx = distance between the points x1, x2 to estimate dy.
  # fun, dfun = the functions f(x) and f'(x) respectively. 
  
  x = seq(a, b, dx)
  y = fun(x)
  
  dy = vector(mode = 'double', length(x)) #Initialising a vector to insert values of x into.
  dy[1] = dfun(a)
  
  
  for (i in 2:((b-a)/dx + 1)) {
    dy[i] = y[i] - y[i-1]
  }
  
  ans = dy/dx
  ans[1] <- dy[1]
  
  return(ans)
  #ans = sequence of estimated slopes for a given x. 
}
make_function_vis <- function(a, b, fun, ylab) {
  plot <- tibble(x = seq(a, b, (b-a)/100),
                 y = fun(x)) %>% 
    ggplot(aes(x = x, y = y)) +
    geom_line() + 
    labs(title = sprintf('Plot of f(x) against x where f(x) = %s', ylab),
         y = ylab) +
    theme_minimal() +
    theme(axis.title.y = element_text(margin = margin(r = 20)),
          axis.title.x = element_text(margin = margin(t = 20))) +
    geom_hline(yintercept = 0, col = 'darkgray') +
    geom_vline(xintercept = 0, col = 'darkgray')
  
  return(plot)
}
make_derivative_vis <- function(a, b, fun, dfun, ylab) {
  base_plot <- tibble(x = seq(a, b, (b-a)/100),
                      actualdy = dfun(x)) %>% 
    ggplot(aes(x = x, y = actualdy)) +
    geom_line(col = 'red') +
    theme_minimal() +
    theme(axis.title.y = element_text(margin = margin(r = 20)),
          axis.title.x = element_text(margin = margin(t = 20))) +
    labs(title = sprintf("f'(x) and approximates against x where f(x) = %s", ylab),
         y = "f'(x)") +
    geom_hline(yintercept = 0, col = 'darkgray') +
    geom_vline(xintercept = 0, col = 'darkgray')
  
  for (dx in seq(0.1, 1, 0.1)) {
    temp_tib <- tibble(x = seq(a, b, dx),
                       y = derivative_approx(a, b, dx, fun, dfun))
    base_plot <- base_plot +
      geom_line(data = temp_tib, aes(x = x, y = y), alpha = 1 - dx)
  }
  
  return(base_plot)
}

f_name = 'f(x)'

f <- function(x) {
  return(-2.4 * x^5 + 3.7 * x^4 + 0.5 * x^3 + -1.9 * x^2 + 0.7 * x + 1 + 0.39 * x^6)
}
df <- function(x) {
  return(-2.4 * 5 * x^4 + 3.7 * 4 * x^3 + 0.5 * 3 * x^2 + -1.9 * 2 * x + 0.7 + 0.39 * 6 * x^5)
}

a = -1
b = 3

make_function_vis(a, b, f, f_name)
make_derivative_vis(a, b, f, df, f_name)
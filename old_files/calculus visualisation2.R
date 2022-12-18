#Create a script which takes a function f and a distance/change in x (dx) and computes the derivative function and plots both.

derivative_approx <- function(a, b, dx, fun, dfun) {
  x = seq(a, b, dx)
  y = fun(x)
  
  dy = vector(mode = 'double', length(x)) #Initialising a vector to insert values of x into.
  dy[1] = dfun(a)
  
  
  for (i in 2:(abs(b-a)/dx + 1)) {
    dy[i] = y[i] - y[i-1]
  }
  
  ans = dy/dx
  ans[1] <- dy[1]
  
  return(ans)
}




#define the function

f <- function(x) {
  return(x^5+2*x^4)
}

#define the ACTUAL derivative

df <- function(x) {
  return(5 * x^4 + 8 * x^3)
}




a = -2
b = 1

# dx = 0.1
# 


#Calculate the predicted line:

# df_predict <- derivative_approx(a, b, dx, f, df)
# 




plot(1,type='n', xlim = c(a, b), ylim = c(min(df(seq(a,b,0.01))), max(df(seq(a,b,0.01)))),
     xlab = 'x', ylab = 'y')


# Actual derivative

lines(seq(a,b,(b-a)/100), df(seq(a,b,(b-a)/100)), col = 'red')




# Predicted derivative

# lines(seq(a,b,dx), df_predict)




#Increasing dx from 1 to 0.1

for (i in seq(0.1, 1, 0.1)) {
  df_predict <- derivative_approx(a, b, i, f, df)
  lines(seq(a,b,i), df_predict, col = scales::alpha('black', 1-i))
}








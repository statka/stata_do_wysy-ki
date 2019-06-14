#lab_4
#zad_1
vec <- seq(4, 40, 2)
vec
i <- length(vec)
while(i > 0)
{
  cat(paste(vec[i], ' '))
  i <- i - 1
}
rm(vec, i)
##################################################

#zad_2
x <- c()
tab <- cbind(0,0,0)
for(i in 1:10)
{
  x[i] <- i
}
tab <- cbind(x, x^2, x^3)
print(tab)
rm(x, i, tab)
##################################################
#zad_3
factorial <- function(fact)
{
  if(fact > 1)
  {
    return(fact * factorial(fact - 1))
  }
  else
  {
    return(1)
  }
}
vec <- seq(3,13,2)
vec2 <- c()
n <- length(vec)
for(i in 1:n)
{
  vec2[i] <- factorial(vec[i])
}
vec
vec2
myFrame <- data.frame(odd <- vec, factorial <- vec2)
print(myFrame)
rm(vec, vec2, i, n, factorial, myFrame, odd)
##################################################
#zad_4
myFrame <- data.frame(var1 <- c(1:10),
                      var2 <- var1*2,
                      var3 <- var1*3,
                      var4 <- var1*4,
                      var5 <- var1*5,
                      var6 <- var1*6,
                      var7 <- var1*7,
                      var9 <- var1*8,
                      var10 <- var1*10)
colnames(myFrame) <- c(1:9)
print(myFrame)
rm(myFrame, var1, var2, var3, var4, var5, var6, var7, var8, var9, var10)
var <- c(1:10)
var2 <- factor(var)
print(var2)
rm(var, var2)

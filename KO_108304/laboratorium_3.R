height <- sample(140:207, 30)
gender <- c()
for(i in height)
{
  if(i < 170)
  {
    gender = c(gender, "f")
  }
  else
  {
    gender = c(gender, "m")
  }
}
myFrame <- data.frame(height <- height, gender <- gender)
t.test(myFrame$height, mu=180)
var_f <- with(myFrame, height[gender == "f"])
var_m <- with(myFrame, height[gender == "m"])
t.test(var_f, var_m)
barplot(var_f, var_m, beside = TRUE, col = c("red", "blue"))
hist(myFrame$height, breaks = 15)
plot(density(myFrame$height), main = "Height")
install.packages("ggplot2")
library(ggplot2)
ggplot(myFrame, aes(x = height, fill = gender)) + geom_density()

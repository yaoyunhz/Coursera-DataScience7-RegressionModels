
# Coursera - Data Science - Course7: Regression

# Week 3
n = 100; x = rnorm(n); x2 = rnorm(n); x3 = rnorm(n)
y = 1 + x + x2 + x3 + rnorm(n, sd = 0.1)
ey = resid(lm(y ~ x2 + x3))
ex = resid(lm(x ~ x2 + x3))
sum(ey * ex) / sum(ex ^ 2)
coef(lm(ey ~ ex - 1))
coef(lm(y ~ x + x2 + x3))

require(datasets)
data(swiss)
?swiss
require(GGally)
require(ggplot2)
g = ggpairs(swiss, lower = list(continuous = "smooth"), params = c(method = "loess"))
g

# Simpson's perceived paradox
summary(lm(Fertility ~., data = swiss))$coefficients # negative effect
summary(lm(Fertility ~ Agriculture, data = swiss))$coefficients # positive effect

n <- 100
x2 <- 1:n
x1 <- 0.01 * x2 + runif(n, -0.1, 0.1)
plot(x1)
y = -x1 + x2 + rnorm(n, sd = 0.01)
summary(lm(y ~ x1))$coef
summary(lm(y ~ x1 + x2))$coef

dat = data.frame(y = y, x1 = x1, x2 = x2, 
                 ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
g = ggplot(dat, aes(y = y, x = x1, colour = x2))
g = g + geom_point(color = "grey50", size = 5) + 
    geom_smooth(method = lm, se = FALSE, color = "black") + geom_point(size = 2)
g

dat = data.frame(y = y, x1 = x1, x2 = x2, 
                 ey = resid(lm(y ~ x2)), ex1 = resid(lm(x1 ~ x2)))
g2 = ggplot(dat, aes(y = ey, x = ex1, color = x2))
g2 = g2 + geom_point(color = "grey50", size = 5) + 
    geom_smooth(method = lm, se = FALSE, color = "black") + geom_point(size = 4)
g2

z <- swiss$Agriculture + swiss$Education
lm(Fertility ~. + z, data = swiss)

data("InsectSprays")
require(stats)
g = ggplot(data = InsectSprays, aes(y = count, x = spray, fill = spray))
g = g + geom_violin(color = "black", size = 2)
g = g + xlab("Type of spray") + ylab("Insect count")
g
head(InsectSprays)
summary(lm(count ~ spray, data = InsectSprays))$coef # in comparison with A

# hard coding with dummy variables
summary(lm(count ~ 
               I(1 * (spray == 'B')) + 
               I(1 * (spray == 'C')) + 
               I(1 * (spray == 'D')) + 
               I(1 * (spray == 'E')) + 
               I(1 * (spray == 'F')), data = InsectSprays))$coef
# I - perform a function inside the regression model
# 1 - change it from boolean to numeric

# include A, NA value because it's redundant
summary(lm(count ~ 
               I(1 * (spray == 'B')) + 
               I(1 * (spray == 'C')) + 
               I(1 * (spray == 'D')) + 
               I(1 * (spray == 'E')) + 
               I(1 * (spray == 'F')) + 
               I(1 * (spray == 'A')), data = InsectSprays))

summary(lm(count ~ spray - 1, data = InsectSprays))$coef
# six parameters, but the intercept has to be dropped
# mean for each spray
# tests whether the mean is different from zero

# ususally you want one to be the reference level
# tests whether spray B is different from spray A

# re-level
# spray C is the reference level
spray2 <- relevel(InsectSprays$spray, 'C')
summary(lm(count ~ spray2, data = InsectSprays))$coef
# the intercept is the mean of the reference level
# the test for the reference level is whether it's different from zero
# the other tests are whether the mean is different from the reference level


# create a binary variable
hist(swiss$Catholic) # bimodal
library(dplyr)
swiss = mutate(swiss, CatholicBin = 1 * (Catholic > 50))

# plot the data
g = ggplot(swiss, aes(x = Agriculture, y = Fertility, colour = factor(CatholicBin)))
g = g + geom_point(size = 6, color = "black") + geom_point(size = 4)
g = g + xlab("% in Agriculture") + ylab("Fertility")
g

# model without religion
fit = lm(Fertility ~ Agriculture, data = swiss)
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1
summary(fit)$coef

# model with two parallel lines
fit = lm(Fertility ~ Agriculture + factor(CatholicBin), data = swiss)
summary(fit)$coef
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], slope = coef(fit)[2], size = 2)
g1

# interaction
fit = lm(Fertility ~ Agriculture * factor(CatholicBin), data = swiss)
summary(fit)$coef
g1 = g
g1 = g1 + geom_abline(intercept = coef(fit)[1], slope = coef(fit)[2], size = 2)
g1 = g1 + geom_abline(intercept = coef(fit)[1] + coef(fit)[3], 
                      slope = coef(fit)[2] + coef(fit)[4], size = 2)
g1

# multivariable regression
n <- 100; t <- rep(c(0, 1), c(n/2, n/2)); x <- c(runif(n/2), runif(n/2));
beta0 <- 0; beta1 <- 2; tau <- 1; sigma <- .2
y <- beta0 + x * beta1 + t * tau + rnorm(n, sd = sigma)
plot(x, y, type = "n", frame = FALSE)
abline(lm(y ~ x), lwd = 2)
abline(h = mean(y[1 : (n/2)]), lwd = 3)
abline(h = mean(y[(n/2 + 1) : n]), lwd = 3)
fit <- lm(y ~ x + t)
abline(coef(fit)[1], coef(fit)[2], lwd = 3)
abline(coef(fit)[1] + coef(fit)[3], coef(fit)[2], lwd = 3)
points(x[1 : (n/2)], y[1 : (n/2)], pch = 21, col = "black", bg = "lightblue", cex = 2)
points(x[(n/2 + 1) : n], y[(n/2 + 1) : n], pch = 21, col = "black", bg = "salmon", cex = 2)

data(swiss)
par(mfrow = c(2, 2))
fit <- lm(Fertility ~., data = swiss)
plot(fit) # residuals and diagnosis plots

?influence.measures

n <- 100
x <- c(10, rnorm(n))
y <- c(10, c(rnorm(n)))
plot(x, y, frame = FALSE, cex = 2, pch = 21, bg = "lightblue", col = "black")
abline(lm(y ~ x))

# diagnostic values
fit <- lm(y ~ x)
round(dfbetas(fit)[1:10, 2], 3) # three digits
round(hatvalues(fit)[1:10], 3) #has to be between 0 and 1
round(dfbeta(fit2)[1:10, 2], 3)
round(hatvalues(fit2)[1:10], 3)

# multiple variables - simulations
n <- 100
nosim <- 1000
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
betas <- sapply(1:nosim, function(i){
    y <- x1 + rnorm(n, sd = 0.3)
    c(coef(lm(y ~ x1))[2],
      coef(lm(y ~ x1 + x2))[2],
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)

n <- 100
nosim <- 1000
x1 <- rnorm(n)
x2 <- x1 / sqrt(2) + rnorm(n) / sqrt(2)
x3 <- x1 * 0.95 + rnorm(n) * sqrt(1 - 0.95 ^ 2)
betas <- sapply(1:nosim, function(i){
    y <- x1 + rnorm(n, sd = 0.3)
    c(coef(lm(y ~ x1))[2],
      coef(lm(y ~ x1 + x2))[2],
      coef(lm(y ~ x1 + x2 + x3))[2])
})
round(apply(betas, 1, sd), 5)
# if the variable that you include is highly correlated with the thing that you are interested, you are going to inflate the standard error even more
# be concerned with putting correlated variables in your model unnecessarily

# variance inflation factors (VIF)
y <- x1 + rnorm(n, sd = 0.3)
a <- summary(lm(y ~ x1)$cov.unscaled[2, 2])
c(summary(lm(y ~ x1 + x2))$cov.unscaled[2, 2],
  summary(lm(y ~ x1 + x2 + x3))$cov.unscaled[2, 2]) / a
temp <- apply(betas, 1, var)
temp[2:3] / temp[1]

library(car)
fit <- lm(Fertility ~ ., data = swiss) #all variables
vif(fit)
sqrt(vif(fit)) #prefer sd

# nested model search
fit1 <- lm(Fertility ~ Agriculture, data = swiss)
fit3 <- update(fit, Fertility ~ Agriculture + Examination + Education)
fit5 <- update(fit, Fertility ~ Agriculture + Examination + Education + Catholic + Infant.Mortality)
anova(fit1, fit3, fit5)

################################################################################
# Quiz 3
# Q1
data("mtcars")
head(mtcars)
fit <- lm(mpg ~ factor(cyl) + wt, data = mtcars)
summary(fit)

# Q2
fit2 <- lm(mpg ~ factor(cyl), data = mtcars)
summary(fit2)

# Q3
fit3 <- lm(mpg ~ factor(cyl) + wt + factor(cyl):wt, data = mtcars)
summary(fit3)
anova(fit, fit3)

# Q4
fit4 <- lm(mpg ~ I(wt * 0.5) + factor(cyl), data = mtcars)
summary(fit4)
summary(fit)

# Q5
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit5 <- lm(y ~ x)
hatvalues(fit5)

# Q6
x <- c(0.586, 0.166, -0.042, -0.614, 11.72)
y <- c(0.549, -0.026, -0.127, -0.751, 1.344)
fit6 <- lm(y ~ x)
dfbetas(fit6)



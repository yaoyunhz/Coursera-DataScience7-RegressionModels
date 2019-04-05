# Coursera Data Science Specilization
# Course 7: Regression Models

library(UsingR)
library(ggplot2)
library(reshape2)
library(manipulate)

data("galton")
head(galton)
long <- melt(galton)
g <- ggplot(galton, aes(x = value, fill = variable))
g <- g + geom_histogram(color = "black", binwidth = 1)
g <- g + facet_grid(. ~ variable)
g

myHist <- function(mu) {
    mse <- mean((galton$child - mu)^2)
    g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", 
                                                          color = "black",
                                                          binwidth = 1)
    g <- g + geom_vline(xintercept = mu, size = 3)
    g <- g + ggtitle(paste("mu = ", mu, ", MSE = ", round(mse, 2), sep = ""))
    g
}

manipulate(myHist(mu), mu = slider(62, 74, step = 0.5))

# the least squares est. is the empirical mean
g <- ggplot(galton, aes(x = child)) + geom_histogram(fill = "salmon", color = "black", binwidth = 1)
g <- g + geom_vline(xintercept = mean(galton$child), size = 1)
g

lm(I(child - mean(child)) ~ I(parent - mean(parent)) - 1, data = galton)

data("father.son")
y <- (father.son$sheight - mean(father.son$sheight)) / sd(father.son$sheight)
x <- (father.son$fheight - mean(father.son$fheight)) / sd(father.son$fheight)
rho <- cor(x, y)
rho
g <- ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g <- g + geom_point(size = 6, color = "black", alpha = 0.2)
g <- g + geom_point(size = 4, color = "salmon", alpha = 0.2)
g = g + xlim(-4, 4) + ylim(-4, 4)
g = g + geom_abline(intercept = 0, slope = 1)
g = g + geom_vline(xintercept = 0)
g = g + geom_hline(yintercept = 0)
g = g + geom_abline(intercept = 0, slope = rho, size = 2) # father's height as predictor
g = g + geom_abline(intercept = 0, slope = 1/rho, size = 2) # son's height as predictor
g

################################################################################
# quiz1
# Q1
x <- c(0.18, 0.18, -1.54, 0.42, 0.42, 0.42, 0.95)
mean(x)

# Q2
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit <- lm(y ~ x - 1) #slop only, through the origin
fit$coefficients

# Q3
data("mtcars")
fit <- lm(mpg ~ wt, mtcars)
fit$coefficients

# Q4
0.5 * 2

# Q5
0.4 * 1.5

# Q6
x <- c(8.58, 10.46, 9.01, 9.64, 8.86)
m <- mean(x)
sd <- sd(x)
x_nor <- (x - m) / sd
x_nor

# Q7
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
y <- c(1.39, 0.72, 1.55, 0.48, 1.19, -1.59, 1.23, -0.65, 1.49, 0.05)
fit1 <- lm(y ~ x)
fit2 <- lm(x ~ y)
fit1$coefficients
fit2$coefficients
(fit1$coefficients) / (fit2$coefficients)
2*(sd(y)/sd(x))
var(y)/var(x)
cor(y, x)
cor(x, y)

# Q8

# Q9
x <- c(0.8, 0.47, 0.51, 0.73, 0.36, 0.58, 0.57, 0.85, 0.44, 0.42)
mean(x)

# Q10
beta <- cor(y, x) * (sd(y) / sd(x))
rho <- cor(x, y) * (Sd(x) / sd(y))
beta / rho = var(y) / var(x)

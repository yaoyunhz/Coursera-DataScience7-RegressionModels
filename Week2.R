# Course7 Regression Model
# week2

library(UsingR)
library(ggplot2)
data("diamond")

# explain price by mass
g = ggplot(diamond, aes(x = carat, y = price), )
g = g + xlab("Mass (carats)")
g = g + ylab("Price (SIN $)")
g = g + geom_point(size = 6, color = "black", alpha = 0.2)
g = g + geom_point(size = 5, color = "blue", alpha = 0.2)
g = g + geom_smooth(method = "lm", color = "black")
g

fit <- lm(price ~ carat, data = diamond)
fit
coef(fit)
summary(fit)

# center data
fit2 <- lm(price ~ I(carat - mean(carat)), data = diamond) #arithmetic function inside regression model
coef(fit2) # same slope, intercept changed (average size diamond)

# price increase for per 1/10 carat
fit3 <- lm(price ~ I(carat * 10), data = diamond)
coef(fit3)

# estimate prices based on weight
newx <- c(0.16, 0.27, 0.34)
coef(fit)[1] + coef(fit)[2] * newx #manual
predict(fit, newdata = data.frame(carat = newx)) #use "predict" function
predict(fit) #y hat values

# residuals
data("diamond")
y <- diamond$price; x <- diamond$carat; n <- length(y)
fit <- lm(y ~ x)
e <- resid(fit)
yhat <- predict(fit)
max(abs(e - (y - yhat)))
max(abs(e - (y - coef(fit)[1] - coef(fit)[2] * x)))
sum(e)

# regression plot with residuals
plot(diamond$carat, diamond$price,
     xlab = "Mass (carats)",
     ylab = "Price (SIN $)",
     bg = "lightblue",
     col = "black", cex = 1.1, pch = 21, frame = FALSE)
abline(fit, lwd = 2)
for (i in 1:n)
    lines(c(x[i], x[i]), c(y[i], yhat[i]), col = "red", lwd = 2)

# residual plot
plot(x, e,
     xlab = "Mass (carats)",
     ylab = "Residuals (SIN $)",
     bg = "lightblue",
     col = "black", cex = 2, pch = 21, frame = FALSE)
abline(h = 0, lwd = 2)
for (i in 1:n)
    lines(c(x[i], x[i]), c(e[i], 0), col = "red", lwd = 2)
# if an intercept is included, residuals should sum up to zero

# non linear data
x = runif(100, -3, 3) #uniform between -3 and 3
y = x + sin(x) + rnorm(100, sd = 0.2) # y occilates around with a little noise
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", color = "black") #first, bottom layer
g = g + geom_point(size = 7, color = "black", alpha = 0.4)
g = g + geom_point(size = 5, color = "red", alpha = 0.4)
g

# residual plot
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2) # reference line
g = g + geom_point(size = 7, color = "black", alpha = 0.4)
g = g + geom_point(size = 5, color= "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g
# the sim term is very apparent in this plot, the model may not be the best fit

# heteroskedasticity
x <- runif(100, 0, 6)
y <- x + rnorm(100, mean = 0, sd = 0.001 * x)
g = ggplot(data.frame(x = x, y = y), aes(x = x, y = y))
g = g + geom_smooth(method = "lm", color = "black")
g = g + geom_point(size = 7, color = "black", alpha = 0.4)
g = g + geom_point(size = 5, color = "red", alpha = 0.4)
g

# residuals
g = ggplot(data.frame(x = x, y = resid(lm(y ~ x))),
           aes(x = x, y = y))
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, color = "black", alpha = 0.4)
g = g + geom_point(size = 5, color = "red", alpha = 0.4)
g = g + xlab("X") + ylab("Residual")
g
# trend - more variation towards the x axis

# diamond data
diamond$e <- resid(lm(price ~ carat, data = diamond))
g = ggplot(diamond, aes(x = carat, y = e))
g = g + xlab("Mass (carats)")
g = g + ylab("Residual Price (SIN $)")
g = g + geom_hline(yintercept = 0, size = 2)
g = g + geom_point(size = 7, color = "black", alpha = 0.5)
g = g + geom_point(size = 5, color = "blue", alpha = 0.2)
g
# residuals have the same unit as y
# no pattern in residuals - good model

# compare intercept only model and the regression model
e = c(resid(lm(price ~ 1, data = diamond)), 
      resid(lm(price ~ carat, data = diamond)))
# variation around average vs. variation around regression line
# create a factor variable to name the dots
fit = factor(c(rep("Itc", nrow(diamond)), rep("Itc, slope", nrow(diamond))))
g = ggplot(data.frame(e = e, fit = fit), aes(y = e, x = fit, fill = fit))
g = g + geom_dotplot(binaxis = "y", size = 2, stackdir = "center", binwidth = 30)
g = g + xlab("Fitting approach")
g = g + ylab("Residual price")
g

# residual variation: variation around the regression line
y <- diamond$price
x <- diamond$carat
n <- length(y)
fit <- lm(y ~ x)
summary(fit) # residual standard error
summary(fit)$sigma
sqrt(sum(resid(fit)^2) / (n - 2))

# inference in regression
y <- diamond$price; x <- diamond$carat; n <- length(y)
beta1 <- cor(y, x) * sd(y) / sd(x)
beta0 <- mean(y) - beta1 * mean(x)
e <- y - beta0 - beta1 * x
sigma <- sqrt(sum(e ^ 2) / (n - 2))
ssx <- sum((x - mean(x)) ^ 2) # sum of squares of x
seBeta0 <- (1 / n + mean(x) ^ 2 / ssx) ^ 0.5 * sigma
seBeta1 <- sigma / sqrt(ssx)
tBeta0 <- beta0 / seBeta0
tBeta1 <- beta1 / seBeta1
pBeta0 <- 2 * pt(abs(tBeta0), df = n - 2, lower.tail = FALSE) #p value
pBeta1 <- 2 * pt(abs(tBeta1), df = n - 2, lower.tail = FALSE)
coefTable <- rbind(c(beta0, seBeta0, tBeta0, pBeta0), c(beta1, seBeta1, tBeta1, pBeta1))
colnames(coefTable) <- c("Estimate", "Std.Error", "t value", "P(>|t|)")
rownames(coefTable) <- c("(Intercept)", "x")
coefTable
# easier way
fit <- lm(y ~ x)
summary(fit)$coefficients

# confidence interval
summary(fit)
sumCoef <- summary(fit)$coefficients
sumCoef
sumCoef[1, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[1, 2] #intercept
sumCoef[2, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[2, 2] #slope
(sumCoef[2, 1] + c(-1, 1) * qt(0.975, df = fit$df) * sumCoef[2, 2]) / 10 #slope, 0.1 increase

# prediction
# data wrangling
newx = data.frame(x = seq(min(x), max(x), length = 100))
newx
p1 = data.frame(predict(fit, newdata = newx, interval = ("confidence")))
p1 #interval around the estimated line at the particular of x
p2 = data.frame(predict(fit, newdata = newx, interval = ("prediction")))
p2 #interval around y at the particular x
p1$interval = "confidence"
p2$interval = "prediction"
p1$x = newx$x
p2$x = newx$x
dat = rbind(p1, p2)
names(dat)[1] = "y"

g = ggplot(dat, aes(x = x, y = y))
g = g + geom_ribbon(aes(ymin = lwr, ymax = upr, fill = interval), alpha = 0.2)
g = g + geom_line()
g = g + geom_point(data = data.frame(x = x, y = y), aes(x = x, y = y), size = 4)
g
# the confidence interval is much narrower than the prediction interval
# the more data, the narrower the confidence interval
# prediction: because of the existence of error term, it's n+1, 
#             it does not go away with the increase number of x collected
# both lines: narrower in the center, wider on both sides




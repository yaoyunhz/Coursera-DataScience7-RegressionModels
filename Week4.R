
# Coursera - Data Science - Course7: Regression Models

# Week4
setwd("C:/Users/yun.yao/Desktop/data science/Course7 Regression Models")
download.file("https://dl.dropboxusercontent.com/u/7710864/data/ravensData.rda",
              destfile = "./ravensData.rda", method = "curl")
load("./ravensData.rda")

library(manipulate)

x <- seq(-10, 10, length = 1000)
manipulate(
    plot(x, exp(beta0 + beta1 * x) / (1 + exp(beta0 + beta1 * x)),
         type = "l", lwd = 3, frame = FALSE),
    beta1 = slider(-2, 2, step = 0.1, initial = 2),
    beta0 = slider(-2, 2, step = 0.1, initial = 0)
)

# poisson regression
# mean and variance are equal
x <- 0:10000
lambda = 3
mu <- sum(x * dpois(x, lambda = lambda))
sigmasq <- sum((x - mu) ^ 2 * dpois(x, lambda = lambda))
c(mu, sigmasq)

# swirl: Binary Outcomes
mdl <- glm(ravenWinNum ~ ravenScore, family = binomial, data = ravenData)
lodds <- predict(mdl, data.frame(ravenScore = c(0, 3, 6)))
exp(lodds) / (1 + exp(lodds)) # convert log odds to probabilities
exp(confint(mdl))
anova(mdl)
qchisq(0.95, 1)

# swirl: Count Outcomes
var(rpois(1000, 50))
mdl <- glm(visits ~ date, poisson, hits)
exp(confint(mdl, 'date'))
which.max(hits[, 'visits'])
hits[704, ]
lambda <- mdl$fitted.values[704]
qpois(0.95, lambda)
mdl2 <- glm(formula = simplystats ~ date, family = poisson, date = hits, offset = log(visits + 1))
qpois(0.95, mdl2$fitted.values[704])

################################################################################
# quiz4
############
# Q1
library(MASS)
head(shuttle)
fit <- glm(use ~ wind, family = binomial, data = shuttle)
summary(fit)
exp(summary(fit)$coef)

# answer
library(MASS)
data(shuttle)
## Make our own variables just for illustration
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind, data = shuttle, family = binomial)
exp(coef(fit))
## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail"), data = shuttle, family = binomial)
exp(coef(fit))

############
# Q2
fit <- glm(use ~ wind + magn, family = binomial, data = shuttle)
summary(fit)
exp(summary(fit)$coef)
table(shuttle$magn)
table(shuttle$wind)
table(shuttle$use)

# answer
shuttle$auto <- 1 * (shuttle$use == "auto")
shuttle$headwind <- 1 * (shuttle$wind == "head")
fit <- glm(auto ~ headwind + magn, data = shuttle, family = binomial)
exp(coef(fit))
## Another way without redifing variables
fit <- glm(relevel(use, "noauto") ~ relevel(wind, "tail") + magn, data = shuttle, 
           family = binomial)
exp(coef(fit))

############
# Q3
shuttle$auto[shuttle$use == "auto"] <- 1
shuttle$auto[shuttle$use == "noauto"] <- 0
shuttle$noauto[shuttle$use == "noauto"] <- 1
shuttle$auto[shuttle$use == "auto"] <- 0
fit1 <- glm(auto ~ wind, family = binomial, data = shuttle)
summary(fit1)
fit2 <- glm(noauto ~ wind, family = binomial, data = shuttle)
summary(fit2)

############
# Q4
data("InsectSprays")
head(InsectSprays)
spray2 <- relevel(InsectSprays$spray, 'B')
fit <- glm(count ~ spray2, poisson, InsectSprays)
exp(summary(fit)$coef)

############
# Q6
x <- -5:5
y <- c(5.12, 3.93, 2.67, 1.87, 0.52, 0.08, 0.93, 2.05, 2.54, 3.87, 4.97)
knot <- 0
splineTerms <- sapply(knot, function(knot) (x > knot) * (x - knot))
xMat <- cbind(1, x, splineTerms)
yhat <- predict(lm(y ~ xMat - 1))
summary(yhat)
yhat
plot(x, y, frame = FALSE, pch = 21, bg = "lightblue", cex = 2)
lines(x, yhat, col = "red", lwd = 2)
# answer
z <- (x > 0) * x
fit <- lm(y ~ x + z)
sum(coef(fit)[2:3])

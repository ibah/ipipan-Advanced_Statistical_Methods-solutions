
    # Combinatorics

perm_without_replacement <- function(n, r){
    return(factorial(n)/factorial(n - r))
}

x <- c(34,16,1,21,33)
n <- length(x)
mean(x)  # population mean & sample mean = estimator of pop.mean
sqrt(sum((x - mean(x))^2)/n)  # population std.dev.
sqrt(sum((x - mean(x))^2)/(n - 1))  # sample std.err., estimator of population std.dev.

k of n
- ordered & repetition -> n^k
- ordered & no repetition -> n!/(n-k)! (filling in k ordered slots)
    - sepcial: k = n -> n! (number of permutations)
- unordered & no repetition -> n!/[k!*(n-k)!] (filling in k ordered slots, dividing for number of possible rearrangements i.e. (k of k) = k!/0!)
- unordered & repetition -> by "observational induction" choose(n,k)+n+n(n-1)+...+n(n-1)...(n-(k-2))
                                                                                           (n-k+2)?
x <- sapply(c(49, 51), function(x) pnorm((x-71)/8))
x[2] - x[1]

output:
    pdf_document:
        fig_width: 6
        fig_height: 3
# small/large
10*(0.4)^5+(1 - .4^5)*(-1)
15*(0.3)^3+(1 - .3^3)*(-5)
# a/b
400*(604*0.25+90*0.45)/800
400*(92*0.4+620*0.3)/800
# light/no light
0.1*2000 + 0.15 * 4000 + 150
0.1*2000 + 0.15 * 4000 + 0.2 * 2000
# av./no av.
100 + 400 * 0.1
200 * 0.25 + 100 * 0.15 + 400 * 0.1
48.4 *  sin(51.7*pi/180) / 9.8
sqrt(1/4.9)
46*sin(47*pi/180)/sin(109*pi/180)
acos(213/312)*180/pi
6.8*sin(40*pi/180)
d <- 4.37^2 + 4*4.9*4.5
(4.37 + sqrt(d))/(2*4.9)
6.8*cos(40*pi/180)*1.5
asin(25*sin(41*pi/180)/30)*180/pi

    # Module 2

• Graphics:
hist() - draws a histogram of the given data,
boxplot() - produce box-and-whisker plot(s) of the given (grouped) values,
plot(x,y,type="p",...) - scatterplot,
curve(function name,xlim=c(a,b)) - draws a curve defined as a function in the range of argument from a to b,
points(x,...) - draws a sequence of points at the specified coordinates,
lines(x,...) - joins the corresponding points with line segments,
abline(a,b) - adds a line with intercept a and slope b to the current plot,
qqnorm(x) - produces a normal QQ plot of the values x,
par(mfrow=c(2,2)) - divides a graphics window into two rows and two colums (enables to produce several plots in one display),
get("windows")() - opens a new graphics window (without closing the current one)


    # Module 3

# trees data
?trees
head(trees)
plot(trees)

fit1 <- lm(Volume ~ Girth, data = trees)
par(mfrow = c(2, 2))
plot(fit1) # here we see a strong nonlinearity
par(mfrow = c(1, 1))

fit2 <- lm(Volume ~ ., data = trees)
summary(fit2)

anova(fit1, fit2)

# accoutning nonlinearity
fit <- lm(I(sqrt(Volume)) ~ I(Girth^2) + Height, data = trees)
par(mfrow = c(2, 2))
plot(fit) # still nonlinearity
par(mfrow = c(1, 1))
summary(fit)

# but from domain & theory: volume ~ pi * girth^2 * height / 3
fit3 <- lm(log(Volume) ~ pi + log(Girth) + log(Height), data = trees)
summary(fit3)

# fitness data - explaining the oxygen intake
# no data

# Prediction


    # Module 4

# residuals
str(trees)
fit <- lm(Volume ~ ., data = trees)
plot(fit, which = 1)  # residuals
plot(fit, which = 3)  # standardized residuals
plot(trees$Girth, rstudent(fit))  # studendized residuls vs. Girth (just as in the lecture)
plot(trees$Girth, fit$res)

fit1 <- lm(Volume ~ Girth + I(Girth*Girth) + Height, data = trees)
summary(fit1)

anova(fit, fit1)

fit2 <- lm(log(Volume) ~ log(Girth) + log(Height), data = trees)
par(mfrow = c(1, 2))
plot(trees$Girth, rstudent(fit1))
plot(trees$Girth, rstudent(fit2))
plot(fit1, which = 1)
plot(fit2, which = 1)

# normality
par(mfrow = c(1, 1))
n <- 20
x1 <- rnorm(n)
x2 <- rnorm(n)
q1 <- quantile(x1, probs = seq(0, 1, .01))
q2 <- quantile(x1, probs = seq(0, 1, .01))
plot(q1, q2)
plot(sort(x1), sort(x2))
plot(q1, qnorm(seq(0, 1, .01)))

#  outcome transformation
library(MASS)
?boxcox


    # Module 5

# Lecture 8 (M5)

# stepwise method for linear models:
step(object, direction = c("both", "backward", "forward"), k)
# k = 2, Akaike
# k = log(n), Schwarc BIC
?step
# example:
n <- 10
x1 <- 1:n
x2 <- sin(1:n)
x3 <- exp(1:n)
y <- x1 + x2 + x3 + rnorm(n)
d <- data.frame(y, x1, x2, x3)
fit <- lm(y ~ ., data = d)
summary(fit)
# forward, AIC
s <- step(lm(y ~ 1, data = d), direction = 'forward', scope = list(upper = y ~ x1 + x2 + x3))
s
summary(s)
# backward, AIC
step(fit, direction = 'backward', k = 2)
# both, starting minimal, AIC
step(lm(y ~ 1, data = d), scope = y ~ x1 + x2 + x3)

# ridge regression
library(MASS)
?lm.ridge
# library(datasets)
?longley  # a well-known example for a highly collinear regression
library(car)
?vif
# ridge example 1
str(longley)
# LS fit:
fit <- lm(Employed ~ ., data = longley)
summary(fit)
vif(fit)
sqrt(vif(fit)['GNP'])  # this much times larger is the std.err than when pred. were independent
# ridge reg. (RR) fit with lambda=0:
rfit <- lm.ridge(Employed ~ ., data = longley)  # default lambda=0
summary(rfit)
rfit$lambda
rfit$coef  # lambda is 0 so we should get the same coefficients as from LS (lm.fit)
fit$coef  # but we don't get the same...
# the reason is: lm.ridge is done for standardized predictors
# so try standardize them and run the lm.fit to check:
longley_std <- as.data.frame(scale(longley))  # scale gives a matrix, so conversion to data.frame
str(longley_std)
summary(longley_std)
signif(sapply(longley_std, mean), 2)
round(sapply(longley_std, mean), 4)
sapply(longley_std, sd) # so it's standardized now; let's run lm:
# LS fit on standardized predictors:
fit_std <- lm(Employed ~ . - 1, data = longley_std)
fit_std$coef
rfit$coef
# you should not standardize the response!
longley.std <- as.data.frame(scale(longley[!names(longley) %in% c('Employed')]))
longley.std$Employed <- longley$Employed
str(longley.std)
round(sapply(longley.std, mean), 4)
sapply(longley.std, sd)
fit.std <- lm(Employed ~ . - 1, data = longley.std) # should intercept be excluded?
fit.std
rfit$coef  # it looks like we are getting closer but it's still not perfect
#--->>> question: how to run LS to get the same coef. as for rigid with lambda=0?
# ridge regression for many non-zero values of lambda:
rfit <- lm.ridge(Employed ~ ., data = longley, lambda = seq(0,0.1,0.001))
summary(rfit)
plot(rfit)  # I can't change the labels ,  xlab = 'lambda', ylab = 'beta'
# ridge help example
# names(longley)[1] <- "y"
# lm.ridge(y ~ ., longley)
# plot(lm.ridge(y ~ ., longley,
#               lambda = seq(0,0.1,0.001)))
# select(lm.ridge(y ~ ., longley,
#                 lambda = seq(0,0.1,0.0001)))

# LASSO regression
# packages: lars, glmnet
# install.packages("lars")
library(lars)
?lars  # type = 'lasso'

# Robust Regression
# M-estimators
?rlm  # Robust Fitting of Linear Models, in MASS
# Least Trimmed Squares (LTS)
?lqs

# Lecutre 9 (M5)

# Principal Components Analysis
?princomp  # in stats,
# cor=TRUE -> standardize data

# example:
n <- 10
x1 <- 1:n
x2 <- sin(1:n)
x3 <- exp(1:n)
y <- x1 + x2 + x3 + rnorm(n)
d <- data.frame(y, x1, x2, x3)
# CPA for all variables:
d.pc <- princomp( ~ ., cor = T, data = d)  # using formula and data
summary(d.pc)
d.pc <- princomp(d, cor = T)  # the same shorter, using x only
summary(d.pc)
# components 1-2 are enough to describe 91% variability of data (convetional treshold of 80%)
# component 4 is not needed as one of the columns in the data (y)
    # is a linear combination if the other trhee and a random noise
plot(d.pc)
# the same graphically
d.pc$loadings  # loadings
d.pc$scores  # values of components (for particular observations)

# example 2 - the same but on predictors only:
d[-1]
d.pc <- princomp(d[-1], cor = T)  # the same shorter, using x only
summary(d.pc)
plot(d.pc)
d.pc$loadings
d.pc$scores
d[-1]  # for comparison
which.max(abs(d.pc$scores[,1]))  # most extreme observation (in terms of most variable direction i.e. component 1)
d[order(abs(d.pc$scores[,1]), decreasing = T),]  # observations sorted from most to least extreme
    # makes sense; the data are linear growth + exponential growth + a cycle
    # the last and the first observation should be most extreme, and so on

# Pricipal Component Regression (PCR)


    # Module 6

# Lecture 10 (M6)
# Logit funtions
library(boot)
?logit  # boot, car
?inv.logit  # boot
x <- seq(0.01, 0.99, 0.01)
y <- logit(x)
plot(x, logit(x))
plot(y, inv.logit(y))
# Logistic function (probability of success) = ilogit
pbbt.of.success <- function(x, b1 = 1, b0 = 0) {
    1 / (1 + exp(-b1*x - b0))
}
x <- seq(-10, 10, 0.1)
c = 1
plot(x, pbbt.of.success(x, b1 = 1), col = c, type='l')
for(i in 1:3) {
    if(i==1)
        plot(x, pbbt.of.success(x, b1 = 2^(i-2)), col = i, type = 'l')
    else
        lines(x, pbbt.of.success(x, b1 = 2^(i-2)), col = i)   
}
# the same:
for(i in 1:3) {
    if(i==1)
        plot(x, ilogit(x * 2^(i-2)), col = i, type = 'l')
    else
        lines(x, ilogit(x * 2^(i-2)), col = i)   
}
# Example
# Functions and datasets for books by Julian Faraway
install.packages('faraway')
library(faraway) # ilogit
?bliss  # Bliss (1935). The calculation of the dosage-mortality curve. Annals of Applied Biology 22, 134-167.
bliss  # each row represents 30 experiments (each on one insect, that either dies or survives)
str(bliss)
# logit model
g <- glm(cbind(dead, alive) ~ conc, family = binomial, data = bliss)
# Y consists of two columns:
    # number of successes
    # number of failures
summary(g)
g$fit  # i.e. the higher concentration the higher probability of an insect being killed
       # sic: the columns names are the row numbers? the conc. is 0..4, here we have 1..5
       # sic: number of different values of the predictor -> number of fitted values
# directly: linear component -> inverse logit function -> success probability
ilogit(g$coef[1] + g$coef[2]*bliss$conc)
pbbt.of.success(bliss$conc, g$coef[2], g$coef[1])
# probit model
gp <- glm(cbind(dead, alive) ~ conc, family = binomial(link = probit), data = bliss)
# comparison logit vs. probit
x <- seq(-2, 8, 0.2)
# fitted/predicted probabilities:
pl = ilogit(g$coef[1] + g$coef[2] * x)
pp = pnorm(gp$coef[1] + gp$coef[2] * x)
plot(x, pl, type = 'l', col = 2)
lines(x, pp, col = 3)
# ratios of probabilities:
  # pbbt of success probit/logit
  # pbbt of failure probit/logit
matplot(x, cbind(pp/pl, (1-pp)/(1-pl)), type='l', xlab='Dose', ylab='Ratio')
  # Plot Columns of Matrices
# inference
summary(g)
names(g)
names(summary(g))
# testing significance of all predictors
# test statistic:
g$null.deviance - g$deviance
summary(g)$null.deviance - summary(g)$deviance
with(g, null.deviance - deviance)
    # how to use chi-squared distribution
    qchisq(0.1, 1)
    qchisq(0.5, 1)
    qchisq(0.9, 1)
# rejection region is the he upper part of the chi-squared distribution
# here: significance level alpha = 0.01
qchisq(0.01, df = 1, lower.tail = F)
# rejection region is > 6.63 => so reject H0
pchisq(64.3, df = 1, lower.tail = F)  # 'p value' for H0 => reject H0

# Can we include other terms?
g2 <- glm(cbind(dead, alive) ~ conc + I(conc^2),
          family = binomial, data = bliss)
summary(g2)
# testing significance of conc^2
# H0: conc^2 isn't significant in the model
g$deviance
g2$deviance
x <- g$deviance - g2$deviance
pchisq(x, df = 1, lower.tail = T)
  # so there's 33% probability of achiving such a deviance between
  # the model with conc only and conc with conc^2 under H0
  # so no grounds to reject H0.

# the same test using anova() function
anova(g, g2)  # from smallest to the biggest models
  # deviance should be big to be significant
  # e.g for a difference of just one variable (= 1 degree of freedom)
  # at the significance level of 1%
  # the deviance should be at least 6.63
  qchisq(0.01, df = 1, lower.tail = F)
  # here it's only 0.18, so we accept the smaller model

  
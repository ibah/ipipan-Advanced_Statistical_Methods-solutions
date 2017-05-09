
    # Module 1

# Questions
1. Chebyshev Inequality
I don't understand how applying g(x) = x^2 yields Var(Y). Here's my reasoning:
The numerator of the RHS is (|Y - E[Y]|)^2 = (Y - E[Y])^2, this is a function of Y, so a random variable itself.
Var(Y) = E[(Y - E[Y])^2] is a particular value.
I assume |X| is the absolute value of |X|.

2. In task 6 we could derive sigma from mu under H0 by assuming the underlying distribution is binomial.
So we could ask how unusual is ^p under the distribution stipulated by H0 and get the p-value.
In Task 7 we couldn't derive the sigma from mu so we used SE obtained from the sample instead.'
SE is an estimator of the actual sigma given the sample we have.
Following the lecture I used normal distribution in the former case and t distribution in the latter to perform the test.
Am I right to think that:
1) in task 6 we could use binomial distribution to get the exact interval/test but we use normal distribution as a simplification and approximation
2) in task 7 we use t distribution as it gives more accurate interval than normal distribution (by CLT) and isn't much more mathematically complicated'
?
Also:
If we knew the population true sigma in task 7, would we use the t distribution or normal distribution?

# Task 1

p_red <- c(2/3, 1/2, 1/3)
p_green <- 1 - p_red
all_lights <- 1:3
FX <- sapply(0:3, function(x) {  # construct the distribution for x = 0,1,2,3
    elem_events <- combn(all_lights, x)  # all elementary events giving exactly x red lights each,
                                    # described by indeces of the traffic light showing red
    p_elem_events <- apply(elem_events, 2, function(red_lights) {  # calculating the probability of each elementary event
            green_lights <- setdiff(all, red_lights) # indeces of the traffic light showing green
            prod(p_red[red_lights], p_green[green_lights]) # probability of an elementary event
        })
    c(x, sum(p_elem_events))  # sum of probabilities of all elementary events giving rise to x red lights
})
rownames(FX) <- c("x", "F(x)")
FX  # the distribution function

#           [,1]      [,2]      [,3]      [,4]
# [1,] 0.0000000 1.0000000 2.0000000 3.0000000
# [2,] 0.1111111 0.3888889 0.3888889 0.1111111

# Task 2

mn <- 100
sd <- 15
# a)
pnorm(130, mean = mn, sd = sd, lower.tail = F)
# [1] 0.02275013
# b)
pnorm(120, mean = mn, sd = sd) - pnorm(100, mean = mn, sd = sd)
# [1] 0.4087888

# Task 3

pxy <- matrix(
        c(c(.1, .1, 0),
          c(.2, .2, .1),
          c(.1, .1, .1)),
        nrow = 3,
        byrow = T,
        dimnames = list(X=c(0, 1, 2), Y=c(-1, 0, 1))
    )
pxy
# a)
px <- apply(pxy, 1, sum)
x <- as.numeric(names(px))
px
py <- apply(pxy, 2, sum)
y <- as.numeric(names(py))
py
# b)
cond <- matrix(nrow = 3, ncol = 3)
for(i in seq_len(nrow(pxy)))
    for(j in seq_len(ncol(pxy)))
        cond[i,j] <- as.numeric(rownames(pxy)[i]) > 2 * as.numeric(colnames(pxy)[j])
# or
cond <- matrix(
    sapply(1:9, function(i) {
        ind <- arrayInd(i, dim(pxy))
        as.numeric(rownames(pxy)[ind[, 1]]) > 2 * as.numeric(colnames(pxy)[ind[, 2]])
    }),
    nrow = 3
)
sum(pxy[cond])
# c)
mn_x <- sum(x * px)
mn_y <- sum(y * py)
tmp <- matrix(nrow = 3, ncol = 3)
for(i in seq_len(nrow(pxy)))
    for(j in seq_len(ncol(pxy)))
        tmp[i, j] <- pxy[i,j] * (x[i] - mn_x) * (y[j] - mn_y)
cov_xy <- sum(tmp)
var_x <- sum(px * (x - mn_x)^2)
var_y <- sum(py * (y - mn_y)^2)
cor_xy <- cov_xy/sqrt(var_x * var_y)
cov_xy # (they are not intependent. if independent then all fields should be equal 1/9
cor_xy
# d)
FX_y <- sapply(pxy[, 1], function(p) p / sum(pxy[, 1]))  # given y = -1
FY_x <- sapply(pxy[1, ], function(p) p / sum(pxy[1, ]))  # given x = 0
FX_y
FY_x

# Task 4

x_min <- 1.4
x_max <- 1.8
f_x <- 1 / (x_max - x_min)
mn_x <- 2.5 * x_max^2 / 2 - 2.5 * x_min^2 / 2 # from the definition
# or mn_x <- mean(c(x_min, x_max))
exp_val_of_squared_x <- 2.5 * x_max^3 / 3 - 2.5 * x_min^3 / 3
var_x <- exp_val_of_squared_x - (mn_x)^2  # why this is wrong? ((x_max - x_min) / 4)^2
sd_x <- sqrt(var_x)
# sum of 50 randoms
# E(Sn) = sum(E(X1)+...+E(Xn))
n <- 50
mn <- n * mn_x
sd <- n * sd_x
mn
sd

# Task 5

n <- 50
mn <- 28.40
sd <- 4.75
mn + c(-1, 1) * qnorm(.975) * sd / sqrt(n)
# or
sd_of_mn <- sd / sqrt(n)
qnorm(.025, mean = mn, sd = sd_of_mn)
qnorm(.975, mean = mn, sd = sd_of_mn)

# Task 6

n <- 400
rec <- 79
# alpha <- .05
# two sided test
# H0: mu = 25%, H1: mu != 25%
# binomial trails
mu0 <- .25
mn <- rec / n
var0 <- mu0 * (1 - mu0) / n # sample variance in H0
t.stat <- (mn - mu0) / sqrt(var0)
2 * pnorm(t.stat)
2 * pt(t.stat, df = n - 1)  # slightly higher
# H0 rejected p-val < alpha
# following is all wrong: it uses empirical var. instead of H0 implied variance
var1 <- mn * (1 - mn) / n # popul. variance using mu=mn estimatior
t.stat <- (mn - mu0) / sqrt(var1)
2 * pt(t.stat, df = n - 1)
var2 <- (rec*(1 - mn)^2 + (n - rec) * (0 - mn)^2) / (n - 1) # sample variance as population variance estimator
t.stat <- (mn - mu0) / sqrt(var2)
2 * pt(t.stat, df = n - 1)

# Task 7

mu0 <- 3.2
n <- 50
mn <- 3.05
se <- 0.34
t.stat <- sqrt(n) * (mn - mu0) / se
pt(t.stat, n - 1)
# H0 rejected


    # Module 2

# Task 1
# Quic Introduction to R

# Task 2
attach(trees)
par(mfrow = c(1, 2))
plot(Girth, Volume)
plot(Height, Volume)
cor(Girth, Volume)  # the default method is pearson
cor(Girth, Volume, method = "spearman")
cor(Height, Volume)
cor(Height, Volume, method = "spearman")
cor.test(Girth, Volume)  # the default method is pearson, significance level is 0.5
cor.test(Girth, Volume, method = "spearman")
cor.test(Height, Volume)
cor.test(Height, Volume, method = "spearman")
detach(trees)

# Task 3

# checking
getwd()
list.files() # no data here
file <- "Module 02/patients.txt"
data <- read.table(file, header = T)
# fix(data)  # oh, you have a header here
str(data)  # factors and one numeric
summary(data)  # no missing data
# subsetting
# data <- subset(read.table(file, header = T), select = c(education, marital))

table(data[c("education", "marital")])

summary(data)
attach(data)
test <- chisq.test(education, marital)
test  # p-value too high to reject the test
# checking
t <- table(data[c("education", "marital")])
n <- sum(t)
edu_marg <- apply(t, 1, sum)/n
mar_marg <- apply(t, 2, sum)/n
exp_counts <- n * edu_marg %o% mar_marg  # == outer(), outer product
exp_counts
q_squared <- sum((t - exp_counts)^2 / exp_counts)
df <- prod(dim(t) - 1)
1 - pchisq(q_squared, df)  # so the same value as the chisq.test()

# residuals
test$residuals  # pearson residuals
# analyze XXX

sub_male <- data[data$gender == "male",]
sub_female <- data[data$gender == "female",]
table(sub_male$education, sub_male$marital)
table(sub_female$education, sub_female$marital)  # very small counts, 3 cells below 5
chisq.test(sub_male$education, sub_male$marital)
chisq.test(sub_female$education, sub_female$marital)  # warning, too few observations

test <- fisher.test(education, marital)
test
# similar p-value like chisq.test (a bit lower)
# checking
t <- table(data[c("education", "marital")])
N <- sum(t)
n <- apply(t, 1, sum)
m <- apply(t, 2, sum)
fisher_prob <- function(x, n, m, N) {
    choose(n, x) * choose(N - n, m - x) / choose(N, m)
}
result <- matrix(nrow = 4, ncol = 2)
for(i in 1:4)
    for(j in 1:2) {
        result[i, j] <- fisher_prob(x = t[i, j], n = n[i], m = m[j], N = N)
    }
result  # is this correct?
# now to get the p-value it's more complicated
# ... more complicated
detach(data)

# Task 4

file <- "Module 02/kids.txt"
data <- read.table(file, header = T)
fix(data)
str(data)  # factors and numerical
summary(data)  # no NA's
attach(data)

# Is the importance of Looks dependent on the gender?
# tests for nominal data
chisq.test(Gender, Looks)
fisher.test(Gender, Looks)
# very low p-values
cor(as.numeric(Gender), Looks)
# -0.36

# We should use measures of dependence for nominal and ordinal data

# Gini index for Looks
n <- length(Looks)
t <- table(Looks)
l <- length(t)
p <- t / n
V_looks <- 1 - sum(p^2)
V_looks
1 - 1/l  # max value (=uniform) (so it's almost uniform)
# comparing with the library
library(reldist)
gini(Looks)
# testing
gini(c(1,0,0))  # 1 (should be 0)
gini(c(1,1,1))  # 0 (should be 26/27)
# external implementation
gini2 <- function(x, unbiased = TRUE, na.rm = FALSE){
    if (!is.numeric(x)){
        warning("'x' is not numeric; returning NA")
        return(NA)
    }
    if (!na.rm && any(na.ind <- is.na(x)))
        stop("'x' contain NAs")
    if (na.rm)
        x <- x[!na.ind]
    n <- length(x)
    mu <- mean(x)
    N <- if (unbiased) n * (n - 1) else n * n
    ox <- x[order(x)]
    dsum <- drop(crossprod(2 * 1:n - n - 1,  ox))
    dsum / (mu * N)
}
gini2(Looks)  # the same

# Goodman Kruskal Tau - implementation 1
t <- table(Gender, Looks)
p <- t / n
p_marg_gender <- apply(t, 1, sum) / n
p_cond_looks_on_gender <- p / p_marg_gender
V_cond_looks_on_gender <- 1 - apply((p_cond_looks_on_gender)^2, 1, sum)
V_looks_expt <- sum(p_marg_gender * V_cond_looks_on_gender)
Goodman_Kruskal_tau <- (V_looks - V_looks_expt) / V_looks
Goodman_Kruskal_tau

# Goodman Kruskal Tau - implementation 2
gini_index <- 1 - sum((table(Looks)/length(Looks))^2)
n_boy <- sum(data$Gender == "boy")
gini_boy <- 1 - sum((table(data[data$Gender == "boy", "Looks"]) / n_boy)^2)
n_girl <- sum(data$Gender == "girl")
gini_girl <- 1 - sum((table(data[data$Gender == "girl", "Looks"]) / n_girl)^2)
gini_expected <- gini_boy * n_boy / n + gini_girl * n_girl / n
goodman_kruskal_tau <- (gini_index - gini_expected) / gini_index
goodman_kruskal_tau


# Goodman-Kruskal's gamma dependence index for Gender and Looks
t <- table(Gender, Looks)
c <- sum(t[1, 1] * t[2, 2:4]) +
    sum(t[1, 2] * t[2, 3:4]) +
    t[1, 3] * t[2, 4]
d <- sum(t[2, 1] * t[1, 2:4]) +
    sum(t[2, 2] * t[1, 3:4]) +
    t[2, 3] * t[1, 4]
goodman_kruskal_gamma <- (c - d) / (c + d)
goodman_kruskal_gamma

# negative dependence of -.54 on gender: boy means looks are less important
# if it was continuous:
n <- sum(t)
kendall_tau <- (c - d) / (n * (n - 1) / 2)
kendall_tau

# Kendall's tau for all pairs of ordered variables
# Kendall library
library(Kendall)
?Kendall
Kendall(Looks, Gender)
detach(data)


    # Module 3

# Task 1
# a
str(trees)
summary(trees)
attach(trees)
fit1 <- lm(Volume ~ Girth)
fit2 <- lm(Volume ~ Height)
names(fit1)
fit1$coefficients
fit1$fitted.values
fit1$residuals
# b
summary(fit1)
names(summary(fit))
# c
par(mfrow = c(1, 2))
plot(Girth, Volume)
abline(fit1)
plot(Height, Volume)
abline(fit2)
# d
data.frame(r.squared = c(summary(fit1)$r.squared, summary(fit2)$r.squared),
           row.names = c(fit1$call, fit2$call))
# so much lower for fit2
# e
summary(fit1)
summary(fit2)
# assuming linear is correct (it's not, see diagnostics -> heteroscedasticity)
# we can say that in absence of any other predictor Girth and Heights turns out
# to be significant. but there may be other relevant predictors that may change
# this conclusion if included
# f
# CI for the slope coefficient - 3 different derivations
n <- nrow(trees)
p <- 2
df <- n - p
b0 <- fit1$coefficients["(Intercept)"]
b1 <- fit1$coefficients["Girth"]
# derivation using scalar formulas
SigSq <- sum(fit1$residuals^2) / (n - p)
var_b0 <- sigSq * (1 / n + (mean(Girth))^2 / sum((Girth - mean(Girth))^2))  # not used, done just for completness
var_b1 <- sigSq * (1 / sum((Girth - mean(Girth))^2))
b1 + c(-1, 1) * sqrt(var_b1) * qt(.975, df)
# derivation using matrices (variance-covariance matrix)
xMat <- matrix(c(rep(1, length(Girth)), Girth), ncol = 2)
vcm <- (summary(fit1)$sigma)^2 * solve(t(xMat) %*% xMat)
vcm <- matrix(vcm, ncol = 2, dimnames = list(c("(Intercept)", "Girth"), c("(Intercept)", "Girth")))
b1 + c(-1, 1) * sqrt(vcm["Girth", "Girth"]) * qt(.975, df)
# the same using summary(fit):
b1 + c(-1, 1) * summary(fit1)$sigma * sqrt(summary(fit1)$cov.unscaled["Girth", "Girth"]) * qt(.975, df)
# the same using confint function:
confint(fit1)["Girth",]
# g
sigSq
# h
detach(trees)
unname(b0 + b1 * 15)
unname(predict(fit1, newdata = data.frame(Girth = 15)))
predict(fit1, newdata = data.frame(Girth = 15), interval = "prediction")[1]

# Task 2
file <- "Module 03/anscombe_quartet.txt"
data <- read.table(file, header = T)
n <- nrow(data)
m <- ncol(data) / 2  # number of pairs of data
allX <- data[,grep("^X", colnames(data))]
allY <- data[,grep("^Y", colnames(data))]
# a
rm(fit)
fit <- list()
for(i in 1:m)
    fit <- c(fit, list(lm(allY[, i] ~ allX[, i])))
# b
sapply(1:m, function(i) {
        setNames(c(fit[[i]]$coefficients,
                   summary(fit[[i]])$r.squared,
                   cor(allX[, i], allY[, i])),
                 c("b0", "b1", "R^2", "cor"))
    })
# very simliar values
# c
par(mfrow = c(2, 2))
for(i in 1:m) {
    plot(allX[, i], allY[, i], main = paste("pair", i))
    abline(fit[[i]])
}
# 1 reasonable, 2 nonlinear (quadratic), 3 linear but high-leverage outlier (different slope)
# 4 almost all data at one x (no variation, can't infere anything), regr. hinges on just one data point


# Task 3
file <- "Module 03/realest.txt"
data <- read.table(file, header = T)
str(data)
summary(data)
n <- nrow(data)
# data <- data[-11,] outlier, big house
fit <- lm(Price ~ ., data)
summary(fit)
par(mfrow = c(2, 2))
plot(fit)
# a
# +1 bedroom -> exp. decrease in the house's price by 7.76
fit1 <- lm(Price ~ Bedroom, data)
summary(fit1)
# 3.92 increase in the price
# the reason might be: the size of the house is cought by four variables:
# "Bedroom", "Space", "Room", "Bathroom", see correlation:
cor(data[,c("Bedroom", "Space", "Room", "Bathroom")])
# probably the vatiation is cought by the other three.
# let's leave Bedroom
summary(lm(Price ~ . - Room - Space - Bathroom, data))  # best if Bedroom removed, Bathroom left
par(mfrow = c(1, 1))
with(data, plot(Bedroom, Price))
par(mfrow = c(2, 2))
#..................
# b
new_house <- data.frame(Condition = 1,
                        Bedroom = 3,
                        Room = 8,
                        Bathroom = 2,
                        Garage = 1,
                        Space = 1500,
                        Lot = 40,
                        Tax = 1000)
predict(fit, new_house, interval = "confidence")
predict(fit, new_house, interval = "prediction")

# Task 4
file <- "Module 03/cheese.txt"
data <- read.table(file, header = T)
str(data)
summary(data)
fit1 <- lm(taste ~ Acetic, data)
fit2 <- lm(taste ~ Acetic + Lactic + H2S, data)
summary(fit1)
summary(fit2)
# step by step
n <- nrow(data)
q <- 2
p <- 4
ESS1 <- sum(fit1$residuals^2)
ESS2 <- sum(fit2$residuals^2)
F_stat <- ((ESS1 - ESS2) / (p - q)) / (ESS2 / (n - p))
pf(F_stat, p - q, n - p, lower.tail = F)
# anova
anova(fit1, fit2)
# at the significance level of 0.05 we reject the null hypothesis that the smaller model is better


    # Module 4

# Task 3
file <- "Module 04/realest.txt"
data <- read.table(file, header = T)
str(data)
summary(data)
fit <- lm(Price ~ ., data = data)
# a
par(mfrow = c(2, 2))
plot(fit)
# interpret
# b
res <- rstudent(fit)
res[abs(res) > 2]
# c
cooks.distance(fit)
hatvalues(fit)

# Task 5
file <- "Module 04/strongx.txt"
data <- read.table(file, header = T)
str(data)
summary(data)
# For  each  value  of  momentum  an  experiment  was  performed  repeatedly.
# Thus  an  estimator  of standard deviation of cross-section
# could be calculated for each value of momentum.
# It is expected that cross-section should be
# a linear function of the inverse of energy of a particle.
# a
fit1 <- lm(crossx ~ energy, data = data)
# b
fit2 <- lm(crossx ~ energy, weights = sd^-2, data = data)
# c
summary(fit1)
summary(fit2)
plot(fit1)
plot(fit2)
# d
X propose modification
# e
par(mfrow = c (1, 1))
plot(data$energy, data$crossx)
lines(data$energy, fit2$fitted.values, col = 2)

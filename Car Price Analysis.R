################################
# Car Data Analysis            #
################################

# Author: Ann-Katrin Reuel 

# Load/install relevant packages:
#install.packages("reshape")
#install.packages("ggplot2")
#install.packages("reshape2")
library(reshape)
library(reshape2)
library(ggplot2)

############
# Abstract #
############

# The following code is written to analyze the automobile data taken obtained
# by UC Irivine [Link: https://archive.ics.uci.edu/ml/datasets/Automobile].
# The main focus of the analysis is on how price is affected by factors
# such as fueltype, highway mpg, and car weight, among others.
# Analysis techniques include simple, multiple and logistic regression,
# correlation analysis, CI analysis and bootstrapping, as well as descriptive
# statistics.

# (Note: See link for further information on the data sources and contact info
#        for the creator/donor of the data set, Jeffrey C. Schlimmer)

##################################
# General description of dataset #
##################################

auto <- read.csv("auto2.csv")
colnames(auto) <- c("number", "symbol","normloss","make","fueltype","aspiration","doors",
                    "body","wheels","engineloc","wheelbase","length","width","height","weight","enginetype",
                    "cylinders","enginesize","fuelsys","bore","stroke","compression",
                    "horsepw","rpm","citympg","highmpg","price")
attach(auto)

# This is data on varous automobiles models from different manufacturers, which
# describes aspects of cars produced in 1985.
# The meaning of the variables of interest to us should mostly be obvious.
# Those which are not, e.g. number, symbol, normloss, will not play a significant
# role in our analysis -- thus, further exposition on their meaning seems superfluous.

# The following provide a general overview of the variable types, as well as the
# structure of the dataset.
head(auto)
summary(auto) # Note the various variable types (categorical and continuous & noncontinuous numeric)
dim(auto) # nrow = # of cars in dataset; ncol = number of variables

########################
# Correlation Analysis #
########################

# Correlation analysis of data at hand

# Restrict the dataset to the numeric variable for which this analysis makes sense.
cont.auto <- auto[,c("normloss","wheelbase","length","width","height","weight",
                 "enginesize","bore","stroke","compression",
                 "horsepw","rpm","citympg","highmpg","price")]
cor.matrix <- round(cor(cont.auto),2)
cor.matrix # correlation matrix

# Get lower triangle of correlation matrix
get_lower_tri <- function(cor.matrix)
{
  cor.matrix[upper.tri(cor.matrix)] <- NA
  return(cor.matrix)
}
# Get upper triangle of correlation matrix
get_upper_tri <- function(cor.matrix)
{
  cor.matrix[lower.tri(cor.matrix)]<- NA
  return(cor.matrix)
}

upper_tri <- get_upper_tri(cor.matrix)
upper_tri
melted_cormat <- melt(upper_tri, na.rm = T)

# Heatmap
ggplot(data = melted_cormat, aes(X2, X1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "aquamarine2", high = "aquamarine4", mid = "aquamarine3", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1)) +
  coord_fixed()
# Gray squares are redundant. Each correlation appears once.

# Selected box- and scatterplots
price.split <- split(price, cylinders)
boxplot(price.split$three, price.split$four, price.split$five, price.split$six,
        price.split$eight,
        names = c("three", "four", "five", "six", "eight"),
        xlab = "Number of cylinders", ylab = "Price")
# There appears to be a positive relationship between the number of cylinders
# price of the car.

# To get a lay of the land, view scatterplots of price against height, weight, & highway mpg
plot(height, price)
plot(weight, price)
plot(highmpg, price)

# The relationship (a positive one) appears strongest beween price and weight
# as our heatmap indicated.
# This may, in large measure, explain the negative relationship we observe between
# highway mpg and price, as one would expect larger (generally more expensive) 
# vehicles to be less fuel efficient.

################################
# Simple & Multiple Regression #
################################

# Weight showed highest absolute correlation with price
# Scatterplot & simple regression line
price.simple.lm <-lm(price~weight, data=auto)
summary(price.simple.lm)
# Each additional unit of weight is, according to our simple linear model,
# associated with a 10.9 unit increase in price.
plot(weight, price)
abline(price.simple.lm, col ="red")
# This model appeals to our intuition that larger vehicles are likely more expensive
# to produce and, in turn, price more highly for the consumer.
# Of course, a simple linear model doesn't prove such a causal relationship,
# but this linear association appeals to reason.

#Diagnostics
old.par <- par(mfrow=c(2,2))
plot(lm(price~weight, data=auto))
par(old.par)

#Residual vs. Fitted Plot: This graph is used to examine whether residuals have non-linear patterns.
#An ideal plot would feature uniformly distributed residuals
#with no distinct pattern observable. In our case, we can see a curvature of the line as well as 
#clustered points, indicating that there could be a non-linear relationship between predictor variables and an outcome variable
#which we haven't captured.

#Normal QQ-Plot: This plot indicates whether the residuals
#are distributed normally. We can observe rather heavy tail
#i.e. our data has more extreme values than we would see
#in a normal distribution with the same parameters. Especially
#observations 115 and 49 seem to be problematic.

#Scale-Location Plot: This plot is used to identify any violation
#of the homoscedasticity assumption. The residuals should be
#equally spread across the graph if the assumption is not violated.
#In our case, we see a strong kink at the beginning of the curve, indicating 
#heteroscedasticity.

#Residuals vs. Leverage Plot: This plot checks for infuential outliers which
#could potentially alter our regression result.In our case, all 
#observations lie within the dashed lines, thus we do not see the necessity
#to exclude any data points.

#Due to the number of problems associated with our simple linear regression
#model, we try to fit a more complex model, including multiple explanatory
#variables.
#We are aware of the fact that this does not resolve all of the above problems.
#However, a diligent linear regression analysis would go well beyond the scope
#of this project.

# Multiple regression
# Selection of variables based on absolute correlation with price
price.lm <- lm(price~weight+enginesize+horsepw)
summary(price.lm)
# This linear multiple regression yields positive coefficients for all three
# dependent variables.
# Weight:      each unit increase in weight associated with 8.10 unit price increase
#              (stat. significant at .001 level)
# Engine size: each unit increase in engine size associated with 34.26 unit price increase
#              (stat. significant at .05 level)
# Horsepower:  each unit increase in engine size associated with 17.46 unit price increase
#              (NOT, however, stat. significant)
# Our R-squared statistic indicates that this linear model accounts for 80.83%
# of the variation in the price variable, indicating only a marginal improvement
# compared to the simple model.

# We note, however, that this regression is problematic, as the dependent variables
# are clearly not independent; engine size and horsepower should, generally,
# increase as the weight of the car increases.
# A crucial assumption of the regression is independence of the explanatory variables,
# which these variables clearly violate.

old.par <- par(mfrow=c(2,2))
plot(price.lm)
par(old.par)

#This can be observed in the diagnostic plots, too. They didn't change much
#compared to our simple linear regression model.

#However, if we log-transform our variables (based on the fact that for
#all variables, we have a mean greater than the median, indicating a skewed distribution), 
#we get:

log.price <- log(price)
log.weight <- log(weight)
log.enginesize <- log(enginesize)
log.horsepw <- log(horsepw)

price.lm.log <- lm(log.price~log.weight+log.enginesize+log.horsepw)
summary(price.lm.log)

#Our adjusted R squared increased by 5% and our RSE went down drastically, too.
#Horsepower gained statistical significance and has a positive coefficient of
#0.333 while enginesize lost its statistical sigificance. 

old.par <- par(mfrow=c(2,2))
plot(price.lm.log)
par(old.par)

#Looking at the diagnostic plots, we can see that especially the normal QQ plot
#improved, showing that the residulas better align with a normal distribution,
#although we still observe somewhat fat tails. The scale-location plot slightly
#improved, too. Overall, we can say, based on the adjusted R squared, the
#diagnostic plots as well as the reduced error that our regression model improved.
#However, as noted above, we are still facing heteroscedasticity and dependence
#issues, thus our results need to be taken with a grain of salt. But, as stated above,
#a further development of our model is beyond the scope of the project.

########################
# CI for mean of price #
########################

# population standard deviation and population mu unknown
hist(price) #distribution of price is very right-skewed
# problem for t confidence interval: distribution is skewed, not symmetric and sample size
# isn't large enough to compensate
qqnorm(price)
qqline(price, col = 2)

# To demonstrate the importance of the scewness in the price data,
# we will compare conventional (formula-based) t confidence intervals with
# the t bootstrap confidence interval, which compensates for scew in the data.

# Conventional t confidence interval:
CI.formula <- t.test(price, conf.level = .95)$conf; CI.formula

# Due to skewness, we need a skewness-adjusted version of 
# the t interval (which a t bootstrap CI provides).

# But first, we remind ourselves of the meaning of confidence intervals.
# From a normal distribution with the same mean and sd as our price data,
# we generate 159 independent random samples, which we use to find a 95% 
# t confidence interval by the conventional method.
# Repeating this 10,000 times, we can observe that approx. 95% of the confidence
# intervals contain the mean of the normal distribution we used to generate the
# data.
# (Note that we do that our actual data is most definitely NOT normal.)
mean.price <- mean(price); mean.price
sd.price <- sd(price); sd.price
q <- qt(.975, df = 158)
N <- 10^4

counter <- 0      # set counter to 0 
plot (x = c(9000, 14000), y = c(1, 100), type = "n", xlab = "",   ylab = "")# set up a blank plot with specified ranges 
for (i in 1:N) 
{   
  x <- rnorm(159, mean.price, sd.price)   # draw a random sample of size 159 
  L <- mean(x) - q*sd(x)/sqrt(159)   # lower limit   
  U <- mean(x) + q*sd(x)/sqrt(159)   # upper limit   
  if (L < mean.price && mean.price < U)   # check if mean of price is in interval     
    counter <- counter + 1   # if yes, increase counter by 1   
  if (i <= 100) # plot first 100 intervals
  {
    if (L < mean.price & mean.price < U)
    { segments(L, i, U, i) } else { segments(L, i, U, i, col = "red") }
  }
} 
abline(v = mean.price, col = "blue")   # vertical line at mu 
counter/N     # proportion of times interval contains mu. 

# Returning to our data, we compute a 95% t bootstrap CI.

xbar <- mean(price) 
N <- 10^4 
n <- length(price); n
Tstar <- numeric(N) 

for (i in 1:N) 
{  
  x <- sample(price, size = n, replace = T)  
  Tstar[i] <- (mean(x)-xbar) / (sd(x)/sqrt(n)) 
} 
# We observe that the bootstrap distribution doesn't look quite symmetric.
hist(Tstar, breaks = "FD", main = "t bootstrap distribution")
# It's slightly left-scewed
# More rigorously, it has negative scewness
scewness.Tstar <- sum((Tstar- mean(Tstar))^3/length(Tstar)); scewness.Tstar
# This left scew of the bootstrap is a result of the right scew of the price data.

Q.boot <- quantile(Tstar, c(.025, .975)); Q.boot # 2.5% and 97.5% quantiles of our
                                                 # bootstrapped T statistic, generated
                                                 # from scewed data.

# In our run, we got a lower bootstrap t-statistic of -2.24 and an upper 
# bootstrap t-statistic of 1.84 compared to +/- 1.975 in case where the data is 
# normally distributed (and therefore the parametric t distribution is accurate).
# (Note: 1.975 = qt(.975, df = 158).)
# This disparity results from the skewness of the data (which prevents the CLT
# from taking hold for the the number of observation that we have).
# Using our bootstrap t distribution, we obtain a CI of 

L.boot.price <- mean(price) - Q.boot[2] * sd(price) / sqrt(n)
L.boot.price
U.boot.price <- mean(price) - Q.boot[1] * sd(price) / sqrt(n)
U.boot.price

CI.boot <- c(L.boot.price, U.boot.price); CI.boot

# In our run, the bootstrap CI is [10589.34, 12491.02], compared to the CI 
# obtained by the classical method of [10525.05, 12366.41]. 

# We note the asymmetry of the bootstrap CI about xbar, compared to symmetry
# of the conventional CI about xbar
CI.formula - xbar # symmetric about xbar
CI.boot - xbar 
# The boostrap CI is not symmetric about xbar, which accounts for right scew of 
# car price. Because of the left scew in Tstar, the empirical Q_1 for Tstar is
# is absolutely further from the mean than is the empirical Q_2.
# As it turns out, this is a more accurate confidence interval for our data.

##############################################
# Comparison of price depending on fuel type #
##############################################

# Permutation test for mu1 - mu2
# mu1 = mean of price of cars with fueltype = gas
# mu2 = mean of price of cars with fueltype = diesel

price.gas <- subset(auto, select = price, subset = fueltype == "gas", drop = T) 
price.diesel <- subset(auto, select = price, subset = fueltype == "diesel", drop = T)
price <- subset(auto, select = price, drop=T)

# Observed difference of sample mean prices
observed <- mean(price.gas) - mean(price.diesel) 
observed

# We conduct a p-test to determine whether the observed difference could reasonably
# be attributed to chance (the null hypothesis).
N <- 10^4-1 
result <- numeric(N) 
for (i in 1:N) 
{   
  index <- sample(length(price), size = length(price.gas), replace = FALSE)   
  result[i] <- mean(price[index]) - mean(price[-index]) 
}

hist(result, xlab = "mu1 - mu2", main = "Permutation distribution for Prices") 
abline(v = observed,lty = 2, col = "blue") 
# Seem fairly unlikely based on the data

# Note that this is a two-sided test
p.value <- 2*(sum(result <= observed) + 1)/(N + 1); p.value # p-value 
# The observed p-value indicates that the difference is significant at 
# the 1% level.


# Bootstrap percentile confidence interval for mu1 - mu2
N <- 10^4 
times.diff.mean <- numeric(N) 
n.gas <- length(price.gas); n.gas
n.diesel <- length(price.diesel); n.diesel
for (i in 1:N) 
{   
  gas.sample <- sample(price.gas, n.gas, replace = T)   
  diesel.sample <- sample(price.diesel, n.diesel, replace = T)   
  times.diff.mean[i] <- mean(gas.sample) - mean(diesel.sample) 
}

hist(times.diff.mean, main = "Bootstrap distribution of difference in means") 
abline(v = mean(price.gas) - mean(price.diesel), col = "blue", lty = 2)
qqnorm(times.diff.mean, col = rgb(0,0,1,.05), pch = 19) 
qqline(times.diff.mean)

CI.pctboot <- quantile(times.diff.mean, c(0.025, 0.975))
CI.pctboot
# In our run of the code, we come to the conclusion that, at the 95% confidence
# level, the mean price of a deisel car is between 995.13 and 9689.94 higher than
# the mean price for gas cars.

#Bootstrap t-confidence interval for mu1 - mu2
thetahat <- mean(price.gas) - mean(price.diesel) 
nx <- length(price.gas) 
ny <- length(price.diesel)
SE <- sqrt(var(price.gas)/nx + var(price.diesel)/ny) 
N <- 10^4 
Tstar <- numeric(N) 

for (i in 1 :N) 
{   
  bootx <- sample(price.gas, nx, replace = T)   
  booty <- sample(price.diesel, ny, replace = T)   
  Tstar[i] <- (mean(bootx) - mean(booty) - thetahat)/sqrt(var(bootx)/nx + var(booty)/ny) 
  
}

CI.tboot <- thetahat - quantile(Tstar, c(.975, .025)) * SE; CI.tboot

# Interestingly, the t boostrap yields a confidence interval that significantly
# differs from the one we found via the percent bootstrap method.

CI.tboot - CI.pctboot
(CI.tboot - CI.pctboot) / CI.pctboot

# In both cases, however, the result indicates that the mean of diesel cars is, 
# with high certainty, greater than the mean of gas cars.
# Chihara and Hesterberg indicate (without further elaboration) that the t
# bootstrap method is more accurate.

# Again, note that the t bootstrap CI is asymmetric, while the t bootstrap CI
# is not.
CI.conventional <- t.test(price.gas, price.diesel)$conf
CI.conventional

CI.conventional - thetahat
CI.tboot - thetahat

# This serves reminds us of the importance of analyzing the distribution of our
# data when deciding on confidence interval methods.

#######################
# Logistic Regression #
#######################

# One would suspect that manufacturers are more apt to equip larger cars with
# more doors than they are smaller cars.
# Thus, we think it makes sense to model the probability of a car having four
# doors (sybolized by 1) instead of two (symbolized by 0), by the weight of the
# car.
# Therefore, we conduct a logistic regression on door number against car weight.

doors.vect <- doors == "four"
doors.vect <- as.numeric(doors.vect)
table(doors.vect) # 1 represents "four doors", 0 represents "2 doors"

# Visually, it seems our suspicion is correct; 
# heavier cars are more likely to have 4 doors.
plot(doors.vect ~ weight, col = rgb(1,0,0,.25), pch = 19,
     xlab = "weight", ylab = "probability of four doors", 
     main = "Logistic regression")

# Running the logistic regression:
fit.lm <- glm(doors.vect ~ weight, family = binomial)
coef(fit.lm) # positive coefficient
summary(fit.lm)

alpha <- coef(fit.lm)[1]; alpha
beta <- coef(fit.lm)[2]; beta

x <- seq(1400, 4200, length=1000)
y <- plogis(alpha + beta * x)

# A visual representation of the logistic regression.
lines(x, y)

# save these fore later
obs.x <- x; obs.y <- y

# Alternatively, we can make a more attractive plot with ggplots:
ggplot(auto, aes(x=weight, y=doors.vect)) + geom_point() + 
       stat_smooth(method="glm", method.args=list(family="binomial"), se=F)

# Bootstrapping the beta coefficient:

# But how confident are we that our regression is correct?
# It does, after all, depend on our 159 observations.
# Thus, we bootstrap the logistic distribution.

# We'll find a 95% CI for both the beta value. 
# We also define a function to generate the 95% bootstrap CI 
# for the probability that a car of a given weight has four doors.

obs.beta <- beta; obs.beta
n <- nrow(auto); n
N <- 10^3
alpha.boot <- numeric(N)
beta.boot <- numeric(N)

for (i in 1:N)
{
        index <- sample(1:n, replace = TRUE)
        fit.boot <- glm(doors.vect[index] ~ weight[index], family = binomial)
        alpha.boot[i] <- coef(fit.boot)[1]
        beta.boot[i] <- coef(fit.boot)[2]
}
hist(beta.boot, breaks = "FD")
abline(v = obs.beta, col = "red")

# The 95% bootstrap confidence interval for beta
CI.betaboot <- quantile(beta.boot, c(.025, .975)); CI.betaboot
abline(v = CI.betaboot, col = "gray")

# Define the function for the 95% bootstrap CI for the probability preditions
# that a car of an arbitrary weight has four doors.
my.prob_predition <- function(car.weight)
{
        pPred.boot <- numeric(N)
        pPred.boot <- plogis(alpha.boot + car.weight * beta.boot)        
        CI.predictboot <- quantile(pPred.boot, c(.025, .975))
        return(CI.predictboot)
}
# Examples:
my.prob_predition(2000)
my.prob_predition(3000)
my.prob_predition(4000)

# Visually:
plot(x = 0, y = 0, xlim = c(1400, 4200), ylim = c(0,1), type = "n",
     xlab = "weight", ylab = "probability of four doors", 
     main = "Log regression bootstrap")

for (i in 1:1000)
{
        x.new <- seq(1400, 4200, length=1000)        
        y.new <- plogis(alpha.boot[i] + beta.boot[i] * x)
        lines(y.new ~ x.new, col = rgb(1,0,0,.05))
}
lines(obs.y ~ obs.x, col = "black") # regression curve from observed data

# Example: weight 3000
# Plotting 95% bootstrap confidence interval

CI.3000 <- my.prob_predition(3000)

abline(h = CI.3000, col = "blue", lty = 2) #95% bootstrap CI
abline(v = 3000, col = "blue", lty = 2)







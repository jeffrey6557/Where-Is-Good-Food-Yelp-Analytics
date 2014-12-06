# 1) model the ratings 
# 2) relationship between reviews and ratings
# 8) correlation between age and # restaurants/# reviews
# 9) correlation between Chinese population and # Chinese restaurants and # reviews on Chinese restaurants 


library("scales")
library("e1071")
library("ggplot2")
# 1) modeling the ratings 
# Yelp's business proposition is the word of mouth: user-generated reviews and ratings.
# How does rating work? You give a rating (1~5) when you review a restaurant on Yelp.
# Then Yelp presumably collects all the ratings for each restuarant and generate a 1-5 with 
# 0.5 increments. We have no knowledge about how actually Yelp computed these
# displayed ratings, but can we model the ratings?

rm(list = ls())
Yelp <- read.csv("DATA_JLH_CL.csv")

# set up the variables
rating<-Yelp$rating
review<-Yelp$num_reviews

# look at a boxplot
boxplot(rating,main="Boxplot of sample restaurant ratings in Boston",
        ylab="Ratings (1-5)",col="yellow") # there are no ratings below 3 ?! Are ALL restaurants that good?
# look at a histogram of ratings - BONUS 6 (new plots)
hist.rating<-qplot(rating, geom = "blank") + 
  geom_histogram(aes(y = ..density..),breaks=seq(2.75,5.25,0.5),
                 , alpha = 0.4,colour="black",fill="white") + 
  stat_function(fun=dnorm, args=list(mean=mean(rating), sd=sd(rating)),
                colour="blue") +
  ggtitle("Distribution of Restaurant Ratings on Yelp") + 
  xlab("Restaurant Ratings on Yelp") + 
  ylab("Density") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))
hist.rating
hist.rating+geom_vline(xintercept =quantile(rating,c(0.025,0.975)) , color="red", label="zero")
# The distribution of ratings is so centered on the 3.5 to 4.5 and looks a bit normal. 
# Why so? 
# We suspect it is the CLT at work:
# for large enough samples, the distribution of the sum or mean is approxiamtely normal.
# If we assume ratings are rounded averages of all the ratings people give a restuarant, 
# then the more reviews/ratings (larger sample size), 
# the more normal the displayed rating scores (sample means) will be.

# Although we don't know the exact algorithm for computing the ratings, 
# we can verify our hypothesis by segmenting our sample by the number of reviews (n)

# We have three ways to measure how normal the ratings get when the sample size increases,
# or when we get rid of more smaller samples

# Approach 1: skewness and kurtosis study - BONUS 13
# We look at the skewness and kurtosis for different sample sizes

# define our own functions to automate the study - BONUS 11
# input: n - split sample into above and below n reviews
# output: skewness and kurtosis for each segmented sample
CLT<-function(n) {
  index<-which(review<n)
  rating0<-rating[index]
  rating1<-rating[-index]
  clt<-c(skewness(rating0),skewness(rating1),kurtosis(rating0),kurtosis(rating1))
  clt
}
summary(review) # the maximum review is 2132, the minimum 7 so we can choose n from the range
level<-seq(8,2132,1) # specify the level (n) by which we split the sample
clt<-data.frame(matrix(,ncol=0,nrow=0))
for (n in level) {
  clt<-rbind(clt,c(n,CLT(n)))
  colnames(clt)<-c("n","skewness below n","skewness above n",
                   "kurtosis below n","kurtosis above n")
}
head(clt) 

# skewness study
#ggplot graph - BONUS 6
ggplot(clt, aes(x=level, y=clt[,2])) +
  geom_point(shape=1)+ 
  geom_hline(yintercept = 0, color="yellow", label="zero") +
  ggtitle("Skewness values for ratings with below n reviews") + 
  xlab("n") + 
  ylab("Skewness") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))

# R built-in graphs
plot(level,clt[,2],type ="b",col = rgb(1/8,1/4,1,1/2),
     xlab="n, the number of reviews for the ratings",
     ylab="Skewness",main="Skewness for ratings with below n reviews")
abline(h=0,col='red') # this is very convincing, skewness approaches 0, as n increases 
plot(level,clt[,3],type ="b",col = rgb(3/4,0,1,1/2), 
     xlab="n, the number of reviews for the ratings",
     ylab="Skewness",main="Skewness for ratings with above n reviews")
abline(h=0,col='red') # also approaches 0, as we get rid of the smaller samples

length(which(abs(clt[,2])<=0.5))/2132 # percentage of skewness within -0.5 and 0.5
length(which(abs(clt[,3])<=0.5))/2132 # percentage of skewness within -0.5 and 0.5
# Most of the skewness values are within -0.5 and 0.5, normality is quite a good fit.

#kurtosis study
plot(level,clt[,4],type ="b",add=TRUE,col = rgb(0,0,1/4,1/4),
     xlab="n, the number of reviews for the ratings",
     ylab="Excess Kurtosis",main="Excess Kurtosis for ratings with below n reviews")
# excess kurtosis approaches 0, as n increases
abline(h=0,col='yellow')
# close to zero, so normality is a good fit

# limitations of normality
plot(level,clt[,5],type ="b",pch = 16, col = rgb(1,0,1,1/2),
     xlab="n, the number of reviews for the ratings",
     ylab="Excess Kurtosis",main="Excess Kurtosis for ratings with above n reviews")
abline(h=0,col='red')
# as we get rid of the smaller samples, kurtosis is close to 0 when n< 500 but
# then strays away far from 0 when n > 500 
length(rating[which(review>500)]) # the observations with above 500 reviews
length(rating[which(review>1000)]) # the observations with above 1000 reviews
# since the observations in our data get fewer and fewer until exhausted.
# Therefore, 500 is roughly the limit above which the data are too scarce to be telling



# Approach 2: Central Limit Theorem (CLT) approximation 
# Since there are more ratings in the middle values, we wonder: 
# What exactly is the probability that a rating score is 3.5 to 4.5? 

# we define a function to automate our study
# input: n - divide the samples into ones with below n reviews and ones with above n reviews
# output:  for each divided sample,
# the exact proportion of ratings from 3.5 to 4.5;
# CLT approximation with continuity correction; 
# and error - how much in percentage CLT approximation misses the exact answer
CLTprob<-function(n) {
  index<-which(review<n)
  rating0<-rating[index]
  rating1<-rating[-index]
  exact0<-length(which((rating0<=4.5)&(rating0>=3.5)))/length(rating0)
  prob0<-pnorm(4.75, mean(rating0),sd(rating0))-pnorm(3.25, mean(rating0),sd(rating0)) 
  error0<-abs(prob0-exact0)/exact0
  exact1<-length(which((rating1<=4.5)&(rating1>=3.5)))/length(rating1)
  prob1<-pnorm(4.75, mean(rating1),sd(rating1))-pnorm(3.25, mean(rating1),sd(rating1)) 
  error1<-abs(prob1-exact1)/exact1
  cltprob<-cbind(c(exact0,exact1),c(prob0,prob1),c(error0,error1)*100)
  rownames(cltprob)<-c("Above n","Below n")
  colnames(cltprob)<-c("Exact probability","CLT approximation","Error in %")
  cltprob
}

# the probability of ratings from 3.5 to 4.5
CLTprob(8)[2,] # the original sample
CLTprob(30)
CLTprob(70)
CLTprob(100)   
CLTprob(200)
CLTprob(500)
CLTprob(1000)  

plot(1,1, type ="n", xlim = c(5,2100), ylim = c(0,10),
  ylab="Errors in %", main="CLT Approximation Errors in %" ,
  xlab="n, the number of reviews for the restaurants")
legend(1500,6.5, lty=c(1,1),c("Below n","Above n"), lwd=c(2.5,2.5),col=c("black","red")) 
for (i in 10:2000) {
  points(i,CLTprob(i)[1,3])
  points(i,CLTprob(i)[2,3], col ="red")
} 
# We observe from these tables that as the sample sizes increases,
# the errors get smaller, CLT approximations get better;
# if we get rid of the small samples until n=500, the CLT approximations also get better
# again, above 500 reviews the data get so few and the errors stray away.



# Approach 3: Graphical approach - BONUS 12 (Watch the rainbow curves!)
# We will fit a normal curve to different sample sizes from 30 to 500 reviews
rating30<-rating[which(review<30)]
rating70<-rating[which(review<70)]
rating100<-rating[which(review<100)]
rating200<-rating[which(review<200)]
rating500<-rating[which(review<500)]

# we observe the shift of distribution from left-skewed to right-skewed
hist(rating30, breaks=seq(2.75,5.25,0.5),xlim=c(2.5,5.5),freq=FALSE,main="Ratings with below 30 reviews") 
curve(dnorm(x,mean(rating30),sd(rating30)),col="red",add=TRUE) 
hist(rating70, breaks=seq(2.75,5.25,0.5),xlim=c(2.5,5.5),freq=FALSE,main="Ratings with below 70 reviews") 
curve(dnorm(x,mean(rating70),sd(rating70)),col="red",add=TRUE) 
hist(rating100, breaks=seq(2.75,5.25,0.5),xlim=c(2.5,5.5),freq=FALSE,main="Ratings with below 100 reviews") 
curve(dnorm(x,mean(rating100),sd(rating100)),col="orange",add=TRUE) 
hist(rating200, breaks=seq(2.75,5.25,0.5),xlim=c(2.5,5.5),freq=FALSE,main="Ratings with below 200 reviews") 
curve(dnorm(x,mean(rating200),sd(rating200)),col="yellow",add=TRUE) 
hist(rating500, breaks=seq(2.75,5.25,0.5),xlim=c(2.5,5.5),freq=FALSE,main="Ratings with below 500 reviews") 
curve(dnorm(x,mean(rating500),sd(rating500)),col="blue",add=TRUE) 
hist(rating, breaks=seq(2.75,5.25,0.5),xlim=c(2.5,5.5),freq=FALSE,main="All sample ratings") # the whole sample 
curve(dnorm(x,mean(rating),sd(rating)),col="violet",add=TRUE) 
# the normal curve is not a bad fit for all these levels

# if we overlay these curves together, notice the shift of the rainbow curves
hist(rating, breaks=seq(2.75,5.25,0.5), xlim=c(2.5,5.5),freq=FALSE) # the whole sample
curve(dnorm(x,mean(rating30),sd(rating30)),col="red",add=TRUE) # sample size < 30
curve(dnorm(x,mean(rating70),sd(rating70)),col="orange",add=TRUE) 
curve(dnorm(x,mean(rating100),sd(rating100)),col="yellow",add=TRUE) 
curve(dnorm(x,mean(rating200),sd(rating200)),col="green",add=TRUE) 
curve(dnorm(x,mean(rating500),sd(rating500)),col="blue",add=TRUE) 
curve(dnorm(x,mean(rating),sd(rating)),col="violet",add=TRUE) # fits the whole sample
# as the sample size increases, the curves shift toward the left! 
# That means, as the number of reviews increase, the ratings tend to decrease!
# Who would have guessed this?
# To dig into the truth, we will look at the correlation between review and ratings later. 


# With the 3 approaches to model the ratings, we verified our CLT hypothesis: 
# With enough reviews, a normal distribution could have generated the ratings in our sample!
# Who would have thought that!
# We have found a relationship that might have not been statistically significant!
# BONUS 9

# REQUIREMENT 2 - Student t confidence interval:
# What is the average rating of Bostonian restaurants?
# Now that we assume each rating is a sample mean of the ratings for each restaurant, 
# and show that they are quite normally distributed; with a large sample of these means,
# we naturally want a student t confidence interval for the whole population in Boston.
student<-(rating-mean(rating))/(sd(rating)/sqrt(length(rating)))
hist(student,breaks=seq(-150,150,20),freq=FALSE,main="A t distribution?",xlab="rescaled student variable")
curve(dt(x,n-1),col='red',add=TRUE) # does not fit at all
# We failed! Why?
# This is because our ratings are discrete values, and are not strictly normal variables,
# so we are not justified to use student t confidence interval 
# but for the sake of comparison later, we can bend the theory: 
classic.t<-t.test(rating,conf.level=.99);classic.t 
# a very small interval with so many degrees of freedom!
# we are 99% confident that the true mean ratings is practically 4. Quite high a number!

# with a large sample, the best is to use a boostrap t confidence interval
xbar <- mean(rating); xbar  #sample mean
S <- sd(rating); S          #sample standard deviation
n <- length(rating);n
SE <- S/(sqrt(n)) ; SE       #sample standard error

#Check our methodology with a single bootstrap resample
x <-sample(rating, size = n, replace = TRUE) #resample
Tstar<-(mean(x) - xbar)/(sd(x)/sqrt(n)); Tstar #a t statistic
#Now we will estimate the distribution of the t statistic

N = 10^4; Tstar = numeric(N) #vector of t statistics
means = numeric(N); StdErrs = numeric(N)
for (i in 1:N) {
  x <-sample(rating, size = n, replace = TRUE)
  Tstar[i] <-(mean(x) - xbar)/(sd(x)/sqrt(n))
  means[i] = mean(x); StdErrs[i] = sd(x)/sqrt(n)
}
#The bootstrap t statistic is approximately normal except for the tails
qqnorm(Tstar)
qqline(Tstar)

# BONUS 6 - new plots
qplot(Tstar, geom = "blank") + 
  geom_histogram(aes(y = ..density..)
                 , alpha = 0.4,colour="black",fill="white") + 
  stat_function(fun=dt, args=n-1,
                colour="blue") +
  ggtitle("Boostrap Distribution of Ratings") + 
  xlab("Boostrap statistics") + 
  ylab("Density") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))
#A Student t curve is quite good a match except for the center
#To get a confidence interval, use the bootstrap quantiles along with the sample mean and standard deviation
q<-quantile(Tstar, c(.005, .995),names=FALSE)
L <- xbar - q[2]*SE; U <- xbar - q[1]*SE; L; U
#Here, for comparison, is the bootstrap percentile and the classical t confidence interval
quantile(means, c(.005, .995),names=FALSE)
classic.t$conf.int
# we can display these 3 intervals
hist(rating, breaks=seq(2.75,5.25,0.5), xlim=c(2.5,5.5),freq=FALSE,
     main="Distribution of sample restaurant ratings in Boston") # the whole sample
abline(v=c(L,U),col='black',lty=3)
abline(v=quantile(means, c(.005, .995),names=FALSE),col='red',lty=4)
abline(v=classic.t$conf.int,col='blue',lty=5)
# these 3 intervals are so close to each other that we may practically use any one
# but theoretically, we are only allowed to use the boostrap t statistics.
# so we can claim with 99% confidence that the true mean rating of Bostonian restaurants is 4!
# Good job, Boston! 
# Well, now we also have a sense of how inflated the ratings can be.  

# BONUS 14 - Chi-square distribution  
# As I recall from W7 section problem 2, we rescaled a discrete random variable X~binom
# and produced chi-square statistics out of it with the student t approach; 
# Knowing that our ratings are roughly normal but discrete, 
# are we able to replicate Chi-square distributions?

mu<-mean(rating);mu # unbiased estimator of the mean of the population
sigma<-sqrt((n-1)/n*var(rating)); sigma # unbiased estimator of signma of the population
x<-(sample(rating, 10,replace=T)-mu)/sigma; x # we make it a standard normal variable

# let's simulate 10000 times
# large sample is a data luxury: we can choose a small sample this time
df<-50
N<-10000; means <-numeric(N); vars<-numeric(N); sumsq <- numeric(N) 
for (i in 1:N) {
  x<-(sample(rating,df,replace=FALSE)-mu)/sigma
  means[i] <- mean(x) 
  vars[i]<- var(x)     
  sumsq[i]<-sum(x^2)
}

hist(means^2*df,freq=FALSE,main="Chi-square Distribution") # this quantity is called W in proof 1 of week 7
curve(dchisq(x,1),col='green',add=TRUE) # Big surpise! it fits well!

hist(sumsq,freq=FALSE,main="Chi-square Distribution")  # this quantity is called U in proof 1 of week 7 
curve(dchisq(x,df),col='red',add=TRUE) # Wow, it fits nicely

hist(vars*(df-1),freq=FALSE,main="Chi-square Distribution") # this quantity is called V in proof 1 of week 7 
curve(dchisq(x,df-1),col='violet',add=TRUE) # again, fits well

cor(means^2,vars) # highly uncorrelated 

hist(means/sqrt(vars/df),freq=FALSE,main="Student t Distribution") # the quantity T=(X.bar-mu)/(S/sqrt(n))
# we painstakingly proved in class!
curve(dt(x,df-1),col="green",add=TRUE) # the fit is ok
# This surprising connection with Chi-square distributions might qualify for BONUS 9


# 2) BONUS 16 and REQUIREMENT 1: correlation between reviews and ratings
# As we observe earlier, the normal curves shift towards the left as reviews increase.
# Do more reviews tend to generate lower ratings?
plot(review,rating,xlab="Number of Reviews",ylab='Ratings',main="Scatter plot of ratings against reviews")
cor(review,rating) # they seem to be negatively correlated
ReviewRating.lm <- lm(rating~num_reviews, data =Yelp);ReviewRating.lm
intercept<-coef(ReviewRating.lm)[1]
slope<-coef(ReviewRating.lm)[2] 
abline(intercept,slope,col='blue') # downward slope
# Fit a non-linear line
lines(smooth.spline(rating~review), col = "red")
legend(1000,5, lty=c(1,1),c("linear regression","non-linear"), lwd=c(2.5,2.5),col=c("blue","red")) 

PredictRating <- intercept + slope * review
points(review,PredictRating, col = "green",add = TRUE, pch = 20)  #these lie on the regression line
ResidRating <- rating - PredictRating
plot(review,ResidRating,main="Residual plot of ratings against reviews")    # this is clearly not random 
abline(h=0, col = "red")
summary(ReviewRating.lm)$r.squared # BONUS 13 - R^2
# only 1% data is explained by our model;our model clearly does not work 

#Now do 5000 permutations to see whether the actual beta could arise by chance.
N <- 4999; n <- nrow(Yelp)
cor.perm <- numeric(N); beta.perm <- numeric(N)
for (i in 1:N) {
  index <- sample(n, replace = FALSE) 
  rating.perm <- rating[index]
  beta.perm[i] <-coef(lm(rating.perm~ Yelp$num_reviews))[2]  #beta
  cor.perm[i] <-cor(Yelp$num_reviews, rating.perm) #correlation
}
hist(beta.perm, xlim = c(-0.0005,0.0005))
abline(v = slope, col = "red")    
# although off the chart, our beta is so close to zero that our linear model fails 

hist(cor.perm, xlim = c(-0.2,0.2))
abline(v = cor(review,rating), col = "red")    # off the charts
# although significant, the correlation between ratings and reviews is quite weak!






# With census data about the population relating to the Yelp restaurant data we have,
# we wonder about the correlation between age and the number of reviews:
# Does the population of 20-to-40-year-old correlate with 
# the number of restaurants reviewed on Yelp?

# Create by zip code data frame of # restaurants, # of people
byZip <- data.frame(table(Yelp$zip));head(byZip)
add_column <- numeric(nrow(byZip))
byZip <- data.frame(byZip, add_column);head(byZip)
colnames(byZip) <- c("zip", "num_rest", "age")
for(i in 1:length(byZip$zip))
{
  # each zip code has the same population, whichever row that zip code appears in
  #  so just take the value in the first record returned by this which
  byZip$age[i] = Yelp$perct_20.40[min(which(Yelp$zip == byZip$zip[i]))]
}

# make a scatter plot
plot(byZip$age, byZip$num_rest,col=rgb(1/2,0,1/2,1),xlab="% population of 20-to-40-year-old by zipcode",
     ylab="Number of restaurants by zipcode",main="Correlation between 20s-to-40s and # of restaurants") 
# looks roughly like positive correlation

# check correlation of these two independent variables - BONUS POINT #16
our_corr <- cor(byZip$age, byZip$num_rest); our_corr
# .56 indicates positive correlation

# find a regression line
rest_by_age <- lm(byZip$num_rest~byZip$age, data=byZip); rest_by_age
intercept<-coef(rest_by_age)[1]
slope<-coef(rest_by_age)[2] 
# this tells us that, for every additional percentage of 20-to-40-year-old living within a zipcode,
#   we will have about roughly 1 more restaurant for that zipcode.
abline(rest_by_age, col="blue")
legend(20,57, lty=c(1,1),c("linear regression line","non-linear line"), lwd=c(2.5,2.5),col=c("blue","red")) 

# let's just check that something nonlinear isn't going on:
lines(smooth.spline(byZip$num_rest~byZip$age), col="blue")
# wow, it overlapps nicely with our linear model!
summary(rest_by_age)$r.squared # R^2 is 30%
# 30% of the data is explained by our linear model 

# To use the t distribution, let's first check if we have independent, normally distributed residuals
predictions <- intercept + slope * byZip$age
residuals <- byZip$num_rest - predictions
plot(byZip$age, residuals)
abline(h=0, col="red")
# it looks pretty randomly distributed except a few outliers in the 40-50% range

# we can also bootstrap to create confidence interval.  (for BONUS 8)
N <- 10^4
cor.boot <- numeric(N)
slope.boot <- numeric(N)
n <- nrow(byZip)

get_intercept <- function(X, Y)
{
  mean(Y) - slope*mean(X)
}

get_slope <- function(X, Y)
{
  sum(( X - mean(X)) * ( Y - mean(Y)) / sum((X - mean(X))^2))
}

for(i in 1:N)
{
  index <- sample(1:n, n, replace = TRUE)
  data.boot <- byZip[index, ]
  cor.boot[i] <- cor(data.boot$num_rest, data.boot$age)
  slope.boot[i] <- get_slope(data.boot$age, data.boot$num_rest)
}

# REQUIREMENT 3 
# Display a 95% Confidence Interval for the correlation
# between the population % and number of restaurants
hist(cor.boot,freq=FALSE,main="Boostrap distribution of correlation coefficients")
abline(v=quantile(cor.boot, c(.025, .975)), col="blue")
abline(v=our_corr, col="red")
quantile(cor.boot, c(.025, .975))

# Find a 95% Confidence Interval for the slope of our model
hist(slope.boot,main="Boostrap distribution of slope of our linear model")
abline(v=quantile(slope.boot, c(.025, .975)), col="blue")
abline(v=slope, col="red")
quantile(slope.boot, c(.025, .975))

# I love Chinese cuisine but I struggled to locate a Chinese restaurant nearby,
# so a question arises naturally: Am I living not close to where my food lives?
# Statistically, this question translates into the following:
# Is there any correlation between Asian population and # Chinese restaurants 

asian<-Yelp$perct_Asian
category<-Yelp$category
index<-which(Yelp$category=="chinese")
chinese<-Yelp[index,] # select all the Chinese restaurants 
# Create by zip code data frame of # restaurants, % of Asian
byZip <- data.frame(table(chinese$zip));head(byZip)
add_column <- numeric(nrow(byZip))
byZip <- data.frame(byZip, add_column);head(byZip)
colnames(byZip) <- c("zip", "num_rest", "asian")
for (i in 1:length(byZip$zip))
{
  # each zip code has the same Asian population, whichever row that zip code appears in
  #  so just take the value in the first record returned by this which
  byZip$asian[i] <- Yelp$perct_Asian[min(which(Yelp$zip == byZip$zip[i]))]  
}

# make a scatter plot
plot(byZip$num_rest,byZip$asian,main="Correlation: Asian population vs Chinese restaurants",
     ylab="Asian population % by zipcode",xlab="# of Chinese restuarants by zipcode") #
cor(byZip$num_rest,byZip$asian) # very strong postive correlaton!

# find a regression line
rest_by_asian <-lm(byZip$asian~byZip$num_rest); rest_by_asian
intercept<-coef(rest_by_asian)[1]
slope<-coef(rest_by_asian)[2]
# this tells us that, for every additional percentage of asian living in a zipcode,
#   we will have about roughly 1.5 more Chinese restaurant for that zipcode.
abline(rest_by_asian, col="blue")
legend(10,17, lty=c(1,1),c("linear regression line","non-linear line"), lwd=c(2.5,2.5),col=c("blue","red")) 
# let's just check that something nonlinear isn't going on:
lines(smooth.spline(byZip$asian~byZip$num_rest), col="red")
# Wow, it overlapps perfectly with our linear model!
summary(rest_by_asian) # R^2 is 72%! 72% of the data is explained by our model
# this finding fits with our intuition, and clearly I have not been living in Asian community


# Logistic regression - BONUS 4
# Local businesses can choose to "claim" their business on Yelp, maintaining their Yelp
# profiles and hoping to increase their popularity. But we challenge this assumption:
# Does claiming your business on Yelp correlate with publicity?

Yelp$claimed<-as.logical(toupper(Yelp$claimed))
claimed<-Yelp$claimed
fit <- glm(claimed ~ num_reviews, data = Yelp, family = binomial)
coef(fit)    # the coefficients as a vector

# Calculation: my logistic equation is ln(p/1-p)=0.713716164+0.001051541*x

# we can plot the probability of whether a business is claimed against number of reviews
x <- seq(7, 2132, length=500) # vector spanning the #review range
# compute predicted probabilities
y1 <- exp(coef(fit)[1]+coef(fit)[2]*x) / (1+exp(coef(fit)[1]+coef(fit)[2]*x)) 
y2 <- plogis(coef(fit)[1] + coef(fit)[2] * x)
plot(Yelp$num_reviews,Yelp$claimed, xlab='number of reviews',main="Correlation: claiming your business vs publicity",
     ylab = "Probability of claimed your business")
lines(x, y2,col='blue') # our logistic regression curve
legend(1000,0.5, lty=1,"Built-in glm function", lwd=2.5,col="blue") 

# BONUS 15 - Maximum likelihood estimation 
# MLE also gives us the parameters
library(stats4)
x<-Yelp$num_reviews;y<-Yelp$claimed
MLL<- function(alpha, beta) -sum( log( exp(alpha+beta*x)/(1+exp(alpha+beta*x)) )*y+ 
                                    log(1/(1+exp(alpha+beta*x)))*(1-y) )
results<-mle(MLL,start = list(alpha = -0.1, beta = -0.02))
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "red", add=TRUE)
legend(1000,0.5, lty=c(1,1),c("Built-in glm function","MLE estimation"), lwd=c(2.5,2.5),col=c("blue","red")) 
# almost overlays the curve as we got from the glm function 


# BONUS 8 + Technical Display 3: We can make boostrap confidence intervals too
N <- 10^3
n <- length(claimed)                   # number of observations
alpha.boot <- numeric(N)
beta.boot <- numeric(N)
for (i in 1:N)
{
  index <- sample(n, replace = TRUE)
  claimed.boot <- Yelp[index, ]     # resampled data
  fit.boot <- glm(claimed ~ num_reviews, data = claimed.boot,
                  family = binomial)
  alpha.boot[i] <- coef(fit.boot)[1]    # new intercept
  beta.boot[i] <- coef(fit.boot)[2]     # new slope
}
quantile(beta.boot, c(.025, .975))      # 95% percentile intervals for the slope

#Now we can look at the distribution of the resampled results
hist(beta.boot,,main="Boostrap distribution of beta",freq=FALSE)  
abline(v=quantile(beta.boot, c(.025, .975)),col="green") #95% confidence interval for beta
abline(v=coef(fit)[2],col="blue") # the observation
quantile( beta.boot, c(.025, .975)) # the bootstrap confidence interval for beta 

hist(alpha.boot,freq=FALSE,main="Boostrap distribution of alpha")  
abline(v=quantile( alpha.boot, c(.025, .975)),col="red") #95% confidence interval for alpha
abline(v=coef(fit)[1],col="blue") # the observation
quantile( alpha.boot, c(.025, .975)) # the bootstrap confidence interval for alpha






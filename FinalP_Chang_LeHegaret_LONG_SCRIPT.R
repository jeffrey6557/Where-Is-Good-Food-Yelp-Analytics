# MATH E-156
# Final Project
# submitted by Chang Liu and Jennifer Le Hegaret

# LONG SCRIPT

# Setup

rm(list = ls())
Yelp <- read.csv("DATA_JLH_CL.csv")

#install.packages("scales")
library("scales")
#install.packages("e1071")
library("e1071")
#install.packages("ggplot2")
library("ggplot2")
#get our own function library - BONUS POINT 11
source("FinalP_Chang_LeHegaret_utilities.R")



# Fix leading zeros in zip code
Yelp$zip <- paste("0", Yelp$zip, sep="")
# Fix town of "Bostonn"
Yelp[which(Yelp$town=="Bostonn"), ]$town <- "Boston"
Yelp$town <- droplevels(Yelp$town)



# TOPIC ONE:   modeling the ratings 

# Yelp's business proposition is the word of mouth: user-generated reviews and ratings.
# How does rating work? You give a rating (1~5) when you review a restaurant on Yelp.
# Then Yelp presumably collects all the ratings for each restuarant and generate a 1-5 star with 
# 0.5 increments. We have no knowledge about how actually Yelp computed these
# displayed ratings, but can we model the ratings?

# set up the variables
rating<-Yelp$rating
review<-Yelp$num_reviews


# A basic box plot
qplot(y=rating)+geom_boxplot(fill="yellow",alpha=0.4)+
  ggtitle("Boxplot of sample restaurant ratings in Boston") + 
  ylab("Restaurant Ratings on Yelp (1-5 stars)" ) +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))
# there are no ratings less than 3 ?! Are ALL restaurants that good?
# look at a histogram of ratings - BONUS 6 (new plots)


hist.rating<-qplot(rating, geom = "blank") + 
  geom_histogram(aes(y = ..density..),breaks=seq(2.75,5.25,0.5),
                 , alpha = 0.4,colour="black",fill="white") + 
  ggtitle("Distribution of Restaurant Ratings on Yelp") + 
  xlab("Restaurant Ratings on Yelp") + 
  ylab("Density") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2))
hist.rating+ stat_function(fun=dnorm, args=list(mean=mean(rating), sd=sd(rating)),
                           colour="#0072B2") +
  geom_vline(xintercept =quantile(rating,c(0.025,0.975)) , color="red", label="zero")

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

# Approach 1: skewness and kurtosis study - BONUS POINT 13
# We look at the skewness and kurtosis for different sample sizes

# define our own functions to automate the study - BONUS POINT 11
# CLT(n)
# input: n - split sample into ratings with more than n and less than n reviews
# output: skewness and kurtosis for each segmented sample

summary(review) # the maximum review is 2132, the minimum 7 so we can choose n from the range
level<-seq(8,2132,1) # specify the level (n) by which we split the sample
clt<-data.frame(matrix(,ncol=0,nrow=0))
for (n in level) {
  clt<-rbind(clt,c(n,CLT(n)))
  colnames(clt)<-c("n","skewness less than n","skewness more than n",
                   "kurtosis less than n","kurtosis more than n")
}
head(clt)  # a table with skewness and kurtosis

# Skewness study
#ggplot graph - BONUS POINT 6
ggplot(clt, aes(x=level, y=clt[,2])) +
  geom_point(shape=1,col=rgb(1/2,0,1/4,1/4)) +
  geom_hline(yintercept = 0, color="yellow", label="zero") +
  ggtitle("Skewness for ratings with more than n reviews") + 
  xlab("n number of reviews") + 
  ylab("Skewness values") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2)) 
# this is very convincing, as n increases, the skewness approaches 0

ggplot(clt, aes(x=level, y=clt[,3])) +
  geom_point(shape=1,col=rgb(1/2,0,1/2,1/4)) +
  geom_hline(yintercept = 0, color="7", label="zero") +
  ggtitle("Skewness for ratings with less than n reviews") + 
  xlab("n number of reviews") + 
  ylab("Skewness values") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2)) 
# also approaches 0, as we get rid of the smaller samples

length(which(abs(clt[,2])<=0.5))/2132 # percentage of skewness within -0.5 and 0.5
length(which(abs(clt[,3])<=0.5))/2132 # percentage of skewness within -0.5 and 0.5
# Most of the skewness values are within -0.5 and 0.5, normality is quite a good fit.

#Kurtosis study
ggplot(clt, aes(x=level, y=clt[,4])) +
  geom_point(shape=1,col=rgb(0,0,1/4,1/4)) +
  geom_hline(yintercept = 0, color="yellow", label="zero") +
  ggtitle("Excess kurtosis for ratings with less than n reviews") + 
  xlab("n number of reviews") + 
  ylab("Excess kurtosis values") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2)) 

# excess kurtosis approaches 0, as n increases; so normality is a good fit

# limitations of normality
ggplot(clt, aes(x=level, y=clt[,5])) +
  geom_point(shape=1,col=rgb(1/4,1/2,1/5,1/2)) +
  geom_hline(yintercept = 0, color="yellow", label="zero") +
  ggtitle("Excess kurtosis for ratings with more than n reviews") + 
  xlab("n number of reviews") + 
  ylab("Excess kurtosis  values") +
  theme(plot.title = element_text(size=20, face="bold", vjust=2)) 
# as we get rid of the smaller samples, kurtosis is close to 0 when n< 500 but
# then strays away far from 0 when n > 500 
length(rating[which(review>500)]) # the observations with more than 500 reviews
length(rating[which(review>1000)]) # the observations with more than 1000 reviews
# since the observations in our data get fewer and fewer until exhausted.
# 500 is the limit beyond which the data are too scarce to be showing normality



# Approach 2: Central Limit Theorem (CLT) approximation 
# Since there are more ratings in the middle values, we wonder: 
# What exactly is the probability that a rating score is 3.5 to 4.5? 

# we define a function to automate our study
# input: n - divide the samples into ones with less than n reviews and ones with more than n reviews
# output:  for each divided sample,
# the exact proportion of ratings from 3.5 to 4.5;
# CLT approximation with continuity correction; 
# and error - how much in percentage CLT approximation misses the exact answer

# the probability of ratings from 3.5 to 4.5 for different levels 
CLTprob(8)[2,] # the original sample
CLTprob(30)
CLTprob(70)
CLTprob(100)   
CLTprob(200)
CLTprob(500)

plot(1,1, type ="n", xlim = c(5,2100), ylim = c(0,10),
  ylab="Errors in %", main="CLT Approximation Errors in %" ,
  xlab="n, the number of reviews for the restaurants")
legend(1300,6.5, lty=c(1,1),c("Less than n","More than n"), lwd=c(2.5,2.5),col=c(rgb(1/4,3/4,1/4,1/2),rgb(3/4,0,1/4,1/2))) 
abline(h=0,col="black")
for (i in 10:2000) {
  points(i,CLTprob(i)[1,3],col=rgb(1/4,3/4,1/4,1/2))
  points(i,CLTprob(i)[2,3], col =rgb(3/4,0,1/4,1/2))
} 

# We observe from these tables that as the sample sizes increases,
# the errors get smaller, CLT approximations get better;
# if we get rid of the small samples until n=500, the CLT approximations also get better
# again, above 500 reviews the data get so few and the errors stray away.



# Approach 3: Graphical approach - BONUS POINTS 6 and 12 (Watch the rainbow curves!)
# We will fit a normal curve to different sample sizes from 30 to 500 reviews

# Creating a function that generates a accustomed histogram and overlays a normal curve,
# we find that the normal curves are not a bad fit
CLT.hist(30)# ratings with less than 30 reviews
CLT.hist(70)# ratings with less than 70 reviews
CLT.hist(100)# ratings with less than 100 reviews
CLT.hist(200)# ratings with less than 200 reviews
CLT.hist(300) # ratings with less than 300 reviews
CLT.hist(500) # ratings with less than 500 reviews
CLT.hist(2132) # the entire sample
# Also note the shift of distribution from left-skewed to right-skewed:
# the "shoulders" of the histograms move from one way to the other, like breakdancing!


# If we overlay these normal curves, we discover something interesting going on:

hist.rating# set the historgram of the entire sample as the background

rainbow<-c("Red","Orange","Yellow",'Green','Blue','Violet','magenta')

g1<-CLT.curve(hist.rating,30,rainbow[1]);g1 # ratings with less than 30 reviews
g2<-CLT.curve(g1,70,rainbow[2]);g2 # ratings with less than 70 reviews
g3<-CLT.curve(g2,100,rainbow[3]);g3 # ratings with less than 100 reviews
g4<-CLT.curve(g3,200,rainbow[4]);g4 # ratings with less than 200 reviews
g5<-CLT.curve(g4,300,rainbow[5]);g5 # ratings with less than 300 reviews
g6<-CLT.curve(g5,500,rainbow[6]);g6 # ratings with less than 500 reviews
g<-CLT.curve(g5,2132,rainbow[7]);g # our entire sample
# As the sample size increases, the rainbow curves shift to the left! 
# That means, as the number of reviews increase, the ratings tend to decrease!
# To dig into the truth, we will look at the correlation between review and ratings later. 


# With the 3 approaches to model the ratings, we verified our CLT hypothesis: 
# With enough reviews, a normal distribution could have generated the ratings in our sample!
# If Yelp ignored the users' ratings, could they have used this distribution for the ratings?
# We have found a relationship that might have not been statistically significant
# but actually is!    BONUS POINT 9



# TOPIC TWO:  What is the average rating of Bostonian restaurants?

# REQUIRED ANALYSIS 2 - Student t confidence interval:
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
# we are 99% confident that the true mean ratings is practically 4. Quite large a number!

# with a large sample, the best is to use a bootstrap t confidence interval
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
qqnorm(Tstar, main="Normal quantile plot for bootstrap t statistics")
qqline(Tstar)

# BONUS 6 - new plots
qplot(Tstar, geom = "blank") + 
  geom_histogram(aes(y = ..density..),
                 alpha = 0.4,colour="black",fill="white") + 
  stat_function(fun=dt, args=n-1,
                colour="blue") +
  ggtitle("Bootstrap Distribution of Ratings") + 
  xlab("Bootstrap statistics") + 
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
# but theoretically, we are only allowed to use the bootstrap t statistics.
# so we can claim with 99% confidence that the true mean rating of Bostonian restaurants is 4!
# Good job, Boston! 
# Well, now we also have a sense of how inflated the ratings can be.  

# BONUS 14 - Chi-square distribution  
# As I recall from W7 section problem 2, we rescaled a discrete random variable X~binom
# and produced chi-square statistics out of it with the student t approach; 
# Knowing that our ratings are roughly normal but discrete, 
# we can theoretically replicate Chi-square distributions.

# we can treat our sample as a population 
mu<-mean(rating);mu # unbiased estimator of the mean of the population
sigma<-sqrt((n-1)/n*var(rating)); sigma # unbiased estimator of signma of the population
x<-(sample(rating, 10,replace=FALSE)-mu)/sigma; x # we make it a standard normal variable

# let's simulate 10000 times
# large sample is a luxury in real life: we can choose a small sample this time
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
# This surprising connection with Chi-square distributions might qualify for BONUS POINT 9


# 2) BONUS POINT 16, REQUIRED ANALYSIS 1, REQUIRED GRAPHICS 1: correlation between reviews and ratings
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
points(review,PredictRating, col = "green", pch = 20)  #these lie on the regression line
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


# Now that we've looked at the overall rating a Boston restaurant would hope to have,
#  how many reviews might a restaurant hope to have?

# TOPIC THREE:  BAYESIAN APPROACH - how many reviews do we expect per restaurant?
#  Adding this analysis to the other already-fulfilled requirements -> BONUS POINT 1

# Let's imagine we are polling restaurants town-by-town.
# Restaurants might like to know if they are getting more than or less than
#   the average amount of "buzz".
# So, what is the average number of reviews a restaurant should expect to get?
# And how do we want to detemine "average"?

# Let's look at data for our first town, Allston:
Allston <- Yelp$num_reviews[which(Yelp$town == "Allston")]
hist(Allston, breaks=25, probability=TRUE,
     xlab="Number of Reviews",
     ylab="Frequency",
     main="Allston Restaurants - Number of Reviews",
     col="blue")

# It looks roughly like some sort of gamma distribution
#install.packages("MASS")
library(MASS)
x <- Yelp$num_reviews
try <- fitdistr(x, "gamma"); try
curve(dgamma(x, try$estimate[1], try$estimate[2]), col="red", add=TRUE)
# But it is far from perfect, and the logic behind the gamma does not really make sense
# We are not wondering how much time it will take before something happens.
# Looking a list of the discrete probability models out there, I don't see
#  anything that fits our situation.  This is not about events over time.

# Also, what do we want to know?  We want to know if we have more or less
#   number of reviews than other restaurants.  But we want the MEDIAN number of reviews,
#   not an average!  If most restaurants have 10 reviews, and we have 12, we should be 
#   happy.  But, if one restaurant has 1000 reviews, we won't know that we should be happy,
#   because that outlier will drag up the mean.
hist(Allston, breaks=25, probability=TRUE,
     xlab="Number of Reviews",
     ylab="Density",
     main="Allston Restaurants - Number of Reviews",
     col="blue")
mean(Allston)
abline(v=mean(Allston), col="red", lwd=5)
median(Allston)
abline(v=median(Allston), col="yellow", lwd=5)


# So, let's figure out the expected median number of reviews!
# This is something that's not even possible with a frequentist approach.  :-)

# We will use a two-step process.  For a number of values, we will use
#   Bayesian analysis to determine the probability that a restaurant
#   will have at least that number of reviews.
# Once we have assembled all of those probabilities, we will determine
#   the average number of reviews a restaurant should expect
#   by identifying which minimum number of reviews has a probability
#   of around 50%.  Like that, half of the restaurants are expected to 
#   have less than that number, and half are expected to have at least
#   that many.


# Let's set these buckets of possible review counts:

minimums <- seq(0, 1000)
Num_AtLeast <- setup_bayesian_matrix(Yelp$town, minimums)
Num_NotAtLeast <- setup_bayesian_matrix(Yelp$town, minimums)
Alphas <- setup_bayesian_matrix(Yelp$town, minimums)
Betas <- setup_bayesian_matrix(Yelp$town, minimums)
E_Percent <- setup_bayesian_matrix(Yelp$town, minimums)
CI_lows <- setup_bayesian_matrix(Yelp$town, minimums)
CI_highs <- setup_bayesian_matrix(Yelp$town, minimums)

# we will use a noninformative prior of Beta(0,0) to start:
Alphas[1,] <- rep(0, ncol(Alphas))
Betas[1,] <- rep(0, ncol(Betas))

# Now let's add to this town-by-town in a Bayesian fashion

# 1st, set some constants
num_towns <- nrow(Alphas) # not quite the num. of towns, as we have the prior in there, but it'll work

for ( i in 2:num_towns)
{
    town <- row.names(Alphas)[i]; town
    print(paste("Incorporating", town, "data"))
    
    # First, get all the values of how many restaurants fall above and below each minimum 
    this_town <- Yelp$num_reviews[which(Yelp$town == town)]
    Num_AtLeast[i, ] <- get_counts_atleast(minimums, this_town); Num_AtLeast[i, ]
    Num_NotAtLeast[i, ] <- rep(length(this_town), length(minimums)) - Num_AtLeast[i, ]; Num_NotAtLeast[i, ]
    
    # The new alpha and betas incorporate those into the prior alphas and betas
    Alphas[i,] <- Alphas[i-1, ] + Num_AtLeast[i, ]; Alphas[i,]
    Betas[i,] <- Betas[i-1, ] + Num_NotAtLeast[i, ]; Betas[i,]
    
    # We calculate the new expected percentage for each minimum
    E_Percent[i,] <- Alphas[i,] / ( Alphas[i,] + Betas[i,]); E_Percent[i,]
    
    # And store the boundaries of a 95% CI for that estimate
    CI_lows[i,] <- qbeta(.025, Alphas[i,] , Betas[i,]); CI_lows[i,]
    CI_highs[i,] <- qbeta(.975, Alphas[i,] , Betas[i,]); CI_highs[i,]
    
}

# Now to graph our results
# Where is our best estimate of the population median?

# First, to check our Bayesian prior and posterior distributions,
#   let's take a guess and see how the likelihood that a restaurant has
#   at least 200 reviews evolved over time, as we collected the data town-by-town
num_rows <- nrow(Alphas); num_rows
test_min <- 200
caption <- "The non-informative prior"
curve(dbeta(x, Alphas[1, test_min], Betas[1, test_min]), ylim=c(0,30), xlim=c(0, 1),
      xlab=paste("Probability of", test_min,"Reviews"),
      ylab="Density",
      main="Bayesian Prior and Posterior Distributions\nstarting with a non-informative prior",
      col="blue")

for(i in 2:num_rows)
{
    polygon(c(.45, .45, 1, 1), c(30, 20 ,20, 30), border="white", col="white")
    curve(dbeta(x, Alphas[i, test_min], Betas[i, test_min]), add=TRUE, 
          main="test", col=i)
    text(.7, 25, paste("After incorporating", row.names(Alphas)[i])); caption
    if(interactive() && i != num_rows) 
    { 
        n<-readline(prompt=paste("Press enter to incorporate data from", row.names(Alphas)[i+1])) 
    }
}
estimated_prob <- E_Percent[num_rows, test_min]; estimated_prob
abline(v=estimated_prob)

# While all this analysis has fulfilled REQUIRED ANALYSIS #3 and REQUIRED GRAPHIC #2, 
# we see that this particular number of reviews is NOT the median number in the Metro Boston area,
# as the estimated percentage, with 95% probability, is not 50%, it is only 36.5%.
# Let's look at all of the possible number of reviews we tested to see
# which one has a probability of 50% of restaurants having at least that
# many reviews.

# Now, let's look at the overall picture of the estimated percents
#  across all possible "at least" values:
plot(E_Percent[nrow(E_Percent),],
     xlab="Minimum Number of Reviews",
     ylab="Probability",
     main="Probability of a Restaurant Having a Given Minimum Number of Reviews\nMetro Boston area")
abline(h=.5, col="blue")
abline(v=median(Yelp$num_reviews), col="blue")  
# This is the median of our data, and it does match the exact 50% mark

# However, the interesting bit: let's show the credible intervals around each
points(CI_lows[nrow(CI_lows),], col="yellow")
points(CI_highs[nrow(CI_lows),], col="green")

# What we really want is the range of values that have a 95% chance
#  of being the population median

# each column is the minimum number of reviews
# we need columns which have a minimum <= .5 and a maximum >= .5

couldbe_median <- which(CI_lows[nrow(CI_lows), ] <= .5 & CI_highs[nrow(CI_highs), ] >= .5); couldbe_median
# need to subtract one off of column index due to 1st column actually being "at least 0" reviews, not "at least 1"
lowest_estimate <- couldbe_median[1] - 1; lowest_estimate
highest_estimate <- couldbe_median[length(couldbe_median)] - 1; highest_estimate
# So, our confidence interval for the population median is 129 - 150

# Redrawing our plot:
plot(E_Percent[nrow(E_Percent), ], xlim=c(lowest_estimate-5, highest_estimate+5), ylim=c(.4, .6),
     xlab="Minimum Number of Reviews",
     ylab="Probability",
     main="Possible Medians for the Number of\nReviews a Restaurant Can Expect\nMetro Boston area",
     pch=18)
points(CI_lows[nrow(CI_lows),], col="yellow", pch=18)
points(CI_highs[nrow(CI_lows),], col="green", pch=18)
abline(v=c(lowest_estimate, highest_estimate), col="blue")
abline(h=.5)
# The graph is a little misleading - 150 really is the cutoff:
CI_highs[nrow(E_Percent), 150:152]


# Let's try redoing this with a bootstrap approach
#   using our whole dataset at once.

N <- 10^5
num_data <- length(Yelp$num_reviews); num_data
medians_boot <- numeric(N)
for(i in 1:N)
{
    sample_boot <- sample(Yelp$num_reviews, num_data, replace=TRUE); 
    medians_boot[i] <- median(sample_boot); median(sample_boot)
}
hist(medians_boot, breaks=35, probability=TRUE,
     xlab="Minimum Number of Reviews",
     ylab="Probability within Bootstrapped Samples",
     main="Distribution of Bootstrap Medians of\nthe Number of Reviews a Restaurant Can Expect\nMetro Boston area",
     col="blue")
# This distribution is kind of lumpy.
# Our 95% CI is:
CI_boot <- quantile(medians_boot, c(.025, .975)); CI_boot
abline(v=CI_boot, col="yellow", lwd=5)
# 128 - 150 almost exactly matches!  

# What are we able to do with frequentist approaches?
# As far as I know, nothing but estimate the mean, not the median.
# Given that our data was far from following a normal distribution,
# and that our samples were not random across the population each time, 
# but distinct by town, this is not going to lead to a robust answer
# from a frequentist approach.    BONUS POINT #3 


# Now that we know how much "buzz" a restaurant should expect,
#  what can a restaurant do to improve its visibility if it
#  has fewer reviews than it wants?



# TOPIC FOUR: EFFECT OF CLAIMING ONE'S BUSINESS


# Local businesses can choose to "claim" their business on Yelp, maintaining their Yelp
# profiles and hoping to increase their popularity. But we challenge this assumption:
# Does claiming your business on Yelp correlate with publicity?
# This is a logistic regression - BONUS POINT 4

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

# BONUS POINT 15 - Maximum likelihood estimation 
# MLE also gives us the parameters
library(stats4)
x<-Yelp$num_reviews;y<-Yelp$claimed
results<-mle(MLL,start = list(alpha = -0.1, beta = -0.02))
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "red", add=TRUE)
legend(1000,0.5, lty=c(1,1),c("Built-in glm function","MLE estimation"), lwd=c(2.5,2.5),col=c("blue","red")) 
# almost overlays the curve as we got from the glm function 


# BONUS POINT 8 + REQUIRED GRAPHIC 3: We can make bootstrap confidence intervals too
N <- 10^3
n <- length(claimed)  # number of observations
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
hist(beta.boot,,main="Bootstrap distribution of beta",freq=FALSE)  
abline(v=quantile(beta.boot, c(.025, .975)),col="green") #95% confidence interval for beta
abline(v=coef(fit)[2],col="blue") # the observation
quantile( beta.boot, c(.025, .975)) # the bootstrap confidence interval for beta 

hist(alpha.boot,freq=FALSE,main="Bootstrap distribution of alpha")  
abline(v=quantile( alpha.boot, c(.025, .975)),col="red") #95% confidence interval for alpha
abline(v=coef(fit)[1],col="blue") # the observation
quantile( alpha.boot, c(.025, .975)) # the bootstrap confidence interval for alpha



# Now that we have explored ratings and the number of reviews each restaurant may
#  hope to have on Yelp, let's explore the factors which lead to the number
#  of restaurants listed on Yelp in the first place.


# TOPIC FIVE:  How does the number of restaurants relate to the number of people?
# Presumably, the more people in an area, the more restaurants.
# This seems like a no-brainer!
hist(Yelp$population, breaks=100,
     xlab="Number of Inhabitants within ZipCode",
     ylab="Number of Restaurants",
     main="Number of Restaurants by Population\nMetro Boston area",
     col="blue")
# Actually, this is a lot more independent looking than I expected

# Create by zip code data frame of # restaurants, # of people
byZip <- data.frame(table(Yelp$zip))
add_column <- numeric(nrow(byZip))
byZip <- data.frame(byZip, add_column)
colnames(byZip) <- c("zip", "num_rest", "num_people")
for(i in 1:length(byZip$zip))
{
    # each zip code has the same population, whichever row that zip code appears in
    #  so just take the value in the first record returned by this which
    byZip$num_people[i] = Yelp$population[min(which(Yelp$zip == byZip$zip[i]))]
}

# make a scatter plot
plot(byZip$num_people, byZip$num_rest,
     xlab="Number of Inhabitants within ZipCode",
     ylab="Number of Restaurants",
     main="Number of Restaurants Per Zip Code\nMetro Boston area",
     col="blue", pch=18)

# check correlation of these two independent variables (for BONUS POINT #16)
our_corr <- cor(byZip$num_rest, byZip$num_people); our_corr
# .03 is shockingly low

# find a regression line
rest_by_people <- lm(byZip$num_rest ~ byZip$num_people, data=byZip); rest_by_people

# using homemade formulas (and outsourcing them to a utilities file to earn BONUS POINT #11 ):
slope <- get_slope(byZip$num_people, byZip$num_rest); slope
intercept <- get_intercept(byZip$num_people, byZip$num_rest); intercept

# this tells us that, for every additional person living within a zipcode,
#   we will have an extremely slight positive change in the number of restaurants for that zipcode.
abline(rest_by_people, col="red")

# let's just check that something nonlinear isn't going on:
lines(smooth.spline(byZip$num_rest ~ byZip$num_people), col="blue")
# well!  That's interesting, but I doubt it has much meaning.
# I think it's still just oscillating around an overall zero effect.

# Going back to linear regression:
# Zero will quite probably be within our confidence interval for our slope coefficient:

# To use the t distribution, let's first check if we have independent, normally distributed residuals
predictions <- intercept + slope * byZip$num_people
residuals <- byZip$num_rest - predictions
plot(byZip$num_people, residuals,
     xlab="Number of Inhabitants within ZipCode",
     ylab="Residual",
     main="Residual Values when Evaluating Number of Restaurants\nto Number of Inhabitants\nMetro Boston area",
     col="blue", pch=18)
abline(h=0, col="red")

# Hmm... it doesn't really look like it.  
hist(residuals, breaks="fd", prob=TRUE,
     xlab="Residual Value",
     ylab="Density",
     main="Histogram View of Residual Values",
     col="blue")

curve(dnorm(x, mean(resid(rest_by_people)), sd(resid(rest_by_people))), 
      col="red", add=TRUE,
      xlab="Residual Value",
      ylab="Density",
      main="Histogram View of Residual Values with Normal Distribution Overlay")
# No, definitely not normal.  Seems that smooth.spline had meaning after all!

# OK, so we can't use Student's t after all to create a confidence interval.
# Well, then, let's bootstrap.  (for BONUS POINT # 8)

N <- 10^4
cor.boot <- numeric(N)
slope.boot <- numeric(N)
n <- nrow(byZip)

for(i in 1:N)
{
    index <- sample(1:n, n, replace = TRUE)
    data.boot <- byZip[index, ]
    cor.boot[i] <- cor(data.boot$num_rest, data.boot$num_people)
    slope.boot[i] <- get_slope(data.boot$num_people, data.boot$num_rest)
}

# Find a 95% CI for the correlation between population level and number of restaurants
hist(cor.boot, 
     xlab="Bootstrapped Correlations",
     ylab="Frequency",
     main="Histogram View of Bootstrapped Correlations",
     col="blue")
abline(v=quantile(cor.boot, c(.025, .975)), col="blue")
abline(v=our_corr, col="red")


# Find a 95% CI for the "slope" between population level and number of restaurants
hist(slope.boot,
     xlab="Bootstrapped Slope Values",
     ylab="Frequency",
     main="Histogram View of Bootstrapped Regression Line Slopes",
     col="blue")
abline(v=quantile(slope.boot, c(.025, .975)), col="blue")
abline(v=slope, col="red")

# both of these confidence intervals clearly show that 0 is firmly within the confidence interval,
#  so that it seems there is really no evidence that there is a correlation between the number
#  of people within an area and the number of restaurants within that area.
# Weird!!  But hopefully this has been a convincing demonstration that a relationship that
#  might have been statistically significant actually wasn't, and so counts for
#  BONUS POINT #10. 

# For BONUS POINT # 2, let us reiterate that simulation methods allowed us to find an
#  answer when classical methods did not.  Classical methods are often limited to only
#  the simpler, normally distributed cases.  When data is nonlinear, classical methods
#  provide misleading results.  However, with simulation, we are able to work with
#  any kind of distribution, to see how likely or unlikely a certain result might be.
#  We can recreate "over time" through brute force, rather than relying on an 
#  assumption-ridden model.





# TOPIC SIX:  Correlation between age distribution in area and the number of restaurants listed on Yelp

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

# we can also bootstrap to create confidence interval.  (for BONUS POINT 8)
N <- 10^4
cor.boot <- numeric(N)
slope.boot <- numeric(N)
n <- nrow(byZip)

for(i in 1:N)
{
  index <- sample(1:n, n, replace = TRUE)
  data.boot <- byZip[index, ]
  cor.boot[i] <- cor(data.boot$num_rest, data.boot$age)
  slope.boot[i] <- get_slope(data.boot$age, data.boot$num_rest)
}

 
# Display a 95% Confidence Interval for the correlation
# between the population % aged 20-40  and number of restaurants
# REQUIRED GRAPHIC DISPLAY 3
hist(cor.boot,freq=FALSE,main="Bootstrap distribution of correlation coefficients")
abline(v=quantile(cor.boot, c(.025, .975)), col="blue")
abline(v=our_corr, col="red")
quantile(cor.boot, c(.025, .975))

# Find a 95% Confidence Interval for the slope of our model
hist(slope.boot,freq=FALSE,main="Bootstrap distribution of slope of our linear model")
abline(v=quantile(slope.boot, c(.025, .975)), col="blue")
abline(v=slope, col="red")
quantile(slope.boot, c(.025, .975))



# TOPIC SEVEN:  correlation between Asian population and # Chinese restaurants 

# I love Chinese cuisine but I struggled to locate a Chinese restaurant nearby,
# so a question arises naturally: Am I living not close enough to where my food lives?
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
# Where there are more Asian, there are more Chinese restaurants
# This fits with our intuition, and clearly I have not been living in Asian community

# But wait a minute, there is an outlier on the graph; 
# well, it's Chinatown when I double-check...
# what if we remove the outlier...
r<-byZip$num_rest[-which(byZip$num_rest>15)]
a<-byZip$asian[-which(byZip$num_rest>15)]
plot(r,a,main="Correlation: Asian population vs Chinese restaurants",
     ylab="Asian population % by zipcode",xlab="# of Chinese restuarants by zipcode") #

cor(r,a) # much weaker correlaton!
rest_by_asian<-lm(a~r)
# find a regression line
abline(rest_by_asian, col="blue")
legend(2.5,8, lty=c(1,1),c("linear regression line","non-linear line"), lwd=c(2.5,2.5),col=c("blue","red")) 
# let's just check that something nonlinear isn't going on:
lines(smooth.spline(a~r), col="red")
# Wow, it overlapps perfectly with our linear model!
summary(rest_by_asian) # but R^2 is only 13%! 
# Outside of Chinatown, the correlation is as not linear
# It makes it harder to find Chinese restaurants just based on the Asian population 
# Well, anyway, Yelp is there to help. 







# FinalP_Chang_LeHegaret_utilities.R
# developed by Jennifer Le Hegaret in support of 
#  the final project for MATH E-156,
#  Mathematical Foundations of Statistical Software
#  V1.0, 12/8/2014

# This file contains some home-grown versions of otherwise built-in functions
# as well as some consolidation of project-specific code needs
#       CLT(n)                              summarizes skewness and kurtosis for ratings with more than and less than n reviews
#       CLT.curve(g,n,col)                  adds a color normal curve to a given graph
#       CLT.hist(n)                         overlay a normal curve onto a histogram of ratings
#       CLTprob(n)                          summarizes CLT approximations and errors for probability of 3.5-4.5 ratings
#       get_intercept(x,y)                  hand-calculates the intercept for linear regression
#       get_slope(x,y)                      hand-calculates the intercept for linear regression
#       get_counts_atleast(ranges, data)    counts data that falls within certain buckets
#       get_percent_atleast(ranges, data)   figures out distribution of data within given ranges
#       MLL(alpha, beta)                    minus log of the likelihood function
#       setup_bayesian_matrix(rows, columns)create matrix with given row names and column names

CLT<-function(n) {
    index<-which(review<n)
    rating0<-rating[index]
    rating1<-rating[-index]
    clt<-c(skewness(rating0),skewness(rating1),kurtosis(rating0),kurtosis(rating1))
    clt
}

CLT.curve<-function(g,n,col){
    index<-which(review<n)
    rating0<-rating[index]
    hist.rating<-g+ stat_function(fun=dnorm, args=list(mean=mean(rating0), sd=sd(rating0)),
                                  color=col) 
    hist.rating
}

CLT.hist<-function(n){
    index<-which(review<n)
    rating0<-rating[index]
    hist.rating<-qplot(rating0, geom = "blank") + 
        geom_histogram(aes(y = ..density..),breaks=seq(2.75,5.25,0.5),
                       , alpha = 0.4,colour="black",fill="white") + 
        stat_function(fun=dnorm, args=list(mean=mean(rating0), sd=sd(rating0)),
                      color=sample(20,1)) + 
        ggtitle("Distribution of Restaurant Ratings on Yelp") + 
        xlab("Restaurant Ratings on Yelp") + 
        ylab("Density") +
        theme(plot.title = element_text(size=20, face="bold", vjust=2))
    hist.rating
}

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
    rownames(cltprob)<-c("more than n","less than n")
    colnames(cltprob)<-c("Exact probability","CLT approximation","Error in %")
    cltprob
}


get_intercept <- function(X, Y)
{
    mean(Y) - slope*mean(X)
}

get_slope <- function(X, Y)
{
    sum(( X - mean(X)) * ( Y - mean(Y)) / sum((X - mean(X))^2))
}

get_counts_atleast <- function(minimums, data)
{
    counts<-numeric(0)
    num_mins <- length(minimums)
    for(i in 1:num_mins)
    {
        counts <- c(counts, length(which(data >= minimums[i])))
    }
    counts
}

get_percent_atleast <- function(minimums, data)
{
    counts <- get_counts_atleast(minimums, data) 
    counts/length(data)
}

MLL<- function(alpha, beta) -sum( log( exp(alpha+beta*x)/(1+exp(alpha+beta*x)) )*y+ 
                                      log(1/(1+exp(alpha+beta*x)))*(1-y) )

setup_bayesian_matrix <- function(mrows, mcolumns)
{
    matrix(nrow=length(levels(mrows)) + 1, 
           ncol=length(mcolumns),
           dimnames=list(c("prior", levels(mrows)), paste("at least", mcolumns)))
}



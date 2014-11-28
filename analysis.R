# FINAL PROJECT -YELP ANALYTICS 
# analysis


# exploratory questions:
# 1) model the ratings 
# 2) relationship between reviews and ratings
# 3) predict the restaurant with the highest ratings or the most reviews based on the zip code/neighborhood/cuisine type?
# 4) predict whether restaurant is closed given its ratings/  
# 5) predict whether more educated neighborhoods tend to have a specific cuisine type (e.g. healthy, comfort food?)
# 6) predict whether wealthier neighborhoods tend to have a specific cuisine type (e.g.high-end restaurant?)
# 7) correlation between sex versus a cuisine categories (girls like desserts? guys like steakhouse?) 
# 8) correlation between age and # restaurants/# reviews
# 9) correlation between Chinese population and # Chinese restaurants and # reviews on Chinese restaurants 
# 10) other correlations between ratings/deals


# this file contains census data by zip codes
census<-read.csv("censusData.csv")

yelp<-read.csv("yelp.csv")
rating<-yelp[,1] 
review<-yelp[,2] 

lm(rating~review, data=yelp)
plot(rating~review)
abline(3.99,-0.000183)

cor(rating, review)
hist(rating)



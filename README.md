Where-Is-Good-Food-Yelp-Analytics
=================================
Final Project, Harvard MATH 156. Chang Liu and Jennifer Le Hégaret

You can view the presentation at my R pub profile: http://rpubs.com/jeffrey6557/52256

**Introduction**

Chang’s personal inspiration for doing a Yelp analytics project comes from his college years, where he was stuck in where there was little access to his favorite: Chinese food. Struggling to find out which restaurant to go to when there was finally a chance, he was obsessed with Yelp; knowing more statistics, he was inspired to dig into the business secrets of Yelp – reviews and rating – and find out more, hence the project. 

**Data**

From the Yelp API (http://www.yelp.com/developers/), we downloaded 999 restaurant data in the Boston Metro Area:
- average rating across all reviews
- number of reviews
- restaurant is "claimed" by the owner?
- location 
- type of food (Chinese, Vegan, Dive Bar, etc)

From the US Census (http://factfinder2.census.gov/faces/nav/jsf/pages/index.xhtml), we added this information by zip code: median family income, overall population, % aged 20-40 years old, % Asia population

**What questions are we most curious about?**

- When does a restaurant know it's got enough "buzz"? *
- Can the ratings be trusted? *
- How good is Boston food, anyway?
- Can it do anything about that by getting "claimed"?
- Do more people in an area lead to more restaurants?  Or just certain people?
- Is Chinatown the only place to find Chinese food?

**And the Answers (Surprises)!**


We innovatively use Bayesian techniques to estimate the median number of restaurant reviews in the Boston Metro area to be somewhere between 129 and 150.  Bootstrapping our sample of 1000 restaurants backs this up.

Is there any known distribution that could have generated the ratings? Using the kurtosis and skewness and plots of normal curves as measurements, we found out that we can model the ratings using a normal distribution due to the CLT effect! 

From the question above, we stumbled upon a discovery that in general, the ratings and the number of reviews for a restaurant have a weak but negative correlation. 

Surprisingly, linear regression shows us that overall, more people in an area is not correlated with a greater number of restaurants in the area.  However, a higher percentage of people aged 20-40 in the area is correlated with an increase in the number of restaurants in that area.

Chang loves Chinese cuisine but struggled to locate a Chinese restaurant nearby, so a question arises naturally: Was he living not close enough to where his food lives? The answer is Yes. And outside of Chinatown, it is harder to find where the Chinese restaurants are just based on the Asian population in an area. 


#Point Estimate of Population Mean
#Find a point estimate of mean university student height with the sample data from survey.
library(MASS)                  # load the MASS package 
head(survey)
height.survey = survey$Height
mean(height.survey, na.rm=TRUE)  # skip missing values 

#Interval Estimate of Population Mean with Known Variance
#Assume the population standard deviation σ of the student height in survey is 9.48. Find the margin of error and interval estimate at 95% confidence level.
height.response = na.omit(survey$Height)
n = length(height.response) 
sigma = 9.48                   # population standard deviation 
sem = sigma/sqrt(n); sem  
#Since there are two tails of the normal distribution, the 95% confidence level would imply the 97.5th percentile of the normal distribution at the upper tail. Therefore, zα∕2 is given by qnorm(.975). We multiply it with the standard error of the mean sem and get the margin of error.
E = qnorm(.975)∗sem; E 
xbar = mean(height.response)   # sample mean 
xbar + c(−E, E) 


#Alternative Solution
#Instead of using the textbook formula, we can apply the z.test function in the TeachingDemos package.

library(TeachingDemos)         # load TeachingDemos package 
z.test(height.response, sd=sigma) 


#Interval Estimate of Population Mean with Unknown Variance
#Without assuming the population standard deviation of the student height in survey, find the margin of error and interval estimate at 95% confidence level.
#We will use the t-distribution for that.

s = sd(height.response)        # sample standard deviation 
SE = s/sqrt(n); SE             # standard error estimate 
E = qt(.975, df=n−1)∗SE; E

xbar + c(−E, E) 

#Alternative Solution
#Instead of using the textbook formula, we can apply the t.test function in the built-in stats package.

t.test(height.response) 

#Sampling Size of Population Mean

#Assume the population standard deviation σ of the student height in survey is 9.48. Find the sample size needed to achieve a 1.2 centimeters margin of error at 95% confidence level.

zstar = qnorm(.975) 
sigma = 9.48 
E = 1.2 
zstar^2 ∗ sigma^2/ E^2 


#Point Estimate of Population Proportion

#Find a point estimate of the female student proportion from survey.

gender.response = na.omit(survey$Sex) 
n = length(gender.response)    # val

k = sum(gender.response == "Female") 
pbar = k/n; pbar 

#Interval Estimate of Population Proportion
#Compute the margin of error and estimate interval for the female students proportion in survey at 95% confidence level.

SE = sqrt(pbar∗(1−pbar)/n); SE   
E = qnorm(.975)∗SE; E              # margin of error 
pbar + c(−E, E) 

#Alternative Solution
prop.test(k, n) 


#Sampling Size of Population Proportion
#Using a 50% planned proportion estimate, find the sample size needed to achieve 5% margin of error for the female student survey at 95% confidence level.
zstar = qnorm(.975) 
p = 0.5 
E = 0.05 
zstar^2 ∗ p ∗ (1−p) / E^2 



#Numerical Measures

#Mean
mean(duration) 
#Median
median(duration)
#Quartile
quantile(duration) 
#Percentile
quantile(duration, c(.32, .57, .98)) 
#Range
max(duration) − min(duration) 
range(duration)

#Interquartile Range
#The interquartile range of an observation variable is the difference of its upper and lower quartiles. 
#It is a measure of how far apart the middle portion of data spreads in value.

IQR(duration)     

#Box Plot

boxplot(duration, horizontal=TRUE)
boxplot(duration)
boxplot(waiting)

#Variance
#The variance is a numerical measure of how the data values is dispersed around the mean.
var(duration) 
#StDev
sd(duration)
#Covariance
#The covariance of two variables x and y in a data set measures how the two are linearly related. 
#A positive covariance would indicate a positive linear relationship between the variables, and a negative covariance would indicate the opposite.
cov(duration, waiting) 

#correlation coefficient
#The correlation coefficient of two variables in a data set equals to their covariance divided by the product of their individual standard deviations. 
#It is a normalized measurement of how the two are linearly related.
#If the correlation coefficient is close to 1, it would indicate that the variables are positively linearly related and the scatter plot falls almost along a straight line with positive slope.
#For -1, it indicates that the variables are negatively linearly related and the scatter plot almost falls along a straight line with negative slope. 
#And for zero, it would indicate a weak linear relationship between the variables.

cor(duration, waiting) 

#Central Moment
#In particular, the second central moment of a population is its variance.
library(e1071)  
moment(duration, order=3, center=TRUE) 


#Skewness
#the skewness is a measure of symmetry. As a rule, negative skewness indicates that the mean of the data values is less than the median, and the data distribution is left-skewed. Positive skewness would indicate that the mean of the data values is larger than the median, and the data distribution is right-skewed.
skewness(duration)  

#Kurtosis
#the excess kurtosis describes the tail shape of the data distribution. The normal distribution has zero excess kurtosis and thus the standard tail shape. It is said to be mesokurtic. Negative excess kurtosis would indicate a thin-tailed data distribution, and is said to be platykurtic. Positive excess kurtosis would indicate a fat-tailed distribution, and is said to be leptokurtic.

kurtosis(duration) 

